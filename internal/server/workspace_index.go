package server

import (
	"net/url"
	"os"
	"path/filepath"
	"sort"
	"strings"
	"sync"
	"sync/atomic"
	"time"

	"starlims-lsp/internal/lexer"
	"starlims-lsp/internal/parser"
)

// sslFileExtensions are the file extensions to index.
var sslFileExtensions = []string{".srvscr", ".ssl", ".ssl.txt", ".ds", ".ds.txt"}

// maxSymbolResults caps the number of results returned by SearchSymbols.
const maxSymbolResults = 500

// IndexedProcedure is a lightweight procedure representation for the workspace index.
type IndexedProcedure struct {
	Name       string
	Parameters []string
	StartLine  int
	EndLine    int
	// Doc carries the parsed docblock for cross-file hover/completion.
	// Raw is zeroed at index time to avoid retaining full comment text.
	Doc parser.ProcedureDoc
	// IsPrivate marks /*@private; or /*@protected; annotated procedures —
	// unreachable via DoProc/ExecFunction, excluded from cross-file
	// completion (navigation still resolves them).
	IsPrivate bool
}

// FileSymbols stores the indexed symbols for a single file.
type FileSymbols struct {
	URI          string
	Procedures   []IndexedProcedure
	IsClass      bool
	IsDataSource bool
	ModTime      time.Time
	// Script identity (spec feature.cross_file_resolution/A1-A3):
	// ScriptName is the basename minus SSL extension; Category comes from
	// a canonical export-tree anchor ("" without one); HasLayoutAnchor
	// records whether an anchor was found.
	ScriptName      string
	Category        string
	HasLayoutAnchor bool
	// EntryParameters/EntryParamsLine describe the script's top-level
	// :PARAMETERS (the 2-part ExecFunction entry-point signature).
	// EntryParamsLine is 1-based; -1 when the script has none.
	EntryParameters []string
	EntryParamsLine int
}

// IndexResolution is one candidate returned by the resolver methods.
type IndexResolution struct {
	URI      string
	Line     int    // 0-based target line
	IsEntry  bool   // script entry point (vs a specific procedure)
	ProcName string // resolved procedure name when !IsEntry
	Anchored bool   // candidate came from a canonical-layout match
}

// maxResolutionCandidates caps ambiguous resolution result sets.
const maxResolutionCandidates = 10

// WorkspaceSymbolResult is returned by SearchSymbols.
type WorkspaceSymbolResult struct {
	Name      string
	Kind      int // 12 = Function, 6 = Method, 5 = Class
	URI       string
	StartLine int
	EndLine   int
}

// WorkspaceIndex is a thread-safe index of symbols across all workspace files.
type WorkspaceIndex struct {
	mu       sync.RWMutex
	files    map[string]*FileSymbols
	rootURIs []string
	indexing atomic.Bool
	stopCh   chan struct{}
	doneCh   chan struct{}

	// Secondary lookups, all keyed lowercase, maintained alongside files
	// under mu. Values are URI sets stored as sorted-on-read slices.
	byScriptName map[string][]string            // script name -> URIs
	byCategory   map[string]map[string][]string // category -> script -> URIs
	byProcName   map[string][]string            // procedure name -> URIs
}

// NewWorkspaceIndex creates a new workspace index for the given root URIs.
func NewWorkspaceIndex(rootURIs []string) *WorkspaceIndex {
	return &WorkspaceIndex{
		files:        make(map[string]*FileSymbols),
		rootURIs:     rootURIs,
		stopCh:       make(chan struct{}),
		doneCh:       make(chan struct{}),
		byScriptName: make(map[string][]string),
		byCategory:   make(map[string]map[string][]string),
		byProcName:   make(map[string][]string),
	}
}

// StartBackgroundIndex walks the workspace roots and indexes all SSL files.
func (wi *WorkspaceIndex) StartBackgroundIndex() {
	wi.indexing.Store(true)
	go func() {
		defer close(wi.doneCh)
		defer wi.indexing.Store(false)

		sem := make(chan struct{}, 4) // bounded concurrency
		var wg sync.WaitGroup

		for _, rootURI := range wi.rootURIs {
			rootPath := uriToPath(rootURI)
			if rootPath == "" {
				continue
			}

			filepath.WalkDir(rootPath, func(path string, d os.DirEntry, err error) error {
				if err != nil {
					return nil // skip errors, continue walking
				}

				// Check for cancellation
				select {
				case <-wi.stopCh:
					return filepath.SkipAll
				default:
				}

				if d.IsDir() {
					return nil
				}

				if !isSSLFile(path) {
					return nil
				}

				uri := pathToURI(path)
				wg.Add(1)
				sem <- struct{}{} // acquire
				go func() {
					defer wg.Done()
					defer func() { <-sem }() // release

					// Check for cancellation before parsing
					select {
					case <-wi.stopCh:
						return
					default:
					}

					wi.IndexFile(uri)
				}()

				return nil
			})
		}

		wg.Wait()
	}()
}

// IndexFile reads a file from disk and indexes its symbols.
func (wi *WorkspaceIndex) IndexFile(uri string) error {
	path := uriToPath(uri)
	if path == "" {
		return nil
	}

	data, err := os.ReadFile(path)
	if err != nil {
		return err
	}

	info, err := os.Stat(path)
	if err != nil {
		return err
	}

	content := string(data)
	lex := lexer.NewLexer(content)
	tokens := lex.Tokenize()
	p := parser.NewParser(tokens)
	ast := p.Parse()
	procs := p.ExtractProcedures(ast)

	indexed := make([]IndexedProcedure, len(procs))
	for i, proc := range procs {
		doc := proc.Doc
		doc.Raw = "" // full comment text is not needed at workspace scale
		indexed[i] = IndexedProcedure{
			Name:       proc.Name,
			Parameters: proc.Parameters,
			StartLine:  proc.StartLine,
			EndLine:    proc.EndLine,
			Doc:        doc,
			IsPrivate:  proc.IsPrivate,
		}
	}

	category, scriptName, anchored := deriveScriptIdentity(path)
	entryParams, entryLine := p.ExtractTopLevelParameters(ast)

	fs := &FileSymbols{
		URI:             uri,
		Procedures:      indexed,
		IsClass:         isClassFileFromTokens(tokens),
		IsDataSource:    isDataSourceURI(uri),
		ModTime:         info.ModTime(),
		ScriptName:      scriptName,
		Category:        category,
		HasLayoutAnchor: anchored,
		EntryParameters: entryParams,
		EntryParamsLine: entryLine,
	}

	wi.mu.Lock()
	if old, ok := wi.files[uri]; ok {
		wi.removeFromLookupsLocked(old)
	}
	wi.files[uri] = fs
	wi.addToLookupsLocked(fs)
	wi.mu.Unlock()

	return nil
}

// RemoveFile removes a file from the index.
func (wi *WorkspaceIndex) RemoveFile(uri string) {
	wi.mu.Lock()
	if old, ok := wi.files[uri]; ok {
		wi.removeFromLookupsLocked(old)
	}
	delete(wi.files, uri)
	wi.mu.Unlock()
}

func (wi *WorkspaceIndex) addToLookupsLocked(fs *FileSymbols) {
	script := strings.ToLower(fs.ScriptName)
	if script != "" {
		wi.byScriptName[script] = appendUnique(wi.byScriptName[script], fs.URI)
	}
	if fs.Category != "" {
		cat := strings.ToLower(fs.Category)
		if wi.byCategory[cat] == nil {
			wi.byCategory[cat] = make(map[string][]string)
		}
		wi.byCategory[cat][script] = appendUnique(wi.byCategory[cat][script], fs.URI)
	}
	for _, proc := range fs.Procedures {
		name := strings.ToLower(proc.Name)
		wi.byProcName[name] = appendUnique(wi.byProcName[name], fs.URI)
	}
}

func (wi *WorkspaceIndex) removeFromLookupsLocked(fs *FileSymbols) {
	script := strings.ToLower(fs.ScriptName)
	wi.byScriptName[script] = removeString(wi.byScriptName[script], fs.URI)
	if len(wi.byScriptName[script]) == 0 {
		delete(wi.byScriptName, script)
	}
	if fs.Category != "" {
		cat := strings.ToLower(fs.Category)
		if scripts := wi.byCategory[cat]; scripts != nil {
			scripts[script] = removeString(scripts[script], fs.URI)
			if len(scripts[script]) == 0 {
				delete(scripts, script)
			}
			if len(scripts) == 0 {
				delete(wi.byCategory, cat)
			}
		}
	}
	for _, proc := range fs.Procedures {
		name := strings.ToLower(proc.Name)
		wi.byProcName[name] = removeString(wi.byProcName[name], fs.URI)
		if len(wi.byProcName[name]) == 0 {
			delete(wi.byProcName, name)
		}
	}
}

func appendUnique(list []string, v string) []string {
	for _, s := range list {
		if s == v {
			return list
		}
	}
	return append(list, v)
}

func removeString(list []string, v string) []string {
	for i, s := range list {
		if s == v {
			return append(list[:i], list[i+1:]...)
		}
	}
	return list
}

// SearchSymbols searches the index for symbols matching the query.
// It skips URIs in the openURIs set (those are handled by the caller from open documents).
func (wi *WorkspaceIndex) SearchSymbols(query string, openURIs map[string]struct{}) []WorkspaceSymbolResult {
	wi.mu.RLock()
	defer wi.mu.RUnlock()

	queryLower := strings.ToLower(query)
	var results []WorkspaceSymbolResult

	for uri, fs := range wi.files {
		if _, open := openURIs[uri]; open {
			continue
		}

		for _, proc := range fs.Procedures {
			if query != "" && !strings.Contains(strings.ToLower(proc.Name), queryLower) {
				continue
			}

			kind := 12 // SymbolKindFunction
			if fs.IsClass {
				kind = 6 // SymbolKindMethod
			}

			results = append(results, WorkspaceSymbolResult{
				Name:      proc.Name,
				Kind:      kind,
				URI:       uri,
				StartLine: proc.StartLine,
				EndLine:   proc.EndLine,
			})

			if len(results) >= maxSymbolResults {
				return results
			}
		}
	}

	return results
}

// Stop signals the background indexer to stop and waits for it to finish.
func (wi *WorkspaceIndex) Stop() {
	close(wi.stopCh)
	<-wi.doneCh
}

// IsIndexing returns whether background indexing is in progress.
func (wi *WorkspaceIndex) IsIndexing() bool {
	return wi.indexing.Load()
}

// FileCount returns the number of indexed files.
func (wi *WorkspaceIndex) FileCount() int {
	wi.mu.RLock()
	defer wi.mu.RUnlock()
	return len(wi.files)
}

// isSSLFile checks if a path has a recognized SSL file extension.
func isSSLFile(path string) bool {
	lower := strings.ToLower(path)
	for _, ext := range sslFileExtensions {
		if strings.HasSuffix(lower, ext) {
			return true
		}
	}
	return false
}

// isClassFileFromTokens checks if tokens represent a class file.
func isClassFileFromTokens(tokens []lexer.Token) bool {
	for _, token := range tokens {
		if token.Type == lexer.TokenWhitespace || token.Type == lexer.TokenComment {
			continue
		}
		return token.Type == lexer.TokenKeyword && strings.EqualFold(token.Text, ":CLASS")
	}
	return false
}

// uriToPath converts a file:// URI to a filesystem path.
func uriToPath(uri string) string {
	if strings.HasPrefix(uri, "file://") {
		parsed, err := url.Parse(uri)
		if err != nil {
			return strings.TrimPrefix(uri, "file://")
		}
		return parsed.Path
	}
	// If it's already a path, return as-is
	return uri
}

// pathToURI converts a filesystem path to a file:// URI.
func pathToURI(path string) string {
	abs, err := filepath.Abs(path)
	if err != nil {
		abs = path
	}
	return "file://" + filepath.ToSlash(abs)
}

// --- Cross-file resolution (spec feature.cross_file_resolution) ---

// entryResolutionLocked builds a script-entry resolution for a file: the
// target line is the file-level :PARAMETERS line when present, else 0.
func entryResolutionLocked(fs *FileSymbols) IndexResolution {
	line := 0
	if fs.EntryParamsLine > 0 {
		line = fs.EntryParamsLine - 1
	}
	return IndexResolution{URI: fs.URI, Line: line, IsEntry: true, Anchored: fs.HasLayoutAnchor}
}

// procResolutionLocked builds a procedure resolution when the named
// procedure exists in the file; ok is false otherwise. Private procedures
// resolve too — navigation follows author intent; completion filters
// separately.
func procResolutionLocked(fs *FileSymbols, procName string) (IndexResolution, bool) {
	for _, proc := range fs.Procedures {
		if strings.EqualFold(proc.Name, procName) {
			return IndexResolution{
				URI:      fs.URI,
				Line:     proc.StartLine - 1,
				ProcName: proc.Name,
				Anchored: fs.HasLayoutAnchor,
			}, true
		}
	}
	return IndexResolution{}, false
}

// orderResolutions applies the normative candidate ordering: anchored
// canonical-layout matches first, then path-lexicographic by URI; the set
// is capped at maxResolutionCandidates.
func orderResolutions(res []IndexResolution) []IndexResolution {
	sort.SliceStable(res, func(i, j int) bool {
		if res[i].Anchored != res[j].Anchored {
			return res[i].Anchored
		}
		return res[i].URI < res[j].URI
	})
	if len(res) > maxResolutionCandidates {
		res = res[:maxResolutionCandidates]
	}
	return res
}

// ResolveDispatchTarget resolves a dotted DoProc/ExecFunction target.
// 1-part targets are same-script by language semantics and return nil.
//
// 2-part "A.B": category A + script B -> entry point; script-basename A
// with procedure B -> procedure (flat form); both rule sets are returned
// when both hit. 3+-part "...Cat.Script.Proc": category chain first, then
// script-basename degradation ignoring the category. Final fallback for
// either shape: a workspace-unique procedure name (last segment) when no
// other rule hit.
func (wi *WorkspaceIndex) ResolveDispatchTarget(target string) []IndexResolution {
	parts := strings.Split(target, ".")
	if len(parts) < 2 {
		return nil
	}
	for _, p := range parts {
		if strings.TrimSpace(p) == "" {
			return nil
		}
	}

	wi.mu.RLock()
	defer wi.mu.RUnlock()

	var results []IndexResolution

	if len(parts) == 2 {
		// Rule 1: Category.Script -> entry point.
		for _, uri := range wi.byCategory[strings.ToLower(parts[0])][strings.ToLower(parts[1])] {
			if fs := wi.files[uri]; fs != nil && !fs.IsDataSource {
				results = append(results, entryResolutionLocked(fs))
			}
		}
		// Rule 2: Script.Procedure (flat form).
		for _, uri := range wi.byScriptName[strings.ToLower(parts[0])] {
			if fs := wi.files[uri]; fs != nil && !fs.IsDataSource {
				if r, ok := procResolutionLocked(fs, parts[1]); ok {
					results = append(results, r)
				}
			}
		}
	} else {
		category := strings.ToLower(strings.Join(parts[:len(parts)-2], "."))
		script := strings.ToLower(parts[len(parts)-2])
		procName := parts[len(parts)-1]

		// Rule 1: Category.Script.Proc.
		for _, uri := range wi.byCategory[category][script] {
			if fs := wi.files[uri]; fs != nil && !fs.IsDataSource {
				if r, ok := procResolutionLocked(fs, procName); ok {
					results = append(results, r)
				}
			}
		}
		// Rule 2: degrade to script-basename match, ignoring the category.
		if len(results) == 0 {
			for _, uri := range wi.byScriptName[script] {
				if fs := wi.files[uri]; fs != nil && !fs.IsDataSource {
					if r, ok := procResolutionLocked(fs, procName); ok {
						results = append(results, r)
					}
				}
			}
		}
	}

	// Final fallback: workspace-unique procedure name (uniqueness gate
	// keeps flat-layout guessing from producing noise).
	if len(results) == 0 {
		procName := parts[len(parts)-1]
		if uris := wi.byProcName[strings.ToLower(procName)]; len(uris) == 1 {
			if fs := wi.files[uris[0]]; fs != nil && !fs.IsDataSource {
				if r, ok := procResolutionLocked(fs, procName); ok {
					results = append(results, r)
				}
			}
		}
	}

	return orderResolutions(results)
}

// ResolveIncludeTarget resolves an :INCLUDE target ("Name" or
// "Category.Script", already unquoted) to candidate files at line 0.
func (wi *WorkspaceIndex) ResolveIncludeTarget(target string) []IndexResolution {
	parts := strings.Split(target, ".")
	for _, p := range parts {
		if strings.TrimSpace(p) == "" {
			return nil
		}
	}

	wi.mu.RLock()
	defer wi.mu.RUnlock()

	var results []IndexResolution

	if len(parts) >= 2 {
		category := strings.ToLower(strings.Join(parts[:len(parts)-1], "."))
		script := strings.ToLower(parts[len(parts)-1])
		for _, uri := range wi.byCategory[category][script] {
			if fs := wi.files[uri]; fs != nil && !fs.IsDataSource {
				results = append(results, IndexResolution{URI: fs.URI, IsEntry: true, Anchored: fs.HasLayoutAnchor})
			}
		}
	}
	if len(results) == 0 {
		// Bare name, or dotted target degrading to a basename match.
		for _, uri := range wi.byScriptName[strings.ToLower(parts[len(parts)-1])] {
			if fs := wi.files[uri]; fs != nil && !fs.IsDataSource {
				results = append(results, IndexResolution{URI: fs.URI, IsEntry: true, Anchored: fs.HasLayoutAnchor})
			}
		}
	}

	return orderResolutions(results)
}

// ResolveDataSourceTarget resolves a RunDS target ("Category.Name" or bare
// "Name") to data-source files only. 1-part targets resolve by basename —
// unlike dispatch targets, a data source is always a separate file
// (spec feature.cross_file_resolution/A15-A17). Targets resolve to the
// file's entry (its file-level :PARAMETERS line when present).
func (wi *WorkspaceIndex) ResolveDataSourceTarget(target string) []IndexResolution {
	parts := strings.Split(target, ".")
	for _, p := range parts {
		if strings.TrimSpace(p) == "" {
			return nil
		}
	}

	wi.mu.RLock()
	defer wi.mu.RUnlock()

	var results []IndexResolution

	if len(parts) >= 2 {
		category := strings.ToLower(strings.Join(parts[:len(parts)-1], "."))
		name := strings.ToLower(parts[len(parts)-1])
		for _, uri := range wi.byCategory[category][name] {
			if fs := wi.files[uri]; fs != nil && fs.IsDataSource {
				results = append(results, entryResolutionLocked(fs))
			}
		}
	}
	if len(results) == 0 {
		// Bare name, or dotted target degrading to a basename match.
		for _, uri := range wi.byScriptName[strings.ToLower(parts[len(parts)-1])] {
			if fs := wi.files[uri]; fs != nil && fs.IsDataSource {
				results = append(results, entryResolutionLocked(fs))
			}
		}
	}

	return orderResolutions(results)
}

// CategoryNames returns all known category names (original casing of the
// first file indexed per category), sorted.
func (wi *WorkspaceIndex) CategoryNames() []string {
	wi.mu.RLock()
	defer wi.mu.RUnlock()

	seen := make(map[string]string, len(wi.byCategory))
	for _, fs := range wi.files {
		if fs.Category != "" {
			key := strings.ToLower(fs.Category)
			if _, ok := seen[key]; !ok {
				seen[key] = fs.Category
			}
		}
	}
	names := make([]string, 0, len(seen))
	for _, display := range seen {
		names = append(names, display)
	}
	sort.Strings(names)
	return names
}

// ScriptsInCategory returns the FileSymbols of every script in a category.
func (wi *WorkspaceIndex) ScriptsInCategory(category string) []*FileSymbols {
	wi.mu.RLock()
	defer wi.mu.RUnlock()

	var out []*FileSymbols
	for _, uris := range wi.byCategory[strings.ToLower(category)] {
		for _, uri := range uris {
			if fs := wi.files[uri]; fs != nil {
				out = append(out, fs)
			}
		}
	}
	sort.Slice(out, func(i, j int) bool { return out[i].URI < out[j].URI })
	return out
}

// ScriptsNamed returns the FileSymbols of every file whose script name
// matches, case-insensitively.
func (wi *WorkspaceIndex) ScriptsNamed(name string) []*FileSymbols {
	wi.mu.RLock()
	defer wi.mu.RUnlock()

	var out []*FileSymbols
	for _, uri := range wi.byScriptName[strings.ToLower(name)] {
		if fs := wi.files[uri]; fs != nil {
			out = append(out, fs)
		}
	}
	sort.Slice(out, func(i, j int) bool { return out[i].URI < out[j].URI })
	return out
}

// FileSymbolsFor returns the indexed symbols for a URI. The returned value
// is replaced wholesale on re-index, never mutated in place, so it is safe
// to read after the lock is released.
func (wi *WorkspaceIndex) FileSymbolsFor(uri string) (*FileSymbols, bool) {
	wi.mu.RLock()
	defer wi.mu.RUnlock()
	fs, ok := wi.files[uri]
	return fs, ok
}
