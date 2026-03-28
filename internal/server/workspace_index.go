package server

import (
	"net/url"
	"os"
	"path/filepath"
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
}

// FileSymbols stores the indexed symbols for a single file.
type FileSymbols struct {
	URI        string
	Procedures []IndexedProcedure
	IsClass    bool
	ModTime    time.Time
}

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
}

// NewWorkspaceIndex creates a new workspace index for the given root URIs.
func NewWorkspaceIndex(rootURIs []string) *WorkspaceIndex {
	return &WorkspaceIndex{
		files:    make(map[string]*FileSymbols),
		rootURIs: rootURIs,
		stopCh:   make(chan struct{}),
		doneCh:   make(chan struct{}),
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
		indexed[i] = IndexedProcedure{
			Name:       proc.Name,
			Parameters: proc.Parameters,
			StartLine:  proc.StartLine,
			EndLine:    proc.EndLine,
		}
	}

	fs := &FileSymbols{
		URI:        uri,
		Procedures: indexed,
		IsClass:    isClassFileFromTokens(tokens),
		ModTime:    info.ModTime(),
	}

	wi.mu.Lock()
	wi.files[uri] = fs
	wi.mu.Unlock()

	return nil
}

// RemoveFile removes a file from the index.
func (wi *WorkspaceIndex) RemoveFile(uri string) {
	wi.mu.Lock()
	delete(wi.files, uri)
	wi.mu.Unlock()
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
