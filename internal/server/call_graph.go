package server

// The ssl/callGraph request: scripts, procedures and data sources joined
// by their DoProc, ExecFunction, RunDS and :INCLUDE calls, resolved the way
// go to definition resolves them. Normative behavior:
// catalog/features/call_graph.md.

import (
	"slices"
	"sort"
	"strings"

	"starlims-lsp/internal/providers"

	protocol "github.com/tliron/glsp/protocol_3_16"
)

// MethodCallGraph is the custom request for the workspace call graph.
const MethodCallGraph = "ssl/callGraph"

// Call graph node kinds.
const (
	GraphScript     = "script"
	GraphProcedure  = "procedure"
	GraphDataSource = "dataSource"
	GraphUnresolved = "unresolved"
)

// Call graph edge kinds.
const (
	EdgeDoProc       = "DoProc"
	EdgeExecFunction = "ExecFunction"
	EdgeRunDS        = "RunDS"
	EdgeInclude      = "Include"
)

// CallGraphParams are the parameters of an ssl/callGraph request. With no
// textDocument the result is the whole workspace graph; with one, it is
// the part reachable from that document, or from its procedure when
// procedure names one, within depth calls in the given direction.
type CallGraphParams struct {
	TextDocument *TextDocumentIdentifier `json:"textDocument,omitempty"`
	Procedure    string                  `json:"procedure,omitempty"`
	// Depth bounds the calls followed from the scope; omitted, there is
	// no bound. 0 returns the scope's own nodes and the edges among them.
	Depth *int `json:"depth,omitempty"`
	// Direction is "both" (the default), "outgoing" (what the scope
	// calls) or "incoming" (what calls the scope).
	Direction string `json:"direction,omitempty"`
}

// CallGraphResult is the graph. Indexing is true while the workspace index
// is still being built, so targets in files not yet read show unresolved.
type CallGraphResult struct {
	Nodes    []CallGraphNode `json:"nodes"`
	Edges    []CallGraphEdge `json:"edges"`
	Indexing bool            `json:"indexing"`
}

// CallGraphNode is a script, procedure, data source, or a call target
// that resolved to nothing.
type CallGraphNode struct {
	ID   string `json:"id"`
	Kind string `json:"kind"`
	// Name is the procedure's name, the script's Category.Script (its
	// file name without a canonical layout), or an unresolved target as
	// written.
	Name string `json:"name"`
	// Container is the id of a procedure's script.
	Container string `json:"container,omitempty"`
	// Location is the file for a script or data source, and the lines
	// from :PROCEDURE through :ENDPROC for a procedure. Unresolved nodes
	// have none.
	Location *protocol.Location `json:"location,omitempty"`
	// Regions names the comment regions around a procedure, outermost
	// first.
	Regions []string `json:"regions,omitempty"`
	IsClass bool     `json:"isClass,omitempty"`
}

// CallGraphEdge is one call site.
type CallGraphEdge struct {
	From string `json:"from"`
	To   string `json:"to"`
	Kind string `json:"kind"`
	// Target is the call's target string as written.
	Target string `json:"target"`
	// Location is the target string at the call site.
	Location protocol.Location `json:"location"`
	Resolved bool              `json:"resolved"`
	// Ambiguous is set on each edge of a call that resolved to more than
	// one candidate; there is one edge per candidate.
	Ambiguous bool `json:"ambiguous,omitempty"`
}

// graphFile is what the graph needs from one file: from the open buffer
// when the client holds the file open, from the workspace index otherwise.
type graphFile struct {
	uri          string
	name         string
	isClass      bool
	isDataSource bool
	procedures   []graphProcedure
	regions      []providers.RegionInfo
	sites        []providers.CallSite
	mapper       *utf16Mapper
}

type graphProcedure struct {
	name               string
	startLine, endLine int // 1-based
}

// handleCallGraph answers ssl/callGraph.
func (s *SSLServer) handleCallGraph(params *CallGraphParams) (*CallGraphResult, error) {
	g := &callGraph{s: s, byID: map[string]int{}}
	files := s.graphFiles()
	for _, f := range files {
		g.addFile(f)
	}
	for _, f := range files {
		g.addCalls(f)
	}

	result := &CallGraphResult{
		Nodes:    g.nodes,
		Edges:    g.edges,
		Indexing: s.workspaceIndex != nil && s.workspaceIndex.IsIndexing(),
	}
	if params.TextDocument != nil {
		uri := canonicalURI(params.TextDocument.URI)
		var seeds []string
		if params.Procedure != "" {
			seeds = []string{procedureID(uri, params.Procedure)}
		} else {
			seeds = append(seeds, uri)
			for _, n := range g.nodes {
				if n.Container == uri {
					seeds = append(seeds, n.ID)
				}
			}
		}
		result.Nodes, result.Edges = g.reachable(seeds, params.Depth, params.Direction)
	}
	if result.Nodes == nil {
		result.Nodes = []CallGraphNode{}
	}
	if result.Edges == nil {
		result.Edges = []CallGraphEdge{}
	}
	return result, nil
}

// graphFiles returns every indexed or open file, sorted by URI.
func (s *SSLServer) graphFiles() []graphFile {
	var files []graphFile
	open := map[string]bool{}
	for _, uri := range s.documents.URIs() {
		open[uri] = true
		files = append(files, s.openGraphFile(uri))
	}
	if s.workspaceIndex != nil {
		for _, fs := range s.workspaceIndex.Files() {
			if !open[fs.URI] {
				files = append(files, s.indexedGraphFile(fs))
			}
		}
	}
	sort.Slice(files, func(i, j int) bool { return files[i].uri < files[j].uri })
	return files
}

func (s *SSLServer) openGraphFile(uri string) graphFile {
	content, _ := s.documents.GetDocument(uri)
	cache := s.documents.ParseDocument(uri, s.documentVersion[uri])
	category, script, _ := deriveScriptIdentity(uriToPath(uri))
	f := graphFile{
		uri:          uri,
		name:         scriptDisplayName(&FileSymbols{Category: category, ScriptName: script}),
		isClass:      isClassFileFromTokens(cache.Tokens),
		isDataSource: isDataSourceURI(uri),
		regions:      providers.ExtractRegions(cache.Tokens),
		sites:        providers.ExtractCallSites(cache.Tokens),
		mapper:       newUTF16Mapper(content),
	}
	for _, p := range cache.Procedures {
		f.procedures = append(f.procedures, graphProcedure{name: p.Name, startLine: p.StartLine, endLine: p.EndLine})
	}
	return f
}

func (s *SSLServer) indexedGraphFile(fs *FileSymbols) graphFile {
	f := graphFile{
		uri:          fs.URI,
		name:         scriptDisplayName(fs),
		isClass:      fs.IsClass,
		isDataSource: fs.IsDataSource,
		regions:      fs.Regions,
	}
	if fs.HasAstral {
		f.mapper = newUTF16Mapper(s.documentText(fs.URI))
	}
	for _, p := range fs.Procedures {
		f.procedures = append(f.procedures, graphProcedure{name: p.Name, startLine: p.StartLine, endLine: p.EndLine})
	}
	for _, site := range fs.CallSites {
		f.sites = append(f.sites, providers.CallSite{
			Kind:     site.Kind,
			Raw:      site.Raw,
			IsDoProc: site.IsDoProc,
			Range: providers.Range{
				Start: providers.Position{Line: site.Line, Character: site.StartChar},
				End:   providers.Position{Line: site.Line, Character: site.EndChar},
			},
		})
	}
	return f
}

// procedureID is the node id of the procedure name in the file uri.
func procedureID(uri, name string) string {
	return uri + "#" + strings.ToLower(name)
}

type callGraph struct {
	s     *SSLServer
	nodes []CallGraphNode
	edges []CallGraphEdge
	byID  map[string]int // node id -> index in nodes
}

func (g *callGraph) add(n CallGraphNode) {
	if _, ok := g.byID[n.ID]; ok {
		return
	}
	g.byID[n.ID] = len(g.nodes)
	g.nodes = append(g.nodes, n)
}

// addFile adds the file's node and one node per procedure.
func (g *callGraph) addFile(f graphFile) {
	kind := GraphScript
	if f.isDataSource {
		kind = GraphDataSource
	}
	g.add(CallGraphNode{
		ID:       f.uri,
		Kind:     kind,
		Name:     f.name,
		Location: &protocol.Location{URI: f.uri},
		IsClass:  f.isClass,
	})
	for _, p := range f.procedures {
		g.add(CallGraphNode{
			ID:        procedureID(f.uri, p.name),
			Kind:      GraphProcedure,
			Name:      p.name,
			Container: f.uri,
			Location:  procedureLocation(f.uri, p.startLine, p.endLine),
			Regions:   regionPath(f.regions, p.startLine),
		})
	}
}

// procedureLocation covers the lines from :PROCEDURE through :ENDPROC
// (1-based), ending at the start of the line after.
func procedureLocation(uri string, startLine, endLine int) *protocol.Location {
	return &protocol.Location{URI: uri, Range: protocol.Range{
		Start: protocol.Position{Line: protocol.UInteger(startLine - 1)},
		End:   protocol.Position{Line: protocol.UInteger(endLine)},
	}}
}

// regionPath names the regions that hold the 1-based line, outermost
// first.
func regionPath(regions []providers.RegionInfo, line int) []string {
	var around []providers.RegionInfo
	for _, r := range regions {
		if r.StartLine <= line && line <= r.EndLine {
			around = append(around, r)
		}
	}
	slices.SortStableFunc(around, func(a, b providers.RegionInfo) int {
		if a.StartLine != b.StartLine {
			return a.StartLine - b.StartLine
		}
		return b.EndLine - a.EndLine
	})
	var names []string
	for _, r := range around {
		names = append(names, r.Name)
	}
	return names
}

// addCalls adds one edge per call site of f and resolution candidate.
func (g *callGraph) addCalls(f graphFile) {
	for _, site := range f.sites {
		from := f.uri
		for _, p := range f.procedures {
			if line := site.Range.Start.Line + 1; p.startLine <= line && line <= p.endLine {
				from = procedureID(f.uri, p.name)
				break
			}
		}
		edge := CallGraphEdge{
			From:     from,
			Kind:     edgeKind(site),
			Target:   site.Raw,
			Location: protocol.Location{URI: f.uri, Range: f.mapper.rng(site.Range)},
		}
		targets := g.resolve(f, site)
		if len(targets) == 0 {
			edge.To = g.unresolved(f, site)
			g.edges = append(g.edges, edge)
			continue
		}
		for _, to := range targets {
			e := edge
			e.To, e.Resolved, e.Ambiguous = to, true, len(targets) > 1
			g.edges = append(g.edges, e)
		}
	}
}

func edgeKind(site providers.CallSite) string {
	switch {
	case site.Kind == providers.CallDataSource:
		return EdgeRunDS
	case site.Kind == providers.CallInclude:
		return EdgeInclude
	case site.IsDoProc:
		return EdgeDoProc
	default:
		return EdgeExecFunction
	}
}

// resolve returns the node ids a call site resolves to: a 1-part dispatch
// target is a procedure of the same file, anything else goes through the
// workspace index (feature.cross_file_resolution).
func (g *callGraph) resolve(f graphFile, site providers.CallSite) []string {
	if site.Kind == providers.CallDispatch && !strings.Contains(site.Raw, ".") {
		for _, p := range f.procedures {
			if strings.EqualFold(p.name, site.Raw) {
				return []string{procedureID(f.uri, p.name)}
			}
		}
		return nil
	}
	wi := g.s.workspaceIndex
	if wi == nil {
		return nil
	}
	var resolutions []IndexResolution
	switch site.Kind {
	case providers.CallDispatch:
		resolutions = wi.ResolveDispatchTarget(site.Raw)
	case providers.CallDataSource:
		resolutions = wi.ResolveDataSourceTarget(site.Raw)
	case providers.CallInclude:
		resolutions = wi.ResolveIncludeTarget(site.Raw)
	}
	var ids []string
	for _, r := range resolutions {
		id := r.URI
		if !r.IsEntry {
			id = procedureID(r.URI, r.ProcName)
			// The index read the file from disk; an open buffer may no
			// longer hold the procedure. Keep the node the edge points at.
			g.add(CallGraphNode{
				ID:        id,
				Kind:      GraphProcedure,
				Name:      r.ProcName,
				Container: r.URI,
				Location:  procedureLocation(r.URI, r.Line+1, r.Line+1),
			})
		}
		if !slices.Contains(ids, id) {
			ids = append(ids, id)
		}
	}
	return ids
}

// unresolved returns the id of the node for a target that resolved to
// nothing, adding it on first use. A missing same-file procedure is
// missing from that file only; any other target is one node however many
// files call it.
func (g *callGraph) unresolved(f graphFile, site providers.CallSite) string {
	var id string
	switch {
	case site.Kind == providers.CallDispatch && !strings.Contains(site.Raw, "."):
		id = "unresolved:" + procedureID(f.uri, site.Raw)
	case site.Kind == providers.CallDispatch:
		id = "unresolved:dispatch:" + strings.ToLower(site.Raw)
	case site.Kind == providers.CallDataSource:
		id = "unresolved:dataSource:" + strings.ToLower(site.Raw)
	default:
		id = "unresolved:include:" + strings.ToLower(site.Raw)
	}
	g.add(CallGraphNode{ID: id, Kind: GraphUnresolved, Name: site.Raw})
	return id
}

// reachable returns the nodes reached from seeds within depth edges in
// direction (nil depth: no bound), plus the script of each procedure
// reached, and the edges between reached nodes.
func (g *callGraph) reachable(seeds []string, depth *int, direction string) ([]CallGraphNode, []CallGraphEdge) {
	outgoing := direction != "incoming"
	incoming := direction != "outgoing"
	next := map[string][]string{}
	for _, e := range g.edges {
		if outgoing {
			next[e.From] = append(next[e.From], e.To)
		}
		if incoming {
			next[e.To] = append(next[e.To], e.From)
		}
	}

	reached := map[string]bool{}
	var frontier []string
	for _, id := range seeds {
		if _, ok := g.byID[id]; ok && !reached[id] {
			reached[id] = true
			frontier = append(frontier, id)
		}
	}
	for hop := 0; len(frontier) > 0 && (depth == nil || hop < *depth); hop++ {
		var later []string
		for _, id := range frontier {
			for _, to := range next[id] {
				if !reached[to] {
					reached[to] = true
					later = append(later, to)
				}
			}
		}
		frontier = later
	}

	var edges []CallGraphEdge
	for _, e := range g.edges {
		if reached[e.From] && reached[e.To] {
			edges = append(edges, e)
		}
	}
	var containers []string
	for id := range reached {
		if c := g.nodes[g.byID[id]].Container; c != "" {
			containers = append(containers, c)
		}
	}
	for _, c := range containers {
		reached[c] = true
	}
	var nodes []CallGraphNode
	for _, n := range g.nodes {
		if reached[n.ID] {
			nodes = append(nodes, n)
		}
	}
	return nodes, edges
}
