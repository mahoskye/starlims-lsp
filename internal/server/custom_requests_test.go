package server

import (
	"encoding/json"
	"testing"

	"github.com/tliron/glsp"
	protocol "github.com/tliron/glsp/protocol_3_16"
)

// callCustom sends a custom request through the wrapper handler, the way
// a client's JSON reaches it, and decodes the result into out.
func callCustom(t *testing.T, s *SSLServer, method string, params any, out any) {
	t.Helper()
	inner := &s.handler
	inner.SetInitialized(true)
	raw, err := json.Marshal(params)
	if err != nil {
		t.Fatal(err)
	}
	r, validMethod, validParams, err := NewWrapperHandler(inner, s).Handle(&glsp.Context{Method: method, Params: raw})
	if err != nil || !validMethod || !validParams {
		t.Fatalf("%s: (%v, %v, %v)", method, validMethod, validParams, err)
	}
	encoded, err := json.Marshal(r)
	if err != nil {
		t.Fatal(err)
	}
	if err := json.Unmarshal(encoded, out); err != nil {
		t.Fatalf("%s: decoding %s: %v", method, encoded, err)
	}
}

func TestInitialize_AdvertisesCustomRequests(t *testing.T) {
	// [spec feature.syntax_tree/A11] [spec feature.call_graph/A7]
	s := NewSSLServer()
	res, err := s.handleInitialize(nil, &protocol.InitializeParams{})
	if err != nil {
		t.Fatal(err)
	}
	raw, _ := json.Marshal(res)
	var decoded struct {
		Capabilities struct {
			Experimental struct {
				SSL map[string]int `json:"ssl"`
			} `json:"experimental"`
		} `json:"capabilities"`
	}
	if err := json.Unmarshal(raw, &decoded); err != nil {
		t.Fatal(err)
	}
	got := decoded.Capabilities.Experimental.SSL
	if got["syntaxTree"] != 1 || got["callGraph"] != 1 {
		t.Errorf("experimental.ssl = %v, want syntaxTree and callGraph at version 1", got)
	}
}

func TestSyntaxTreeRequest(t *testing.T) {
	// [spec feature.syntax_tree/A10]
	s := newTestServerWithDocument(":PROCEDURE Greet;\n\tsMsg := \"😀\"; /* hi;\n:ENDPROC;\n")
	s.documentVersion[testURI] = 7

	var tree SyntaxTreeResult
	callCustom(t, s, MethodSyntaxTree, SyntaxTreeParams{TextDocument: TextDocumentIdentifier{URI: testURI}}, &tree)
	if tree.URI != testURI || tree.Version != 7 || len(tree.Nodes) != 1 {
		t.Fatalf("got uri %q version %d, %d nodes; want %q, 7, 1", tree.URI, tree.Version, len(tree.Nodes), testURI)
	}
	proc := tree.Nodes[0]
	if proc.Kind != "procedure" || proc.Name != "Greet" || proc.HeaderRange == nil || proc.CloserRange == nil {
		t.Fatalf("procedure node = %+v", proc)
	}
	if len(proc.Children) != 2 {
		t.Fatalf("procedure children = %+v, want the statement and the comment", proc.Children)
	}
	// The emoji is one rune but two UTF-16 units: the comment after it
	// starts at UTF-16 offset 15, not at its rune column 14.
	if c := proc.Children[1]; c.Kind != "comment" || c.Range.Start.Character != 15 {
		t.Errorf("comment = %+v, want it at UTF-16 character 15", c)
	}

	var missing *SyntaxTreeResult
	callCustom(t, s, MethodSyntaxTree, SyntaxTreeParams{TextDocument: TextDocumentIdentifier{URI: "file:///not/open.ssl"}}, &missing)
	if missing != nil {
		t.Errorf("unopened document: got %+v, want null", missing)
	}
}

const ordersScript = `:INCLUDE LIMS.Helpers;
/* region Entry;
:PROCEDURE Main;
	DoProc("Total");
	ExecFunction("LIMS.Helpers.Calc", {1});
	aRows := RunDS("LIMS.GetOrders");
	ExecFunction("LIMS.Missing");
	DoProc("Gone");
:ENDPROC;
/* endregion;
:PROCEDURE Total;
:ENDPROC;
`

const helpersScript = `:PROCEDURE Calc;
	:PARAMETERS n;
	:RETURN n;
:ENDPROC;
`

// newCallGraphServer indexes a small workspace: Orders calls into itself,
// into Helpers, into a data source, and at two targets that do not exist.
func newCallGraphServer(t *testing.T) (s *SSLServer, orders, helpers, ds string) {
	t.Helper()
	wi, dir := newResolverIndex(t)
	orders = writeAndIndex(t, wi, dir, "Server Scripts/LIMS/Orders.srvscr", ordersScript)
	helpers = writeAndIndex(t, wi, dir, "Server Scripts/LIMS/Helpers.srvscr", helpersScript)
	ds = writeAndIndex(t, wi, dir, "Data Sources/LIMS/GetOrders.ds", "SELECT * FROM orders\n")
	s = NewSSLServer()
	s.workspaceIndex = wi
	return s, orders, helpers, ds
}

func edgeTo(edges []CallGraphEdge, target string) *CallGraphEdge {
	for i := range edges {
		if edges[i].Target == target {
			return &edges[i]
		}
	}
	return nil
}

func nodeByID(nodes []CallGraphNode, id string) *CallGraphNode {
	for i := range nodes {
		if nodes[i].ID == id {
			return &nodes[i]
		}
	}
	return nil
}

func TestCallGraphNodes(t *testing.T) {
	// [spec feature.call_graph/A1]
	s, orders, helpers, ds := newCallGraphServer(t)
	var g CallGraphResult
	callCustom(t, s, MethodCallGraph, CallGraphParams{}, &g)

	for id, want := range map[string]string{
		orders:                             "script",
		helpers:                            "script",
		ds:                                 "dataSource",
		procedureID(orders, "Main"):        "procedure",
		procedureID(orders, "Total"):       "procedure",
		procedureID(helpers, "Calc"):       "procedure",
		"unresolved:dispatch:lims.missing": "unresolved",
	} {
		n := nodeByID(g.Nodes, id)
		if n == nil || n.Kind != want {
			t.Errorf("node %s = %+v, want kind %s", id, n, want)
		}
	}
	if n := nodeByID(g.Nodes, orders); n.Name != "LIMS.Orders" {
		t.Errorf("script name = %q, want LIMS.Orders", n.Name)
	}
	main := nodeByID(g.Nodes, procedureID(orders, "Main"))
	if main.Container != orders || len(main.Regions) != 1 || main.Regions[0] != "Entry" {
		t.Errorf("Main = %+v, want container %s and region Entry", main, orders)
	}
	if loc := main.Location; loc == nil || loc.URI != orders || loc.Range.Start.Line != 2 || loc.Range.End.Line != 9 {
		t.Errorf("Main location = %+v, want lines 2 through 8", main.Location)
	}
	if total := nodeByID(g.Nodes, procedureID(orders, "Total")); len(total.Regions) != 0 {
		t.Errorf("Total regions = %v, want none", total.Regions)
	}
}

func TestCallGraphEdges(t *testing.T) {
	// [spec feature.call_graph/A2] [spec feature.call_graph/A3]
	s, orders, helpers, ds := newCallGraphServer(t)
	var g CallGraphResult
	callCustom(t, s, MethodCallGraph, CallGraphParams{}, &g)

	main := procedureID(orders, "Main")
	for _, want := range []CallGraphEdge{
		{From: orders, To: helpers, Kind: "Include", Target: "LIMS.Helpers", Resolved: true},
		{From: main, To: procedureID(orders, "Total"), Kind: "DoProc", Target: "Total", Resolved: true},
		{From: main, To: procedureID(helpers, "Calc"), Kind: "ExecFunction", Target: "LIMS.Helpers.Calc", Resolved: true},
		{From: main, To: ds, Kind: "RunDS", Target: "LIMS.GetOrders", Resolved: true},
		{From: main, To: "unresolved:dispatch:lims.missing", Kind: "ExecFunction", Target: "LIMS.Missing"},
		{From: main, To: "unresolved:" + procedureID(orders, "Gone"), Kind: "DoProc", Target: "Gone"},
	} {
		e := edgeTo(g.Edges, want.Target)
		if e == nil {
			t.Errorf("no edge for %s", want.Target)
			continue
		}
		got := *e
		got.Location = protocol.Location{}
		if got != want {
			t.Errorf("edge %s = %+v, want %+v", want.Target, got, want)
		}
	}
	// The call site is the target string, quotes excluded.
	e := edgeTo(g.Edges, "Total")
	if e.Location.URI != orders || e.Location.Range != (protocol.Range{
		Start: protocol.Position{Line: 3, Character: 9},
		End:   protocol.Position{Line: 3, Character: 14},
	}) {
		t.Errorf("DoProc(\"Total\") location = %+v", e.Location)
	}
}

func TestCallGraphScope(t *testing.T) {
	// [spec feature.call_graph/A4] [spec feature.call_graph/A5]
	s, orders, helpers, ds := newCallGraphServer(t)
	zero, one := 0, 1

	var g CallGraphResult
	callCustom(t, s, MethodCallGraph, CallGraphParams{
		TextDocument: &TextDocumentIdentifier{URI: helpers},
		Procedure:    "calc",
		Depth:        &one,
		Direction:    "incoming",
	}, &g)
	if len(g.Edges) != 1 || g.Edges[0].Target != "LIMS.Helpers.Calc" {
		t.Errorf("callers of Calc: edges %+v, want only Main's call", g.Edges)
	}
	// Main came with its script; nothing Main calls did.
	if nodeByID(g.Nodes, procedureID(orders, "Main")) == nil || nodeByID(g.Nodes, orders) == nil || nodeByID(g.Nodes, ds) != nil {
		t.Errorf("callers of Calc: nodes %+v", g.Nodes)
	}

	callCustom(t, s, MethodCallGraph, CallGraphParams{
		TextDocument: &TextDocumentIdentifier{URI: orders},
		Depth:        &zero,
	}, &g)
	for _, e := range g.Edges {
		if e.Target != "Total" {
			t.Errorf("depth 0 from Orders: unexpected edge %+v", e)
		}
	}
	if nodeByID(g.Nodes, helpers) != nil {
		t.Errorf("depth 0 from Orders reached Helpers")
	}

	callCustom(t, s, MethodCallGraph, CallGraphParams{
		TextDocument: &TextDocumentIdentifier{URI: orders},
		Direction:    "outgoing",
	}, &g)
	if len(g.Edges) != 6 || nodeByID(g.Nodes, ds) == nil {
		t.Errorf("everything Orders calls: %d edges, nodes %+v", len(g.Edges), g.Nodes)
	}
}

func TestCallGraphOpenBufferWins(t *testing.T) {
	// [spec feature.call_graph/A6]
	s, orders, _, _ := newCallGraphServer(t)
	// The buffer drops every call but one and adds a procedure.
	s.documents.SetDocument(orders, ":PROCEDURE Main;\n\tDoProc(\"Added\");\n:ENDPROC;\n:PROCEDURE Added;\n:ENDPROC;\n", 2)
	s.documentVersion[orders] = 2

	var g CallGraphResult
	callCustom(t, s, MethodCallGraph, CallGraphParams{TextDocument: &TextDocumentIdentifier{URI: orders}, Direction: "outgoing"}, &g)
	if len(g.Edges) != 1 || g.Edges[0].To != procedureID(orders, "Added") || !g.Edges[0].Resolved {
		t.Errorf("edges from the open buffer = %+v", g.Edges)
	}
	if g.Edges[0].Location.Range.Start.Line != 1 {
		t.Errorf("call site from the buffer = %+v, want line 1", g.Edges[0].Location)
	}
}
