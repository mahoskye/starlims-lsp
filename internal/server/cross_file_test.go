package server

import (
	"os"
	"path/filepath"
	"strings"
	"testing"

	"starlims-lsp/internal/providers"

	protocol "github.com/tliron/glsp/protocol_3_16"
)

// newResolverIndex builds a WorkspaceIndex over a temp workspace without
// background walking (existing convention: close doneCh, index directly).
func newResolverIndex(t *testing.T) (*WorkspaceIndex, string) {
	t.Helper()
	dir := t.TempDir()
	wi := NewWorkspaceIndex([]string{pathToURI(dir)})
	close(wi.doneCh)
	return wi, dir
}

func writeAndIndex(t *testing.T, wi *WorkspaceIndex, dir, rel, content string) string {
	t.Helper()
	path := filepath.Join(dir, filepath.FromSlash(rel))
	if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(path, []byte(content), 0o644); err != nil {
		t.Fatal(err)
	}
	uri := pathToURI(path)
	if err := wi.IndexFile(uri); err != nil {
		t.Fatal(err)
	}
	return uri
}

const helperScript = `/* helpers;
:PARAMETERS sMode;
:DECLARE nCount;

:PROCEDURE CalculateTotal;
:PARAMETERS nQty, nPrice;
:RETURN nQty * nPrice;
:ENDPROC;

/*@private;
:PROCEDURE internalHelper;
:ENDPROC;`

// [spec feature.cross_file_resolution/A4]
func TestResolveDispatch_TwoPart_CategoryScript_EntryPoint(t *testing.T) {
	wi, dir := newResolverIndex(t)
	uri := writeAndIndex(t, wi, dir, "Server Scripts/LIMS_UTILS/HELPERS.srvscr", helperScript)

	res := wi.ResolveDispatchTarget("LIMS_UTILS.HELPERS")
	if len(res) != 1 {
		t.Fatalf("expected 1 resolution, got %d: %+v", len(res), res)
	}
	if res[0].URI != uri || !res[0].IsEntry {
		t.Errorf("expected entry resolution in %s, got %+v", uri, res[0])
	}
	// Entry point is the file-level :PARAMETERS line (1-based line 2 -> 0-based 1).
	if res[0].Line != 1 {
		t.Errorf("expected entry line 1 (:PARAMETERS), got %d", res[0].Line)
	}
}

// [spec feature.cross_file_resolution/A5]
func TestResolveDispatch_TwoPart_FlatScriptProc(t *testing.T) {
	wi, dir := newResolverIndex(t)
	uri := writeAndIndex(t, wi, dir, "lib/Helpers.ssl", helperScript)

	res := wi.ResolveDispatchTarget("Helpers.CalculateTotal")
	if len(res) != 1 {
		t.Fatalf("expected 1 resolution, got %d: %+v", len(res), res)
	}
	if res[0].URI != uri || res[0].IsEntry {
		t.Errorf("expected procedure resolution in %s, got %+v", uri, res[0])
	}
	if res[0].Line != 4 { // :PROCEDURE CalculateTotal on 1-based line 5
		t.Errorf("expected procedure line 4, got %d", res[0].Line)
	}
}

// [spec feature.cross_file_resolution/A6]
func TestResolveDispatch_ThreePart_CategoryChain_AndDegradation(t *testing.T) {
	wi, dir := newResolverIndex(t)
	uri := writeAndIndex(t, wi, dir, "Server Scripts/LIMS_UTILS/HELPERS.srvscr", helperScript)

	// Canonical category chain.
	res := wi.ResolveDispatchTarget("LIMS_UTILS.HELPERS.CalculateTotal")
	if len(res) != 1 || res[0].URI != uri || res[0].IsEntry {
		t.Fatalf("expected procedure via category chain, got %+v", res)
	}

	// Unknown category degrades to script-basename matching.
	res = wi.ResolveDispatchTarget("NO_SUCH_CATEGORY.HELPERS.CalculateTotal")
	if len(res) != 1 || res[0].URI != uri {
		t.Fatalf("expected basename degradation to find the procedure, got %+v", res)
	}
}

// [spec feature.cross_file_resolution/A7]
func TestResolveDispatch_MissingProcedure_TruthfulNull(t *testing.T) {
	wi, dir := newResolverIndex(t)
	writeAndIndex(t, wi, dir, "Server Scripts/LIMS_UTILS/HELPERS.srvscr", helperScript)

	if res := wi.ResolveDispatchTarget("LIMS_UTILS.HELPERS.NoSuchProc"); len(res) != 0 {
		t.Errorf("expected no resolutions for a missing procedure, got %+v", res)
	}
}

// [spec feature.cross_file_resolution/A8]
func TestResolveDispatch_CaseInsensitive(t *testing.T) {
	wi, dir := newResolverIndex(t)
	uri := writeAndIndex(t, wi, dir, "Server Scripts/LIMS_UTILS/HELPERS.srvscr", helperScript)

	for _, target := range []string{
		"lims_utils.helpers.calculatetotal",
		"LIMS_UTILS.helpers.CALCULATETOTAL",
		"Lims_Utils.Helpers.CalculateTotal",
	} {
		res := wi.ResolveDispatchTarget(target)
		if len(res) != 1 || res[0].URI != uri {
			t.Errorf("target %q: expected the procedure, got %+v", target, res)
		}
	}
}

// [spec feature.cross_file_resolution/A9]
func TestResolveDispatch_Ambiguous_AnchoredFirst(t *testing.T) {
	wi, dir := newResolverIndex(t)
	flatURI := writeAndIndex(t, wi, dir, "aflat/Helpers.ssl", helperScript)
	anchoredURI := writeAndIndex(t, wi, dir, "Server Scripts/LIMS_UTILS/HELPERS.srvscr", helperScript)

	res := wi.ResolveDispatchTarget("Helpers.CalculateTotal")
	if len(res) != 2 {
		t.Fatalf("expected both candidates, got %d: %+v", len(res), res)
	}
	if res[0].URI != anchoredURI || res[1].URI != flatURI {
		t.Errorf("expected anchored candidate first, got %+v", res)
	}
}

// [spec feature.cross_file_resolution/A10]
func TestResolveDispatch_UniqueProcFallback(t *testing.T) {
	wi, dir := newResolverIndex(t)
	uri := writeAndIndex(t, wi, dir, "lib/Util.ssl", ":PROCEDURE UniqueProc;\n:ENDPROC;")

	// No category or script rule matches "Nowhere.UniqueProc", but the
	// procedure name is workspace-unique.
	res := wi.ResolveDispatchTarget("Nowhere.UniqueProc")
	if len(res) != 1 || res[0].URI != uri {
		t.Fatalf("expected unique-procedure fallback, got %+v", res)
	}

	// A second file with the same procedure breaks uniqueness: nothing.
	writeAndIndex(t, wi, dir, "lib/Other.ssl", ":PROCEDURE UniqueProc;\n:ENDPROC;")
	if res := wi.ResolveDispatchTarget("Nowhere.UniqueProc"); len(res) != 0 {
		t.Errorf("expected no resolutions once the name is ambiguous, got %+v", res)
	}
}

// [spec feature.cross_file_resolution/A11]
func TestResolveInclude_BareAndDotted(t *testing.T) {
	wi, dir := newResolverIndex(t)
	uri := writeAndIndex(t, wi, dir, "Server Scripts/LIMS_UTILS/SHAREDLIB.srvscr", ":PROCEDURE Shared;\n:ENDPROC;")

	for _, target := range []string{"SharedLib", "LIMS_UTILS.SHAREDLIB", "WRONG_CAT.SharedLib"} {
		res := wi.ResolveIncludeTarget(target)
		if len(res) != 1 || res[0].URI != uri || res[0].Line != 0 {
			t.Errorf("include target %q: expected file at line 0, got %+v", target, res)
		}
	}
}

// [spec feature.cross_file_resolution/A13]
func TestResolveDispatch_PrivateProcedureStillNavigable(t *testing.T) {
	wi, dir := newResolverIndex(t)
	uri := writeAndIndex(t, wi, dir, "Server Scripts/LIMS_UTILS/HELPERS.srvscr", helperScript)

	res := wi.ResolveDispatchTarget("LIMS_UTILS.HELPERS.internalHelper")
	if len(res) != 1 || res[0].URI != uri {
		t.Errorf("expected private procedure to resolve for navigation, got %+v", res)
	}

	// And the index records its privacy for completion-side filtering.
	fs, ok := wi.FileSymbolsFor(uri)
	if !ok {
		t.Fatal("FileSymbolsFor missing")
	}
	foundPrivate := false
	for _, proc := range fs.Procedures {
		if proc.Name == "internalHelper" && proc.IsPrivate {
			foundPrivate = true
		}
	}
	if !foundPrivate {
		t.Error("expected internalHelper to be indexed as private")
	}
}

// [spec feature.cross_file_resolution/A14]
func TestResolveDispatch_OnePartNeverResolves(t *testing.T) {
	wi, dir := newResolverIndex(t)
	writeAndIndex(t, wi, dir, "Server Scripts/LIMS_UTILS/HELPERS.srvscr", helperScript)

	if res := wi.ResolveDispatchTarget("CalculateTotal"); len(res) != 0 {
		t.Errorf("1-part targets are same-script only, got %+v", res)
	}
}

// [spec feature.cross_file_resolution/A12]
func TestLiveResolver_OpenDocumentOverlay(t *testing.T) {
	s := NewSSLServer()
	wi, dir := newResolverIndex(t)
	s.workspaceIndex = wi
	uri := writeAndIndex(t, wi, dir, "Server Scripts/LIMS_UTILS/HELPERS.srvscr", helperScript)

	// Open the document with unsaved edits: two blank lines added above,
	// so CalculateTotal moved from 1-based line 5 to line 7.
	edited := "\n\n" + helperScript
	s.documents.SetDocument(uri, edited, 2)
	s.documentVersion[uri] = 2

	res := (liveResolver{s}).ResolveDispatch("LIMS_UTILS.HELPERS.CalculateTotal")
	if len(res) != 1 {
		t.Fatalf("expected 1 resolution, got %+v", res)
	}
	if res[0].Line != 6 {
		t.Errorf("expected live-buffer line 6, got %d", res[0].Line)
	}
	if res[0].Kind != providers.ResolvedProcedure {
		t.Errorf("expected procedure kind, got %v", res[0].Kind)
	}

	// Delete the procedure in the live buffer: the candidate is dropped.
	s.documents.SetDocument(uri, ":DECLARE nOnly;\n", 3)
	s.documentVersion[uri] = 3
	if res := (liveResolver{s}).ResolveDispatch("LIMS_UTILS.HELPERS.CalculateTotal"); len(res) != 0 {
		t.Errorf("expected stale candidate dropped, got %+v", res)
	}
}

// Nil-index safety: a server without a workspace root resolves nothing.
func TestLiveResolver_NilIndexSafe(t *testing.T) {
	s := NewSSLServer()
	if res := (liveResolver{s}).ResolveDispatch("A.B.C"); res != nil {
		t.Errorf("expected nil with no index, got %+v", res)
	}
	if res := (liveResolver{s}).ResolveInclude("A.B"); res != nil {
		t.Errorf("expected nil with no index, got %+v", res)
	}
}

// End-to-end: definition request on a dotted ExecFunction target jumps to
// the procedure in another workspace file. [spec feature.definition/A8]
func TestHandleDefinition_CrossFileDispatch(t *testing.T) {
	s := NewSSLServer()
	wi, dir := newResolverIndex(t)
	s.workspaceIndex = wi
	targetURI := writeAndIndex(t, wi, dir, "Server Scripts/LIMS_UTILS/HELPERS.srvscr", helperScript)

	source := `result := ExecFunction("LIMS_UTILS.HELPERS.CalculateTotal", {});`
	s.documents.SetDocument(testURI, source, 1)
	s.documentVersion[testURI] = 1

	result, err := s.handleDefinition(nil, &protocol.DefinitionParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: testURI},
			Position:     protocol.Position{Line: 0, Character: 30},
		},
	})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	loc, ok := result.(protocol.Location)
	if !ok {
		t.Fatalf("expected a single protocol.Location, got %T", result)
	}
	if string(loc.URI) != targetURI {
		t.Errorf("expected location in %s, got %s", targetURI, loc.URI)
	}
	if loc.Range.Start.Line != 4 { // :PROCEDURE CalculateTotal, 0-based
		t.Errorf("expected line 4, got %d", loc.Range.Start.Line)
	}
}

// References stay single-file: a dispatch target that resolves cross-file
// for definition must NOT leak other-file locations into references
// (feature.references contract preserved).
func TestHandleReferences_StaySingleFileWithWorkspaceIndex(t *testing.T) {
	s := NewSSLServer()
	wi, dir := newResolverIndex(t)
	s.workspaceIndex = wi
	writeAndIndex(t, wi, dir, "Server Scripts/LIMS_UTILS/HELPERS.srvscr", helperScript)

	source := `result := ExecFunction("LIMS_UTILS.HELPERS.CalculateTotal", {});`
	s.documents.SetDocument(testURI, source, 1)
	s.documentVersion[testURI] = 1

	refs, err := s.handleReferences(nil, &protocol.ReferenceParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: testURI},
			Position:     protocol.Position{Line: 0, Character: 45},
		},
		Context: protocol.ReferenceContext{IncludeDeclaration: true},
	})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	for _, ref := range refs {
		if string(ref.URI) != testURI {
			t.Errorf("references must stay single-file, got location in %s", ref.URI)
		}
	}
}

// [spec feature.hover/A10]
func TestHandleHover_CrossFileDispatchProcedure(t *testing.T) {
	s := NewSSLServer()
	wi, dir := newResolverIndex(t)
	s.workspaceIndex = wi
	writeAndIndex(t, wi, dir, "Server Scripts/LIMS_UTILS/HELPERS.srvscr", `/*
 * Description: Multiplies quantity by price.
;
:PROCEDURE CalculateTotal;
:PARAMETERS nQty, nPrice;
:ENDPROC;`)

	source := `result := ExecFunction("LIMS_UTILS.HELPERS.CalculateTotal", {});`
	s.documents.SetDocument(testURI, source, 1)
	s.documentVersion[testURI] = 1

	hover, err := s.handleHover(nil, &protocol.HoverParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: testURI},
			Position:     protocol.Position{Line: 0, Character: 30},
		},
	})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if hover == nil {
		t.Fatal("expected cross-file dispatch hover")
	}
	md := hover.Contents.(protocol.MarkupContent).Value
	for _, want := range []string{"CalculateTotal", "LIMS_UTILS.HELPERS", "Multiplies quantity by price.", "nQty"} {
		if !strings.Contains(md, want) {
			t.Errorf("hover missing %q:\n%s", want, md)
		}
	}
}

// [spec feature.hover/A11]
func TestHandleHover_CrossFileEntryPoint(t *testing.T) {
	s := NewSSLServer()
	wi, dir := newResolverIndex(t)
	s.workspaceIndex = wi
	writeAndIndex(t, wi, dir, "Server Scripts/LIMS_UTILS/HELPERS.srvscr", helperScript)

	source := `result := ExecFunction("LIMS_UTILS.HELPERS", {});`
	s.documents.SetDocument(testURI, source, 1)
	s.documentVersion[testURI] = 1

	hover, err := s.handleHover(nil, &protocol.HoverParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: testURI},
			Position:     protocol.Position{Line: 0, Character: 28},
		},
	})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if hover == nil {
		t.Fatal("expected entry-point hover")
	}
	md := hover.Contents.(protocol.MarkupContent).Value
	for _, want := range []string{"LIMS_UTILS.HELPERS", "entry point", "sMode"} {
		if !strings.Contains(md, want) {
			t.Errorf("hover missing %q:\n%s", want, md)
		}
	}
}

// [spec feature.hover/A12]
func TestHandleHover_IncludeTarget(t *testing.T) {
	s := NewSSLServer()
	wi, dir := newResolverIndex(t)
	s.workspaceIndex = wi
	writeAndIndex(t, wi, dir, "Server Scripts/LIMS_UTILS/SHAREDLIB.srvscr", helperScript)

	source := `:INCLUDE SharedLib;`
	s.documents.SetDocument(testURI, source, 1)
	s.documentVersion[testURI] = 1

	hover, err := s.handleHover(nil, &protocol.HoverParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: testURI},
			Position:     protocol.Position{Line: 0, Character: 12},
		},
	})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if hover == nil {
		t.Fatal("expected include hover")
	}
	md := hover.Contents.(protocol.MarkupContent).Value
	if !strings.Contains(md, "LIMS_UTILS.SHAREDLIB") {
		t.Errorf("hover missing script identity:\n%s", md)
	}
}

// [spec feature.hover/A13]
func TestHandleHover_UnresolvableDispatchStaysNull(t *testing.T) {
	s := NewSSLServer()
	wi, _ := newResolverIndex(t)
	s.workspaceIndex = wi

	source := `result := ExecFunction("No.Such.Target", {});`
	s.documents.SetDocument(testURI, source, 1)
	s.documentVersion[testURI] = 1

	hover, err := s.handleHover(nil, &protocol.HoverParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: testURI},
			Position:     protocol.Position{Line: 0, Character: 28},
		},
	})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if hover != nil {
		t.Errorf("expected null hover for unresolvable target, got %+v", hover)
	}
}

func dispatchCompletionLabels(t *testing.T, s *SSLServer, source string, character uint32) []string {
	t.Helper()
	s.documents.SetDocument(testURI, source, 1)
	s.documentVersion[testURI] = 1
	result, err := s.handleCompletion(nil, &protocol.CompletionParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: testURI},
			Position:     protocol.Position{Line: 0, Character: character},
		},
	})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	items := result.([]protocol.CompletionItem)
	labels := make([]string, 0, len(items))
	for _, item := range items {
		labels = append(labels, item.Label)
	}
	return labels
}

func containsLabel(labels []string, want string) bool {
	for _, l := range labels {
		if l == want {
			return true
		}
	}
	return false
}

// [spec feature.completion/A7]
func TestDispatchCompletion_LevelZero_CategoriesOnly(t *testing.T) {
	s := NewSSLServer()
	wi, dir := newResolverIndex(t)
	s.workspaceIndex = wi
	writeAndIndex(t, wi, dir, "Server Scripts/LIMS_UTILS/HELPERS.srvscr", helperScript)

	source := `:PROCEDURE LocalProc;
:ENDPROC;` // procedures defined below; completion happens in the call
	source = `result := ExecFunction("");` + "\n" + source
	labels := dispatchCompletionLabels(t, s, source, 24)

	if !containsLabel(labels, "LIMS_UTILS") {
		t.Errorf("expected category LIMS_UTILS at level 0, got %v", labels)
	}
	if !containsLabel(labels, "LocalProc") {
		t.Errorf("expected same-file procedure at level 0, got %v", labels)
	}
	// Workspace script names must NOT appear before a dot is typed.
	if containsLabel(labels, "HELPERS") {
		t.Errorf("script names must not appear at level 0, got %v", labels)
	}
	// Workspace procedures must not appear either.
	if containsLabel(labels, "CalculateTotal") {
		t.Errorf("workspace procedures must not appear at level 0, got %v", labels)
	}
}

// [spec feature.completion/A8]
func TestDispatchCompletion_LevelOne_CategoryScripts(t *testing.T) {
	s := NewSSLServer()
	wi, dir := newResolverIndex(t)
	s.workspaceIndex = wi
	writeAndIndex(t, wi, dir, "Server Scripts/LIMS_UTILS/HELPERS.srvscr", helperScript)
	writeAndIndex(t, wi, dir, "Server Scripts/LIMS_UTILS/TASKS.srvscr", ":PROCEDURE Run;\n:ENDPROC;")

	labels := dispatchCompletionLabels(t, s, `result := ExecFunction("LIMS_UTILS.");`, 35)

	for _, want := range []string{"HELPERS", "TASKS"} {
		if !containsLabel(labels, want) {
			t.Errorf("expected script %s after category dot, got %v", want, labels)
		}
	}
}

// [spec feature.completion/A9]
func TestDispatchCompletion_LevelTwo_ProceduresExcludePrivate(t *testing.T) {
	s := NewSSLServer()
	wi, dir := newResolverIndex(t)
	s.workspaceIndex = wi
	writeAndIndex(t, wi, dir, "Server Scripts/LIMS_UTILS/HELPERS.srvscr", helperScript)

	labels := dispatchCompletionLabels(t, s, `result := ExecFunction("LIMS_UTILS.HELPERS.");`, 43)

	if !containsLabel(labels, "CalculateTotal") {
		t.Errorf("expected CalculateTotal after script dot, got %v", labels)
	}
	if containsLabel(labels, "internalHelper") {
		t.Errorf("private procedures must be excluded, got %v", labels)
	}
}

// [spec feature.completion/A10]
func TestDispatchCompletion_FlatScriptDot(t *testing.T) {
	s := NewSSLServer()
	wi, dir := newResolverIndex(t)
	s.workspaceIndex = wi
	writeAndIndex(t, wi, dir, "lib/Helpers.ssl", helperScript)

	labels := dispatchCompletionLabels(t, s, `result := DoProc("Helpers.");`, 26)

	if !containsLabel(labels, "CalculateTotal") {
		t.Errorf("expected flat-layout procedures after Script., got %v", labels)
	}
	if containsLabel(labels, "internalHelper") {
		t.Errorf("private procedures must be excluded, got %v", labels)
	}
}

const ordersDataSource = `/* orders query;
:PARAMETERS sStatus;
SELECT ORDER_ID FROM ORDERS WHERE STATUS = ?sStatus?;`

// [spec feature.cross_file_resolution/A15]
// [spec feature.cross_file_resolution/A16]
func TestResolveDataSource_CategoryAndFlat(t *testing.T) {
	wi, dir := newResolverIndex(t)
	anchoredURI := writeAndIndex(t, wi, dir, "Data Sources/QUERIES/ORDERS.ds", ordersDataSource)
	flatURI := writeAndIndex(t, wi, dir, "lib/Inventory.ds", ordersDataSource)

	res := wi.ResolveDataSourceTarget("QUERIES.ORDERS")
	if len(res) != 1 || res[0].URI != anchoredURI || !res[0].IsEntry {
		t.Fatalf("expected anchored data-source entry, got %+v", res)
	}
	if res[0].Line != 1 { // file-level :PARAMETERS on 1-based line 2
		t.Errorf("expected entry line 1 (:PARAMETERS), got %d", res[0].Line)
	}

	// 1-part data-source targets resolve by basename (unlike dispatch).
	res = wi.ResolveDataSourceTarget("Inventory")
	if len(res) != 1 || res[0].URI != flatURI {
		t.Fatalf("expected flat 1-part basename match, got %+v", res)
	}
}

// [spec feature.cross_file_resolution/A17]
func TestResolveDataSource_PartitionFromScripts(t *testing.T) {
	wi, dir := newResolverIndex(t)
	scriptURI := writeAndIndex(t, wi, dir, "Server Scripts/QUERIES/ORDERS.srvscr", helperScript)
	dsURI := writeAndIndex(t, wi, dir, "Data Sources/QUERIES/ORDERS.ds", ordersDataSource)

	// Dispatch and include resolution return only the script.
	for _, res := range [][]IndexResolution{
		wi.ResolveDispatchTarget("QUERIES.ORDERS"),
		wi.ResolveIncludeTarget("QUERIES.ORDERS"),
		wi.ResolveIncludeTarget("ORDERS"),
	} {
		if len(res) != 1 || res[0].URI != scriptURI {
			t.Errorf("dispatch/include must return only the script, got %+v", res)
		}
	}

	// Data-source resolution returns only the data source.
	for _, res := range [][]IndexResolution{
		wi.ResolveDataSourceTarget("QUERIES.ORDERS"),
		wi.ResolveDataSourceTarget("ORDERS"),
	} {
		if len(res) != 1 || res[0].URI != dsURI {
			t.Errorf("data-source resolution must return only the data source, got %+v", res)
		}
	}
}

// [spec feature.definition/A13]
func TestHandleDefinition_RunDSTarget(t *testing.T) {
	s := NewSSLServer()
	wi, dir := newResolverIndex(t)
	s.workspaceIndex = wi
	dsURI := writeAndIndex(t, wi, dir, "Data Sources/QUERIES/ORDERS.ds", ordersDataSource)

	source := `aRows := RunDS("QUERIES.ORDERS", {sStatus});`
	s.documents.SetDocument(testURI, source, 1)
	s.documentVersion[testURI] = 1

	result, err := s.handleDefinition(nil, &protocol.DefinitionParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: testURI},
			Position:     protocol.Position{Line: 0, Character: 20},
		},
	})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	loc, ok := result.(protocol.Location)
	if !ok {
		t.Fatalf("expected a single protocol.Location, got %T", result)
	}
	if string(loc.URI) != dsURI {
		t.Errorf("expected location in %s, got %s", dsURI, loc.URI)
	}
	if loc.Range.Start.Line != 1 { // entry :PARAMETERS line
		t.Errorf("expected line 1, got %d", loc.Range.Start.Line)
	}

	// A RunDS target resolving nowhere is null.
	s.documents.SetDocument(testURI, `aRows := RunDS("NO.SUCHDS", {});`, 2)
	s.documentVersion[testURI] = 2
	result, err = s.handleDefinition(nil, &protocol.DefinitionParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: testURI},
			Position:     protocol.Position{Line: 0, Character: 20},
		},
	})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if result != nil {
		t.Errorf("expected null for unresolvable RunDS target, got %+v", result)
	}
}

// [spec feature.hover/A14]
func TestHandleHover_RunDSTarget(t *testing.T) {
	s := NewSSLServer()
	wi, dir := newResolverIndex(t)
	s.workspaceIndex = wi
	writeAndIndex(t, wi, dir, "Data Sources/QUERIES/ORDERS.ds", ordersDataSource)

	source := `aRows := RunDS("QUERIES.ORDERS", {sStatus});`
	s.documents.SetDocument(testURI, source, 1)
	s.documentVersion[testURI] = 1

	hover, err := s.handleHover(nil, &protocol.HoverParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: testURI},
			Position:     protocol.Position{Line: 0, Character: 20},
		},
	})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if hover == nil {
		t.Fatal("expected RunDS data-source hover")
	}
	md := hover.Contents.(protocol.MarkupContent).Value
	for _, want := range []string{"QUERIES.ORDERS", "Data source", "sStatus"} {
		if !strings.Contains(md, want) {
			t.Errorf("hover missing %q:\n%s", want, md)
		}
	}

	// Unresolvable RunDS target keeps the string suppression (null).
	s.documents.SetDocument(testURI, `aRows := RunDS("NO.SUCHDS", {});`, 2)
	s.documentVersion[testURI] = 2
	hover, err = s.handleHover(nil, &protocol.HoverParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: testURI},
			Position:     protocol.Position{Line: 0, Character: 20},
		},
	})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if hover != nil {
		t.Errorf("expected null hover for unresolvable RunDS target, got %+v", hover)
	}
}

// [spec feature.cross_file_resolution/A18]
func TestIncludeDeclaredVariables_TransitiveAndCycle(t *testing.T) {
	s := NewSSLServer()
	wi, dir := newResolverIndex(t)
	s.workspaceIndex = wi
	// LibB -> LibC -> LibB is a cycle; the closure must terminate and
	// carry both files' declarations.
	writeAndIndex(t, wi, dir, "Server Scripts/LIMS_UTILS/LIBB.srvscr",
		":PUBLIC gShared;\n:INCLUDE LibC;\n:DECLARE nFileLevel;")
	writeAndIndex(t, wi, dir, "Server Scripts/LIMS_UTILS/LIBC.srvscr",
		":PUBLIC gDeep;\n:INCLUDE LibB;")

	source := ":INCLUDE LibB;\nnTotal := gShared + gDeep;"
	s.documents.SetDocument(testURI, source, 1)
	s.documentVersion[testURI] = 1
	cache := s.documents.ParseDocument(testURI, 1)

	names := (liveResolver{s}).includeDeclaredVariables(cache.Tokens, testURI)
	for _, want := range []string{"gShared", "gDeep", "nFileLevel"} {
		if !containsFold(names, want) {
			t.Errorf("closure missing %q, got %v", want, names)
		}
	}

	// End to end: the closure suppresses undeclared_variable for included
	// names while a genuinely undeclared name still flags.
	opts := providers.DefaultDiagnosticOptions()
	opts.CheckUndeclaredVars = true
	opts.IncludeDeclaredVariables = names
	source = ":INCLUDE LibB;\n:PROCEDURE Demo;\nnTotal := gShared + gDeep + nMissing;\n:ENDPROC;"
	s.documents.SetDocument(testURI, source, 2)
	s.documentVersion[testURI] = 2
	cache = s.documents.ParseDocument(testURI, 2)
	var flagged []string
	for _, d := range providers.GetDiagnosticsFromTokens(cache.Tokens, cache.AST, opts) {
		if d.Code == providers.CodeUndeclaredVariable {
			flagged = append(flagged, d.Message)
		}
	}
	if len(flagged) != 1 || !strings.Contains(flagged[0], "nMissing") {
		t.Errorf("expected exactly one undeclared flag for nMissing, got %v", flagged)
	}
}

// [spec feature.cross_file_resolution/A19]
func TestIncludeDeclaredVariables_AmbiguousUnionAndUnresolvable(t *testing.T) {
	s := NewSSLServer()
	wi, dir := newResolverIndex(t)
	s.workspaceIndex = wi
	writeAndIndex(t, wi, dir, "Server Scripts/CATA/SHAREDLIB.srvscr", ":PUBLIC gFromA;")
	writeAndIndex(t, wi, dir, "Server Scripts/CATB/SHAREDLIB.srvscr", ":PUBLIC gFromB;")

	s.documents.SetDocument(testURI, ":INCLUDE SharedLib;", 1)
	s.documentVersion[testURI] = 1
	cache := s.documents.ParseDocument(testURI, 1)

	names := (liveResolver{s}).includeDeclaredVariables(cache.Tokens, testURI)
	for _, want := range []string{"gFromA", "gFromB"} {
		if !containsFold(names, want) {
			t.Errorf("ambiguous include must union candidates, missing %q in %v", want, names)
		}
	}

	// Unresolvable target contributes nothing.
	s.documents.SetDocument(testURI, ":INCLUDE NoSuchLib;", 2)
	s.documentVersion[testURI] = 2
	cache = s.documents.ParseDocument(testURI, 2)
	if names := (liveResolver{s}).includeDeclaredVariables(cache.Tokens, testURI); len(names) != 0 {
		t.Errorf("unresolvable include must contribute nothing, got %v", names)
	}
}

// Open included documents contribute live-buffer declarations (overlay
// consistency with feature.cross_file_resolution/A12).
func TestIncludeDeclaredVariables_OpenDocumentOverlay(t *testing.T) {
	s := NewSSLServer()
	wi, dir := newResolverIndex(t)
	s.workspaceIndex = wi
	libURI := writeAndIndex(t, wi, dir, "Server Scripts/LIMS_UTILS/LIBB.srvscr", ":PUBLIC gShared;")

	// The included file is open with an unsaved edit renaming the public.
	s.documents.SetDocument(libURI, ":PUBLIC gLive;", 1)
	s.documentVersion[libURI] = 1

	s.documents.SetDocument(testURI, ":INCLUDE LibB;", 1)
	s.documentVersion[testURI] = 1
	cache := s.documents.ParseDocument(testURI, 1)

	names := (liveResolver{s}).includeDeclaredVariables(cache.Tokens, testURI)
	if !containsFold(names, "gLive") || containsFold(names, "gShared") {
		t.Errorf("expected live-buffer declarations only, got %v", names)
	}
}

func containsFold(names []string, want string) bool {
	for _, n := range names {
		if strings.EqualFold(n, want) {
			return true
		}
	}
	return false
}
