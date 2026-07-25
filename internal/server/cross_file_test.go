package server

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
	"unicode/utf8"

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

// --- Cross-file references (issue #125, feature.references A10-A14) ---

// helperWithSelfSite is a definition script whose Wrapper procedure calls
// its own CalculateTotal through a dotted dispatch string — invisible to
// the same-file whole-content match.
const helperWithSelfSite = `/* helpers;
:PROCEDURE CalculateTotal;
:PARAMETERS nQty, nPrice;
:RETURN nQty * nPrice;
:ENDPROC;

:PROCEDURE Wrapper;
:RETURN ExecFunction("LIMS_UTILS.HELPERS.CalculateTotal", {1, 2});
:ENDPROC;`

func refsContain(refs []protocol.Location, uri string) bool {
	for _, ref := range refs {
		if string(ref.URI) == uri {
			return true
		}
	}
	return false
}

// [spec feature.references/A10] — references from the declaration include
// dotted dispatch sites in other files, at the string-content range.
func TestHandleReferences_CrossFileFromDeclaration(t *testing.T) {
	s := NewSSLServer()
	wi, dir := newResolverIndex(t)
	s.workspaceIndex = wi
	defURI := writeAndIndex(t, wi, dir, "Server Scripts/LIMS_UTILS/HELPERS.srvscr", helperScript)
	callerURI := writeAndIndex(t, wi, dir, "Server Scripts/ORDERS/PROCESS.srvscr",
		`:PROCEDURE Run;
:RETURN ExecFunction("LIMS_UTILS.HELPERS.CalculateTotal", {2, 3});
:ENDPROC;`)

	s.documents.SetDocument(defURI, helperScript, 1)
	s.documentVersion[defURI] = 1

	refs, err := s.handleReferences(nil, &protocol.ReferenceParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: defURI},
			Position:     protocol.Position{Line: 4, Character: 15}, // on CalculateTotal
		},
		Context: protocol.ReferenceContext{IncludeDeclaration: true},
	})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if !refsContain(refs, callerURI) {
		t.Fatalf("expected a reference in the caller file, got %+v", refs)
	}
	for _, ref := range refs {
		if string(ref.URI) != callerURI {
			continue
		}
		if ref.Range.Start.Line != 1 {
			t.Errorf("caller site line = %d, want 1", ref.Range.Start.Line)
		}
		wantStart := strings.Index(`:RETURN ExecFunction("`, `"`) + 1
		if int(ref.Range.Start.Character) != wantStart {
			t.Errorf("caller site start char = %d, want %d (string content)", ref.Range.Start.Character, wantStart)
		}
		wantLen := len("LIMS_UTILS.HELPERS.CalculateTotal")
		if int(ref.Range.End.Character-ref.Range.Start.Character) != wantLen {
			t.Errorf("caller site span = %d, want %d", ref.Range.End.Character-ref.Range.Start.Character, wantLen)
		}
	}
}

// [spec feature.references/A10] — references requested from the call-site
// string return the declaration (per includeDeclaration) and the site.
func TestHandleReferences_FromCallSiteString(t *testing.T) {
	s := NewSSLServer()
	wi, dir := newResolverIndex(t)
	s.workspaceIndex = wi
	defURI := writeAndIndex(t, wi, dir, "Server Scripts/LIMS_UTILS/HELPERS.srvscr", helperScript)

	source := `result := ExecFunction("LIMS_UTILS.HELPERS.CalculateTotal", {});`
	s.documents.SetDocument(testURI, source, 1)
	s.documentVersion[testURI] = 1

	params := &protocol.ReferenceParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: testURI},
			Position:     protocol.Position{Line: 0, Character: 45},
		},
		Context: protocol.ReferenceContext{IncludeDeclaration: true},
	}
	refs, err := s.handleReferences(nil, params)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if !refsContain(refs, defURI) {
		t.Errorf("expected the declaration in the definition file, got %+v", refs)
	}
	if !refsContain(refs, testURI) {
		t.Errorf("expected the call site itself, got %+v", refs)
	}

	params.Context.IncludeDeclaration = false
	refs, err = s.handleReferences(nil, params)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if refsContain(refs, defURI) {
		t.Errorf("includeDeclaration=false must exclude the declaration, got %+v", refs)
	}
	if !refsContain(refs, testURI) {
		t.Errorf("call site must remain without the declaration, got %+v", refs)
	}
}

// [spec feature.references/A11] — a 1-part DoProc("Proc") in another file
// is NOT a reference (same-file scoping rule, cross_file_resolution A14).
func TestHandleReferences_OnePartOtherFileNotReturned(t *testing.T) {
	s := NewSSLServer()
	wi, dir := newResolverIndex(t)
	s.workspaceIndex = wi
	defURI := writeAndIndex(t, wi, dir, "Server Scripts/LIMS_UTILS/HELPERS.srvscr", helperScript)
	onePartURI := writeAndIndex(t, wi, dir, "Server Scripts/ORDERS/LOCALCALL.srvscr",
		`:PROCEDURE Run;
:RETURN DoProc("CalculateTotal");
:ENDPROC;`)

	s.documents.SetDocument(defURI, helperScript, 1)
	s.documentVersion[defURI] = 1

	refs, err := s.handleReferences(nil, &protocol.ReferenceParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: defURI},
			Position:     protocol.Position{Line: 4, Character: 15},
		},
		Context: protocol.ReferenceContext{IncludeDeclaration: true},
	})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if refsContain(refs, onePartURI) {
		t.Errorf("1-part dispatch in another file must not be a reference, got %+v", refs)
	}
}

// [spec feature.references/A12] — open documents are scanned from the live
// buffer: a site deleted in unsaved edits disappears from results.
func TestHandleReferences_OpenDocOverlay(t *testing.T) {
	s := NewSSLServer()
	wi, dir := newResolverIndex(t)
	s.workspaceIndex = wi
	defURI := writeAndIndex(t, wi, dir, "Server Scripts/LIMS_UTILS/HELPERS.srvscr", helperScript)
	callerURI := writeAndIndex(t, wi, dir, "Server Scripts/ORDERS/PROCESS.srvscr",
		`:PROCEDURE Run;
:RETURN ExecFunction("LIMS_UTILS.HELPERS.CalculateTotal", {2, 3});
:ENDPROC;`)

	// The caller is open with the dispatch site edited away (unsaved).
	s.documents.SetDocument(callerURI, `:PROCEDURE Run;
:RETURN 0;
:ENDPROC;`, 1)
	s.documentVersion[callerURI] = 1

	s.documents.SetDocument(defURI, helperScript, 1)
	s.documentVersion[defURI] = 1

	refs, err := s.handleReferences(nil, &protocol.ReferenceParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: defURI},
			Position:     protocol.Position{Line: 4, Character: 15},
		},
		Context: protocol.ReferenceContext{IncludeDeclaration: true},
	})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if refsContain(refs, callerURI) {
		t.Errorf("live buffer deleted the site; the stale indexed site must not return, got %+v", refs)
	}
}

// [spec feature.references/A13] — without a workspace index, references
// behave exactly as the single-file feature.
func TestHandleReferences_NilIndexSingleFile(t *testing.T) {
	s := NewSSLServer()

	source := `:PROCEDURE TargetProc;
:ENDPROC;
:PROCEDURE Caller;
:RETURN DoProc("TargetProc");
:ENDPROC;`
	s.documents.SetDocument(testURI, source, 1)
	s.documentVersion[testURI] = 1

	refs, err := s.handleReferences(nil, &protocol.ReferenceParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: testURI},
			Position:     protocol.Position{Line: 0, Character: 14},
		},
		Context: protocol.ReferenceContext{IncludeDeclaration: true},
	})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(refs) == 0 {
		t.Fatal("expected same-file references with a nil index")
	}
	for _, ref := range refs {
		if string(ref.URI) != testURI {
			t.Errorf("nil index must keep references single-file, got %s", ref.URI)
		}
	}
}

// [spec feature.references/A14] — a dotted self-site inside the definition
// file is returned exactly once (deduped against the same-file pass).
func TestHandleReferences_DottedSelfSiteOnce(t *testing.T) {
	s := NewSSLServer()
	wi, dir := newResolverIndex(t)
	s.workspaceIndex = wi
	defURI := writeAndIndex(t, wi, dir, "Server Scripts/LIMS_UTILS/HELPERS.srvscr", helperWithSelfSite)

	s.documents.SetDocument(defURI, helperWithSelfSite, 1)
	s.documentVersion[defURI] = 1

	refs, err := s.handleReferences(nil, &protocol.ReferenceParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: defURI},
			Position:     protocol.Position{Line: 1, Character: 14}, // on CalculateTotal
		},
		Context: protocol.ReferenceContext{IncludeDeclaration: true},
	})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	selfSiteLine := 7 // 0-based line of the ExecFunction call
	count := 0
	for _, ref := range refs {
		if string(ref.URI) == defURI && int(ref.Range.Start.Line) == selfSiteLine {
			count++
		}
	}
	if count != 1 {
		t.Errorf("dotted self-site must appear exactly once, got %d (refs: %+v)", count, refs)
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

// Bare 1-part dispatch targets hover with the same-file procedure's
// docblock, matched case-insensitively. [spec feature.hover/A17]
func TestHandleHover_SameFileDispatchProcedure(t *testing.T) {
	s := NewSSLServer()
	source := `/*
 * Description: Builds the shell object.
 * Parameters:
 *   oContext - context object
 * Returns: initialized shell object
;
:PROCEDURE BuildShell;
:PARAMETERS oContext;
:ENDPROC;

oResult := DoProc("buildshell", {oContext});`
	s.documents.SetDocument(testURI, source, 1)
	s.documentVersion[testURI] = 1

	hover, err := s.handleHover(nil, &protocol.HoverParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: testURI},
			Position:     protocol.Position{Line: 10, Character: 22},
		},
	})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if hover == nil {
		t.Fatal("expected same-file dispatch hover")
	}
	md := hover.Contents.(protocol.MarkupContent).Value
	for _, want := range []string{"BuildShell", "Builds the shell object.", "oContext", "initialized shell object"} {
		if !strings.Contains(md, want) {
			t.Errorf("hover missing %q:\n%s", want, md)
		}
	}
}

// A 1-part target naming no same-file procedure keeps the string
// suppression — it never resolves cross-file. [spec feature.hover/A18]
func TestHandleHover_SameFileDispatchNoMatchStaysNull(t *testing.T) {
	s := NewSSLServer()
	source := `oResult := DoProc("NoSuchProc", {});`
	s.documents.SetDocument(testURI, source, 1)
	s.documentVersion[testURI] = 1

	hover, err := s.handleHover(nil, &protocol.HoverParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: testURI},
			Position:     protocol.Position{Line: 0, Character: 22},
		},
	})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if hover != nil {
		t.Errorf("expected null hover for unmatched 1-part target, got %+v", hover)
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

// --- Cross-file rename (issue #125 Phase B, feature.rename A10-A16) ---

// renameDefScript defines CalcTotal and calls it through a dotted self-site.
const renameDefScript = `/* helpers;
:PROCEDURE CalcTotal;
:PARAMETERS nQty;
:RETURN nQty;
:ENDPROC;

:PROCEDURE Wrapper;
:RETURN ExecFunction("LIMS_UTILS.HELPERS.CalcTotal", {1});
:ENDPROC;`

const renameCallerScript = `:PROCEDURE Run;
:RETURN ExecFunction("LIMS_UTILS.HELPERS.CalcTotal", {2});
:ENDPROC;`

func renameParamsAt(uri string, line, char uint32, newName string) *protocol.RenameParams {
	return &protocol.RenameParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: uri},
			Position:     protocol.Position{Line: line, Character: char},
		},
		NewName: newName,
	}
}

// [spec feature.rename/A10] [spec feature.rename/A14] — the WorkspaceEdit
// spans the definition file (declaration, uses, dotted self-site) and the
// closed caller file (last segment only, prefix and quotes intact, exact
// supplied casing).
func TestHandleRename_CrossFileWorkspaceEdit(t *testing.T) {
	s := NewSSLServer()
	wi, dir := newResolverIndex(t)
	s.workspaceIndex = wi
	defURI := writeAndIndex(t, wi, dir, "Server Scripts/LIMS_UTILS/HELPERS.srvscr", renameDefScript)
	callerURI := writeAndIndex(t, wi, dir, "Server Scripts/ORDERS/PROCESS.srvscr", renameCallerScript)

	s.documents.SetDocument(defURI, renameDefScript, 1)
	s.documentVersion[defURI] = 1

	edit, err := s.handleRename(nil, renameParamsAt(defURI, 1, 14, "ComputeTotal"))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if edit == nil {
		t.Fatal("expected a workspace edit")
	}

	defEdits := edit.Changes[protocol.DocumentUri(defURI)]
	if len(defEdits) == 0 {
		t.Fatal("expected edits in the definition file")
	}
	// Dotted self-site: last segment on the Wrapper line (0-based line 7).
	selfSiteSeen := false
	for _, te := range defEdits {
		if te.Range.Start.Line == 7 {
			selfSiteSeen = true
			wantStart := uint32(strings.Index(`:RETURN ExecFunction("LIMS_UTILS.HELPERS.`, `"`) + 1 + len("LIMS_UTILS.HELPERS."))
			if te.Range.Start.Character != wantStart {
				t.Errorf("self-site edit start = %d, want %d (last segment only)", te.Range.Start.Character, wantStart)
			}
			if te.NewText != "ComputeTotal" {
				t.Errorf("self-site new text = %q, want exact supplied casing", te.NewText)
			}
		}
	}
	if !selfSiteSeen {
		t.Error("expected the dotted self-site to be edited")
	}

	callerEdits := edit.Changes[protocol.DocumentUri(callerURI)]
	if len(callerEdits) != 1 {
		t.Fatalf("expected exactly one caller edit, got %d", len(callerEdits))
	}
	ce := callerEdits[0]
	if ce.Range.Start.Line != 1 {
		t.Errorf("caller edit line = %d, want 1", ce.Range.Start.Line)
	}
	wantStart := uint32(len(`:RETURN ExecFunction("LIMS_UTILS.HELPERS.`))
	if ce.Range.Start.Character != wantStart {
		t.Errorf("caller edit start = %d, want %d (prefix intact)", ce.Range.Start.Character, wantStart)
	}
	if ce.Range.End.Character != wantStart+uint32(len("CalcTotal")) {
		t.Errorf("caller edit end = %d, want %d (quotes intact)", ce.Range.End.Character, wantStart+uint32(len("CalcTotal")))
	}
	if ce.NewText != "ComputeTotal" {
		t.Errorf("caller new text = %q, want ComputeTotal", ce.NewText)
	}
}

// [spec feature.rename/A11] — a site resolving to multiple candidates is
// skipped silently.
func TestHandleRename_AmbiguousSiteSkipped(t *testing.T) {
	s := NewSSLServer()
	wi, dir := newResolverIndex(t)
	s.workspaceIndex = wi
	defURI := writeAndIndex(t, wi, dir, "Server Scripts/LIMS_UTILS/HELPERS.srvscr", renameDefScript)
	writeAndIndex(t, wi, dir, "Server Scripts/OTHER/HELPERS.srvscr", `/* other helpers;
:PROCEDURE CalcTotal;
:ENDPROC;`)
	ambCallerURI := writeAndIndex(t, wi, dir, "Server Scripts/ORDERS/AMBIG.srvscr", `:PROCEDURE Run;
:RETURN DoProc("HELPERS.CalcTotal");
:ENDPROC;`)

	s.documents.SetDocument(defURI, renameDefScript, 1)
	s.documentVersion[defURI] = 1

	edit, err := s.handleRename(nil, renameParamsAt(defURI, 1, 14, "ComputeTotal"))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if edit == nil {
		t.Fatal("expected a workspace edit")
	}
	if _, present := edit.Changes[protocol.DocumentUri(ambCallerURI)]; present {
		t.Error("ambiguous site (two HELPERS scripts) must be skipped")
	}
}

// [spec feature.rename/A12] — class-file procedures refuse the cross-file
// path: declaration rename stays same-file; renaming from a caller's
// dispatch string is refused outright.
func TestHandleRename_ClassFileGate(t *testing.T) {
	s := NewSSLServer()
	wi, dir := newResolverIndex(t)
	s.workspaceIndex = wi
	classScript := `:CLASS CalcEngine;

:PROCEDURE CalcTotal;
:ENDPROC;`
	classURI := writeAndIndex(t, wi, dir, "Server Scripts/LIMS_UTILS/CALCENGINE.srvscr", classScript)
	callerScript := `:PROCEDURE Run;
:RETURN ExecFunction("LIMS_UTILS.CALCENGINE.CalcTotal", {});
:ENDPROC;`
	callerURI := writeAndIndex(t, wi, dir, "Server Scripts/ORDERS/PROCESS.srvscr", callerScript)

	// (a) From the declaration inside the class file: same-file edits only.
	s.documents.SetDocument(classURI, classScript, 1)
	s.documentVersion[classURI] = 1
	edit, err := s.handleRename(nil, renameParamsAt(classURI, 2, 14, "ComputeTotal"))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if edit == nil {
		t.Fatal("expected same-file edits for the class file itself")
	}
	if _, present := edit.Changes[protocol.DocumentUri(callerURI)]; present {
		t.Error("class-file rename must not edit cross-file call sites (D8)")
	}

	// (b) From the caller's dispatch string: refused, no edits.
	s.documents.SetDocument(callerURI, callerScript, 1)
	s.documentVersion[callerURI] = 1
	col := uint32(strings.Index(`:RETURN ExecFunction("LIMS_UTILS.CALCENGINE.CalcTotal`, "CalcTotal") + 2)
	edit, err = s.handleRename(nil, renameParamsAt(callerURI, 1, col, "ComputeTotal"))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if edit != nil {
		t.Errorf("rename from a dispatch string targeting a class-file procedure must be refused, got %+v", edit)
	}
}

// [spec feature.rename/A13] — without a workspace index, rename is
// single-file exactly as before.
func TestHandleRename_NilIndexSingleFile(t *testing.T) {
	s := NewSSLServer()
	source := `:PROCEDURE CalcTotal;
:ENDPROC;
:PROCEDURE Caller;
:RETURN DoProc("CalcTotal");
:ENDPROC;`
	s.documents.SetDocument(testURI, source, 1)
	s.documentVersion[testURI] = 1

	edit, err := s.handleRename(nil, renameParamsAt(testURI, 0, 14, "ComputeTotal"))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if edit == nil {
		t.Fatal("expected a single-file workspace edit")
	}
	if len(edit.Changes) != 1 {
		t.Errorf("nil index must keep edits single-file, got %d documents", len(edit.Changes))
	}
	if _, present := edit.Changes[protocol.DocumentUri(testURI)]; !present {
		t.Error("expected edits against the current document")
	}
}

// [spec feature.rename/A16] — edits come from current disk content: a
// caller rewritten after indexing (site moved) is edited at its fresh
// position.
func TestHandleRename_StaleIndexFreshEdit(t *testing.T) {
	s := NewSSLServer()
	wi, dir := newResolverIndex(t)
	s.workspaceIndex = wi
	defURI := writeAndIndex(t, wi, dir, "Server Scripts/LIMS_UTILS/HELPERS.srvscr", renameDefScript)
	callerPath := filepath.Join(dir, "Server Scripts", "ORDERS", "PROCESS.srvscr")
	writeAndIndex(t, wi, dir, "Server Scripts/ORDERS/PROCESS.srvscr", renameCallerScript)

	// Rewrite on disk WITHOUT re-indexing: the site moves down one line.
	moved := "/* moved;\n" + renameCallerScript
	if err := os.WriteFile(callerPath, []byte(moved), 0o644); err != nil {
		t.Fatal(err)
	}

	s.documents.SetDocument(defURI, renameDefScript, 1)
	s.documentVersion[defURI] = 1

	edit, err := s.handleRename(nil, renameParamsAt(defURI, 1, 14, "ComputeTotal"))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if edit == nil {
		t.Fatal("expected a workspace edit")
	}
	callerURI := pathToURI(callerPath)
	callerEdits := edit.Changes[protocol.DocumentUri(callerURI)]
	if len(callerEdits) != 1 {
		t.Fatalf("expected one caller edit from fresh content, got %d", len(callerEdits))
	}
	if callerEdits[0].Range.Start.Line != 2 {
		t.Errorf("edit line = %d, want 2 (fresh position, not the stale indexed line 1)", callerEdits[0].Range.Start.Line)
	}
}

// --- v0.14.0 pre-release review regressions (F1-F4) ---

// F1: a procedure whose name is a substring of the :PROCEDURE keyword must
// still locate its declaration as a whole word — not inside the keyword.
func TestHandleReferences_ProcNamedProc(t *testing.T) {
	s := NewSSLServer()
	wi, dir := newResolverIndex(t)
	s.workspaceIndex = wi
	defScript := `/* h;
:PROCEDURE Proc;
:ENDPROC;

:PROCEDURE Other;
:ENDPROC;`
	defURI := writeAndIndex(t, wi, dir, "Server Scripts/LIMS_UTILS/HELPERS.srvscr", defScript)
	callerURI := writeAndIndex(t, wi, dir, "Server Scripts/ORDERS/PROCESS.srvscr", `:PROCEDURE Run;
:RETURN ExecFunction("LIMS_UTILS.HELPERS.Proc", {});
:ENDPROC;`)

	s.documents.SetDocument(defURI, defScript, 1)
	s.documentVersion[defURI] = 1

	refs, err := s.handleReferences(nil, &protocol.ReferenceParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: defURI},
			Position:     protocol.Position{Line: 1, Character: 12},
		},
		Context: protocol.ReferenceContext{IncludeDeclaration: true},
	})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if !refsContain(refs, callerURI) {
		t.Errorf("expected caller reference for procedure named Proc, got %+v", refs)
	}
	for _, ref := range refs {
		if string(ref.URI) == defURI && ref.Range.Start.Line != 1 {
			t.Errorf("unexpected def-file reference on line %d — keyword lines must not match", ref.Range.Start.Line)
		}
	}
}

// F1 (rename side): renaming a procedure named Proc from its declaration
// must carry the caller edit, not silently fall back to same-file.
func TestHandleRename_ProcNamedProc(t *testing.T) {
	s := NewSSLServer()
	wi, dir := newResolverIndex(t)
	s.workspaceIndex = wi
	defScript := `/* h;
:PROCEDURE Proc;
:ENDPROC;`
	defURI := writeAndIndex(t, wi, dir, "Server Scripts/LIMS_UTILS/HELPERS.srvscr", defScript)
	callerURI := writeAndIndex(t, wi, dir, "Server Scripts/ORDERS/PROCESS.srvscr", `:PROCEDURE Run;
:RETURN ExecFunction("LIMS_UTILS.HELPERS.Proc", {});
:ENDPROC;`)

	s.documents.SetDocument(defURI, defScript, 1)
	s.documentVersion[defURI] = 1

	edit, err := s.handleRename(nil, renameParamsAt(defURI, 1, 12, "ComputeTotal"))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if edit == nil {
		t.Fatal("expected a workspace edit")
	}
	if _, present := edit.Changes[protocol.DocumentUri(callerURI)]; !present {
		t.Errorf("caller edit missing — declaration position matched inside the keyword? changes: %+v", edit.Changes)
	}
}

// F2: a multi-byte character in the dispatch target must not skew the
// last-segment edit range (rune columns, not byte lengths).
func TestHandleRename_MultiByteTargetEditRange(t *testing.T) {
	s := NewSSLServer()
	wi, dir := newResolverIndex(t)
	s.workspaceIndex = wi
	defURI := writeAndIndex(t, wi, dir, "Server Scripts/LIMS_UTILS/HELPERS.srvscr", renameDefScript)
	callerLine := `:RETURN DoProc("CATÉ.HELPERS.CalcTotal");`
	callerURI := writeAndIndex(t, wi, dir, "Server Scripts/ORDERS/PROCESS.srvscr",
		":PROCEDURE Run;\n"+callerLine+"\n:ENDPROC;")

	s.documents.SetDocument(defURI, renameDefScript, 1)
	s.documentVersion[defURI] = 1

	edit, err := s.handleRename(nil, renameParamsAt(defURI, 1, 14, "ComputeTotal"))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if edit == nil {
		t.Fatal("expected a workspace edit")
	}
	callerEdits := edit.Changes[protocol.DocumentUri(callerURI)]
	if len(callerEdits) != 1 {
		t.Fatalf("expected one caller edit, got %d", len(callerEdits))
	}
	wantStart := uint32(utf8.RuneCountInString(`:RETURN DoProc("CATÉ.HELPERS.`))
	ce := callerEdits[0]
	if ce.Range.Start.Character != wantStart {
		t.Errorf("edit start = %d, want %d (rune-based)", ce.Range.Start.Character, wantStart)
	}
	if ce.Range.End.Character != wantStart+uint32(len("CalcTotal")) {
		t.Errorf("edit end = %d, want %d — a skewed range eats the closing quote", ce.Range.End.Character, wantStart+uint32(len("CalcTotal")))
	}
}

// F3: a local variable shadowing a procedure's name keeps the scope-aware
// single-file rename — it must not mutate other files or the declaration.
func TestHandleRename_ShadowingVariableStaysLocal(t *testing.T) {
	s := NewSSLServer()
	wi, dir := newResolverIndex(t)
	s.workspaceIndex = wi
	defScript := `:PROCEDURE CalcTotal;
:ENDPROC;

:PROCEDURE Other;
:DECLARE CalcTotal;
CalcTotal := 5;
:ENDPROC;`
	defURI := writeAndIndex(t, wi, dir, "Server Scripts/LIMS_UTILS/HELPERS.srvscr", defScript)
	callerURI := writeAndIndex(t, wi, dir, "Server Scripts/ORDERS/PROCESS.srvscr", renameCallerScript)

	s.documents.SetDocument(defURI, defScript, 1)
	s.documentVersion[defURI] = 1

	// Cursor on the variable USE inside Other (0-based line 5).
	edit, err := s.handleRename(nil, renameParamsAt(defURI, 5, 3, "nLocalTotal"))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if edit == nil {
		t.Fatal("expected a local-variable rename edit")
	}
	if _, present := edit.Changes[protocol.DocumentUri(callerURI)]; present {
		t.Error("local-variable rename must not touch other files")
	}
	for _, te := range edit.Changes[protocol.DocumentUri(defURI)] {
		if te.Range.Start.Line == 0 {
			t.Error("local-variable rename must not touch the procedure declaration")
		}
	}
}

// F4: an UNSAVED buffer deleting a competing procedure must not make a
// disk-ambiguous site editable — the write side requires disk and buffer
// to agree.
func TestHandleRename_UnsavedOverlayCannotDisambiguateWrite(t *testing.T) {
	s := NewSSLServer()
	wi, dir := newResolverIndex(t)
	s.workspaceIndex = wi
	aScript := `/* a;
:PROCEDURE CalcTotal;
:ENDPROC;`
	aURI := writeAndIndex(t, wi, dir, "Server Scripts/A/HELPERS.srvscr", aScript)
	bScript := `/* b;
:PROCEDURE CalcTotal;
:ENDPROC;`
	bURI := writeAndIndex(t, wi, dir, "Server Scripts/B/HELPERS.srvscr", bScript)
	callerURI := writeAndIndex(t, wi, dir, "Server Scripts/ORDERS/AMBIG.srvscr", `:PROCEDURE Run;
:RETURN DoProc("HELPERS.CalcTotal");
:ENDPROC;`)

	// B open with the competing procedure deleted — unsaved.
	s.documents.SetDocument(bURI, "/* b;\n", 1)
	s.documentVersion[bURI] = 1

	s.documents.SetDocument(aURI, aScript, 1)
	s.documentVersion[aURI] = 1

	edit, err := s.handleRename(nil, renameParamsAt(aURI, 1, 14, "ComputeTotal"))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if edit == nil {
		t.Fatal("expected a workspace edit for the definition file")
	}
	if _, present := edit.Changes[protocol.DocumentUri(callerURI)]; present {
		t.Error("disk-ambiguous site edited on the strength of an unsaved buffer (F4)")
	}
}
