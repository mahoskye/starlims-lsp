package server

import (
	"strings"
	"testing"
	"unicode/utf16"

	"starlims-lsp/internal/providers"

	"github.com/tliron/glsp"
	protocol "github.com/tliron/glsp/protocol_3_16"
)

// Issue #307: every position the server sends counts UTF-16 code units.
// Each case puts an emoji (one rune, two UTF-16 units) before the target
// on the same line, where a rune column lands one unit too early.

const emoji = "\U0001F600"

// utf16At returns the UTF-16 offset of the nth (1-based) occurrence of
// sub in line.
func utf16At(t *testing.T, line, sub string, nth int) uint32 {
	t.Helper()
	from := 0
	for i := 0; i < nth; i++ {
		idx := strings.Index(line[from:], sub)
		if idx < 0 {
			t.Fatalf("occurrence %d of %q not found in %q", nth, sub, line)
		}
		from += idx
		if i < nth-1 {
			from += len(sub)
		}
	}
	return uint32(len(utf16.Encode([]rune(line[:from]))))
}

func TestUTF16Mapper(t *testing.T) {
	if m := newUTF16Mapper("sX := \"é\";\nsY := 1;"); m != nil {
		t.Fatal("BMP-only text should map as the identity (nil mapper)")
	}
	var identity *utf16Mapper
	if got := identity.character(0, 7); got != 7 {
		t.Errorf("nil mapper: character = %d, want 7", got)
	}

	m := newUTF16Mapper("a" + emoji + "b\n" + emoji + emoji + "c\nplain")
	cases := []struct{ line, col, want int }{
		{0, 0, 0},
		{0, 1, 1}, // the emoji itself
		{0, 2, 3}, // b
		{0, 3, 4}, // line end
		{0, 5, 6}, // overshoot keeps one unit per rune
		{1, 2, 4}, // c, after two emoji
		{2, 3, 3}, // line without astral characters
		{9, 4, 4}, // past the last line
	}
	for _, c := range cases {
		if got := m.character(c.line, c.col); got != c.want {
			t.Errorf("character(%d, %d) = %d, want %d", c.line, c.col, got, c.want)
		}
	}
}

// [spec feature.diagnostics_pipeline/A32] — an SSL finding.
func TestPublishDiagnostics_RangeAfterAstralCharacter(t *testing.T) {
	line := `bY := "` + emoji + `" !== sX;`
	s := newTestServerWithDocument(":DECLARE sX, bY;\n" + line)

	var published protocol.PublishDiagnosticsParams
	s.validateDocument(&glsp.Context{Notify: func(method string, params any) {
		if method == protocol.ServerTextDocumentPublishDiagnostics {
			published = params.(protocol.PublishDiagnosticsParams)
		}
	}}, testURI)

	want := utf16At(t, line, "!==", 1)
	for _, d := range published.Diagnostics {
		if d.Code != nil && d.Code.Value == "invalid_operator_sequence" {
			if d.Range.Start.Character != want || d.Range.End.Character != want+3 {
				t.Errorf("range = %d-%d, want %d-%d", d.Range.Start.Character, d.Range.End.Character, want, want+3)
			}
			return
		}
	}
	t.Fatalf("expected an invalid_operator_sequence diagnostic, got %+v", published.Diagnostics)
}

// [spec feature.diagnostics_pipeline/A32] — a SQL data-source placeholder,
// whose columns come from the SQL lexer.
func TestPublishDiagnostics_DataSourcePlaceholderAfterAstralCharacter(t *testing.T) {
	const uri = "file:///project/query.ds"
	line := `SELECT '` + emoji + `' AS a, @nOrderId FROM Orders`
	s := NewSSLServer()
	s.documents.SetDocument(uri, line, 1)
	s.documentVersion[uri] = 1

	var published protocol.PublishDiagnosticsParams
	s.validateDocument(&glsp.Context{Notify: func(method string, params any) {
		published = params.(protocol.PublishDiagnosticsParams)
	}}, uri)

	want := utf16At(t, line, "@nOrderId", 1)
	for _, d := range published.Diagnostics {
		if d.Code != nil && d.Code.Value == providers.CodeDatasourceUndeclaredPlaceholder {
			if d.Range.Start.Character != want || d.Range.End.Character != want+uint32(len("@nOrderId")) {
				t.Errorf("range = %d-%d, want %d-%d", d.Range.Start.Character, d.Range.End.Character, want, want+uint32(len("@nOrderId")))
			}
			return
		}
	}
	t.Fatalf("expected an undeclared-placeholder diagnostic, got %+v", published.Diagnostics)
}

// [spec feature.definition/A17]
func TestHandleDefinition_TargetAfterAstralCharacter(t *testing.T) {
	declLine := `/* ` + emoji + `; :DECLARE sTarget;`
	s := newTestServerWithDocument(declLine + "\nsTarget := 1;")

	result, err := s.handleDefinition(nil, &protocol.DefinitionParams{TextDocumentPositionParams: protocol.TextDocumentPositionParams{
		TextDocument: protocol.TextDocumentIdentifier{URI: testURI},
		Position:     protocol.Position{Line: 1, Character: 2},
	}})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	loc, ok := result.(protocol.Location)
	if !ok {
		t.Fatalf("expected a single Location, got %#v", result)
	}
	want := utf16At(t, declLine, "sTarget", 1)
	if loc.Range.Start.Line != 0 || loc.Range.Start.Character != want || loc.Range.End.Character != want+7 {
		t.Errorf("definition range = %+v, want line 0, %d-%d", loc.Range, want, want+7)
	}
}

// sameLineUse holds sX twice, the second after an emoji.
var sameLineUse = `sX := "` + emoji + `" + sX;`

// [spec feature.references/A17]
func TestHandleReferences_AfterAstralCharacter(t *testing.T) {
	s := newTestServerWithDocument(":DECLARE sX;\n" + sameLineUse)

	refs, err := s.handleReferences(nil, &protocol.ReferenceParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: testURI},
			Position:     protocol.Position{Line: 0, Character: 10},
		},
		Context: protocol.ReferenceContext{IncludeDeclaration: true},
	})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	want := utf16At(t, sameLineUse, "sX", 2)
	for _, ref := range refs {
		if ref.Range.Start.Line == 1 && ref.Range.Start.Character == want && ref.Range.End.Character == want+2 {
			return
		}
	}
	t.Errorf("expected a reference at line 1, %d-%d; got %+v", want, want+2, refs)
}

// [spec feature.references/A17] — a BMP character is one rune and one
// UTF-16 unit but two UTF-8 bytes: the word-match reference search used
// to report byte columns.
func TestHandleReferences_AfterTwoByteCharacter(t *testing.T) {
	line := `sX := "é" + sX;`
	s := newTestServerWithDocument(":DECLARE sX;\n" + line)

	refs, err := s.handleReferences(nil, &protocol.ReferenceParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: testURI},
			Position:     protocol.Position{Line: 0, Character: 10},
		},
		Context: protocol.ReferenceContext{IncludeDeclaration: true},
	})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	want := utf16At(t, line, "sX", 2)
	for _, ref := range refs {
		if ref.Range.Start.Line == 1 && ref.Range.Start.Character == want && ref.Range.End.Character == want+2 {
			return
		}
	}
	t.Errorf("expected a reference at line 1, %d-%d; got %+v", want, want+2, refs)
}

// [spec feature.rename/A19] — the rename edits.
func TestHandleRename_EditAfterAstralCharacter(t *testing.T) {
	s := newTestServerWithDocument(":DECLARE sX;\n" + sameLineUse)

	edit, err := s.handleRename(nil, renameParamsAt(testURI, 0, 10, "sY"))
	if err != nil || edit == nil {
		t.Fatalf("expected a workspace edit, got %v, %v", edit, err)
	}
	want := utf16At(t, sameLineUse, "sX", 2)
	for _, te := range edit.Changes[testURI] {
		if te.Range.Start.Line == 1 && te.Range.Start.Character == want && te.Range.End.Character == want+2 {
			return
		}
	}
	t.Errorf("expected an edit at line 1, %d-%d; got %+v", want, want+2, edit.Changes[testURI])
}

// [spec feature.rename/A19] — the prepare-rename range.
func TestHandlePrepareRename_RangeAfterAstralCharacter(t *testing.T) {
	s := newTestServerWithDocument(":DECLARE sX;\n" + sameLineUse)
	want := utf16At(t, sameLineUse, "sX", 2)

	result, err := s.handlePrepareRename(nil, &protocol.PrepareRenameParams{TextDocumentPositionParams: protocol.TextDocumentPositionParams{
		TextDocument: protocol.TextDocumentIdentifier{URI: testURI},
		Position:     protocol.Position{Line: 1, Character: want + 1},
	}})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	rp, ok := result.(protocol.RangeWithPlaceholder)
	if !ok {
		t.Fatalf("expected a RangeWithPlaceholder, got %#v", result)
	}
	if rp.Range.Start.Character != want || rp.Range.End.Character != want+2 {
		t.Errorf("prepare-rename range = %+v, want %d-%d", rp.Range, want, want+2)
	}
}

// [spec feature.formatting/A11]
func TestHandleFormatting_EndAfterAstralCharacter(t *testing.T) {
	last := `sX := "` + emoji + `";`
	s := newTestServerWithDocument(":DECLARE sX;\n" + last)

	edits, err := s.handleFormatting(nil, &protocol.DocumentFormattingParams{
		TextDocument: protocol.TextDocumentIdentifier{URI: testURI},
	})
	if err != nil || len(edits) != 1 {
		t.Fatalf("expected one full-document edit, got %+v, %v", edits, err)
	}
	want := uint32(len(utf16.Encode([]rune(last))))
	if end := edits[0].Range.End; end.Line != 1 || end.Character != want {
		t.Errorf("edit end = %+v, want line 1, character %d (UTF-16 length of the last line)", end, want)
	}
}

// [spec feature.document_symbols/A8]
func TestHandleDocumentSymbol_RangeAfterAstralCharacter(t *testing.T) {
	declLine := `/* ` + emoji + `; :PUBLIC sTarget;`
	s := newTestServerWithDocument(declLine)

	result, err := s.handleDocumentSymbol(nil, &protocol.DocumentSymbolParams{
		TextDocument: protocol.TextDocumentIdentifier{URI: testURI},
	})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	want := utf16At(t, declLine, "sTarget", 1)
	for _, sym := range result.([]protocol.DocumentSymbol) {
		if sym.Name == "sTarget" {
			if sym.SelectionRange.Start.Character != want || sym.SelectionRange.End.Character != want+7 {
				t.Errorf("selection range = %+v, want %d-%d", sym.SelectionRange, want, want+7)
			}
			return
		}
	}
	t.Fatalf("expected a symbol for sTarget, got %+v", result)
}

// [spec feature.inlay_hints/A9]
func TestHandleInlayHint_PositionAfterAstralCharacter(t *testing.T) {
	line := `sX := "` + emoji + `" + Left(sX, 1);`
	s := newTestServerWithDocument(":DECLARE sX;\n" + line)

	hints, err := s.handleInlayHint(nil, &InlayHintParams{
		TextDocument: TextDocumentIdentifier{URI: testURI},
		Range:        InlayHintRange{End: InlayHintPosition{Line: 2}},
	})
	if err != nil || len(hints) == 0 {
		t.Fatalf("expected inlay hints, got %+v, %v", hints, err)
	}
	want := utf16At(t, line, "sX", 2)
	if hints[0].Position.Line != 1 || hints[0].Position.Character != want {
		t.Errorf("first hint at %+v, want line 1, character %d", hints[0].Position, want)
	}
}

// [spec feature.references/A17] [spec feature.rename/A19] — a cross-file
// reference and rename edit land in a closed file, whose text comes from
// disk.
func TestCrossFile_ReferencesAndRenameAfterAstralCharacter(t *testing.T) {
	callerLine := `:RETURN "` + emoji + `" + ExecFunction("LIMS_UTILS.HELPERS.CalcTotal", {2});`
	s := NewSSLServer()
	wi, dir := newResolverIndex(t)
	s.workspaceIndex = wi
	defURI := writeAndIndex(t, wi, dir, "Server Scripts/LIMS_UTILS/HELPERS.srvscr", renameDefScript)
	callerURI := writeAndIndex(t, wi, dir, "Server Scripts/ORDERS/PROCESS.srvscr",
		":PROCEDURE Run;\n"+callerLine+"\n:ENDPROC;")
	s.documents.SetDocument(defURI, renameDefScript, 1)
	s.documentVersion[defURI] = 1

	refs, err := s.handleReferences(nil, &protocol.ReferenceParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: defURI},
			Position:     protocol.Position{Line: 1, Character: 14},
		},
		Context: protocol.ReferenceContext{IncludeDeclaration: true},
	})
	if err != nil {
		t.Fatalf("references: %v", err)
	}
	wantRef := utf16At(t, callerLine, "LIMS_UTILS", 1)
	found := false
	for _, ref := range refs {
		if string(ref.URI) == callerURI {
			found = true
			if ref.Range.Start.Character != wantRef {
				t.Errorf("caller reference start = %d, want %d", ref.Range.Start.Character, wantRef)
			}
		}
	}
	if !found {
		t.Fatalf("expected a reference in the caller file, got %+v", refs)
	}

	edit, err := s.handleRename(nil, renameParamsAt(defURI, 1, 14, "ComputeTotal"))
	if err != nil || edit == nil {
		t.Fatalf("expected a workspace edit, got %v, %v", edit, err)
	}
	callerEdits := edit.Changes[protocol.DocumentUri(callerURI)]
	if len(callerEdits) != 1 {
		t.Fatalf("expected one caller edit, got %+v", callerEdits)
	}
	wantEdit := utf16At(t, callerLine, "CalcTotal", 1)
	if r := callerEdits[0].Range; r.Start.Character != wantEdit || r.End.Character != wantEdit+uint32(len("CalcTotal")) {
		t.Errorf("caller edit = %+v, want %d-%d", r, wantEdit, wantEdit+uint32(len("CalcTotal")))
	}
}
