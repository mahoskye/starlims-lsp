package server

import (
	"strings"
	"testing"

	protocol "github.com/tliron/glsp/protocol_3_16"
)

const testURI = "file:///test.ssl"

func newTestServerWithDocument(content string) *SSLServer {
	s := NewSSLServer()
	s.documents.SetDocument(testURI, content, 1)
	s.documentVersion[testURI] = 1
	return s
}

func containsCompletionLabel(items []protocol.CompletionItem, label string) bool {
	for _, item := range items {
		if item.Label == label {
			return true
		}
	}
	return false
}

func findCompletionItem(items []protocol.CompletionItem, label string) *protocol.CompletionItem {
	for i := range items {
		if items[i].Label == label {
			return &items[i]
		}
	}
	return nil
}

func TestHandleCompletion_ReturnsItems(t *testing.T) {
	s := newTestServerWithDocument(`:PROCEDURE Test;
:DECLARE myVar;
:ENDPROC;`)

	result, err := s.handleCompletion(nil, &protocol.CompletionParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: testURI},
			Position:     protocol.Position{Line: 0, Character: 0},
		},
	})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	items, ok := result.([]protocol.CompletionItem)
	if !ok {
		t.Fatalf("expected completion items, got %T", result)
	}
	if len(items) == 0 {
		t.Fatal("expected completion items")
	}
	if !containsCompletionLabel(items, "proc") {
		t.Error("expected snippet completion 'proc'")
	}
	if !containsCompletionLabel(items, "Test") {
		t.Error("expected procedure completion 'Test'")
	}
	if !containsCompletionLabel(items, "myVar") {
		t.Error("expected variable completion 'myVar'")
	}
}

func TestHandleCompletion_InDoProcString_OffersProcedureNames(t *testing.T) {
	// vs-code-ssl-formatter#74 — when the cursor is inside the string argument
	// of DoProc("…"), the LSP must surface procedures defined in the current
	// script. The default in-string behavior is to suppress completions, so
	// this exercises the explicit exception.
	s := newTestServerWithDocument(`:PROCEDURE Greet;
:ENDPROC;

:PROCEDURE Caller;
DoProc("");
:ENDPROC;`)

	// Cursor sits between the two quotes on line 5 (0-based: 4), column 9
	// (0-based: 8) — i.e. just after the opening quote of "".
	result, err := s.handleCompletion(nil, &protocol.CompletionParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: testURI},
			Position:     protocol.Position{Line: 4, Character: 8},
		},
	})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	items, ok := result.([]protocol.CompletionItem)
	if !ok {
		t.Fatalf("expected completion items, got %T", result)
	}
	if !containsCompletionLabel(items, "Greet") {
		t.Errorf("expected 'Greet' procedure in DoProc string completion list, got %d items", len(items))
	}
	// The insert text should be the bare name, not a DoProc(...) snippet.
	item := findCompletionItem(items, "Greet")
	if item == nil {
		t.Fatal("Greet completion item missing")
	}
	if item.InsertText == nil || *item.InsertText != "Greet" {
		t.Errorf("expected bare-name insert text 'Greet', got %#v", item.InsertText)
	}
}

func TestHandleCompletion_InExecFunctionString_OffersProcedureNames(t *testing.T) {
	s := newTestServerWithDocument(`:PROCEDURE Greet;
:ENDPROC;

:PROCEDURE Caller;
ExecFunction("");
:ENDPROC;`)

	result, err := s.handleCompletion(nil, &protocol.CompletionParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: testURI},
			Position:     protocol.Position{Line: 4, Character: 14},
		},
	})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	items, ok := result.([]protocol.CompletionItem)
	if !ok {
		t.Fatalf("expected completion items, got %T", result)
	}
	if !containsCompletionLabel(items, "Greet") {
		t.Errorf("expected 'Greet' in ExecFunction string completion list, got %d items", len(items))
	}
}

func TestHandleCompletion_InDoProcString_CaseInsensitive(t *testing.T) {
	// SSL identifiers are case-insensitive; `DOPROC`, `doproc`, `DoProc`
	// must all open the procedure-name completion.
	for _, fname := range []string{"DOPROC", "doproc", "DoProc"} {
		src := ":PROCEDURE Greet;\n:ENDPROC;\n\n:PROCEDURE Caller;\n" + fname + "(\"\");\n:ENDPROC;"
		s := newTestServerWithDocument(src)
		col := len(fname) + 2 // position right after the opening quote
		result, err := s.handleCompletion(nil, &protocol.CompletionParams{
			TextDocumentPositionParams: protocol.TextDocumentPositionParams{
				TextDocument: protocol.TextDocumentIdentifier{URI: testURI},
				Position:     protocol.Position{Line: 4, Character: uint32(col)},
			},
		})
		if err != nil {
			t.Fatalf("[%s] unexpected error: %v", fname, err)
		}
		items := result.([]protocol.CompletionItem)
		if !containsCompletionLabel(items, "Greet") {
			t.Errorf("[%s] expected 'Greet' in completion list", fname)
		}
	}
}

// [spec feature.snippets/A6] — the comment half: no items inside comments.
func TestHandleCompletion_InComment_NoProcedureNames(t *testing.T) {
	// Comment context still suppresses completions — the DoProc exception
	// is string-only.
	s := newTestServerWithDocument(`:PROCEDURE Greet;
:ENDPROC;

/* DoProc("") ;
:PROCEDURE Caller;
:ENDPROC;`)
	// Cursor inside the empty string within the comment.
	result, err := s.handleCompletion(nil, &protocol.CompletionParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: testURI},
			Position:     protocol.Position{Line: 3, Character: 12},
		},
	})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	items := result.([]protocol.CompletionItem)
	if len(items) != 0 {
		t.Errorf("expected no completions inside comment, got %d", len(items))
	}
}

func TestHandleCompletion_InDoProcString_NoProceduresDefined(t *testing.T) {
	// Script with no procedures — the in-string exception must return an
	// empty list, not crash and not fall through to keyword completions.
	s := newTestServerWithDocument(`DoProc("");`)
	result, err := s.handleCompletion(nil, &protocol.CompletionParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: testURI},
			Position:     protocol.Position{Line: 0, Character: 8},
		},
	})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	items := result.([]protocol.CompletionItem)
	if len(items) != 0 {
		t.Errorf("expected empty completion list, got %d items", len(items))
	}
}

func TestHandleCompletion_InNestedDoProcString_OffersProcedureNames(t *testing.T) {
	// DoProc nested inside another call: `Foo(DoProc(""))`. The cursor
	// is inside the inner DoProc's first string; the previous-token
	// chain should still resolve to the inner DoProc.
	s := newTestServerWithDocument(`:PROCEDURE Greet;
:ENDPROC;

:PROCEDURE Caller;
Foo(DoProc(""));
:ENDPROC;`)
	// Cursor inside the inner quotes (line 4, col 12).
	result, err := s.handleCompletion(nil, &protocol.CompletionParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: testURI},
			Position:     protocol.Position{Line: 4, Character: 12},
		},
	})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	items := result.([]protocol.CompletionItem)
	if !containsCompletionLabel(items, "Greet") {
		t.Errorf("expected 'Greet' inside nested DoProc string, got %d items", len(items))
	}
}

func TestHandleCompletion_InDoProcString_AtOpeningQuoteBoundary(t *testing.T) {
	// Cursor sitting at the opening quote position (col 8, 0-based: 7).
	// Per GetContextAtPosition, the column == tokenStartCol case counts as
	// inside the string, so the exception applies.
	s := newTestServerWithDocument(`:PROCEDURE Greet;
:ENDPROC;

:PROCEDURE Caller;
DoProc("");
:ENDPROC;`)
	result, err := s.handleCompletion(nil, &protocol.CompletionParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: testURI},
			Position:     protocol.Position{Line: 4, Character: 7},
		},
	})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	items := result.([]protocol.CompletionItem)
	if !containsCompletionLabel(items, "Greet") {
		t.Errorf("expected 'Greet' at opening-quote boundary, got %d items", len(items))
	}
}

func TestHandleCompletion_AfterClosingQuote_NoLongerInString(t *testing.T) {
	// Cursor right after the closing quote — we're no longer in the
	// string context, so the DoProc exception must NOT fire.
	s := newTestServerWithDocument(`:PROCEDURE Greet;
:ENDPROC;

:PROCEDURE Caller;
DoProc("name");
:ENDPROC;`)
	// `DoProc("name")` — closing `"` at col 13 (1-based: 14). Cursor at
	// col 14 (0-based) is immediately after the closing quote.
	result, err := s.handleCompletion(nil, &protocol.CompletionParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: testURI},
			Position:     protocol.Position{Line: 4, Character: 14},
		},
	})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	items := result.([]protocol.CompletionItem)
	// Outside the string, fall through to general completions — these
	// will include keywords/snippets but NOT the bare-name procedure
	// completion (that one carries InsertText = bare proc name).
	for _, it := range items {
		if it.Label == "Greet" && it.InsertText != nil && *it.InsertText == "Greet" {
			t.Errorf("did not expect bare-name DoProc completion outside the string; items=%d", len(items))
		}
	}
}

func TestHandleCompletion_DoProcWithVariableName_NoTrigger(t *testing.T) {
	// `DoProc(sName, {arg})` — first arg is an identifier, not a string
	// literal. The cursor sits inside a later string literal that is the
	// args array. The DoProc exception must NOT trigger because the
	// preceding string token isn't the FIRST positional arg of DoProc.
	s := newTestServerWithDocument(`:PROCEDURE Caller;
:DECLARE sName;
sName := "Greet";
DoProc(sName, {""});
:ENDPROC;`)
	// Cursor inside the literal "" on line 3 (0-based), col 16.
	result, err := s.handleCompletion(nil, &protocol.CompletionParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: testURI},
			Position:     protocol.Position{Line: 3, Character: 16},
		},
	})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	items := result.([]protocol.CompletionItem)
	if len(items) != 0 {
		t.Errorf("expected no completion (not in DoProc's first-arg string), got %d items", len(items))
	}
}

// [spec feature.completion/A5] — the comment half of the criterion is
// exercised by TestHandleCompletion_InComment_NoProcedureNames above.
// [spec feature.snippets/A6] — no snippet (or other) items inside plain
// string literals.
func TestHandleCompletion_InRegularString_NoCompletions(t *testing.T) {
	// Sanity check: a plain string literal (not DoProc/ExecFunction) still
	// suppresses completions.
	s := newTestServerWithDocument(`:PROCEDURE Caller;
x := "";
:ENDPROC;`)

	result, err := s.handleCompletion(nil, &protocol.CompletionParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: testURI},
			Position:     protocol.Position{Line: 1, Character: 6},
		},
	})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	items, ok := result.([]protocol.CompletionItem)
	if !ok {
		t.Fatalf("expected completion items, got %T", result)
	}
	if len(items) != 0 {
		t.Errorf("expected no completions inside non-DoProc string, got %d", len(items))
	}
}

func TestHandleCompletion_ClassMethodUsesMeSnippet(t *testing.T) {
	s := newTestServerWithDocument(`:CLASS MyClass;

:PROCEDURE Helper;
:ENDPROC;

:PROCEDURE Main;
    Hel
:ENDPROC;`)

	result, err := s.handleCompletion(nil, &protocol.CompletionParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: testURI},
			Position:     protocol.Position{Line: 5, Character: 7},
		},
	})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	items, ok := result.([]protocol.CompletionItem)
	if !ok {
		t.Fatalf("expected completion items, got %T", result)
	}

	item := findCompletionItem(items, "Helper")
	if item == nil {
		t.Fatal("expected method completion for 'Helper'")
	}

	if item.InsertText == nil || *item.InsertText != "Me:Helper()" {
		t.Fatalf("expected Me:Helper() insert text, got %#v", item.InsertText)
	}
}

func TestHandleHover_Keyword(t *testing.T) {
	s := newTestServerWithDocument(`:PROCEDURE Test;
:DECLARE myVar;
:ENDPROC;`)

	hover, err := s.handleHover(nil, &protocol.HoverParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: testURI},
			Position:     protocol.Position{Line: 1, Character: 2},
		},
	})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if hover == nil {
		t.Fatal("expected hover content")
	}
	content, ok := hover.Contents.(protocol.MarkupContent)
	if !ok {
		t.Fatalf("expected markup content, got %T", hover.Contents)
	}
	if !strings.Contains(strings.ToUpper(content.Value), "DECLARE") {
		t.Errorf("expected hover to mention DECLARE, got %q", content.Value)
	}
}

// TestHandleHover_InsideString_NoGeneralHover: general symbol hover must not
// fire inside string literals — SQL strings legitimately contain
// function-like words. Only SQL placeholder hover is allowed there, and this
// string has none. [spec feature.hover/A7]
func TestHandleHover_InsideString_NoGeneralHover(t *testing.T) {
	s := newTestServerWithDocument(`x := "SQLExecute is a function";`)

	hover, err := s.handleHover(nil, &protocol.HoverParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: testURI},
			// Over "SQLExecute" inside the string
			Position: protocol.Position{Line: 0, Character: 9},
		},
	})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if hover != nil {
		t.Fatalf("expected nil hover inside string, got %+v", hover)
	}
}

// TestHandleHover_InsideComment_NoHover: hover must not activate inside
// comments. [spec feature.hover/A7]
func TestHandleHover_InsideComment_NoHover(t *testing.T) {
	s := newTestServerWithDocument(`/* SQLExecute would be here;`)

	hover, err := s.handleHover(nil, &protocol.HoverParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: testURI},
			// Over "SQLExecute" inside the comment
			Position: protocol.Position{Line: 0, Character: 5},
		},
	})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if hover != nil {
		t.Fatalf("expected nil hover inside comment, got %+v", hover)
	}
}

func TestHandleHover_NonexistentDocument(t *testing.T) {
	s := NewSSLServer()

	hover, err := s.handleHover(nil, &protocol.HoverParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: testURI},
			Position:     protocol.Position{Line: 0, Character: 0},
		},
	})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if hover != nil {
		t.Fatalf("expected nil hover for missing document")
	}
}

func TestHandleDefinition_Procedure(t *testing.T) {
	s := newTestServerWithDocument(`:PROCEDURE MyProc;
:ENDPROC;

:PROCEDURE Test;
MyProc();
:ENDPROC;`)

	result, err := s.handleDefinition(nil, &protocol.DefinitionParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: testURI},
			Position:     protocol.Position{Line: 4, Character: 1},
		},
	})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	location, ok := result.(protocol.Location)
	if !ok {
		t.Fatalf("expected protocol.Location, got %T", result)
	}
	if location.Range.Start.Line != 0 {
		t.Errorf("expected definition on line 0, got %d", location.Range.Start.Line)
	}
}

func TestHandleDefinition_MissingDocument(t *testing.T) {
	s := NewSSLServer()

	result, err := s.handleDefinition(nil, &protocol.DefinitionParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: testURI},
			Position:     protocol.Position{Line: 0, Character: 0},
		},
	})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if result != nil {
		t.Fatalf("expected nil definition for missing document")
	}
}

// [spec feature.references/A1]
func TestHandleReferences_Variable(t *testing.T) {
	s := newTestServerWithDocument(`:PROCEDURE Test;
:DECLARE myVar;
myVar := 1;
value := myVar + 1;
:ENDPROC;`)

	locations, err := s.handleReferences(nil, &protocol.ReferenceParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: testURI},
			Position:     protocol.Position{Line: 1, Character: 10},
		},
		Context: protocol.ReferenceContext{IncludeDeclaration: true},
	})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	// myVar appears 3 times: :DECLARE, assignment, and usage in value expression
	const expectedReferences = 3
	if len(locations) != expectedReferences {
		t.Errorf("expected %d references to myVar, got %d", expectedReferences, len(locations))
	}
}

func TestHandleDocumentSymbol(t *testing.T) {
	s := newTestServerWithDocument(`:PROCEDURE TestProc;
:ENDPROC;`)

	result, err := s.handleDocumentSymbol(nil, &protocol.DocumentSymbolParams{
		TextDocument: protocol.TextDocumentIdentifier{URI: testURI},
	})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	items, ok := result.([]protocol.DocumentSymbol)
	if !ok {
		t.Fatalf("expected document symbols, got %T", result)
	}
	if len(items) == 0 {
		t.Fatal("expected at least one document symbol")
	}
	if items[0].Name != "TestProc" {
		t.Errorf("expected procedure symbol TestProc, got %q", items[0].Name)
	}
}

// [spec feature.workspace_symbols/A1] — case-insensitive substring match
// over open documents, kind Function, correct URI.
// [spec feature.workspace_symbols/A7] — no workspace root configured (no
// index): only open documents are consulted and the request succeeds.
func TestHandleWorkspaceSymbol_ProceduresOnly(t *testing.T) {
	s := newTestServerWithDocument(`:PROCEDURE ProcA;
:ENDPROC;
:PROCEDURE ProcB;
:ENDPROC;`)
	otherURI := "file:///other.ssl"
	s.documents.SetDocument(otherURI, ":PROCEDURE ProcC;:ENDPROC;", 1)
	s.documentVersion[otherURI] = 1

	results, err := s.handleWorkspaceSymbol(nil, &protocol.WorkspaceSymbolParams{Query: "procb"})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(results) != 1 {
		t.Fatalf("expected 1 result, got %d", len(results))
	}
	if results[0].Name != "ProcB" {
		t.Errorf("expected ProcB, got %q", results[0].Name)
	}
	if results[0].Location.URI != testURI {
		t.Errorf("expected URI %s, got %s", testURI, results[0].Location.URI)
	}
	if results[0].Kind != protocol.SymbolKindFunction {
		t.Errorf("expected function symbol kind, got %v", results[0].Kind)
	}
}

// TestHandleWorkspaceSymbol_OpenClassDocumentReportsMethod: procedures in an
// open :CLASS document report kind Method (6), matching the workspace
// index's classification for the same file when closed (issue #45).
func TestHandleWorkspaceSymbol_OpenClassDocumentReportsMethod(t *testing.T) {
	s := newTestServerWithDocument(`:CLASS UserRecord;
:PROCEDURE Load;
:ENDPROC;`)

	results, err := s.handleWorkspaceSymbol(nil, &protocol.WorkspaceSymbolParams{Query: "load"})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(results) != 1 {
		t.Fatalf("expected 1 result, got %d", len(results))
	}
	if results[0].Kind != protocol.SymbolKindMethod {
		t.Errorf("expected method symbol kind for class member, got %v", results[0].Kind)
	}
}

func TestHandleFoldingRange(t *testing.T) {
	s := newTestServerWithDocument(`/* region Sample;
:PROCEDURE Test;
value := 1;
:ENDPROC;
/* endregion;`)

	ranges, err := s.handleFoldingRange(nil, &protocol.FoldingRangeParams{
		TextDocument: protocol.TextDocumentIdentifier{URI: testURI},
	})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(ranges) == 0 {
		t.Fatal("expected folding ranges")
	}
	// Should have at least 2 ranges: region block and procedure block
	const minExpectedRanges = 2
	if len(ranges) < minExpectedRanges {
		t.Errorf("expected at least %d folding ranges (region + procedure), got %d",
			minExpectedRanges, len(ranges))
	}
	// Verify all ranges have valid structure (start <= end)
	for i, r := range ranges {
		if r.StartLine > r.EndLine {
			t.Errorf("folding range %d has invalid lines: start=%d > end=%d",
				i, r.StartLine, r.EndLine)
		}
	}
}

// [spec feature.signature_help/A1]
func TestHandleSignatureHelp(t *testing.T) {
	s := newTestServerWithDocument(`result := Len("hello", 123);`)

	help, err := s.handleSignatureHelp(nil, &protocol.SignatureHelpParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: testURI},
			Position:     protocol.Position{Line: 0, Character: 18},
		},
	})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if help == nil {
		t.Fatal("expected signature help")
	}
	if len(help.Signatures) == 0 {
		t.Fatal("expected at least one signature")
	}
	// Verify signature has meaningful content
	sig := help.Signatures[0]
	if sig.Label == "" {
		t.Error("expected non-empty signature label")
	}
	// Len function should have parameter information
	if len(sig.Parameters) > 0 {
		param := sig.Parameters[0]
		if param.Label == nil {
			t.Error("expected parameter to have a label")
		}
	}
}

func TestHandleFormatting(t *testing.T) {
	s := newTestServerWithDocument(`:PROCEDURE Test;:ENDPROC;`)

	edits, err := s.handleFormatting(nil, &protocol.DocumentFormattingParams{
		TextDocument: protocol.TextDocumentIdentifier{URI: testURI},
	})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(edits) == 0 {
		t.Fatal("expected formatting edits")
	}
	if !strings.Contains(edits[0].NewText, "\n") {
		t.Errorf("expected formatted output to contain newline")
	}
}

func TestHandleRangeFormatting(t *testing.T) {
	s := newTestServerWithDocument(`:PROCEDURE Test;
:DECLARE x;
:ENDPROC;`)

	edits, err := s.handleRangeFormatting(nil, &protocol.DocumentRangeFormattingParams{
		TextDocument: protocol.TextDocumentIdentifier{URI: testURI},
		Range: protocol.Range{
			Start: protocol.Position{Line: 1, Character: 0},
			End:   protocol.Position{Line: 1, Character: 12},
		},
	})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(edits) == 0 {
		t.Fatal("expected range formatting edits")
	}
	if !strings.Contains(edits[0].NewText, ":DECLARE") {
		t.Errorf("expected formatted range to include DECLARE, got %q", edits[0].NewText)
	}
}

func TestHandleCompletion_ConstructorContext(t *testing.T) {
	// Cursor is right after `Email{` on line 1; expect constructor signatures only.
	s := newTestServerWithDocument(`oEmail := Email{`)

	result, err := s.handleCompletion(nil, &protocol.CompletionParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: testURI},
			Position:     protocol.Position{Line: 0, Character: 16},
		},
	})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	items, ok := result.([]protocol.CompletionItem)
	if !ok {
		t.Fatalf("expected completion items, got %T", result)
	}
	if len(items) == 0 {
		t.Fatal("expected constructor completions for Email{")
	}
	for _, item := range items {
		if !strings.HasPrefix(item.Label, "Email{") {
			t.Errorf("expected only Email constructor labels, got %q", item.Label)
		}
	}
}

func TestHandleCompletion_BuiltinClassMemberContext(t *testing.T) {
	// `Email:` directly suggests Email class members.
	s := newTestServerWithDocument(`Email:`)

	result, err := s.handleCompletion(nil, &protocol.CompletionParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: testURI},
			Position:     protocol.Position{Line: 0, Character: 6},
		},
	})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	items, ok := result.([]protocol.CompletionItem)
	if !ok {
		t.Fatalf("expected completion items, got %T", result)
	}
	if len(items) == 0 {
		t.Fatal("expected member completions for Email:")
	}
	for _, item := range items {
		detail := ""
		if item.Detail != nil {
			detail = *item.Detail
		}
		if !strings.HasPrefix(detail, "Email ") {
			t.Errorf("expected detail to start with 'Email ', got %q", detail)
		}
	}
}

// [spec feature.completion/A4]
func TestHandleCompletion_MeColonInsideClass(t *testing.T) {
	// Inside :CLASS Email, typing `Me:` should suggest Email members.
	s := newTestServerWithDocument(`:CLASS Email;
:PROCEDURE Demo;
    Me:
:ENDPROC;`)

	result, err := s.handleCompletion(nil, &protocol.CompletionParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: testURI},
			Position:     protocol.Position{Line: 2, Character: 7},
		},
	})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	items, ok := result.([]protocol.CompletionItem)
	if !ok {
		t.Fatalf("expected completion items, got %T", result)
	}
	if len(items) == 0 {
		t.Fatal("expected Me: member completions inside :CLASS Email")
	}
	// Email has SendOutgoing or similar — confirm at least one method-kind item.
	hasMethod := false
	for _, item := range items {
		if item.Kind != nil && *item.Kind == protocol.CompletionItemKindMethod {
			hasMethod = true
			break
		}
	}
	if !hasMethod {
		t.Error("expected at least one method-kind completion for Me:")
	}

	// Outside a :CLASS file, class-context forms must NOT be suggested.
	s = newTestServerWithDocument(`:PROCEDURE Test;
:ENDPROC;`)
	result, err = s.handleCompletion(nil, &protocol.CompletionParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: testURI},
			Position:     protocol.Position{Line: 0, Character: 0},
		},
	})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	items, ok = result.([]protocol.CompletionItem)
	if !ok {
		t.Fatalf("expected completion items, got %T", result)
	}
	for _, label := range []string{"Me", "Base", "Constructor"} {
		if containsCompletionLabel(items, label) {
			t.Errorf("did not expect class-context completion %q outside a :CLASS file", label)
		}
	}
}

// Issue #11: ':' trigger should NOT surface keyword completions when the
// preceding character is non-whitespace (e.g. unknown identifier `foo:`).
//
// [spec feature.completion/A6]
func TestHandleCompletion_ColonTriggerSuppressedAfterIdentifier(t *testing.T) {
	s := newTestServerWithDocument(`foo:`)

	result, err := s.handleCompletion(nil, &protocol.CompletionParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: testURI},
			Position:     protocol.Position{Line: 0, Character: 4},
		},
		Context: &protocol.CompletionContext{
			TriggerKind:      protocol.CompletionTriggerKindTriggerCharacter,
			TriggerCharacter: ptrTo(":"),
		},
	})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	items, ok := result.([]protocol.CompletionItem)
	if !ok {
		t.Fatalf("expected completion items, got %T", result)
	}
	if len(items) != 0 {
		t.Errorf("expected zero completions for unknown 'foo:' trigger, got %d", len(items))
	}
}

// Issue #11: ':' trigger SHOULD surface keyword completions when ':' begins
// a new token (preceded by whitespace or SOL).
//
// [spec feature.completion/A1]
func TestHandleCompletion_ColonTriggerOffersKeywordsAtStartOfLine(t *testing.T) {
	s := newTestServerWithDocument(`:`)

	result, err := s.handleCompletion(nil, &protocol.CompletionParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: testURI},
			Position:     protocol.Position{Line: 0, Character: 1},
		},
		Context: &protocol.CompletionContext{
			TriggerKind:      protocol.CompletionTriggerKindTriggerCharacter,
			TriggerCharacter: ptrTo(":"),
		},
	})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	items, ok := result.([]protocol.CompletionItem)
	if !ok {
		t.Fatalf("expected completion items, got %T", result)
	}
	if len(items) == 0 {
		t.Fatal("expected keyword completions when ':' starts a line")
	}
	if !containsCompletionLabel(items, ":IF") {
		t.Error("expected :IF among keyword completions")
	}
	// Keyword items ONLY — no procedures, variables, or snippets.
	for _, item := range items {
		if item.Kind == nil || *item.Kind != protocol.CompletionItemKindKeyword {
			t.Errorf("expected only keyword items on ':' trigger, got %q", item.Label)
		}
	}
	// Accepting an item must yield a single leading ':' — the TextEdit
	// replaces the typed ':' rather than appending after it.
	item := findCompletionItem(items, ":IF")
	if item == nil {
		t.Fatal("expected :IF in keyword completions")
	}
	edit, ok := item.TextEdit.(protocol.TextEdit)
	if !ok {
		t.Fatalf("expected TextEdit on :IF, got %T", item.TextEdit)
	}
	if edit.Range.Start.Character != 0 || edit.Range.End.Character != 1 {
		t.Errorf("expected TextEdit to replace the typed ':' (0-1), got (%d-%d)", edit.Range.Start.Character, edit.Range.End.Character)
	}
	if edit.NewText != ":IF" {
		t.Errorf("expected TextEdit NewText ':IF', got %q", edit.NewText)
	}
}

// Issue #12: keyword completions returned for a ':' trigger must carry a
// TextEdit that replaces the typed ':' with ':KEYWORD' so the editor cannot
// produce '::KEYWORD'.
func TestHandleCompletion_ColonTriggerKeywordsHaveReplacingTextEdit(t *testing.T) {
	s := newTestServerWithDocument(`    :`)

	result, err := s.handleCompletion(nil, &protocol.CompletionParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: testURI},
			Position:     protocol.Position{Line: 0, Character: 5},
		},
		Context: &protocol.CompletionContext{
			TriggerKind:      protocol.CompletionTriggerKindTriggerCharacter,
			TriggerCharacter: ptrTo(":"),
		},
	})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	items, ok := result.([]protocol.CompletionItem)
	if !ok {
		t.Fatalf("expected completion items, got %T", result)
	}
	item := findCompletionItem(items, ":IF")
	if item == nil {
		t.Fatal("expected :IF in keyword completions")
	}
	edit, ok := item.TextEdit.(protocol.TextEdit)
	if !ok {
		t.Fatalf("expected TextEdit on :IF, got %T", item.TextEdit)
	}
	if edit.Range.Start.Line != 0 || edit.Range.Start.Character != 4 {
		t.Errorf("expected TextEdit start at (0,4), got (%d,%d)", edit.Range.Start.Line, edit.Range.Start.Character)
	}
	if edit.Range.End.Line != 0 || edit.Range.End.Character != 5 {
		t.Errorf("expected TextEdit end at (0,5), got (%d,%d)", edit.Range.End.Line, edit.Range.End.Character)
	}
	if edit.NewText != ":IF" {
		t.Errorf("expected TextEdit NewText ':IF', got %q", edit.NewText)
	}
}

// PR #10 (issues #8/#9): ':' is the only advertised completion trigger
// character — ',', '.', and '(' must never auto-open completion.
//
// [spec feature.completion/A2]
func TestHandleInitialize_OnlyColonTriggersCompletion(t *testing.T) {
	s := NewSSLServer()

	result, err := s.handleInitialize(nil, &protocol.InitializeParams{})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	init, ok := result.(ExtendedInitializeResult)
	if !ok {
		t.Fatalf("expected ExtendedInitializeResult, got %T", result)
	}
	if init.Capabilities.CompletionProvider == nil {
		t.Fatal("expected completion provider capability")
	}
	triggers := init.Capabilities.CompletionProvider.TriggerCharacters
	if len(triggers) != 1 || triggers[0] != ":" {
		t.Errorf("expected trigger characters [\":\"], got %v", triggers)
	}
}

// Issue #9 / PR #10: signature help advertises no trigger characters by
// default; opting in via ssl.intellisense.signatureHelp.autoTrigger
// advertises '(' and ',' (with ',' as retrigger).
//
// [spec feature.signature_help/A6]
func TestHandleInitialize_SignatureHelpTriggerCharacters(t *testing.T) {
	// Default: no trigger characters.
	s := NewSSLServer()
	result, err := s.handleInitialize(nil, &protocol.InitializeParams{})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	init, ok := result.(ExtendedInitializeResult)
	if !ok {
		t.Fatalf("expected ExtendedInitializeResult, got %T", result)
	}
	sigOpts := init.Capabilities.SignatureHelpProvider
	if sigOpts == nil {
		t.Fatal("expected signature help provider capability")
	}
	if len(sigOpts.TriggerCharacters) != 0 {
		t.Errorf("expected no trigger characters by default, got %v", sigOpts.TriggerCharacters)
	}

	// Opt-in: '(' and ',' advertised, ',' as retrigger.
	s2 := NewSSLServer()
	s2.settings.SignatureHelpAutoTrigger = true
	result2, err := s2.handleInitialize(nil, &protocol.InitializeParams{})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	init2 := result2.(ExtendedInitializeResult)
	sigOpts2 := init2.Capabilities.SignatureHelpProvider
	if sigOpts2 == nil {
		t.Fatal("expected signature help provider capability")
	}
	if len(sigOpts2.TriggerCharacters) != 2 || sigOpts2.TriggerCharacters[0] != "(" || sigOpts2.TriggerCharacters[1] != "," {
		t.Errorf("expected trigger characters [\"(\" \",\"], got %v", sigOpts2.TriggerCharacters)
	}
	if len(sigOpts2.RetriggerCharacters) != 1 || sigOpts2.RetriggerCharacters[0] != "," {
		t.Errorf("expected retrigger characters [\",\"], got %v", sigOpts2.RetriggerCharacters)
	}
}

// [spec feature.hover/A15]
func TestHandleHover_UDObjectMember(t *testing.T) {
	s := newTestServerWithDocument(`:PROCEDURE Main;
:DECLARE oObj, Unknown;
oObj := CreateUDObject({{"Name", "x"}});
oObj:Total := 5;
nVal := oObj:Total;
sName := oObj:Name;
x := oObj:Unknown;
:ENDPROC;`)

	hover, err := s.handleHover(nil, &protocol.HoverParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: testURI},
			Position:     protocol.Position{Line: 4, Character: 14},
		},
	})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if hover == nil {
		t.Fatal("expected member hover for shaped receiver")
	}
	md := hover.Contents.(protocol.MarkupContent).Value
	for _, want := range []string{"Total", "number", "oObj", "line 4"} {
		if !strings.Contains(md, want) {
			t.Errorf("member hover missing %q:\n%s", want, md)
		}
	}
}

// [spec feature.hover/A16]
func TestHandleHover_UDObjectMemberUnknown_Null(t *testing.T) {
	s := newTestServerWithDocument(`:PROCEDURE Main;
:DECLARE oObj, Unknown;
oObj := CreateUDObject({{"Name", "x"}});
x := oObj:Unknown;
:ENDPROC;`)

	hover, err := s.handleHover(nil, &protocol.HoverParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: testURI},
			Position:     protocol.Position{Line: 3, Character: 12},
		},
	})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if hover != nil {
		t.Errorf("expected null hover for unknown member on shaped receiver, got %+v", hover.Contents)
	}
}

// A SQL-mode data source (plain SQL, or builder directives followed by SQL)
// returns no formatting edits — the SSL formatter would inject semicolons
// and re-case bind variables. [spec feature.formatting/A9]
func TestHandleFormatting_SQLModeDataSourceReturnsNoEdits(t *testing.T) {
	const dsURI = "file:///query.ds"
	contents := []string{
		"SELECT s.sample_id, s.sample_name\nFROM samples s\nWHERE s.sample_status = :status\nORDER BY s.sample_id\n",
		":DSN := myConnection;\n:TABLENAME := samples;\nSELECT sample_id FROM samples WHERE sample_status = ?\n",
	}
	for _, content := range contents {
		s := NewSSLServer()
		s.documents.SetDocument(dsURI, content, 1)
		s.documentVersion[dsURI] = 1

		edits, err := s.handleFormatting(nil, &protocol.DocumentFormattingParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: dsURI},
		})
		if err != nil {
			t.Fatalf("unexpected error: %v", err)
		}
		if len(edits) != 0 {
			t.Errorf("expected no edits for SQL-mode data source, got %d:\n%s", len(edits), edits[0].NewText)
		}

		rangeEdits, err := s.handleRangeFormatting(nil, &protocol.DocumentRangeFormattingParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: dsURI},
			Range: protocol.Range{
				Start: protocol.Position{Line: 0, Character: 0},
				End:   protocol.Position{Line: 1, Character: 0},
			},
		})
		if err != nil {
			t.Fatalf("unexpected error: %v", err)
		}
		if len(rangeEdits) != 0 {
			t.Errorf("expected no range edits for SQL-mode data source, got %d", len(rangeEdits))
		}
	}
}

// An SSL-mode data source still formats normally. [spec feature.formatting/A9]
func TestHandleFormatting_SSLDataSourceStillFormats(t *testing.T) {
	const dsURI = "file:///logic.ds"
	s := NewSSLServer()
	s.documents.SetDocument(dsURI, ":PARAMETERS sStatus := \"A\";\n:DECLARE aRes;\naRes:=SQLExecute(\"SELECT 1 FROM DUAL\");\n", 1)
	s.documentVersion[dsURI] = 1

	edits, err := s.handleFormatting(nil, &protocol.DocumentFormattingParams{
		TextDocument: protocol.TextDocumentIdentifier{URI: dsURI},
	})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(edits) == 0 {
		t.Fatal("expected edits for SSL-mode data source")
	}
	if !strings.Contains(edits[0].NewText, "aRes := SQLExecute") {
		t.Errorf("expected operator spacing applied:\n%s", edits[0].NewText)
	}
}

// --- Issue #123 piece D: returns-category member surface ---

func TestHandleCompletion_EndpointResponseMembers(t *testing.T) {
	s := newTestServerWithDocument(`/*
 * Endpoint: HandleUpload
;
:PROCEDURE Handle;
Response:
:ENDPROC;`)

	result, err := s.handleCompletion(nil, &protocol.CompletionParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: testURI},
			Position:     protocol.Position{Line: 4, Character: 9},
		},
	})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	items, ok := result.([]protocol.CompletionItem)
	if !ok {
		t.Fatalf("expected completion items, got %T", result)
	}
	item := findCompletionItem(items, "Redirect")
	if item == nil {
		t.Fatalf("expected SSLResponse member completion Redirect, got %d items", len(items))
	}
	if item.Detail == nil || !strings.Contains(*item.Detail, "SSLResponse") {
		t.Errorf("expected detail naming SSLResponse, got %#v", item.Detail)
	}
}

func TestHandleCompletion_NonEndpointResponseColon_NoAmbientMembers(t *testing.T) {
	s := newTestServerWithDocument(`:PROCEDURE Handle;
Response:
:ENDPROC;`)

	result, err := s.handleCompletion(nil, &protocol.CompletionParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: testURI},
			Position:     protocol.Position{Line: 1, Character: 9},
		},
	})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	items, ok := result.([]protocol.CompletionItem)
	if !ok {
		t.Fatalf("expected completion items, got %T", result)
	}
	if containsCompletionLabel(items, "Redirect") {
		t.Error("SSLResponse members must not surface outside endpoint files")
	}
}

func TestHandleCompletion_TypedReceiverMembers(t *testing.T) {
	s := newTestServerWithDocument(`:PROCEDURE Demo;
:DECLARE oClient;
oClient := WebServices{}:CreateHttpClient();
oClient:
:ENDPROC;`)

	result, err := s.handleCompletion(nil, &protocol.CompletionParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: testURI},
			Position:     protocol.Position{Line: 3, Character: 8},
		},
	})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	items, ok := result.([]protocol.CompletionItem)
	if !ok {
		t.Fatalf("expected completion items, got %T", result)
	}
	item := findCompletionItem(items, "GetResponse")
	if item == nil {
		t.Fatalf("expected HttpClient member completion GetResponse, got %d items", len(items))
	}
	if item.Detail == nil || !strings.Contains(*item.Detail, "HttpClient") {
		t.Errorf("expected detail naming HttpClient, got %#v", item.Detail)
	}
}

func TestHandleHover_TypedReceiverMember(t *testing.T) {
	s := newTestServerWithDocument(`:PROCEDURE Demo;
:DECLARE oClient, oResp;
oClient := WebServices{}:CreateHttpClient();
oResp := oClient:GetResponse();
:ENDPROC;`)

	hover, err := s.handleHover(nil, &protocol.HoverParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: testURI},
			Position:     protocol.Position{Line: 3, Character: 20},
		},
	})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if hover == nil {
		t.Fatal("expected hover for typed receiver member GetResponse")
	}
	content := hover.Contents.(protocol.MarkupContent)
	for _, want := range []string{"GetResponse", "HttpClient", "HttpResponse"} {
		if !strings.Contains(content.Value, want) {
			t.Errorf("expected hover to contain %q, got:\n%s", want, content.Value)
		}
	}
}

func TestHandleHover_TypedReceiverUnknownMember_Null(t *testing.T) {
	s := newTestServerWithDocument(`:PROCEDURE Demo;
:DECLARE oClient, oResp;
oClient := WebServices{}:CreateHttpClient();
oResp := oClient:Bogus();
:ENDPROC;`)

	hover, err := s.handleHover(nil, &protocol.HoverParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: testURI},
			Position:     protocol.Position{Line: 3, Character: 20},
		},
	})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if hover != nil {
		t.Errorf("expected null hover for unknown member of typed receiver, got %#v", hover)
	}
}

func TestHandleHover_EndpointAmbientMember(t *testing.T) {
	s := newTestServerWithDocument(`/*
 * Endpoint: HandleUpload
;
:PROCEDURE Handle;
Response:Redirect("https://example.test");
:ENDPROC;`)

	hover, err := s.handleHover(nil, &protocol.HoverParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: testURI},
			Position:     protocol.Position{Line: 4, Character: 12},
		},
	})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if hover == nil {
		t.Fatal("expected hover for Response:Redirect in endpoint file")
	}
	content := hover.Contents.(protocol.MarkupContent)
	for _, want := range []string{"Redirect", "SSLResponse"} {
		if !strings.Contains(content.Value, want) {
			t.Errorf("expected hover to contain %q, got:\n%s", want, content.Value)
		}
	}
}

func TestHandleHover_EndpointAmbientWord(t *testing.T) {
	s := newTestServerWithDocument(`/*
 * Endpoint: HandleUpload
;
:PROCEDURE Handle;
Response:Redirect("https://example.test");
:ENDPROC;`)

	hover, err := s.handleHover(nil, &protocol.HoverParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: testURI},
			Position:     protocol.Position{Line: 4, Character: 3},
		},
	})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if hover == nil {
		t.Fatal("expected ambient hover for Response in endpoint file")
	}
	content := hover.Contents.(protocol.MarkupContent)
	for _, want := range []string{"endpoint", "Redirect"} {
		if !strings.Contains(content.Value, want) {
			t.Errorf("expected ambient hover to contain %q, got:\n%s", want, content.Value)
		}
	}
}

// Pre-v0.14.0 review finding M1: a typed receiver can carry ad-hoc
// shape-augmented properties — they must survive in hover and completion.
func TestHandleHover_TypedReceiverShapeFallback(t *testing.T) {
	s := newTestServerWithDocument(`:PROCEDURE Demo;
:DECLARE oClient;
oClient := WebServices{}:CreateHttpClient();
oClient:MyTag := "x";
oClient:MyTag := "y";
:ENDPROC;`)

	hover, err := s.handleHover(nil, &protocol.HoverParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: testURI},
			Position:     protocol.Position{Line: 4, Character: 10},
		},
	})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if hover == nil {
		t.Fatal("expected shape hover for ad-hoc property on a typed receiver")
	}
	content := hover.Contents.(protocol.MarkupContent)
	if !strings.Contains(content.Value, "MyTag") {
		t.Errorf("expected MyTag shape hover, got:\n%s", content.Value)
	}
}

func TestHandleCompletion_TypedReceiverMergesShapeProps(t *testing.T) {
	s := newTestServerWithDocument(`:PROCEDURE Demo;
:DECLARE oClient;
oClient := WebServices{}:CreateHttpClient();
oClient:MyTag := "x";
oClient:
:ENDPROC;`)

	result, err := s.handleCompletion(nil, &protocol.CompletionParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: testURI},
			Position:     protocol.Position{Line: 4, Character: 8},
		},
	})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	items, ok := result.([]protocol.CompletionItem)
	if !ok {
		t.Fatalf("expected completion items, got %T", result)
	}
	if !containsCompletionLabel(items, "GetResponse") {
		t.Error("expected typed member GetResponse")
	}
	if !containsCompletionLabel(items, "MyTag") {
		t.Error("expected shape-augmented property MyTag merged into typed completions")
	}
}
