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
}

// Issue #11: ':' trigger should NOT surface keyword completions when the
// preceding character is non-whitespace (e.g. unknown identifier `foo:`).
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
