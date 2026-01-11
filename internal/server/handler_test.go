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
