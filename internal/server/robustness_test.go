package server

import (
	"encoding/json"
	"strings"
	"testing"

	"github.com/tliron/glsp"
	protocol "github.com/tliron/glsp/protocol_3_16"
)

// Issue #258: request handling must survive non-ASCII text and contain any
// panic, because glsp does not recover and a panic ends the server.

// TestWrapperHandler_RecoversPanic: a panicking handler yields an error
// response instead of crashing the process.
func TestWrapperHandler_RecoversPanic(t *testing.T) {
	s := NewSSLServer()
	inner := &protocol.Handler{
		TextDocumentHover: func(*glsp.Context, *protocol.HoverParams) (*protocol.Hover, error) {
			panic("boom")
		},
	}
	inner.SetInitialized(true)
	w := NewWrapperHandler(inner, s)

	params, _ := json.Marshal(protocol.HoverParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: testURI},
		},
	})
	r, validMethod, validParams, err := w.Handle(&glsp.Context{
		Method: protocol.MethodTextDocumentHover,
		Params: params,
	})
	if err == nil || !strings.Contains(err.Error(), "boom") {
		t.Fatalf("expected an error carrying the panic value, got %v", err)
	}
	if r != nil || !validMethod || !validParams {
		t.Errorf("expected (nil, true, true, err), got (%v, %v, %v, err)", r, validMethod, validParams)
	}
}

// TestHandlers_AstralCharacterPositions drives every position-taking
// request at positions on lines with characters outside the BMP. The
// cursor positions are LSP UTF-16 offsets; before the fix, hover,
// definition and rename indexed a rune slice with a byte-length bound and
// panicked.
func TestHandlers_AstralCharacterPositions(t *testing.T) {
	docs := []struct {
		content string
		pos     protocol.Position
	}{
		{"\U0001F600x", protocol.Position{Line: 0, Character: 3}},
		{"sX := \"\U0001F600\";", protocol.Position{Line: 0, Character: 11}},
		{"sX := \"\U0001F600\";", protocol.Position{Line: 0, Character: 40}},
		{"/* \U0001F600\U0001F600;\nsX := 1;", protocol.Position{Line: 0, Character: 8}},
	}
	for _, d := range docs {
		s := newTestServerWithDocument(d.content)
		doc := protocol.TextDocumentIdentifier{URI: testURI}
		tdp := protocol.TextDocumentPositionParams{TextDocument: doc, Position: d.pos}

		if _, err := s.handleHover(nil, &protocol.HoverParams{TextDocumentPositionParams: tdp}); err != nil {
			t.Errorf("%q hover: %v", d.content, err)
		}
		if _, err := s.handleDefinition(nil, &protocol.DefinitionParams{TextDocumentPositionParams: tdp}); err != nil {
			t.Errorf("%q definition: %v", d.content, err)
		}
		if _, err := s.handleReferences(nil, &protocol.ReferenceParams{TextDocumentPositionParams: tdp}); err != nil {
			t.Errorf("%q references: %v", d.content, err)
		}
		if _, err := s.handlePrepareRename(nil, &protocol.PrepareRenameParams{TextDocumentPositionParams: tdp}); err != nil {
			t.Errorf("%q prepareRename: %v", d.content, err)
		}
		if _, err := s.handleRename(nil, &protocol.RenameParams{TextDocumentPositionParams: tdp, NewName: "sY"}); err != nil {
			t.Errorf("%q rename: %v", d.content, err)
		}
		if _, err := s.handleSignatureHelp(nil, &protocol.SignatureHelpParams{TextDocumentPositionParams: tdp}); err != nil {
			t.Errorf("%q signatureHelp: %v", d.content, err)
		}
		if _, err := s.handleCompletion(nil, &protocol.CompletionParams{TextDocumentPositionParams: tdp}); err != nil {
			t.Errorf("%q completion: %v", d.content, err)
		}
	}
}

// TestHover_WordAfterAstralCharacter: the UTF-16 cursor position is
// converted to the rune column, so the word under the cursor is the one
// the client means even after an emoji on the same line.
func TestHover_WordAfterAstralCharacter(t *testing.T) {
	// "\U0001F600" is two UTF-16 units; `Len` starts at UTF-16 offset 14.
	s := newTestServerWithDocument(":DECLARE sX;\nsX := \"\U0001F600\" + Len(sX);")
	hover, err := s.handleHover(nil, &protocol.HoverParams{TextDocumentPositionParams: protocol.TextDocumentPositionParams{
		TextDocument: protocol.TextDocumentIdentifier{URI: testURI},
		Position:     protocol.Position{Line: 1, Character: 15},
	}})
	if err != nil || hover == nil {
		t.Fatalf("expected a hover for Len, got %v, %v", hover, err)
	}
	content, ok := hover.Contents.(protocol.MarkupContent)
	if !ok || !strings.Contains(content.Value, "Len") {
		t.Errorf("expected hover for Len, got %#v", hover.Contents)
	}
}

// TestHandleDidChange_AstralIncrementalEdit: an incremental edit after an
// astral character lands where the client put it, so the server's copy of
// the document stays in sync.
func TestHandleDidChange_AstralIncrementalEdit(t *testing.T) {
	s := newTestServerWithDocument("s := \"\U0001F600\";")
	ctx := &glsp.Context{Notify: func(string, any) {}}
	err := s.handleDidChange(ctx, &protocol.DidChangeTextDocumentParams{
		TextDocument: protocol.VersionedTextDocumentIdentifier{
			TextDocumentIdentifier: protocol.TextDocumentIdentifier{URI: testURI},
			Version:                2,
		},
		ContentChanges: []any{protocol.TextDocumentContentChangeEvent{
			Range: &protocol.Range{
				Start: protocol.Position{Line: 0, Character: 9},
				End:   protocol.Position{Line: 0, Character: 9},
			},
			Text: " + sY",
		}},
	})
	if err != nil {
		t.Fatal(err)
	}
	got, _ := s.documents.GetDocument(testURI)
	if want := "s := \"\U0001F600\" + sY;"; got != want {
		t.Errorf("document = %q, want %q", got, want)
	}
}

// TestToInternalPosition: LSP UTF-16 positions convert to the providers'
// 1-based rune columns.
func TestToInternalPosition(t *testing.T) {
	content := "a := 1;\ns := \"\U0001F600\" + sName;"
	tests := []struct {
		pos               protocol.Position
		wantLine, wantCol int
	}{
		{protocol.Position{Line: 0, Character: 0}, 1, 1},
		{protocol.Position{Line: 0, Character: 99}, 1, 8},
		// After the emoji: UTF-16 offset 13 is rune index 12.
		{protocol.Position{Line: 1, Character: 13}, 2, 13},
		{protocol.Position{Line: 1, Character: 6}, 2, 7},
		{protocol.Position{Line: 9, Character: 3}, 10, 1},
	}
	for _, tc := range tests {
		line, col := toInternalPosition(content, tc.pos)
		if line != tc.wantLine || col != tc.wantCol {
			t.Errorf("toInternalPosition(%v) = (%d, %d), want (%d, %d)", tc.pos, line, col, tc.wantLine, tc.wantCol)
		}
	}
}

// TestColonStartsToken_NonASCII: the character before a typed ':' is found
// by UTF-16 offset, not by byte.
func TestColonStartsToken_NonASCII(t *testing.T) {
	tests := []struct {
		content string
		char    uint32
		want    bool
	}{
		{":", 1, true},
		{"x :", 3, true},
		{"x:", 2, false},
		// "é" is one UTF-16 unit and two bytes.
		{"é :", 3, true},
		{"é:", 2, false},
		// The emoji is two UTF-16 units and four bytes.
		{"\U0001F600 :", 4, true},
		{"s\U0001F600:", 4, false},
	}
	for _, tc := range tests {
		if got := colonStartsToken(tc.content, protocol.Position{Line: 0, Character: tc.char}); got != tc.want {
			t.Errorf("colonStartsToken(%q, %d) = %v, want %v", tc.content, tc.char, got, tc.want)
		}
	}
}

// TestInitialize_AdvertisesUTF16 pins the advertised position encoding.
func TestInitialize_AdvertisesUTF16(t *testing.T) {
	s := NewSSLServer()
	res, err := s.handleInitialize(nil, &protocol.InitializeParams{})
	if err != nil {
		t.Fatal(err)
	}
	if got := res.(ExtendedInitializeResult).Capabilities.PositionEncoding; got != "utf-16" {
		t.Errorf("positionEncoding = %q, want utf-16", got)
	}
}
