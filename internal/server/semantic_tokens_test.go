package server

import (
	"encoding/json"
	"slices"
	"testing"

	"starlims-lsp/internal/providers"

	"github.com/tliron/glsp"
	protocol "github.com/tliron/glsp/protocol_3_16"
)

func decodeLegendType(t *testing.T, want string) protocol.UInteger {
	t.Helper()
	i := slices.Index(providers.SemanticTokenTypes, want)
	if i < 0 {
		t.Fatalf("legend has no type %q", want)
	}
	return protocol.UInteger(i)
}

// semanticRequest sends a request through the handler the way a client's
// JSON reaches it and returns the result's raw JSON.
func semanticRequest(t *testing.T, s *SSLServer, method string, params any) string {
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
	return string(encoded)
}

func semanticData(t *testing.T, encoded string) []protocol.UInteger {
	t.Helper()
	var result protocol.SemanticTokens
	if err := json.Unmarshal([]byte(encoded), &result); err != nil {
		t.Fatalf("decoding %s: %v", encoded, err)
	}
	return result.Data
}

func TestInitialize_AdvertisesSemanticTokens(t *testing.T) {
	// [spec feature.semantic_tokens/A2]
	s := NewSSLServer()
	res, err := s.handleInitialize(nil, &protocol.InitializeParams{})
	if err != nil {
		t.Fatal(err)
	}
	raw, _ := json.Marshal(res)
	var decoded struct {
		Capabilities struct {
			SemanticTokensProvider struct {
				Legend struct {
					TokenTypes     []string `json:"tokenTypes"`
					TokenModifiers []string `json:"tokenModifiers"`
				} `json:"legend"`
				Full  any `json:"full"`
				Range any `json:"range"`
			} `json:"semanticTokensProvider"`
		} `json:"capabilities"`
	}
	if err := json.Unmarshal(raw, &decoded); err != nil {
		t.Fatal(err)
	}
	got := decoded.Capabilities.SemanticTokensProvider
	if !slices.Equal(got.Legend.TokenTypes, providers.SemanticTokenTypes) ||
		!slices.Equal(got.Legend.TokenModifiers, providers.SemanticTokenModifiers) {
		t.Errorf("legend = %+v", got.Legend)
	}
	if got.Full != true || got.Range != true {
		t.Errorf("full = %v, range = %v; want both true (no delta)", got.Full, got.Range)
	}
}

func TestSemanticTokensFullRequest(t *testing.T) {
	// [spec feature.semantic_tokens/A1]
	s := newTestServerWithDocument(":PROCEDURE Calc;\n:PARAMETERS nQty;\nx := nQty;\n:ENDPROC;\n")
	params := protocol.SemanticTokensParams{TextDocument: protocol.TextDocumentIdentifier{URI: testURI}}
	data := semanticData(t, semanticRequest(t, s, string(protocol.MethodTextDocumentSemanticTokensFull), params))

	function := decodeLegendType(t, "function")
	parameter := decodeLegendType(t, "parameter")
	variable := decodeLegendType(t, "variable")
	want := []protocol.UInteger{
		0, 11, 4, function, providers.SemanticDeclaration,
		1, 12, 4, parameter, providers.SemanticDeclaration,
		1, 0, 1, variable, 0,
		0, 5, 4, parameter, 0,
	}
	if !slices.Equal(data, want) {
		t.Errorf("data = %v, want %v", data, want)
	}

	params.TextDocument.URI = "file:///not/open.ssl"
	if got := semanticRequest(t, s, string(protocol.MethodTextDocumentSemanticTokensFull), params); got != "null" {
		t.Errorf("unopened document: got %s, want null", got)
	}

	empty := newTestServerWithDocument("/* only a comment;\n")
	params.TextDocument.URI = testURI
	if got := semanticRequest(t, empty, string(protocol.MethodTextDocumentSemanticTokensFull), params); got != `{"data":[]}` {
		t.Errorf("no tokens: got %s, want an empty data list", got)
	}
}

func TestSemanticTokensUTF16(t *testing.T) {
	// [spec feature.semantic_tokens/A14]
	s := newTestServerWithDocument("s := \"😀\"; nGot := 1;\n")
	params := protocol.SemanticTokensParams{TextDocument: protocol.TextDocumentIdentifier{URI: testURI}}
	data := semanticData(t, semanticRequest(t, s, string(protocol.MethodTextDocumentSemanticTokensFull), params))
	// s at 0; the emoji is one rune but two UTF-16 units, so nGot starts at
	// UTF-16 offset 11, not at its rune column 10.
	if len(data) != 10 || data[5] != 0 || data[6] != 11 || data[7] != 4 {
		t.Errorf("data = %v, want nGot 11 units after s, 4 long", data)
	}
}

func TestSemanticTokensRangeRequest(t *testing.T) {
	// [spec feature.semantic_tokens/A16]
	s := newTestServerWithDocument("a := 1;\nb := 2;\nc := 3;\nd := 4;\n")
	params := protocol.SemanticTokensRangeParams{
		TextDocument: protocol.TextDocumentIdentifier{URI: testURI},
		Range: protocol.Range{
			Start: protocol.Position{Line: 1, Character: 0},
			End:   protocol.Position{Line: 2, Character: 1},
		},
	}
	data := semanticData(t, semanticRequest(t, s, string(protocol.MethodTextDocumentSemanticTokensRange), params))
	variable := decodeLegendType(t, "variable")
	want := []protocol.UInteger{
		1, 0, 1, variable, 0,
		1, 0, 1, variable, 0,
	}
	if !slices.Equal(data, want) {
		t.Errorf("data = %v, want %v", data, want)
	}

	params.TextDocument.URI = "file:///not/open.ssl"
	if got := semanticRequest(t, s, string(protocol.MethodTextDocumentSemanticTokensRange), params); got != "null" {
		t.Errorf("unopened document: got %s, want null", got)
	}
}

func TestSemanticTokensThroughTheWorkspaceIndex(t *testing.T) {
	// [spec feature.semantic_tokens/A13]
	s, orders, _, _ := newCallGraphServer(t)
	s.documents.SetDocument(orders, ordersScript, 1)
	s.documentVersion[orders] = 1

	params := protocol.SemanticTokensParams{TextDocument: protocol.TextDocumentIdentifier{URI: orders}}
	data := semanticData(t, semanticRequest(t, s, string(protocol.MethodTextDocumentSemanticTokensFull), params))

	// The script's tab and `ExecFunction("` put each target at character 15:
	// line 4 holds LIMS.Helpers.Calc, line 6 LIMS.Missing.
	function := decodeLegendType(t, "function")
	var line, start protocol.UInteger
	sawCalc, sawMissing := false, false
	for i := 0; i+4 < len(data); i += 5 {
		if data[i] > 0 {
			start = 0
		}
		line += data[i]
		start += data[i+1]
		if line == 4 && start == 15 && data[i+2] == protocol.UInteger(len("LIMS.Helpers.Calc")) && data[i+3] == function {
			sawCalc = true
		}
		if line == 6 && start == 15 {
			sawMissing = true
		}
	}
	if !sawCalc {
		t.Errorf("no token for the resolved target in %v", data)
	}
	if sawMissing {
		t.Errorf("a token for the target that does not resolve in %v", data)
	}
}
