package server

// The ssl/lexicalTokens request: where an open document's comments,
// strings and keywords are. Normative behavior:
// catalog/features/lexical_tokens.md.

import (
	"starlims-lsp/internal/providers"

	protocol "github.com/tliron/glsp/protocol_3_16"
)

// MethodLexicalTokens is the custom request for a document's lexical
// tokens.
const MethodLexicalTokens = "ssl/lexicalTokens"

// LexicalTokensParams are the parameters of an ssl/lexicalTokens request.
type LexicalTokensParams struct {
	TextDocument TextDocumentIdentifier `json:"textDocument"`
}

// LexicalTokensResult is the lexical tokens of one document version.
type LexicalTokensResult struct {
	URI string `json:"uri"`
	// Version is the document version the tokens were read from, so a
	// client can drop a result that a later edit has made stale.
	Version int            `json:"version"`
	Tokens  []LexicalToken `json:"tokens"`
}

// LexicalToken is one token (providers.LexicalToken on the wire, positions
// in UTF-16).
type LexicalToken struct {
	Kind  string         `json:"kind"`
	Range protocol.Range `json:"range"`
}

// handleLexicalTokens answers ssl/lexicalTokens. A document the client has
// not opened has no tokens: the result is null.
func (s *SSLServer) handleLexicalTokens(params *LexicalTokensParams) (*LexicalTokensResult, error) {
	uri := canonicalURI(params.TextDocument.URI)
	content, ok := s.documents.GetDocument(uri)
	if !ok {
		return nil, nil
	}

	version := s.documentVersion[uri]
	cache := s.documents.ParseDocument(uri, version)
	tokens := providers.GetLexicalTokens(content, cache.Tokens, isDataSourceURI(uri))

	// Never nil, so a document with no tokens answers with an empty list.
	m := newUTF16Mapper(content)
	out := make([]LexicalToken, len(tokens))
	for i, token := range tokens {
		out[i] = LexicalToken{Kind: token.Kind, Range: m.rng(token.Range)}
	}
	return &LexicalTokensResult{URI: uri, Version: version, Tokens: out}, nil
}
