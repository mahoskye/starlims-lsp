package server

// textDocument/semanticTokens: the names a TextMate grammar cannot tell
// apart, classified by the server. Normative behavior:
// catalog/features/semantic_tokens.md.

import (
	"starlims-lsp/internal/providers"

	"github.com/tliron/glsp"
	protocol "github.com/tliron/glsp/protocol_3_16"
)

// semanticTokensOptions advertises both requests with the legend.
func semanticTokensOptions() *protocol.SemanticTokensOptions {
	return &protocol.SemanticTokensOptions{
		Legend: protocol.SemanticTokensLegend{
			TokenTypes:     providers.SemanticTokenTypes,
			TokenModifiers: providers.SemanticTokenModifiers,
		},
		Full:  true,
		Range: true,
	}
}

// handleSemanticTokensFull answers textDocument/semanticTokens/full. A
// document the client has not opened has no tokens: the result is null.
func (s *SSLServer) handleSemanticTokensFull(context *glsp.Context, params *protocol.SemanticTokensParams) (*protocol.SemanticTokens, error) {
	return s.semanticTokens(canonicalURI(params.TextDocument.URI), nil), nil
}

// handleSemanticTokensRange answers textDocument/semanticTokens/range.
func (s *SSLServer) handleSemanticTokensRange(context *glsp.Context, params *protocol.SemanticTokensRangeParams) (any, error) {
	result := s.semanticTokens(canonicalURI(params.TextDocument.URI), &params.Range)
	if result == nil {
		return nil, nil
	}
	return result, nil
}

// semanticTokens classifies an open document under the options its
// diagnostics run with, and encodes the tokens that lie in within (all of
// them when it is nil).
func (s *SSLServer) semanticTokens(uri string, within *protocol.Range) *protocol.SemanticTokens {
	content, ok := s.documents.GetDocument(uri)
	if !ok {
		return nil
	}
	cache := s.documents.ParseDocument(uri, s.documentVersion[uri])
	opts := providers.SemanticTokenOptions{
		IsDataSourceFile: isDataSourceURI(uri),
		IsEndpointFile:   isEndpointFile(uri, content, s.settings.EndpointPatterns),
		GlobalVariables:  s.settings.Diagnostics.GlobalVariables,
		Resolver:         liveResolver{s},
	}
	opts.IncludeDeclaredVariables, opts.IncludesResolved = (liveResolver{s}).includeClosure(cache.Tokens, uri)

	tokens := providers.GetSemanticTokens(content, cache.Tokens, cache.AST, opts)
	return &protocol.SemanticTokens{Data: encodeSemanticTokens(tokens, newUTF16Mapper(content), within)}
}

// encodeSemanticTokens packs tokens into the protocol's five integers per
// token: line delta, start delta (from the previous token when on its line),
// length, type and modifier bits. The result is never nil, so a document with
// no tokens answers with an empty list.
func encodeSemanticTokens(tokens []providers.SemanticToken, m *utf16Mapper, within *protocol.Range) []protocol.UInteger {
	data := make([]protocol.UInteger, 0, len(tokens)*5)
	var prevLine, prevStart protocol.UInteger
	for _, token := range tokens {
		r := m.rng(token.Range)
		if within != nil && !rangesTouch(r, *within) {
			continue
		}
		start := r.Start.Character
		if r.Start.Line == prevLine {
			start -= prevStart
		}
		data = append(data,
			r.Start.Line-prevLine,
			start,
			r.End.Character-r.Start.Character,
			protocol.UInteger(token.Type),
			protocol.UInteger(token.Modifiers),
		)
		prevLine, prevStart = r.Start.Line, r.Start.Character
	}
	return data
}

// rangesTouch reports whether two ranges share a position.
func rangesTouch(a, b protocol.Range) bool {
	return !positionAfter(a.Start, b.End) && !positionAfter(b.Start, a.End)
}

func positionAfter(a, b protocol.Position) bool {
	return a.Line > b.Line || (a.Line == b.Line && a.Character > b.Character)
}
