package providers

// Lexical tokens: where a document's comments, strings and keywords are, as
// the lexer every other feature reads from sees them. Normative behavior:
// catalog/features/lexical_tokens.md.

import (
	"strings"

	"starlims-lsp/internal/lexer"
)

// The kinds of lexical token. A client ignores a kind it does not know, so
// adding one does not change the result's shape version (D19).
const (
	// LexicalComment is a comment, from `/*` through the `;` that ends it.
	LexicalComment = "comment"
	// LexicalString is a string literal in any of the three quote styles,
	// quotes included.
	LexicalString = "string"
	// LexicalKeyword is a colon keyword, colon included (`:IF`).
	LexicalKeyword = "keyword"
	// LexicalText is the body of a `:REGION` block: stored text, not code.
	LexicalText = "text"
	// LexicalCodeBlock is a `{|params| body}` literal, which the lexer
	// keeps as one opaque token.
	LexicalCodeBlock = "codeBlock"
)

// LexicalToken is one token a client cannot place without lexing SSL. Its
// range may span lines.
type LexicalToken struct {
	Kind  string
	Range Range
}

// GetLexicalTokens returns the document's comments, strings, keywords,
// region bodies and code-block literals in document order. tokens is the
// document's cached lex. An SQL data source is not SSL, so it has none; a
// directive or `:PARAMETERS` header above an SQL body has its own, as for
// semantic tokens (feature.diagnostics_pipeline A10).
func GetLexicalTokens(text string, tokens []lexer.Token, isDataSource bool) []LexicalToken {
	if isDataSource {
		header, body := SplitDataSourceHeader(text)
		if IsSQLModeDataSourceSplit(text, header, body) {
			if strings.TrimSpace(header) == "" {
				return nil
			}
			// The header is a position-preserving prefix, so its ranges
			// are the document's.
			tokens = lexer.NewLexer(header).Tokenize()
		}
	}

	var out []LexicalToken
	for _, token := range tokens {
		kind := lexicalKind(token)
		if kind == "" || token.Text == "" {
			continue
		}
		out = append(out, LexicalToken{Kind: kind, Range: tokenToRange(token)})
	}
	return out
}

// lexicalKind names the kind a lexer token is reported as, or "" for one
// that is not reported: a name, a number, an operator, punctuation, and
// the literal words (`NIL`, `.T.`, `.F.`), which the lexer types as
// keywords but which are values.
func lexicalKind(token lexer.Token) string {
	switch token.Type {
	case lexer.TokenComment:
		return LexicalComment
	case lexer.TokenString:
		return LexicalString
	case lexer.TokenKeyword:
		if strings.HasPrefix(token.Text, ":") {
			return LexicalKeyword
		}
	case lexer.TokenRegionBody:
		return LexicalText
	case lexer.TokenCodeBlock:
		return LexicalCodeBlock
	}
	return ""
}
