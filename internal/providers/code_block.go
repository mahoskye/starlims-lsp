// Package providers implements LSP feature providers for SSL.
package providers

import (
	"strings"

	"starlims-lsp/internal/constants"
	"starlims-lsp/internal/lexer"
)

// normalizeCodeBlockLiteral canonicalizes a code-block literal to the
// schema's `{|params| expression}` form (R42, issue #91): parameters are
// comma-space separated, one space follows the closing '|', and the body
// takes the configured operator/comma spacing. Anything unexpected —
// missing parameter delimiter, nested code block, multi-line body — is
// returned verbatim; normalization must never risk the literal's meaning.
func normalizeCodeBlockLiteral(text string, opts FormattingOptions) string {
	if !strings.HasPrefix(text, "{|") || !strings.HasSuffix(text, "}") {
		return text
	}
	inner := text[2 : len(text)-1]
	pipe := strings.IndexByte(inner, '|')
	if pipe < 0 {
		return text
	}
	params, body := inner[:pipe], inner[pipe+1:]
	if strings.ContainsAny(text, "\n\r") || containsCodeBlock(body) {
		return text
	}

	var normParams []string
	for _, p := range strings.Split(params, ",") {
		if trimmed := strings.TrimSpace(p); trimmed != "" {
			normParams = append(normParams, trimmed)
		}
	}

	normBody := normalizeExpression(strings.TrimSpace(body), opts)
	if normBody == "" {
		return "{|" + strings.Join(normParams, ", ") + "|}"
	}
	return "{|" + strings.Join(normParams, ", ") + "| " + normBody + "}"
}

// containsCodeBlock reports whether a code-block body holds a nested code
// block. It asks the lexer rather than searching for "{|", so a string
// literal that merely contains those characters does not block
// normalization.
func containsCodeBlock(body string) bool {
	for _, t := range lexer.NewLexer(body).Tokenize() {
		if t.Type == lexer.TokenCodeBlock {
			return true
		}
	}
	return false
}

// normalizeExpression re-lexes a single-line expression and rejoins its
// tokens under the configured spacing rules — the code-block body's
// counterpart to the statement writer's operator/comma handling. String
// and comment token content is preserved verbatim. The body also takes the
// canonical forms the statement writer applies (issue #255): keyword and
// literal casing (`.t.` -> `.T.`, `nil` -> `NIL`, fmt.keyword_case), Me/Base
// receivers, and — under builtinFunctionCase "PascalCase" — built-in call
// casing (fmt.builtin_function_case).
func normalizeExpression(expr string, opts FormattingOptions) string {
	tokens := lexer.NewLexer(expr).Tokenize()
	var canonical map[string]string
	if opts.BuiltinFunctionCase == "PascalCase" {
		canonical = constants.CanonicalFunctionNames()
	}
	var b strings.Builder
	var prev lexer.Token
	havePrev := false
	prevWasUnarySign := false
	sawWS := false // source whitespace between prev and the current token

	for i, t := range tokens {
		if t.Type == lexer.TokenEOF {
			break
		}
		if t.Type == lexer.TokenWhitespace {
			sawWS = true
			continue
		}
		if havePrev {
			space := true
			switch {
			case !opts.CommaSpacing && (t.Text == "," || prev.Text == ","):
				space = sawWS // comma spacing off: keep the author's spacing
			case isOpenParen(prev) || isCloseParen(t) || t.Text == ";" || t.Text == ",":
				space = false
			case prev.Text == ":" && prev.Type == lexer.TokenPunctuation,
				t.Text == ":" && t.Type == lexer.TokenPunctuation:
				space = false
			case t.Text == "(" && prev.Type == lexer.TokenIdentifier:
				space = false
			case t.Text == "[" && t.Type == lexer.TokenPunctuation &&
				(prev.Type == lexer.TokenIdentifier || isCloseParen(prev)),
				t.Text == "{" && prev.Type == lexer.TokenIdentifier:
				// A subscript binds to its array, and a built-in class
				// instantiation's '{' to its class name, as in a statement
				// (`aRow[2]`, `Email{}`; issue #326).
				space = false
			case t.Text == "++" || t.Text == "--":
				space = false
			case prev.Text == "!":
				space = false
			case prevWasUnarySign:
				space = false // operand glues to its unary sign
			case !opts.OperatorSpacing && (isOperator(t) || isOperator(prev)):
				space = false
			}
			if space {
				b.WriteString(" ")
			}
		}
		text := t.Text
		switch {
		case t.Type == lexer.TokenOperator:
			text = canonicalDotOperator(text)
		case t.Type == lexer.TokenKeyword:
			text = strings.ToUpper(text)
		case t.Type == lexer.TokenIdentifier:
			if recv, ok := canonicalReceiver(t, tokens, i); ok {
				text = recv
			} else if canonical != nil && isCallSite(tokens, i) && !isMemberName(tokens, i) {
				if pascal, ok := canonical[strings.ToLower(text)]; ok {
					text = pascal
				}
			}
		}
		b.WriteString(text)

		prevWasUnarySign = (t.Text == "-" || t.Text == "+") && isUnaryContext(prev, !havePrev)
		prev = t
		havePrev = true
		sawWS = false
	}
	return b.String()
}
