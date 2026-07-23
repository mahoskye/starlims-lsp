// Package providers implements LSP feature providers for SSL.
package providers

import (
	"strings"

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
	if strings.Contains(body, "{|") || strings.ContainsAny(text, "\n\r") {
		return text
	}

	var normParams []string
	for _, p := range strings.Split(params, ",") {
		if trimmed := strings.TrimSpace(p); trimmed != "" {
			normParams = append(normParams, trimmed)
		}
	}

	normBody := normalizeExpression(strings.TrimSpace(body), opts)
	return "{|" + strings.Join(normParams, ", ") + "| " + normBody + "}"
}

// normalizeExpression re-lexes a single-line expression and rejoins its
// tokens under the configured spacing rules — the code-block body's
// counterpart to the statement writer's operator/comma handling. String
// and comment token content is preserved verbatim.
func normalizeExpression(expr string, opts FormattingOptions) string {
	tokens := lexer.NewLexer(expr).Tokenize()
	var b strings.Builder
	var prev lexer.Token
	havePrev := false
	prevWasUnarySign := false

	for _, t := range tokens {
		if t.Type == lexer.TokenEOF || t.Type == lexer.TokenWhitespace {
			continue
		}
		if havePrev {
			space := true
			switch {
			case isOpenParen(prev) || isCloseParen(t) || t.Text == ";" || t.Text == ",":
				space = false
			case prev.Text == ":" && prev.Type == lexer.TokenPunctuation,
				t.Text == ":" && t.Type == lexer.TokenPunctuation:
				space = false
			case t.Text == "(" && prev.Type == lexer.TokenIdentifier:
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
		if t.Type == lexer.TokenOperator {
			text = canonicalDotOperator(text)
		}
		b.WriteString(text)

		prevWasUnarySign = (t.Text == "-" || t.Text == "+") && isUnaryContext(prev, !havePrev)
		prev = t
		havePrev = true
	}
	return b.String()
}
