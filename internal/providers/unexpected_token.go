package providers

import (
	"fmt"
	"strings"

	"starlims-lsp/internal/constants"
	"starlims-lsp/internal/lexer"
	"starlims-lsp/internal/parser"
)

// checkUnexpectedTokens reports the first token in each statement that the
// statement grammar could not accept (catalog: diag.unexpected_token,
// issue #240). The parser already computes the position; this check owns
// the wording and the one-finding-per-cause deferrals to the lexer- and
// operator-level rules that already name the same token.
func checkUnexpectedTokens(tokens []lexer.Token, stmts []parser.StatementExprs) []Diagnostic {
	var diagnostics []Diagnostic
	for i := range stmts {
		se := &stmts[i]
		idx := se.Unexpected
		if idx < 0 || idx >= len(tokens) {
			continue
		}
		if deferredToOtherRule(tokens, se, idx) {
			continue
		}
		tok := tokens[idx]

		expected := se.Expected
		if expected == "" {
			expected = expectedAt(tokens, se, idx)
		}
		msg := "Unexpected " + describeToken(tok)
		if se.Kind == parser.StmtFor {
			msg += " in ':FOR' header"
		}
		msg += " - expected " + expected
		if se.Kind == parser.StmtFor && expected == "':='" && len(se.Exprs) > 0 &&
			strings.EqualFold(se.Exprs[0].Name, "EACH") {
			msg += " (SSL has no ':FOR EACH' form)"
		}
		if terminatorAcceptable(expected) && lineLeading(tokens, idx) {
			msg += " (is the previous statement missing its ';'?)"
		}

		r := tokenToRange(tok)
		if tok.Type == lexer.TokenEOF {
			r.End.Character = r.Start.Character + 1
		}
		diagnostics = append(diagnostics, Diagnostic{
			Severity: SeverityError,
			Range:    r,
			Message:  msg,
			Source:   "ssl-lsp",
			Code:     CodeUnexpectedToken,
		})
	}
	return diagnostics
}

// describeToken renders a token for the message as `<class> '<text>'`,
// with the class dropped for punctuation (`';'`) and a plain phrase for
// end of input.
func describeToken(tok lexer.Token) string {
	text := tok.Text
	if r := []rune(text); len(r) > 40 {
		text = string(r[:40]) + "…"
	}
	switch tok.Type {
	case lexer.TokenIdentifier:
		return "identifier '" + text + "'"
	case lexer.TokenKeyword:
		return "keyword '" + text + "'"
	case lexer.TokenNumber:
		return "number '" + text + "'"
	case lexer.TokenString:
		return "string " + text
	case lexer.TokenOperator:
		return "operator '" + text + "'"
	case lexer.TokenCodeBlock:
		return "code block"
	case lexer.TokenEOF:
		return "end of file"
	default:
		return "'" + text + "'"
	}
}

// expectedAt words the expectation for an unexpected token the parser
// left unlabelled, from its surroundings inside the statement: inside an
// open delimiter the list wanted a separator or its closer; after an
// operator, a separator, or an opener an operand was due; after a finished
// operand only an operator or the terminator may follow.
func expectedAt(tokens []lexer.Token, se *parser.StatementExprs, idx int) string {
	prev := previousSignificantTokenIndex(tokens, idx-1)
	if prev < se.Start {
		return "a statement"
	}
	p := tokens[prev]
	switch {
	case p.Type == lexer.TokenOperator:
		return "an expression"
	case p.Type == lexer.TokenPunctuation && (p.Text == "," || closerFor[p.Text] != ""):
		return "an expression"
	}
	if opener := innermostOpener(tokens, se.Start, idx); opener != "" {
		return fmt.Sprintf("',' or '%s'", closerFor[opener])
	}
	if tokens[idx].Type == lexer.TokenOperator {
		// The token is an operator the grammar cannot place here — an
		// assignment inside a condition, say — so "an operator" would
		// read as a contradiction.
		return "a binary operator or ';'"
	}
	return "an operator or ';'"
}

var closerFor = map[string]string{"(": ")", "[": "]", "{": "}"}

// innermostOpener returns the delimiter still open at `idx` when scanning
// the statement from `start`, or "" when the position is at depth zero.
func innermostOpener(tokens []lexer.Token, start, idx int) string {
	var stack []string
	for j := start; j < idx && j < len(tokens); j++ {
		t := tokens[j]
		if t.Type != lexer.TokenPunctuation {
			continue
		}
		if closerFor[t.Text] != "" {
			stack = append(stack, t.Text)
		} else if len(stack) > 0 && closerFor[stack[len(stack)-1]] == t.Text {
			stack = stack[:len(stack)-1]
		}
	}
	if len(stack) == 0 {
		return ""
	}
	return stack[len(stack)-1]
}

// terminatorAcceptable reports whether a `;` would have satisfied the
// expectation — the case where a missing terminator on the previous line
// is the likely cause when the unexpected token leads its line.
func terminatorAcceptable(expected string) bool {
	return strings.HasSuffix(expected, "';'")
}

// lineLeading reports whether idx is the first significant token on its
// line: a line break sits between it and the previous significant token.
// Token line numbers alone would misread a multi-line string token ending
// just before idx as a line break.
func lineLeading(tokens []lexer.Token, idx int) bool {
	prev := previousSignificantTokenIndex(tokens, idx-1)
	if prev < 0 {
		return true
	}
	for j := prev + 1; j < idx; j++ {
		if tokens[j].Type == lexer.TokenWhitespace && strings.Contains(tokens[j].Text, "\n") {
			return true
		}
	}
	return false
}

// deferredToOtherRule reports whether another rule already names this
// token, so reporting it here would be a second finding for one cause.
// Each case mirrors the detection of the rule it defers to.
func deferredToOtherRule(tokens []lexer.Token, se *parser.StatementExprs, idx int) bool {
	tok := tokens[idx]

	// unknown_token (and its dot_property_access carve-out) report every
	// token the lexer could not classify.
	if tok.Type == lexer.TokenUnknown {
		return true
	}

	// unclosed_delimiter: an opener never closed swallows the rest of the
	// file into this statement, so whatever the parser stopped on is a
	// symptom of that.
	if unclosedOpenerIn(tokens, se.Start, se.End) {
		return true
	}

	if tok.Type == lexer.TokenOperator {
		// invalid_operator_sequence: C-style operators, and the glued
		// `!==` / `===` pairs whose second half is what the parser trips on.
		switch tok.Text {
		case "&&", "||", "&", "|":
			return true
		}
		if idx > 0 && tokens[idx-1].Type == lexer.TokenOperator &&
			tokens[idx-1].Offset+len(tokens[idx-1].Text) == tok.Offset {
			switch tokens[idx-1].Text + tok.Text {
			case "!==", "===":
				return true
			}
		}
	}

	// unknown_keyword (and endfor_invalid) own a colon form that is not an
	// SSL keyword — including the lexer's reading of a line-leading member
	// call (`:Replace(...)` continuing a chain) as one.
	if tok.Type == lexer.TokenKeyword && strings.HasPrefix(tok.Text, ":") && !isLegacyLabelKeywordForm(tok.Text) &&
		!constants.IsKeyword(strings.ToUpper(strings.TrimPrefix(tok.Text, ":"))) {
		return true
	}

	if tok.Type == lexer.TokenPunctuation && closerFor[tok.Text] == "" {
		if _, isCloser := openerFor[tok.Text]; isCloser {
			// unmatched_delimiter / mismatched_delimiter own a closer with
			// no matching opener; a closer that does match but arrives
			// where an operand was due (`(1 + )`) is this rule's.
			opener := innermostOpener(tokens, se.Start, idx)
			return opener == "" || closerFor[opener] != tok.Text
		}
	}

	if tok.Type == lexer.TokenIdentifier {
		// bare_logical_operator: AND / OR / NOT written without periods, in
		// operator position.
		if bareLogicalOperatorPosition(tokens, idx) {
			return true
		}
		// scientific_notation: `7e2` lexes as number `7` glued to
		// identifier `e2`, and `.5e1` as `.5` glued to `e1`; the number is
		// what that rule reports. Only those shapes — a number with no
		// decimal point, or one starting with the point — are its. A
		// well-formed number glued to a stray `E` (`2.0E+nVar`) is nobody
		// else's and stays here.
		if idx > 0 && tokens[idx-1].Type == lexer.TokenNumber &&
			tokens[idx-1].Offset+len(tokens[idx-1].Text) == tok.Offset &&
			(tok.Text[0] == 'e' || tok.Text[0] == 'E') {
			num := tokens[idx-1].Text
			if !strings.Contains(num, ".") || strings.HasPrefix(num, ".") {
				return true
			}
		}
	}
	return false
}

var openerFor = map[string]string{")": "(", "]": "[", "}": "{"}

// unclosedOpenerIn reports whether an opener inside [start, end] is never
// closed within it.
func unclosedOpenerIn(tokens []lexer.Token, start, end int) bool {
	depth := 0
	for j := start; j <= end && j < len(tokens); j++ {
		t := tokens[j]
		if t.Type != lexer.TokenPunctuation {
			continue
		}
		if closerFor[t.Text] != "" {
			depth++
		} else if openerFor[t.Text] != "" && depth > 0 {
			depth--
		}
	}
	return depth > 0
}

// bareLogicalOperatorPosition mirrors checkBareLogicalOperators: the
// identifier is AND / OR / NOT, not a member name, and sits where an
// operator would.
func bareLogicalOperatorPosition(tokens []lexer.Token, i int) bool {
	upper := strings.ToUpper(tokens[i].Text)
	if upper != "AND" && upper != "OR" && upper != "NOT" {
		return false
	}
	prevIdx := previousSignificantTokenIndex(tokens, i-1)
	nextIdx := nextSignificantTokenIndex(tokens, i+1)
	var prev, next lexer.Token
	if prevIdx >= 0 {
		prev = tokens[prevIdx]
	}
	if nextIdx >= 0 {
		next = tokens[nextIdx]
	}
	if prev.Type == lexer.TokenPunctuation && prev.Text == ":" {
		return false
	}
	if upper == "NOT" {
		return nextIdx >= 0 && operandStart(next) && !(prevIdx >= 0 && operandEnd(prev))
	}
	return prevIdx >= 0 && operandEnd(prev) && nextIdx >= 0 && operandStart(next)
}
