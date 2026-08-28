package parser

// Statement-level entry points for the expression AST (issue #184): walk a
// token stream statement by statement and yield the expression trees each
// one contains. Built on demand by consumers — nothing here runs during
// the structural Parse().

import (
	"strings"

	"starlims-lsp/internal/lexer"
)

// StatementExprs is the parsed expression content of one statement.
type StatementExprs struct {
	// Start and End are inclusive token indices bracketing the statement
	// (End is the terminating `;` when present).
	Start, End int
	// Exprs holds the statement's expression trees in source order. For an
	// assignment this is [target, value]; for `:FOR i := a :TO b :STEP c`
	// it is [i, a, b, c]; for keyword conditions (`:IF x;`) it is [x].
	Exprs []*Expr
	// Assign is the assignment operator text (`:=`, `+=`, ...) when the
	// statement is an assignment, else "".
	Assign string
	// Complete reports whether expression parsing consumed the whole
	// statement: every significant token is inside some tree in Exprs.
	// Consumers wanting zero-risk claims should require Complete.
	Complete bool
}

// statement-leading keywords whose remainder is one expression.
var exprAfterKeyword = map[string]bool{
	"IF": true, "WHILE": true, "CASE": true,
}

// ExtractStatementExpressions walks the token slice and returns expression
// trees for every statement that carries expressions. Statements without
// expression content (:DECLARE, :ENDIF, comments, region bodies, ...) are
// omitted. Data-source directive headers are not handled here — the EBNF
// grammar excludes preprocessing syntax, and callers routing data-source
// files should not ask for expression trees on the header.
func ExtractStatementExpressions(tokens []lexer.Token) []StatementExprs {
	var out []StatementExprs

	i := 0
	for i < len(tokens) {
		// Find the start of the next statement.
		for i < len(tokens) {
			t := tokens[i]
			if t.Type == lexer.TokenWhitespace || t.Type == lexer.TokenComment ||
				t.Type == lexer.TokenRegionBody || t.Type == lexer.TokenEOF ||
				(t.Type == lexer.TokenPunctuation && t.Text == ";") {
				i++
				continue
			}
			break
		}
		if i >= len(tokens) {
			break
		}
		start := i
		end := statementEnd(tokens, i)

		if se, ok := parseStatement(tokens, start, end); ok {
			out = append(out, se)
		}
		i = end + 1
	}
	return out
}

// statementEnd returns the index of the statement's terminating `;` at
// bracket depth zero, or the last token index when unterminated.
func statementEnd(tokens []lexer.Token, start int) int {
	depth := 0
	for j := start; j < len(tokens); j++ {
		t := tokens[j]
		if t.Type != lexer.TokenPunctuation {
			continue
		}
		switch t.Text {
		case "(", "[", "{":
			depth++
		case ")", "]", "}":
			depth--
		case ";":
			if depth <= 0 {
				return j
			}
		}
	}
	return len(tokens) - 1
}

// parseStatement classifies one statement span and parses its expression
// content. ok=false means the statement carries no expressions.
func parseStatement(tokens []lexer.Token, start, end int) (StatementExprs, bool) {
	se := StatementExprs{Start: start, End: end}
	first := tokens[start]

	if first.Type == lexer.TokenKeyword {
		kw := strings.ToUpper(strings.TrimPrefix(first.Text, ":"))
		switch {
		case exprAfterKeyword[kw]:
			e, next := ParseExpression(tokens, start+1)
			se.Exprs = []*Expr{e}
			se.Complete = e.Kind != ExprUnknown && coversStatement(tokens, next, end)
			return se, true
		case kw == "RETURN":
			// Bare `:RETURN;` has no expression — the next significant
			// token is the terminator (or nothing).
			if coversStatement(tokens, start+1, end) {
				return se, false
			}
			e, next := ParseExpression(tokens, start+1)
			se.Exprs = []*Expr{e}
			se.Complete = e.Kind != ExprUnknown && coversStatement(tokens, next, end)
			return se, true
		case kw == "DEFAULT":
			// :DEFAULT ident, expr;
			idIdx := nextSignificantIndex(tokens, start+1, end)
			if idIdx < 0 || tokens[idIdx].Type != lexer.TokenIdentifier {
				return se, false
			}
			commaIdx := nextSignificantIndex(tokens, idIdx+1, end)
			if commaIdx < 0 || tokens[commaIdx].Type != lexer.TokenPunctuation || tokens[commaIdx].Text != "," {
				return se, false
			}
			target := &Expr{Kind: ExprIdentifier, Start: idIdx, End: idIdx, Name: tokens[idIdx].Text}
			e, next := ParseExpression(tokens, commaIdx+1)
			se.Exprs = []*Expr{target, e}
			se.Complete = e.Kind != ExprUnknown && coversStatement(tokens, next, end)
			return se, true
		case kw == "FOR":
			return parseForHeader(tokens, start, end)
		}
		return se, false
	}

	if first.Type != lexer.TokenIdentifier && !(first.Type == lexer.TokenPunctuation && first.Text == "(") &&
		first.Type != lexer.TokenOperator {
		return se, false
	}

	// Assignment or expression statement.
	lhs, next := ParseExpression(tokens, start)
	if lhs.Kind == ExprUnknown {
		se.Exprs = []*Expr{lhs}
		return se, true
	}
	opIdx := nextSignificantIndex(tokens, next, end)
	if opIdx >= 0 && tokens[opIdx].Type == lexer.TokenOperator && isAssignmentOperator(tokens[opIdx].Text) {
		rhs, after := ParseExpression(tokens, opIdx+1)
		se.Exprs = []*Expr{lhs, rhs}
		se.Assign = tokens[opIdx].Text
		se.Complete = rhs.Kind != ExprUnknown && coversStatement(tokens, after, end)
		return se, true
	}
	se.Exprs = []*Expr{lhs}
	se.Complete = coversStatement(tokens, next, end)
	return se, true
}

// parseForHeader parses `:FOR ident := expr :TO expr [:STEP expr];`.
func parseForHeader(tokens []lexer.Token, start, end int) (StatementExprs, bool) {
	se := StatementExprs{Start: start, End: end}
	idIdx := nextSignificantIndex(tokens, start+1, end)
	if idIdx < 0 || tokens[idIdx].Type != lexer.TokenIdentifier {
		return se, false
	}
	opIdx := nextSignificantIndex(tokens, idIdx+1, end)
	if opIdx < 0 || tokens[opIdx].Type != lexer.TokenOperator || tokens[opIdx].Text != ":=" {
		return se, false
	}
	target := &Expr{Kind: ExprIdentifier, Start: idIdx, End: idIdx, Name: tokens[idIdx].Text}
	se.Exprs = []*Expr{target}
	se.Assign = ":="

	fromExpr, next := ParseExpression(tokens, opIdx+1)
	se.Exprs = append(se.Exprs, fromExpr)
	complete := fromExpr.Kind != ExprUnknown

	// :TO expr
	toIdx := nextSignificantIndex(tokens, next, end)
	if toIdx < 0 || tokens[toIdx].Type != lexer.TokenKeyword ||
		!strings.EqualFold(strings.TrimPrefix(tokens[toIdx].Text, ":"), "TO") {
		se.Complete = false
		return se, true
	}
	toExpr, next := ParseExpression(tokens, toIdx+1)
	se.Exprs = append(se.Exprs, toExpr)
	complete = complete && toExpr.Kind != ExprUnknown

	// Optional :STEP expr
	stepIdx := nextSignificantIndex(tokens, next, end)
	if stepIdx >= 0 && tokens[stepIdx].Type == lexer.TokenKeyword &&
		strings.EqualFold(strings.TrimPrefix(tokens[stepIdx].Text, ":"), "STEP") {
		stepExpr, after := ParseExpression(tokens, stepIdx+1)
		se.Exprs = append(se.Exprs, stepExpr)
		complete = complete && stepExpr.Kind != ExprUnknown
		next = after
	}
	se.Complete = complete && coversStatement(tokens, next, end)
	return se, true
}

func isAssignmentOperator(text string) bool {
	switch text {
	case ":=", "+=", "-=", "*=", "/=", "^=", "%=":
		return true
	}
	return false
}

// nextSignificantIndex returns the first significant token index in
// [from, end], or -1.
func nextSignificantIndex(tokens []lexer.Token, from, end int) int {
	for j := from; j <= end && j < len(tokens); j++ {
		switch tokens[j].Type {
		case lexer.TokenWhitespace, lexer.TokenComment:
			continue
		}
		return j
	}
	return -1
}

// coversStatement reports whether nothing significant remains between
// `from` and the statement end (exclusive of the `;` itself).
func coversStatement(tokens []lexer.Token, from, end int) bool {
	idx := nextSignificantIndex(tokens, from, end)
	if idx < 0 {
		return true
	}
	t := tokens[idx]
	return t.Type == lexer.TokenPunctuation && t.Text == ";"
}
