package parser

// Statement-level entry points for the expression AST (issue #184): walk a
// token stream statement by statement and yield the expression trees each
// one contains. Built on demand by consumers — nothing here runs during
// the structural Parse().

import (
	"strings"

	"starlims-lsp/internal/lexer"
)

// StatementKind names the statement shape a StatementExprs came from, so
// consumers can tell an assignment from a `:DEFAULT` or a loop header
// without re-reading the tokens.
type StatementKind int

const (
	// StmtExpression is a bare expression statement (`UsrMes("hi");`).
	StmtExpression StatementKind = iota
	// StmtAssign is `target <op> value`; Exprs is [target, value] and
	// Assign holds the operator.
	StmtAssign
	// StmtDefault is `:DEFAULT ident, value`; Exprs is [target, value].
	StmtDefault
	// StmtFor is a `:FOR` header; Exprs is [target, from, to] plus the
	// optional step.
	StmtFor
	// StmtCondition is the expression after `:IF` / `:WHILE` / `:CASE`.
	StmtCondition
	// StmtReturn is `:RETURN value` with a value present.
	StmtReturn
)

// StatementExprs is the parsed expression content of one statement.
type StatementExprs struct {
	// Kind is the statement shape Exprs was parsed from.
	Kind StatementKind
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
	// statement: every significant token is inside some tree in Exprs and
	// no tree contains an ExprUnknown. Equivalent to Unexpected < 0.
	// Consumers wanting zero-risk claims should require Complete.
	Complete bool
	// Unexpected is the index of the first significant token the statement
	// grammar could not accept (diag.unexpected_token), or -1 when the
	// statement is Complete. It is the token to report, not the statement.
	Unexpected int
	// Expected names what the grammar wanted at Unexpected when the parser
	// knows it structurally — a `:FOR` header piece, a missing condition or
	// operand. It is "" when the wording is better derived from context
	// (an operator was expected after a finished expression, a comma inside
	// an argument list); consumers own that derivation.
	Expected string
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
	se := StatementExprs{Start: start, End: end, Unexpected: -1}
	first := tokens[start]

	if first.Type == lexer.TokenKeyword {
		kw := strings.ToUpper(strings.TrimPrefix(first.Text, ":"))
		switch {
		case exprAfterKeyword[kw]:
			e, next, fail := parseValueAt(tokens, start+1)
			se.Kind = StmtCondition
			se.Exprs = []*Expr{e}
			se.finish(tokens, e, start+1, next, fail, "a condition", "")
			return se, true
		case kw == "RETURN":
			e, next, fail := parseValueAt(tokens, start+1)
			if e.Kind == ExprUnknown && fail >= 0 && isTerminator(tokens[fail]) {
				// Bare `:RETURN;` — the operand is optional, so there is
				// no expression here and nothing to report.
				return se, false
			}
			se.Kind = StmtReturn
			se.Exprs = []*Expr{e}
			se.finish(tokens, e, start+1, next, fail, "an expression", "")
			return se, true
		case kw == "DEFAULT":
			// :DEFAULT ident, expr;
			se.Kind = StmtDefault
			idIdx := nextSignificantIndex(tokens, start+1, end)
			if idIdx < 0 {
				idIdx = end
			}
			if tokens[idIdx].Type != lexer.TokenIdentifier {
				se.Unexpected, se.Expected = idIdx, "a parameter name"
				se.Exprs = []*Expr{unknownAt(idIdx), unknownAt(idIdx)}
				return se, true
			}
			target := &Expr{Kind: ExprIdentifier, Start: idIdx, End: idIdx, Name: tokens[idIdx].Text}
			commaIdx := nextSignificantIndex(tokens, idIdx+1, end)
			if commaIdx < 0 {
				commaIdx = end
			}
			if tokens[commaIdx].Type != lexer.TokenPunctuation || tokens[commaIdx].Text != "," {
				se.Unexpected, se.Expected = commaIdx, "','"
				se.Exprs = []*Expr{target, unknownAt(commaIdx)}
				return se, true
			}
			e, next, fail := parseValueAt(tokens, commaIdx+1)
			se.Exprs = []*Expr{target, e}
			se.finish(tokens, e, commaIdx+1, next, fail, "a default value", "")
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
	lhs, next, fail := parseExpressionAt(tokens, start)
	if fail >= 0 {
		se.Exprs = []*Expr{lhs}
		se.finish(tokens, lhs, start, next, fail, "a statement", "")
		return se, true
	}
	opIdx := nextSignificantIndex(tokens, next, end)
	if opIdx >= 0 && tokens[opIdx].Type == lexer.TokenOperator && isAssignmentOperator(tokens[opIdx].Text) {
		rhs, after, rfail := parseValueAt(tokens, opIdx+1)
		se.Kind = StmtAssign
		se.Exprs = []*Expr{lhs, rhs}
		se.Assign = tokens[opIdx].Text
		se.finish(tokens, rhs, opIdx+1, after, rfail, "an expression", "")
		return se, true
	}
	se.Exprs = []*Expr{lhs}
	se.finish(tokens, lhs, start, next, -1, "", "")
	return se, true
}

// parseForHeader parses `:FOR ident := expr :TO expr [:STEP expr];`. A
// header that departs from that shape is still returned (ok=true) with
// Unexpected on the first departing token and Expected naming the piece
// that belonged there, so the departure can be reported; Exprs holds
// whatever was parsed before it.
func parseForHeader(tokens []lexer.Token, start, end int) (StatementExprs, bool) {
	se := StatementExprs{Start: start, End: end, Kind: StmtFor, Unexpected: -1}
	idIdx := nextSignificantIndex(tokens, start+1, end)
	if idIdx < 0 {
		idIdx = end
	}
	if tokens[idIdx].Type != lexer.TokenIdentifier {
		se.Unexpected, se.Expected = idIdx, "a loop variable"
		return se, true
	}
	target := &Expr{Kind: ExprIdentifier, Start: idIdx, End: idIdx, Name: tokens[idIdx].Text}
	se.Exprs = []*Expr{target}
	se.Assign = ":="

	opIdx := nextSignificantIndex(tokens, idIdx+1, end)
	if opIdx < 0 {
		opIdx = end
	}
	if tokens[opIdx].Type != lexer.TokenOperator || tokens[opIdx].Text != ":=" {
		se.Unexpected, se.Expected = opIdx, "':='"
		return se, true
	}

	fromExpr, next, fail := parseValueAt(tokens, opIdx+1)
	se.Exprs = append(se.Exprs, fromExpr)
	if idx, exp := exprFailure(tokens, opIdx+1, fail, end, "an expression after ':='"); idx >= 0 {
		se.Unexpected, se.Expected = idx, exp
		return se, true
	}

	// :TO expr
	toIdx := nextSignificantIndex(tokens, next, end)
	if toIdx < 0 {
		toIdx = end
	}
	if !isKeyword(tokens[toIdx], "TO") {
		se.Unexpected, se.Expected = toIdx, "':TO'"
		return se, true
	}
	toExpr, next, fail := parseValueAt(tokens, toIdx+1)
	se.Exprs = append(se.Exprs, toExpr)
	if idx, exp := exprFailure(tokens, toIdx+1, fail, end, "an expression after ':TO'"); idx >= 0 {
		se.Unexpected, se.Expected = idx, exp
		return se, true
	}

	// Optional :STEP expr
	tail := "':STEP' or ';'"
	if stepIdx := nextSignificantIndex(tokens, next, end); stepIdx >= 0 && isKeyword(tokens[stepIdx], "STEP") {
		stepExpr, after, sfail := parseValueAt(tokens, stepIdx+1)
		se.Exprs = append(se.Exprs, stepExpr)
		if idx, exp := exprFailure(tokens, stepIdx+1, sfail, end, "an expression after ':STEP'"); idx >= 0 {
			se.Unexpected, se.Expected = idx, exp
			return se, true
		}
		next = after
		tail = "';'"
	}
	if idx := leftover(tokens, next, end); idx >= 0 {
		se.Unexpected, se.Expected = idx, tail
		return se, true
	}
	se.Complete = true
	return se, true
}

// finish sets Unexpected/Expected/Complete for a statement whose last
// parsed piece is e (parsed from `from`, failing at `fail`, leaving
// `next`). `empty` names what belonged at `from` when no expression
// started there; `tail` names what may follow a finished expression when
// that is more specific than the consumer's default wording.
func (se *StatementExprs) finish(tokens []lexer.Token, e *Expr, from, next, fail int, empty, tail string) {
	if idx, exp := exprFailure(tokens, from, fail, se.End, empty); idx >= 0 {
		se.Unexpected, se.Expected = idx, exp
		return
	}
	if idx := leftover(tokens, next, se.End); idx >= 0 {
		se.Unexpected, se.Expected = idx, tail
		return
	}
	se.Complete = true
}

// exprFailure maps an expression parser failure to (index, expected).
// When the failure sits on the first significant token from `from`, no
// expression started at all and `empty` is what belonged there; a failure
// deeper in leaves Expected "" for the consumer to word from context.
func exprFailure(tokens []lexer.Token, from, fail, end int, empty string) (int, string) {
	if fail < 0 {
		return -1, ""
	}
	if nextSignificantIndex(tokens, from, end) == fail {
		return fail, empty
	}
	return fail, ""
}

// leftover returns the first significant token at or after `next` that is
// not the statement's terminator, or -1 when the expression reached the
// `;` or the end of input.
func leftover(tokens []lexer.Token, next, end int) int {
	idx := nextSignificantIndex(tokens, next, end)
	if idx < 0 || isTerminator(tokens[idx]) {
		return -1
	}
	return idx
}

func isTerminator(t lexer.Token) bool {
	return t.Type == lexer.TokenEOF || (t.Type == lexer.TokenPunctuation && t.Text == ";")
}

func isKeyword(t lexer.Token, name string) bool {
	return t.Type == lexer.TokenKeyword && strings.EqualFold(strings.TrimPrefix(t.Text, ":"), name)
}

func unknownAt(idx int) *Expr {
	return &Expr{Kind: ExprUnknown, Start: idx, End: idx}
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
