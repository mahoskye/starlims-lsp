package providers

// Operand checks on the expression AST (issue #255): rules that used to
// read the tokens on either side of an operator now read the operator's
// operands from the tree, so precedence, calls, and member chains are
// seen for what they are.

import (
	"fmt"
	"strings"

	"starlims-lsp/internal/lexer"
	"starlims-lsp/internal/parser"
)

// walkExpr calls fn on e and every node below it, parents first.
func walkExpr(e *parser.Expr, fn func(*parser.Expr)) {
	if e == nil {
		return
	}
	fn(e)
	for _, child := range e.Children {
		walkExpr(child, fn)
	}
}

// walkStatementExprs calls fn on every expression node of every statement.
func walkStatementExprs(stmts []parser.StatementExprs, fn func(*parser.Expr)) {
	for _, stmt := range stmts {
		for _, e := range stmt.Exprs {
			walkExpr(e, fn)
		}
	}
}

// operatorTokenIndex returns the index of the operator token of a unary or
// binary node, or -1 when it cannot be located.
func operatorTokenIndex(tokens []lexer.Token, e *parser.Expr) int {
	from, to := e.Start, e.End
	switch e.Kind {
	case parser.ExprBinary:
		if len(e.Children) != 2 || e.Children[0] == nil || e.Children[1] == nil {
			return -1
		}
		from, to = e.Children[0].End+1, e.Children[1].Start-1
	case parser.ExprUnary:
		if len(e.Children) != 1 || e.Children[0] == nil {
			return -1
		}
		to = e.Children[0].Start - 1
	default:
		return -1
	}
	for i := from; i <= to && i >= 0 && i < len(tokens); i++ {
		if tokens[i].Type == lexer.TokenOperator && strings.EqualFold(tokens[i].Text, e.Op) {
			return i
		}
	}
	return -1
}

// isNilOperand reports whether e is the NIL literal itself — not a group,
// call, or member chain built on it. A literal directly followed by `:` is
// the receiver of a member chain even where the tree stops short of it:
// the lexer reads `NIL:ToString` as NIL plus a colon-word, which the
// expression grammar does not take as member access.
func isNilOperand(tokens []lexer.Token, e *parser.Expr) bool {
	if e == nil || e.Kind != parser.ExprLiteral || !strings.EqualFold(e.Name, "NIL") {
		return false
	}
	if next := nextSignificantTokenIndex(tokens, e.End+1); next >= 0 {
		t := tokens[next]
		if strings.HasPrefix(t.Text, ":") && t.Text != ":=" {
			return false
		}
	}
	return true
}

// checkNilInOperations flags +, -, *, / (and unary minus) whose operand is
// the NIL literal itself (diag.nil_in_operations). `1 + NIL:ToString()`
// is not flagged here: its operand is a member call, reported by
// nil_method_call.
func checkNilInOperations(tokens []lexer.Token, stmts []parser.StatementExprs) []Diagnostic {
	var diagnostics []Diagnostic
	walkStatementExprs(stmts, func(e *parser.Expr) {
		hit := false
		switch e.Kind {
		case parser.ExprBinary:
			switch e.Op {
			case "+", "-", "*", "/":
				hit = len(e.Children) == 2 && (isNilOperand(tokens, e.Children[0]) || isNilOperand(tokens, e.Children[1]))
			}
		case parser.ExprUnary:
			hit = e.Op == "-" && len(e.Children) == 1 && isNilOperand(tokens, e.Children[0])
		}
		if !hit {
			return
		}
		idx := operatorTokenIndex(tokens, e)
		if idx < 0 {
			return
		}
		diagnostics = append(diagnostics, Diagnostic{
			Severity: SeverityWarning,
			Range:    tokenToRange(tokens[idx]),
			Message:  "Using NIL in arithmetic or string operations causes error. Use Empty() to check for NIL first",
			Source:   "ssl-lsp",
			Code:     CodeNilInOperations,
		})
	})
	return diagnostics
}

// plusOperandType infers the type of a `+` operand for
// diag.mixed_type_operator, in the vocabulary of its messages ("string",
// "numeric", "boolean", "date", "array", "object", "codeblock", "nil"),
// or "" for no claim. Evidence is literals, strict Hungarian prefixes on
// bare identifiers, documented builtin return types, and the result of a
// nested operation whose operands are themselves known. Member access and
// indexed access are opaque, and loop-counter / constant names carry no
// type. Nothing is inferred from assignments: a value's type at one point
// of a file says nothing about another.
func plusOperandType(e *parser.Expr, depth int) string {
	if e == nil || depth > maxInferDepth {
		return ""
	}
	switch e.Kind {
	case parser.ExprLiteral:
		switch literalType(e.Name) {
		case typeString:
			return "string"
		case typeNumber:
			return "numeric"
		case typeBoolean:
			return "boolean"
		case typeNIL:
			return "nil"
		}
	case parser.ExprIdentifier:
		if isHungarianExemptName(e.Name) {
			return ""
		}
		return inferTypeFromName(e.Name)
	case parser.ExprGroup:
		if len(e.Children) == 1 {
			return plusOperandType(e.Children[0], depth+1)
		}
	case parser.ExprCall:
		if len(e.Children) > 0 && e.Children[0].Kind == parser.ExprIdentifier {
			return inferFunctionReturnType(e.Children[0].Name)
		}
	case parser.ExprInstantiate:
		return "object"
	case parser.ExprArrayLiteral:
		return "array"
	case parser.ExprCodeBlock:
		return "codeblock"
	case parser.ExprUnary:
		switch strings.ToUpper(e.Op) {
		case ".NOT.", "!":
			return "boolean"
		case "-":
			if len(e.Children) == 1 && plusOperandType(e.Children[0], depth+1) == "numeric" {
				return "numeric"
			}
		}
	case parser.ExprBinary:
		op := strings.ToUpper(e.Op)
		if isBooleanOperator(op) {
			return "boolean"
		}
		if len(e.Children) != 2 {
			return ""
		}
		left := plusOperandType(e.Children[0], depth+1)
		right := plusOperandType(e.Children[1], depth+1)
		if op == "+" && (left == "string" || right == "string") {
			// The only `+` that accepts a string yields a string; any
			// other pairing raises (and is reported where it occurs). So
			// in `oRec:Name + " - " + nCount` the chain left of the last
			// `+` is a string even though its head is unknown.
			return "string"
		}
		if left == "" || right == "" {
			return ""
		}
		return arithmeticResultType(op, left, right)
	}
	return ""
}

// arithmeticResultType is the result of a supported arithmetic pairing,
// or "" when the language documents none (the pairing raises).
func arithmeticResultType(op, left, right string) string {
	switch op {
	case "+", "-", "*", "/", "%", "^", "**":
		if left == "numeric" && right == "numeric" {
			return "numeric"
		}
	default:
		return ""
	}
	switch {
	case op == "+" && left == "string" && right == "string":
		return "string"
	case op == "-" && left == "string" && right == "string":
		return "string"
	case (op == "+" || op == "-") && left == "date" && right == "numeric":
		return "date"
	case op == "-" && left == "date" && right == "date":
		return "numeric"
	}
	return ""
}

// checkMixedTypePlus flags a `+` whose operands infer to different types
// (diag.mixed_type_operator). `date + number` is the one supported mixed
// pairing and is never flagged; a NIL operand belongs to
// nil_in_operations.
func checkMixedTypePlus(tokens []lexer.Token, stmts []parser.StatementExprs) []Diagnostic {
	var diagnostics []Diagnostic
	walkStatementExprs(stmts, func(e *parser.Expr) {
		if e.Kind != parser.ExprBinary || e.Op != "+" || len(e.Children) != 2 {
			return
		}
		left := plusOperandType(e.Children[0], 0)
		right := plusOperandType(e.Children[1], 0)
		if left == "" || right == "" || left == right || left == "nil" || right == "nil" {
			return
		}
		if left == "date" && right == "numeric" {
			return
		}
		idx := operatorTokenIndex(tokens, e)
		if idx < 0 {
			return
		}
		diagnostics = append(diagnostics, Diagnostic{
			Severity: SeverityWarning,
			Range:    tokenToRange(tokens[idx]),
			Message:  fmt.Sprintf("Mixed types in '+' operation: %s + %s. The '+' operator requires both operands to be the same type (both strings or both numeric)", left, right),
			Source:   "ssl-lsp",
			Code:     CodeMixedTypeOperator,
		})
	})
	return diagnostics
}

// conditionExprsByKeyword maps the token index of each :IF / :WHILE /
// :CASE keyword to its parsed condition tree.
func conditionExprsByKeyword(stmts []parser.StatementExprs) map[int]*parser.Expr {
	out := make(map[int]*parser.Expr)
	for _, stmt := range stmts {
		if stmt.Kind == parser.StmtCondition && len(stmt.Exprs) == 1 {
			out[stmt.Start] = stmt.Exprs[0]
		}
	}
	return out
}

// negationGovernsCondition reports whether a condition whose first token
// is a negation is, as a whole, that negation. It is false only when the
// tree definitively shows a binary operation at the root
// (`.NOT. bA .AND. bB` is `(.NOT. bA) .AND. bB`); a missing or
// unresolved tree keeps the token-level answer.
func negationGovernsCondition(cond *parser.Expr) bool {
	for cond != nil && cond.Kind == parser.ExprGroup && len(cond.Children) == 1 {
		cond = cond.Children[0]
	}
	return cond == nil || cond.Kind != parser.ExprBinary
}
