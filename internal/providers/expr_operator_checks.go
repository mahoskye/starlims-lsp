package providers

// Operator type checks over the expression AST (issue #255). These rules
// used to scan adjacent tokens and type them with name heuristics that read
// loop-counter names (i, j, k, x, y, z) and UPPER_SNAKE constants as
// numbers, which reported `"A" $ ALLOWED_CODES` and `dDue - 1` as errors.
// On the tree, each operand is a whole subexpression, and an operand pair
// is judged against the operator type tables the reference documents
// (operatorResults, generated from the element inventory). Unknown is no
// claim, never evidence.
//
// The `+` operator (diag.mixed_type_operator) is deliberately not handled
// here.

import (
	"fmt"
	"strings"

	"starlims-lsp/internal/lexer"
	"starlims-lsp/internal/parser"
)

// arithmeticOps are the binary operators that only accept documented
// operand pairs: `-` (numbers, strings, dates) and the numeric-only rest.
var arithmeticOps = map[string]bool{
	"-": true, "*": true, "/": true, "%": true, "^": true, "**": true,
}

// equalityOps are the comparison operators that raise on a code block.
var equalityOps = map[string]bool{
	"=": true, "==": true, "!=": true, "<>": true, "#": true,
}

// walkExprs visits every node of every statement tree, parents first.
func walkExprs(stmts []parser.StatementExprs, visit func(*parser.Expr)) {
	var walk func(e *parser.Expr, depth int)
	walk = func(e *parser.Expr, depth int) {
		if e == nil || depth > maxInferDepth {
			return
		}
		visit(e)
		for _, c := range e.Children {
			walk(c, depth+1)
		}
	}
	for _, se := range stmts {
		for _, e := range se.Exprs {
			walk(e, 0)
		}
	}
}

// containsMember reports whether a subtree reads a `:` member. A member's
// Hungarian-looking name says nothing reliable about the value the
// receiver holds (commit d134334), so operand typing for these rules never
// rests on one.
func containsMember(e *parser.Expr, depth int) bool {
	if e == nil || depth > maxInferDepth {
		return false
	}
	if e.Kind == parser.ExprMember {
		return true
	}
	for _, c := range e.Children {
		if containsMember(c, depth+1) {
			return true
		}
	}
	return false
}

// operandType infers an operand's type from structure, the element
// inventory, and Hungarian prefixes on bare identifiers. A subtree that
// reads a member makes no claim.
func operandType(e *parser.Expr) sslType {
	if containsMember(e, 0) {
		return typeUnknown
	}
	return inferExprTypeNamed(e)
}

// operatorTokenIndex returns the index of a binary node's operator token:
// the first significant token after its left operand.
func operatorTokenIndex(tokens []lexer.Token, e *parser.Expr) int {
	if e == nil || len(e.Children) != 2 || e.Children[0] == nil {
		return -1
	}
	idx := nextSignificantTokenIndex(tokens, e.Children[0].End+1)
	if idx < 0 || idx >= len(tokens) || idx > e.End {
		return -1
	}
	return idx
}

// checkOperatorOperandTypes reports operand pairs that the operator's
// documented type table rejects: diag.arithmetic_type_mismatch for
// `-` `*` `/` `%` `^` `**`, diag.dollar_string_only for `$`, and
// diag.code_block_comparison for the equality operators.
func checkOperatorOperandTypes(tokens []lexer.Token, stmts []parser.StatementExprs) []Diagnostic {
	var diagnostics []Diagnostic

	walkExprs(stmts, func(e *parser.Expr) {
		if e.Kind != parser.ExprBinary || len(e.Children) != 2 {
			return
		}
		op := strings.ToUpper(e.Op)
		switch {
		case arithmeticOps[op]:
			if d, ok := arithmeticMismatch(tokens, e, op); ok {
				diagnostics = append(diagnostics, d)
			}
		case op == "$":
			if d, ok := dollarMismatch(tokens, e); ok {
				diagnostics = append(diagnostics, d)
			}
		case equalityOps[op]:
			if d, ok := codeBlockComparison(tokens, e, op); ok {
				diagnostics = append(diagnostics, d)
			}
		}
	})

	return diagnostics
}

func arithmeticMismatch(tokens []lexer.Token, e *parser.Expr, op string) (Diagnostic, bool) {
	left := operandType(e.Children[0])
	right := operandType(e.Children[1])
	// NIL operands belong to diag.nil_in_operations; unknown is no claim.
	if left == typeUnknown || right == typeUnknown || left == typeNIL || right == typeNIL {
		return Diagnostic{}, false
	}
	if _, documented := operatorResults[operandPair{op, left, right}]; documented {
		return Diagnostic{}, false
	}
	opIdx := operatorTokenIndex(tokens, e)
	if opIdx < 0 {
		return Diagnostic{}, false
	}
	msg := fmt.Sprintf("Non-numeric type in arithmetic operation '%s': %s %s %s. The operator does not accept this operand pair and raises a runtime error", e.Op, left, e.Op, right)
	if left == typeString || right == typeString {
		msg = fmt.Sprintf("String in arithmetic operation '%s': %s %s %s. Arithmetic operators require numeric operands", e.Op, left, e.Op, right)
	}
	return Diagnostic{
		Severity: SeverityWarning,
		Range:    tokenToRange(tokens[opIdx]),
		Message:  msg,
		Source:   "ssl-lsp",
		Code:     CodeArithmeticTypeMismatch,
	}, true
}

func dollarMismatch(tokens []lexer.Token, e *parser.Expr) (Diagnostic, bool) {
	left := operandType(e.Children[0])
	right := operandType(e.Children[1])
	if (left == typeUnknown || left == typeString) && (right == typeUnknown || right == typeString) {
		return Diagnostic{}, false
	}
	opIdx := operatorTokenIndex(tokens, e)
	if opIdx < 0 {
		return Diagnostic{}, false
	}
	return Diagnostic{
		Severity: SeverityWarning,
		Range:    tokenToRange(tokens[opIdx]),
		Message:  "The '$' containment operator only works on strings. Non-string operands cause error",
		Source:   "ssl-lsp",
		Code:     CodeDollarStringOnly,
	}, true
}

func codeBlockComparison(tokens []lexer.Token, e *parser.Expr, op string) (Diagnostic, bool) {
	if operandType(e.Children[0]) != typeCodeBlock && operandType(e.Children[1]) != typeCodeBlock {
		return Diagnostic{}, false
	}
	opIdx := operatorTokenIndex(tokens, e)
	if opIdx < 0 {
		return Diagnostic{}, false
	}
	return Diagnostic{
		Severity: SeverityWarning,
		Range:    tokenToRange(tokens[opIdx]),
		Message:  fmt.Sprintf("Code blocks (lambdas) cannot be compared with '%s'. This causes an error", e.Op),
		Source:   "ssl-lsp",
		Code:     CodeCodeBlockComparison,
	}, true
}

// checkEqualsEmptyString reports `x = ""` (diag.equals_empty_string).
// `=` on strings is a prefix match that is true whenever the right
// operand is empty, so for a string on the left the comparison is always
// .T.; it is false only when the left value is NIL. A literal on the left
// is skipped: `"" = sName` is a correct emptiness test.
func checkEqualsEmptyString(tokens []lexer.Token, stmts []parser.StatementExprs) []Diagnostic {
	var diagnostics []Diagnostic

	// A statement that is nothing but `x = ""` is not a test of anything;
	// it is almost always a mistyped assignment, which is a different
	// finding. Only comparisons inside a larger expression or a condition
	// are judged here.
	statementLevel := map[*parser.Expr]bool{}
	for _, se := range stmts {
		if se.Kind == parser.StmtExpression && len(se.Exprs) == 1 {
			statementLevel[se.Exprs[0]] = true
		}
	}

	walkExprs(stmts, func(e *parser.Expr) {
		if e.Kind != parser.ExprBinary || e.Op != "=" || len(e.Children) != 2 || statementLevel[e] {
			return
		}
		left, right := e.Children[0], e.Children[1]
		if right == nil || right.Kind != parser.ExprLiteral || !isEmptyStringText(right.Name) {
			return
		}
		if left == nil || left.Kind == parser.ExprLiteral {
			return
		}
		// A left operand known not to be a string is a different finding
		// (the comparison is .F. or raises), not this one.
		if t := operandType(left); t != typeUnknown && t != typeString {
			return
		}
		opIdx := operatorTokenIndex(tokens, e)
		if opIdx < 0 {
			return
		}
		rng := tokenToRange(tokens[opIdx])
		if r, ok := exprRange(tokens, e); ok {
			rng = r
		}
		diagnostics = append(diagnostics, Diagnostic{
			Severity: SeverityWarning,
			Range:    rng,
			Message:  "'= \"\"' is always true for a string: '=' is a prefix match and every string starts with the empty string. Use Empty(x) or x == \"\" to test for an empty value",
			Source:   "ssl-lsp",
			Code:     CodeEqualsEmptyString,
		})
	})

	return diagnostics
}

// isEmptyStringText reports whether a literal's source text is an empty
// string in any of SSL's three quote styles.
func isEmptyStringText(text string) bool {
	switch text {
	case `""`, `''`, `[]`:
		return true
	}
	return false
}

// checkForNumericValues reports `:FOR` header parts that cannot be numeric
// (diag.for_numeric_values): the start, limit, and step expressions when
// they infer to a definite non-number, and the loop variable when its
// Hungarian prefix promises a non-number and the start value does not
// make it one.
func checkForNumericValues(tokens []lexer.Token, stmts []parser.StatementExprs) []Diagnostic {
	var diagnostics []Diagnostic

	report := func(e *parser.Expr, msg string) {
		rng, ok := exprRange(tokens, e)
		if !ok {
			return
		}
		diagnostics = append(diagnostics, Diagnostic{
			Severity: SeverityWarning,
			Range:    rng,
			Message:  msg,
			Source:   "ssl-lsp",
			Code:     CodeForNumericValues,
		})
	}

	for _, se := range stmts {
		if se.Kind != parser.StmtFor || len(se.Exprs) < 3 {
			continue
		}
		target, start := se.Exprs[0], se.Exprs[1]
		startType := operandType(start)
		if target != nil && target.Kind == parser.ExprIdentifier {
			if t := hungarianType(target.Name); t != typeUnknown && t != typeNumber && startType != typeNumber {
				report(target, "':FOR' loop variable should be numeric")
			}
		}
		roles := []string{"start", "limit", "step"}
		for i, e := range se.Exprs[1:] {
			if i >= len(roles) || e == nil || e.Kind == parser.ExprUnknown {
				continue
			}
			if t := operandType(e); t != typeUnknown && t != typeNumber {
				report(e, fmt.Sprintf("':FOR' %s value should be numeric", roles[i]))
			}
		}
	}

	return diagnostics
}
