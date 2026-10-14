package providers

// Expression-AST rules for run-time behaviors verified on STARLIMS 12 that
// the source alone can prove: a Branch() target that is not a literal, a
// literal non-logical condition, a .NOT. that binds before a comparison,
// and a fractional array index (issues #321, #322, #339).

import (
	"fmt"
	"strconv"
	"strings"

	"starlims-lsp/internal/lexer"
	"starlims-lsp/internal/parser"
)

// checkBranchLiteralTarget flags a Branch() call whose target is not a
// single string literal (diag.branch_literal_target, issue #321). Only a
// literal target is resolved against the labels; a computed one never
// jumps and falls through with no error, even when its text matches.
func checkBranchLiteralTarget(tokens []lexer.Token, calls []parser.CallSite) []Diagnostic {
	var out []Diagnostic
	for _, site := range calls {
		if site.Receiver != nil || !strings.EqualFold(site.Name, "Branch") || len(site.Args) == 0 {
			continue
		}
		arg := site.Args[0]
		if arg == nil || arg.Kind == parser.ExprSkipped || arg.Kind == parser.ExprUnknown {
			continue
		}
		if arg.Kind == parser.ExprLiteral && literalType(arg.Name) == typeString {
			continue
		}
		r, ok := exprRange(tokens, arg)
		if !ok {
			continue
		}
		out = append(out, Diagnostic{
			Severity: SeverityError,
			Range:    r,
			Message:  "Branch() only jumps for a string-literal target; this call always falls through. Choose among literal Branch() calls with ':IF' or ':BEGINCASE' instead",
			Source:   "ssl-lsp",
			Code:     CodeBranchLiteralTarget,
		})
	}
	return out
}

// stripGroups returns the expression inside any enclosing parentheses.
func stripGroups(e *parser.Expr) *parser.Expr {
	for e != nil && e.Kind == parser.ExprGroup && len(e.Children) == 1 {
		e = e.Children[0]
	}
	return e
}

// nonLogicalLiteral names the type of a literal that can never be a
// logical value — a string, number, NIL, array, or code-block literal —
// or returns "" when e is not one. Variables and calls make no claim.
func nonLogicalLiteral(e *parser.Expr) string {
	e = stripGroups(e)
	if e == nil {
		return ""
	}
	switch e.Kind {
	case parser.ExprLiteral:
		switch t := literalType(e.Name); t {
		case typeString, typeNumber, typeNIL:
			return t.String()
		}
	case parser.ExprUnary:
		if e.Op == "-" && len(e.Children) == 1 {
			if inner := stripGroups(e.Children[0]); inner != nil && inner.Kind == parser.ExprLiteral &&
				literalType(inner.Name) == typeNumber {
				return typeNumber.String()
			}
		}
	case parser.ExprArrayLiteral:
		return typeArray.String()
	case parser.ExprCodeBlock:
		return typeCodeBlock.String()
	}
	return ""
}

// article returns "a" or "an" for a type name.
func article(word string) string {
	if word != "" && strings.ContainsRune("aeiouAEIOU", rune(word[0])) && word != "NIL" {
		return "an"
	}
	return "a"
}

// checkNonLogicalCondition flags a literal non-logical value where SSL
// tests a logical one: an :IF / :WHILE / :CASE condition, and the left
// operand of .AND. / .OR. (diag.non_logical_condition, issue #322). SSL
// has no truthy/falsy coercion, so each raises when it runs.
func checkNonLogicalCondition(tokens []lexer.Token, stmts []parser.StatementExprs) []Diagnostic {
	var out []Diagnostic
	for _, se := range stmts {
		if se.Kind == parser.StmtCondition && len(se.Exprs) == 1 && se.Start < len(tokens) {
			if t := nonLogicalLiteral(se.Exprs[0]); t != "" {
				if r, ok := exprRange(tokens, se.Exprs[0]); ok {
					kw := strings.ToUpper(tokens[se.Start].Text)
					out = append(out, Diagnostic{
						Severity: SeverityError,
						Range:    r,
						Message:  fmt.Sprintf("'%s' condition is %s %s literal - SSL has no truthy/falsy coercion, so a non-logical condition raises at run time", kw, article(t), t),
						Source:   "ssl-lsp",
						Code:     CodeNonLogicalCondition,
					})
				}
			}
		}
		for _, root := range se.Exprs {
			walkExpr(root, func(e *parser.Expr) {
				if e.Kind != parser.ExprBinary || len(e.Children) != 2 {
					return
				}
				op := strings.ToUpper(e.Op)
				if op != ".AND." && op != ".OR." {
					return
				}
				t := nonLogicalLiteral(e.Children[0])
				if t == "" {
					return
				}
				if r, ok := exprRange(tokens, e.Children[0]); ok {
					out = append(out, Diagnostic{
						Severity: SeverityError,
						Range:    r,
						Message:  fmt.Sprintf("Left operand of '%s' is %s %s literal - '.AND.' and '.OR.' raise at run time on a non-logical left operand", op, article(t), t),
						Source:   "ssl-lsp",
						Code:     CodeNonLogicalCondition,
					})
				}
			})
		}
	}
	return out
}

// negatableComparisons are the operators a reader expects `.NOT.` to
// negate as a whole: equality, containment, and the relational operators.
var negatableComparisons = map[string]bool{
	"=": true, "==": true, "!=": true, "<>": true, "#": true, "$": true,
	"<": true, ">": true, "<=": true, ">=": true,
}

// isBareNegation reports whether e is an unparenthesized `.NOT. x` or `!x`.
func isBareNegation(e *parser.Expr) bool {
	if e == nil || e.Kind != parser.ExprUnary || len(e.Children) != 1 {
		return false
	}
	op := strings.ToUpper(e.Op)
	return op == ".NOT." || op == "!"
}

// checkNegatedComparison flags `.NOT. a = b` without parentheses
// (diag.parenthesize_negated_comparison, issue #339). `.NOT.` binds before
// every comparison, so the expression is `(.NOT. a) = b`: on a string or
// number operand it raises, and on a logical one it silently computes
// something other than the negated comparison.
func checkNegatedComparison(tokens []lexer.Token, stmts []parser.StatementExprs) []Diagnostic {
	var out []Diagnostic
	walkStatementExprs(stmts, func(e *parser.Expr) {
		if e.Kind != parser.ExprBinary || len(e.Children) != 2 || !negatableComparisons[e.Op] {
			return
		}
		neg := e.Children[0]
		if !isBareNegation(neg) {
			return
		}
		r, ok := exprRange(tokens, e)
		if !ok {
			return
		}
		sev := SeverityWarning
		msg := fmt.Sprintf("'%s' binds before '%s', so this negates the left operand alone - write '%s (a %s b)' to negate the comparison", neg.Op, e.Op, neg.Op, e.Op)
		if operand := stripGroups(neg.Children[0]); operand != nil && operand.Kind == parser.ExprLiteral {
			if t := literalType(operand.Name); t == typeString || t == typeNumber {
				sev = SeverityError
				msg = fmt.Sprintf("'%s' binds before '%s', so it applies to %s %s literal and raises at run time - write '%s (a %s b)' to negate the comparison", neg.Op, e.Op, article(t.String()), t, neg.Op, e.Op)
			}
		}
		out = append(out, Diagnostic{
			Severity: sev,
			Range:    r,
			Message:  msg,
			Source:   "ssl-lsp",
			Code:     CodeParenthesizeNegatedComparison,
		})
	})
	return out
}

// checkArrayIndexWholeNumber flags an array subscript that is a
// fractional numeric literal or is computed with `/`
// (diag.array_index_whole_number, issue #339). A non-integer index raises
// at run time, and `/` always divides exactly, so a quotient is whole only
// for some inputs.
func checkArrayIndexWholeNumber(tokens []lexer.Token, stmts []parser.StatementExprs) []Diagnostic {
	var out []Diagnostic
	walkStatementExprs(stmts, func(e *parser.Expr) {
		if e.Kind != parser.ExprIndex || len(e.Children) < 2 {
			return
		}
		for _, sub := range e.Children[1:] {
			reason := fractionalIndex(sub)
			if reason == "" {
				continue
			}
			r, ok := exprRange(tokens, sub)
			if !ok {
				continue
			}
			out = append(out, Diagnostic{
				Severity: SeverityError,
				Range:    r,
				Message:  reason + " - a non-integer array index raises at run time; wrap a computed index in Integer()",
				Source:   "ssl-lsp",
				Code:     CodeArrayIndexWholeNumber,
			})
		}
	})
	return out
}

// fractionalIndex explains why a subscript may not be a whole number, or
// returns "". It looks through parentheses and arithmetic, but not into
// calls, member accesses, or nested subscripts: `Integer(n / 2)` and
// `a[n / 2]` inside another index are judged on their own.
func fractionalIndex(sub *parser.Expr) string {
	sub = stripGroups(sub)
	if sub == nil {
		return ""
	}
	if sub.Kind == parser.ExprLiteral && literalType(sub.Name) == typeNumber {
		if v, ok := numericLiteralValue(sub.Name); ok && v != float64(int64(v)) {
			return fmt.Sprintf("Index %s is not a whole number", sub.Name)
		}
		return ""
	}
	found := ""
	var visit func(e *parser.Expr)
	visit = func(e *parser.Expr) {
		if e == nil || found != "" {
			return
		}
		switch e.Kind {
		case parser.ExprGroup, parser.ExprUnary:
			for _, c := range e.Children {
				visit(c)
			}
		case parser.ExprBinary:
			if e.Op == "/" && len(e.Children) == 2 && !evenLiteralQuotient(e) {
				found = "Index is computed with '/', which yields a fraction unless it divides evenly"
				return
			}
			for _, c := range e.Children {
				visit(c)
			}
		}
	}
	visit(sub)
	return found
}

// evenLiteralQuotient reports whether a division of two numeric literals
// comes out whole (`4 / 2`), which indexes fine.
func evenLiteralQuotient(e *parser.Expr) bool {
	l, r := stripGroups(e.Children[0]), stripGroups(e.Children[1])
	if l == nil || r == nil || l.Kind != parser.ExprLiteral || r.Kind != parser.ExprLiteral {
		return false
	}
	a, okA := numericLiteralValue(l.Name)
	b, okB := numericLiteralValue(r.Name)
	if !okA || !okB || b == 0 {
		return false
	}
	q := a / b
	return q == float64(int64(q))
}

// numericLiteralValue parses an SSL numeric literal's text.
func numericLiteralValue(text string) (float64, bool) {
	v, err := strconv.ParseFloat(text, 64)
	return v, err == nil
}
