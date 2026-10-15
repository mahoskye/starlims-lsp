package providers

// "Atoms of confusion" (issues #339, #347): small expression shapes that
// readers reliably misread, each verified on STARLIMS 12 and decided by
// ssl-style-guide. All four are style findings over the expression AST.

import (
	"fmt"
	"strings"

	"starlims-lsp/internal/lexer"
	"starlims-lsp/internal/parser"
)

// isAssignmentOp reports whether a binary operator is an assignment.
func isAssignmentOp(op string) bool {
	switch op {
	case ":=", "+=", "-=", "*=", "/=", "^=", "%=":
		return true
	}
	return false
}

// exprKey identifies the variable an expression names, for matching an
// increment's target against other uses: a bare name compares
// case-insensitively, anything else by its tree shape.
func exprKey(e *parser.Expr) string {
	if e == nil {
		return ""
	}
	if e.Kind == parser.ExprIdentifier {
		return "id:" + strings.ToUpper(e.Name)
	}
	return strings.ToUpper(e.String())
}

// checkIncrementAsStatement flags `++`/`--` used inside a larger
// expression (diag.increment_as_statement, issue #347). It is info for a
// single use and warning when the same expression uses the incremented
// variable again (`i++ + i++`, `a[i++] := i`): operands evaluate left to
// right and each increment lands before the next operand is read. A whole
// statement (`i++;`) and a `:FOR` step are the intended forms.
func checkIncrementAsStatement(tokens []lexer.Token, stmts []parser.StatementExprs) []Diagnostic {
	var out []Diagnostic
	for _, se := range stmts {
		var incs []*parser.Expr
		for i, root := range se.Exprs {
			if root != nil && root.Kind == parser.ExprIncrement &&
				(se.Kind == parser.StmtExpression || (se.Kind == parser.StmtFor && i == 3)) {
				// The statement itself, or the :FOR step.
				for _, c := range root.Children {
					walkExpr(c, func(e *parser.Expr) {
						if e.Kind == parser.ExprIncrement {
							incs = append(incs, e)
						}
					})
				}
				continue
			}
			walkExpr(root, func(e *parser.Expr) {
				if e.Kind == parser.ExprIncrement {
					incs = append(incs, e)
				}
			})
		}
		if len(incs) == 0 {
			continue
		}
		// Every use of each name in the statement, increment targets
		// included.
		uses := map[string]int{}
		for _, root := range se.Exprs {
			walkExpr(root, func(e *parser.Expr) {
				if e.Kind == parser.ExprIdentifier || e.Kind == parser.ExprMember || e.Kind == parser.ExprIndex {
					uses[exprKey(e)]++
				}
			})
		}
		reported := map[string]bool{}
		for _, inc := range incs {
			if len(inc.Children) != 1 {
				continue
			}
			key := exprKey(inc.Children[0])
			if reported[key] {
				continue
			}
			reported[key] = true
			r, ok := exprRange(tokens, inc)
			if !ok {
				continue
			}
			name := strings.TrimSpace(tokenSpanText(tokens, inc.Children[0]))
			d := Diagnostic{
				Severity: SeverityInfo,
				Range:    r,
				Message:  fmt.Sprintf("'%s' is used inside a larger expression - postfix yields the old value and prefix the new; write the increment as its own statement", inc.Op),
				Source:   "ssl-lsp",
				Code:     CodeIncrementAsStatement,
			}
			if uses[key] > 1 {
				d.Severity = SeverityWarning
				d.Message = fmt.Sprintf("'%s' is incremented and used again in the same expression - operands evaluate left to right and each '%s' lands before the next is read; increment it in its own statement", name, inc.Op)
			}
			out = append(out, d)
		}
	}
	return out
}

// tokenSpanText joins the token texts an expression covers.
func tokenSpanText(tokens []lexer.Token, e *parser.Expr) string {
	var b strings.Builder
	for i := e.Start; i <= e.End && i < len(tokens); i++ {
		if tokens[i].Type == lexer.TokenWhitespace || tokens[i].Type == lexer.TokenComment {
			continue
		}
		b.WriteString(tokens[i].Text)
	}
	return b.String()
}

// checkMixedLogical flags an unparenthesized `.AND.` group that is an
// operand of `.OR.` (diag.parenthesize_mixed_logical, issue #347). `.AND.`
// binds tighter, so `bA .OR. bB .AND. bC` is `bA .OR. (bB .AND. bC)`, and
// in `.T. .OR. X .AND. Y` neither X nor Y runs. One finding per group, on
// the group.
func checkMixedLogical(tokens []lexer.Token, stmts []parser.StatementExprs) []Diagnostic {
	var out []Diagnostic
	walkStatementExprs(stmts, func(e *parser.Expr) {
		if e.Kind != parser.ExprBinary || len(e.Children) != 2 || strings.ToUpper(e.Op) != ".OR." {
			return
		}
		for _, c := range e.Children {
			if c == nil || c.Kind != parser.ExprBinary || strings.ToUpper(c.Op) != ".AND." {
				continue
			}
			r, ok := exprRange(tokens, c)
			if !ok {
				continue
			}
			out = append(out, Diagnostic{
				Severity: SeverityWarning,
				Range:    r,
				Message:  "'.AND.' binds tighter than '.OR.', so this group is evaluated first - parenthesize it to state the grouping",
				Source:   "ssl-lsp",
				Code:     CodeParenthesizeMixedLogical,
			})
		}
	})
	return out
}

// checkEmbeddedAssignment flags an assignment used inside a larger
// expression (diag.one_assignment_per_statement, issue #347): `(n := 5) *
// 2`, `{n := 1}`, `F(n := 3)`, `:RETURN n := 1`. A statement's own
// assignment and a pure chain (`a := b := v`) are the accepted forms, and a
// top-level assignment as a condition is assignment_in_condition's.
func checkEmbeddedAssignment(tokens []lexer.Token, stmts []parser.StatementExprs) []Diagnostic {
	var out []Diagnostic
	for _, se := range stmts {
		// Assignment nodes that are not embedded: the statement-level
		// chain, and a condition's top-level assignment.
		accepted := map[*parser.Expr]bool{}
		if se.Kind == parser.StmtAssign && len(se.Exprs) == 2 {
			for e := se.Exprs[1]; e != nil && e.Kind == parser.ExprBinary && isAssignmentOp(e.Op) && len(e.Children) == 2; e = e.Children[1] {
				accepted[e] = true
			}
		}
		if se.Kind == parser.StmtCondition && len(se.Exprs) == 1 {
			if e := se.Exprs[0]; e != nil && e.Kind == parser.ExprBinary && isAssignmentOp(e.Op) {
				accepted[e] = true
			}
		}
		for _, root := range se.Exprs {
			walkExpr(root, func(e *parser.Expr) {
				if e.Kind != parser.ExprBinary || !isAssignmentOp(e.Op) || accepted[e] {
					return
				}
				r, ok := exprRange(tokens, e)
				if !ok {
					return
				}
				out = append(out, Diagnostic{
					Severity: SeverityInfo,
					Range:    r,
					Message:  fmt.Sprintf("Assignment '%s' is used inside a larger expression - give it a statement of its own", e.Op),
					Source:   "ssl-lsp",
					Code:     CodeOneAssignmentPerStatement,
				})
			})
		}
	}
	return out
}

// checkStringMinus flags `-` between two known strings
// (diag.prefer_explicit_trim_concat, issue #347). On strings, `-` trims
// trailing spaces from the left operand and then appends, which reads
// like subtraction. Operands are typed from literals and builtin return
// types, and from Hungarian prefixes only when useNames is set
// (--hungarian-types).
func checkStringMinus(tokens []lexer.Token, stmts []parser.StatementExprs, useNames bool) []Diagnostic {
	infer := inferExprType
	if useNames {
		infer = inferExprTypeNamed
	}
	var out []Diagnostic
	walkStatementExprs(stmts, func(e *parser.Expr) {
		if e.Kind != parser.ExprBinary || e.Op != "-" || len(e.Children) != 2 {
			return
		}
		if infer(e.Children[0]) != typeString || infer(e.Children[1]) != typeString {
			return
		}
		opIdx := operatorTokenIndex(tokens, e)
		if opIdx < 0 {
			return
		}
		out = append(out, Diagnostic{
			Severity: SeverityInfo,
			Range:    tokenToRange(tokens[opIdx]),
			Message:  "String '-' trims trailing spaces from the left operand, then appends the right - write Trim(a) + b to say so (Trim also removes trailing tabs and line breaks, which '-' keeps)",
			Source:   "ssl-lsp",
			Code:     CodePreferExplicitTrimConcat,
		})
	})
	return out
}
