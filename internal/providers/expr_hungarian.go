package providers

// Hungarian type cross-check (issue #184). SSL's naming convention
// effectively encodes a type annotation on every variable; with an
// expression tree that annotation becomes enforceable, which is the
// stronger CheckHungarianNotation the issue proposed.

import (
	"fmt"

	"starlims-lsp/internal/lexer"
	"starlims-lsp/internal/parser"
)

// checkHungarianTypeMismatch cross-checks the type a variable's Hungarian
// prefix promises against the type its assigned expression actually
// produces (diag.hungarian_type_mismatch, issue #184). SSL's naming
// convention effectively encodes a type annotation; with an expression
// tree that annotation becomes enforceable, which is the stronger check
// #184 proposed for the existing CheckHungarianNotation option.
//
// It gates on definite evidence at both ends: the target's prefix must
// claim a type in its documented shape, and the expression must infer to
// a definite type. NIL is always allowed — any variable may be cleared.
func checkHungarianTypeMismatch(tokens []lexer.Token, stmts []parser.StatementExprs) []Diagnostic {
	var diagnostics []Diagnostic

	for _, stmt := range stmts {
		// Plain `:=` assignments and `:DEFAULT` only. A compound
		// assignment (`+=`) combines the old value with the new one, so
		// the right-hand type is not the resulting type; `:FOR` headers
		// bind loop counters, which carry no prefix by convention.
		switch stmt.Kind {
		case parser.StmtAssign:
			if stmt.Assign != ":=" {
				continue
			}
		case parser.StmtDefault:
		default:
			continue
		}
		if len(stmt.Exprs) != 2 {
			continue
		}
		target, value := stmt.Exprs[0], stmt.Exprs[1]
		if target.Kind != parser.ExprIdentifier {
			continue
		}
		declared := hungarianType(target.Name)
		if declared == typeUnknown {
			continue
		}
		actual := inferExprTypeNamed(value)
		if actual == typeUnknown || actual == typeNIL || actual == declared {
			continue
		}
		rng, ok := exprRange(tokens, target)
		if !ok {
			continue
		}
		if valueRange, ok := exprRange(tokens, value); ok {
			rng.End = valueRange.End
		}
		diagnostics = append(diagnostics, Diagnostic{
			Severity: SeverityWarning,
			Range:    rng,
			Message: fmt.Sprintf("'%s' promises a %s by its prefix, but this expression produces a %s - rename the variable or fix the expression",
				target.Name, declared, actual),
			Source: "ssl-lsp",
			Code:   CodeHungarianTypeMismatch,
		})
	}

	return diagnostics
}
