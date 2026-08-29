package providers

// Diagnostics driven by the expression AST's call-site index (issue #184).
// These two rules shipped on token scanning and Hungarian guesswork and
// recorded #184 as their upgrade path: with the tree they see the real
// callee, the real receiver, and real argument boundaries, so the guards
// they needed against un-analyzable source become genuine judgments.

import (
	"fmt"
	"strings"

	"starlims-lsp/internal/constants"
	"starlims-lsp/internal/lexer"
	"starlims-lsp/internal/parser"
)

// exprRange converts an expression's token span to a source range. An
// out-of-range span (never produced by the parser, but cheap to survive)
// collapses to the nearest valid token.
func exprRange(tokens []lexer.Token, e *parser.Expr) (Range, bool) {
	if e == nil || len(tokens) == 0 {
		return Range{}, false
	}
	start, end := e.Start, e.End
	if start < 0 || start >= len(tokens) || end < start {
		return Range{}, false
	}
	if end >= len(tokens) {
		end = len(tokens) - 1
	}
	return Range{
		Start: tokenToRange(tokens[start]).Start,
		End:   tokenToRange(tokens[end]).End,
	}, true
}

// spanRange brackets a run of sibling expressions, first through last.
func spanRange(tokens []lexer.Token, exprs []*parser.Expr) (Range, bool) {
	if len(exprs) == 0 {
		return Range{}, false
	}
	first, ok := exprRange(tokens, exprs[0])
	if !ok {
		return Range{}, false
	}
	last, ok := exprRange(tokens, exprs[len(exprs)-1])
	if !ok {
		return first, true
	}
	first.End = last.End
	return first, true
}

// checkFormatArgNotArray flags `<string>:Format(...)` calls whose
// replacement values are not passed as a single array
// (diag.format_arg_not_array, issue #194): Format takes ONE array holding
// every replacement value, even for a single placeholder.
//
// Both sides are now typed rather than name-matched (#184): the receiver
// must infer to a string, and the second argument must infer to a
// definite non-array type. Anything the inference cannot pin down stays
// silent.
func checkFormatArgNotArray(tokens []lexer.Token, calls []parser.CallSite) []Diagnostic {
	var diagnostics []Diagnostic

	for _, call := range calls {
		if !call.Qualified() || !strings.EqualFold(call.Name, "Format") {
			continue
		}
		// Hungarian names are read as type evidence here on purpose: for
		// `sFmt` the convention *is* the declared contract, and it is what
		// distinguishes a string variable from `String:Format` (the .NET
		// class, legitimately variadic) and from `oDoc:Format`.
		if inferExprTypeNamed(call.Receiver) != typeString {
			continue
		}
		args := call.EffectiveArgs()
		if len(args) < 2 {
			continue
		}
		rng, ok := exprRange(tokens, args[1])
		if !ok {
			continue
		}
		// More than two arguments is wrong regardless of their types —
		// Format takes exactly a template plus one array.
		if len(args) == 2 {
			switch inferExprTypeNamed(args[1]) {
			case typeArray, typeNIL, typeUnknown:
				continue
			}
		}
		subject := "Format"
		if name := receiverDisplayName(call.Receiver); name != "" {
			subject = name + ":Format"
		}
		diagnostics = append(diagnostics, Diagnostic{
			Severity: SeverityWarning,
			Range:    rng,
			Message: fmt.Sprintf("%s takes ONE array holding every replacement value - wrap the values in braces: %s(template, {...})",
				subject, subject),
			Source: "ssl-lsp",
			Code:   CodeFormatArgNotArray,
		})
	}

	return diagnostics
}

// receiverDisplayName names a receiver for a message when it has a name of
// its own — a bare variable or a member access. Composite receivers
// (calls, subscripts, literals) return "" and the message drops the
// prefix.
func receiverDisplayName(e *parser.Expr) string {
	if e == nil {
		return ""
	}
	switch e.Kind {
	case parser.ExprIdentifier, parser.ExprMember:
		return e.Name
	}
	return ""
}

// builtinMaxArity maps lowercase builtin names to the maximum argument
// count their published signature accepts. Functions whose signature is
// variadic ("..."), unparseable, or absent are NOT in the map — unknown
// arity must never flag. Built once from both signature sources: the
// generated signature string (counts optional [x] parameters) and the
// curated parameter list, taking the larger of the two.
var builtinMaxArity = buildBuiltinMaxArity()

func buildBuiltinMaxArity() map[string]int {
	arity := make(map[string]int, len(constants.GeneratedFunctionSummaries))
	for lower, meta := range constants.GeneratedFunctionSummaries {
		sig := meta.Signature
		open := strings.IndexByte(sig, '(')
		close := strings.LastIndexByte(sig, ')')
		if open < 0 || close <= open {
			continue
		}
		inner := strings.TrimSpace(sig[open+1 : close])
		if strings.Contains(inner, "...") {
			continue
		}
		count := 0
		if inner != "" {
			depth := 0
			count = 1
			for _, r := range inner {
				switch r {
				case '(', '[', '{':
					depth++
				case ')', ']', '}':
					depth--
				case ',':
					if depth == 0 {
						count++
					}
				}
			}
		}
		if curated, ok := constants.SSLFunctionSignatures[lower]; ok && len(curated.Parameters) > count {
			count = len(curated.Parameters)
		}
		arity[lower] = count
	}
	return arity
}

// checkBuiltinExcessArguments flags builtin calls that pass more arguments
// than the builtin's published signature accepts
// (diag.builtin_excess_arguments, issue #200): the SSL compiler silently
// drops surplus arguments at compile time — they are never evaluated and
// produce no warning (runtime-confirmed, issue #210) — so
// `Left(sText, 10, nExtra)` compiles cleanly and behaves as
// `Left(sText, 10)`. The range spans the surplus arguments.
func checkBuiltinExcessArguments(tokens []lexer.Token, calls []parser.CallSite) []Diagnostic {
	var diagnostics []Diagnostic

	for _, call := range calls {
		// A qualified call is the receiver's method, not the builtin
		// (`oDoc:Left(...)` is the object's Left).
		if call.Qualified() {
			continue
		}
		lower := strings.ToLower(call.Name)
		maxArgs, known := builtinMaxArity[lower]
		if !known {
			continue
		}
		args := call.EffectiveArgs()
		if len(args) <= maxArgs {
			continue
		}
		surplus := args[maxArgs:]
		// Anchor on the surplus arguments; when the first surplus slot is
		// a skipped argument there is no expression to point at, so the
		// call name carries the range instead.
		anchor, ok := spanRange(tokens, surplus)
		if !ok || surplus[0].Kind == parser.ExprSkipped {
			anchor, ok = exprRange(tokens, call.Call.Children[0])
			if !ok {
				continue
			}
		}
		plural := "s"
		if len(surplus) == 1 {
			plural = ""
		}
		diagnostics = append(diagnostics, Diagnostic{
			Severity: SeverityWarning,
			Range:    anchor,
			Message: fmt.Sprintf("'%s' accepts at most %d argument(s) - the compiler silently drops the surplus %d argument%s (never evaluated)",
				constants.GeneratedFunctionSummaries[lower].Title, maxArgs, len(surplus), plural),
			Source: "ssl-lsp",
			Code:   CodeBuiltinExcessArguments,
		})
	}

	return diagnostics
}
