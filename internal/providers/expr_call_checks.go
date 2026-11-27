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

// checkDoProcArgsNotArrayLiteral flags `DoProc("Name", <arguments>)` when
// the target is a one-part string literal and the arguments are not written
// as an array literal (diag.doproc_args_not_array_literal, issue #372). A
// literal one-part name is bound when the script compiles, and that form
// takes its arguments only as `{...}`: an array held in a variable or built
// by an expression stops the script from compiling. A name held in a
// variable is resolved at run time and takes any array, so it is not
// reported; nor is a dotted name, whose binding depends on how the server
// compiles. Inside a class the call is already doproc_in_class, and one
// finding per call is enough. A comma with nothing after it
// (`DoProc("Name",)`) fails the same way and is reported on the comma. An
// array literal in parentheses compiles, so it is not reported; a skipped
// slot with a third argument after it and an argument the parser could not
// read stay silent: unknown is not evidence (D17).
func checkDoProcArgsNotArrayLiteral(tokens []lexer.Token, calls []parser.CallSite) []Diagnostic {
	var diagnostics []Diagnostic

	classLine := 0
	for _, token := range tokens {
		if token.Type == lexer.TokenKeyword && strings.EqualFold(token.Text, ":CLASS") {
			classLine = token.Line
			break
		}
	}

	for _, call := range calls {
		if call.Qualified() || !strings.EqualFold(call.Name, "DoProc") || len(call.Args) < 2 {
			continue
		}
		target := call.Args[0]
		if target.Kind != parser.ExprLiteral || target.Start != target.End ||
			target.Start < 0 || target.Start >= len(tokens) {
			continue
		}
		name := tokens[target.Start]
		if name.Type != lexer.TokenString || strings.Contains(name.Text, ".") {
			continue
		}
		if classLine > 0 && name.Line >= classLine {
			continue
		}
		args := call.Args[1]
		switch args.Kind {
		case parser.ExprArrayLiteral, parser.ExprGroup, parser.ExprUnknown:
			continue
		case parser.ExprSkipped:
			// Only the shape that was verified: the comma is the last thing
			// in the call. `DoProc("Name",, x)` is left alone.
			rng, ok := trailingCommaRange(tokens, args)
			if len(call.Args) != 2 || !ok {
				continue
			}
			diagnostics = append(diagnostics, Diagnostic{
				Severity: SeverityError,
				Range:    rng,
				Message: fmt.Sprintf("DoProc(%s,) has a comma and no argument array after it, which is a compile-time error with a literal procedure name: "+
					"write DoProc(%s) or DoProc(%s, {...}).", name.Text, name.Text, name.Text),
				Source: "ssl-lsp",
				Code:   CodeDoProcArgsNotArrayLiteral,
			})
			continue
		}
		rng, ok := exprRange(tokens, args)
		if !ok {
			continue
		}
		diagnostics = append(diagnostics, Diagnostic{
			Severity: SeverityError,
			Range:    rng,
			Message: fmt.Sprintf("DoProc with a literal procedure name takes its arguments as an array literal: write DoProc(%s, {...}). "+
				"An array held in a variable or built by an expression is a compile-time error here.", name.Text),
			Source: "ssl-lsp",
			Code:   CodeDoProcArgsNotArrayLiteral,
		})
	}

	return diagnostics
}

// trailingCommaRange returns the range of the comma that opens a skipped
// last argument (`f(a,)`). The skipped slot has no token of its own: its
// node sits on the closing parenthesis, so the comma is the significant
// token before it.
func trailingCommaRange(tokens []lexer.Token, skipped *parser.Expr) (Range, bool) {
	if skipped == nil || skipped.Start <= 0 || skipped.Start > len(tokens) {
		return Range{}, false
	}
	for i := skipped.Start - 1; i >= 0; i-- {
		switch tokens[i].Type {
		case lexer.TokenWhitespace, lexer.TokenComment:
			continue
		}
		if tokens[i].Type == lexer.TokenPunctuation && tokens[i].Text == "," {
			return tokenToRange(tokens[i]), true
		}
		return Range{}, false
	}
	return Range{}, false
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
// arity must never flag. Built once from both signature fields: the
// published signature string (counts optional [x] parameters) and the
// parameter rows (or the longest call form), taking the larger of the two.
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
		if sig, ok := constants.SSLFunctionSignatures[lower]; ok {
			// A function with alternative call forms accepts at most its
			// longest form, not the union of every form's parameters.
			n := len(sig.Parameters)
			if len(sig.Forms) > 0 {
				n = 0
				for _, f := range sig.Forms {
					n = max(n, len(f.Parameters))
				}
			}
			count = max(count, n)
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
		// Trailing skipped slots pass nothing: `DoProc("P", {1},,)` is
		// exactly `DoProc("P", {1})` and trailing_skip_commas reports it,
		// so only real surplus arguments count (issue #328).
		for len(args) > 0 && args[len(args)-1].Kind == parser.ExprSkipped {
			args = args[:len(args)-1]
		}
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
