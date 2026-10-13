// Package providers implements LSP feature providers.
// This file implements inlay hints for function parameter names.
package providers

import (
	"regexp"
	"strings"

	"starlims-lsp/internal/constants"
	"starlims-lsp/internal/lexer"
	"starlims-lsp/internal/parser"
)

// InlayHintKind defines the kind of inlay hint.
type InlayHintKind int

const (
	// InlayHintKindType is for type annotations.
	InlayHintKindType InlayHintKind = 1
	// InlayHintKindParameter is for parameter hints.
	InlayHintKindParameter InlayHintKind = 2
)

// InlayHintOptions configures inlay hint behavior.
type InlayHintOptions struct {
	// Enabled enables or disables inlay hints.
	Enabled bool
	// MinParameterCount is the minimum number of parameters a function must have
	// before hints are shown. Default is 2.
	MinParameterCount int
}

// DefaultInlayHintOptions returns the default inlay hint options.
func DefaultInlayHintOptions() InlayHintOptions {
	return InlayHintOptions{
		Enabled:           true,
		MinParameterCount: 2,
	}
}

// InlayHint represents an inlay hint to display.
type InlayHint struct {
	// Line is the 1-based line number.
	Line int
	// Character is the 1-based character position.
	Character int
	// Label is the hint text (parameter name).
	Label string
	// Kind is the hint kind.
	Kind InlayHintKind
}

// FunctionCall represents a detected function call with its arguments.
type FunctionCall struct {
	// Name is the function name.
	Name string
	// NameLine is the line of the function name (1-based).
	NameLine int
	// NameColumn is the column of the function name (1-based).
	NameColumn int
	// Arguments is the list of argument positions.
	Arguments []ArgumentPosition
	// OpenParenColumn is the column of the opening parenthesis.
	OpenParenColumn int
}

// ArgumentPosition represents the position of an argument.
type ArgumentPosition struct {
	// Line is the 1-based line number where the argument starts.
	Line int
	// Column is the 1-based column where the argument starts.
	Column int
	// TokenIndex is the index of the argument's first token in the token
	// stream the call was parsed from.
	TokenIndex int
}

// doProcPattern matches DoProc or ExecFunction calls to extract the procedure name.
var inlayDoProcPattern = regexp.MustCompile(`(?i)^(DoProc|ExecFunction)$`)

// GetInlayHints returns inlay hints for the specified range.
// The startLine and endLine are 1-based and inclusive.
func GetInlayHints(tokens []lexer.Token, procedures []parser.ProcedureInfo, startLine, endLine int, opts InlayHintOptions) []InlayHint {
	if !opts.Enabled || len(tokens) == 0 {
		return nil
	}

	// Find all function calls in the range. Member calls are skipped: the
	// callee belongs to the receiver (a .NET passthrough, a class method),
	// not to the like-named built-in (D10, issue #255).
	calls := findFunctionCalls(tokens, parser.IdentifierRoles(tokens), startLine, endLine)
	if len(calls) == 0 {
		return nil
	}

	var hints []InlayHint

	for _, call := range calls {
		callHints := generateHintsForCall(tokens, call, procedures, opts)
		hints = append(hints, callHints...)
	}

	return hints
}

// findFunctionCalls scans tokens to find function calls within the specified range.
func findFunctionCalls(tokens []lexer.Token, roles []parser.IdentifierRole, startLine, endLine int) []FunctionCall {
	var calls []FunctionCall

	for i := 0; i < len(tokens); i++ {
		token := tokens[i]

		// Skip tokens outside the range
		if token.Line < startLine || token.Line > endLine {
			continue
		}

		// Look for identifier followed by (
		if token.Type != lexer.TokenIdentifier {
			continue
		}

		// The member name of a `:` access is never a built-in call.
		if (i < len(roles) && roles[i] == parser.RoleMember) || followsMemberColon(tokens, i) {
			continue
		}

		// Find the next non-whitespace token
		nextIdx := i + 1
		for nextIdx < len(tokens) && tokens[nextIdx].Type == lexer.TokenWhitespace {
			nextIdx++
		}

		if nextIdx >= len(tokens) {
			continue
		}

		nextToken := tokens[nextIdx]
		if nextToken.Type != lexer.TokenPunctuation || nextToken.Text != "(" {
			continue
		}

		// Check if this is inside a string or comment
		ctx := lexer.GetContextAtPosition(tokens, token.Line, token.Column)
		if ctx == lexer.ContextString || ctx == lexer.ContextComment {
			continue
		}

		// Found a function call - extract arguments
		call := FunctionCall{
			Name:            token.Text,
			NameLine:        token.Line,
			NameColumn:      token.Column,
			OpenParenColumn: nextToken.Column,
		}

		// Parse arguments starting after the opening paren
		args := parseArguments(tokens, nextIdx+1)
		call.Arguments = args
		calls = append(calls, call)
	}

	return calls
}

// followsMemberColon reports whether the identifier at i directly follows a
// `:` member-access colon (the fallback when the expression tree leaves the
// position unclassified).
func followsMemberColon(tokens []lexer.Token, i int) bool {
	prev := i - 1
	for prev >= 0 && (tokens[prev].Type == lexer.TokenWhitespace || tokens[prev].Type == lexer.TokenComment) {
		prev--
	}
	return prev >= 0 && tokens[prev].Type == lexer.TokenPunctuation && tokens[prev].Text == ":"
}

// parseArguments extracts argument positions from tokens starting after the opening parenthesis.
func parseArguments(tokens []lexer.Token, startIdx int) []ArgumentPosition {
	var args []ArgumentPosition
	parenDepth := 1
	bracketDepth := 0
	braceDepth := 0
	argStart := -1

	for i := startIdx; i < len(tokens) && parenDepth > 0; i++ {
		token := tokens[i]

		// Skip whitespace and comments
		if token.Type == lexer.TokenWhitespace || token.Type == lexer.TokenComment {
			continue
		}

		// Track nesting
		switch token.Type {
		case lexer.TokenPunctuation:
			switch token.Text {
			case "(":
				// If this is the start of a new argument (nested call), record position
				if argStart < 0 && parenDepth == 1 && bracketDepth == 0 && braceDepth == 0 {
					argStart = i
				}
				parenDepth++
			case ")":
				parenDepth--
				if parenDepth == 0 {
					// End of function call - record final argument if exists
					if argStart >= 0 {
						argToken := tokens[argStart]
						args = append(args, ArgumentPosition{
							Line:       argToken.Line,
							Column:     argToken.Column,
							TokenIndex: argStart,
						})
					}
					return args
				}
			case "[":
				// If this is the start of a new argument, record position
				if argStart < 0 && parenDepth == 1 && bracketDepth == 0 && braceDepth == 0 {
					argStart = i
				}
				bracketDepth++
			case "]":
				bracketDepth--
			case "{":
				// If this is the start of a new argument (array literal), record position
				if argStart < 0 && parenDepth == 1 && bracketDepth == 0 && braceDepth == 0 {
					argStart = i
				}
				braceDepth++
			case "}":
				braceDepth--
			case ",":
				// Only count commas at the top level of this function call
				if parenDepth == 1 && bracketDepth == 0 && braceDepth == 0 {
					// Record the current argument
					if argStart >= 0 {
						argToken := tokens[argStart]
						args = append(args, ArgumentPosition{
							Line:       argToken.Line,
							Column:     argToken.Column,
							TokenIndex: argStart,
						})
					}
					argStart = -1
				}
			default:
				// Other punctuation (operators, etc.) could start an argument
				if argStart < 0 && parenDepth == 1 && bracketDepth == 0 && braceDepth == 0 {
					argStart = i
				}
			}
		default:
			// Start of a new argument (identifier, number, string, etc.)
			if argStart < 0 && parenDepth == 1 && bracketDepth == 0 && braceDepth == 0 {
				argStart = i
			}
		}
	}

	return args
}

// generateHintsForCall creates hints for a single function call.
func generateHintsForCall(tokens []lexer.Token, call FunctionCall, procedures []parser.ProcedureInfo, opts InlayHintOptions) []InlayHint {
	var hints []InlayHint

	// Check if it's DoProc/ExecFunction
	if inlayDoProcPattern.MatchString(call.Name) {
		return generateDoProcHints(tokens, call, procedures, opts)
	}

	// Check built-in function signatures
	sig, ok := constants.GetFunctionSignature(call.Name)
	if !ok {
		return nil
	}

	names := hintParameterNames(sig, len(call.Arguments))

	// Check minimum parameter threshold
	if len(names) < opts.MinParameterCount {
		return nil
	}

	// Generate hints for each argument
	for i, arg := range call.Arguments {
		if i >= len(names) {
			break
		}
		hints = append(hints, InlayHint{
			Line:      arg.Line,
			Character: arg.Column,
			Label:     names[i],
			Kind:      InlayHintKindParameter,
		})
	}

	return hints
}

// hintParameterNames returns the positional parameter names to hint for a
// call with argCount arguments. A function with alternative call forms
// (CreateUdObject) is hinted only when exactly one form can take that many
// arguments; otherwise the form is ambiguous and no hint is better than a
// wrong one.
func hintParameterNames(sig constants.FunctionSignature, argCount int) []string {
	if len(sig.Forms) == 0 {
		names := make([]string, 0, len(sig.Parameters))
		for _, p := range sig.Parameters {
			names = append(names, p.Name)
		}
		return names
	}
	var match []string
	matches := 0
	for _, f := range sig.Forms {
		if argCount <= len(f.Parameters) && argCount > 0 {
			match = f.Parameters
			matches++
		}
	}
	if matches != 1 {
		return nil
	}
	return match
}

// generateDoProcHints creates hints for DoProc/ExecFunction calls.
func generateDoProcHints(tokens []lexer.Token, call FunctionCall, procedures []parser.ProcedureInfo, opts InlayHintOptions) []InlayHint {
	var hints []InlayHint

	// The outer hints are the dispatcher's own published parameter names:
	// DoProc(sProcedureName, [aArguments]), ExecFunction(sName, [aParameters]).
	var doProcParams []string
	if sig, ok := constants.GetFunctionSignature(call.Name); ok {
		for _, p := range sig.Parameters {
			doProcParams = append(doProcParams, p.Name)
		}
	}

	// Add hints for DoProc's own parameters
	for i, arg := range call.Arguments {
		if i >= len(doProcParams) {
			break
		}
		hints = append(hints, InlayHint{
			Line:      arg.Line,
			Character: arg.Column,
			Label:     doProcParams[i],
			Kind:      InlayHintKindParameter,
		})
	}

	// If we have a second argument (aParams), try to resolve inner parameters
	if len(call.Arguments) >= 2 && len(procedures) > 0 {
		// Try to find the procedure name from the first argument
		// This requires looking at the tokens to extract the string literal
		procName := extractProcedureNameFromFirstArg(tokens, call)
		if procName != "" {
			// Find matching procedure
			for _, proc := range procedures {
				if strings.EqualFold(proc.Name, procName) {
					// Found the procedure - generate hints for array elements
					innerHints := generateArrayParamHints(tokens, call, proc, opts)
					hints = append(hints, innerHints...)
					break
				}
			}
		}
	}

	return hints
}

// extractProcedureNameFromFirstArg extracts the target procedure name from a
// DoProc/ExecFunction call whose first argument is a lone string literal
// ("Name" or 'Name'). Dynamic names (variables, concatenations) return ""
// — hints are only offered when the target is statically known (issue #46).
func extractProcedureNameFromFirstArg(tokens []lexer.Token, call FunctionCall) string {
	idx := call.Arguments[0].TokenIndex
	if idx < 0 || idx >= len(tokens) || tokens[idx].Type != lexer.TokenString {
		return ""
	}

	// The string must be the whole argument: the next significant token has
	// to be the separating comma (a concatenation like "A" + sName is not a
	// static target).
	for j := idx + 1; j < len(tokens); j++ {
		next := tokens[j]
		if next.Type == lexer.TokenWhitespace || next.Type == lexer.TokenComment {
			continue
		}
		if next.Type == lexer.TokenPunctuation && next.Text == "," {
			break
		}
		return ""
	}

	text := tokens[idx].Text
	if len(text) < 2 {
		return ""
	}
	quote := text[0]
	if (quote != '"' && quote != '\'') || text[len(text)-1] != quote {
		return ""
	}
	return text[1 : len(text)-1]
}

// generateArrayParamHints hints the elements of a DoProc/ExecFunction
// argument array with the resolved procedure's parameter names:
// DoProc("Calc", {100, 25.50}) shows nQty: and nPrice: inside the braces.
func generateArrayParamHints(tokens []lexer.Token, call FunctionCall, proc parser.ProcedureInfo, opts InlayHintOptions) []InlayHint {
	if len(proc.Parameters) < opts.MinParameterCount {
		return nil
	}

	start := call.Arguments[1].TokenIndex
	if start < 0 || start >= len(tokens) ||
		tokens[start].Type != lexer.TokenPunctuation || tokens[start].Text != "{" {
		return nil
	}

	var hints []InlayHint
	braceDepth := 1
	parenDepth := 0
	elem := 0
	expectElement := true

	for i := start + 1; i < len(tokens) && braceDepth > 0; i++ {
		token := tokens[i]
		if token.Type == lexer.TokenWhitespace || token.Type == lexer.TokenComment {
			continue
		}

		if token.Type == lexer.TokenPunctuation {
			switch token.Text {
			case "{":
				braceDepth++
			case "}":
				braceDepth--
				continue
			case "(":
				parenDepth++
			case ")":
				parenDepth--
			case ",":
				if braceDepth == 1 && parenDepth == 0 {
					elem++
					expectElement = true
					continue
				}
			}
		}

		if expectElement && braceDepth == 1 && parenDepth == 0 {
			expectElement = false
			if elem < len(proc.Parameters) {
				hints = append(hints, InlayHint{
					Line:      token.Line,
					Character: token.Column,
					Label:     proc.Parameters[elem],
					Kind:      InlayHintKindParameter,
				})
			}
		}
	}

	return hints
}
