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
}

// doProcPattern matches DoProc or ExecFunction calls to extract the procedure name.
var inlayDoProcPattern = regexp.MustCompile(`(?i)^(DoProc|ExecFunction)$`)

// GetInlayHints returns inlay hints for the specified range.
// The startLine and endLine are 1-based and inclusive.
func GetInlayHints(tokens []lexer.Token, procedures []parser.ProcedureInfo, startLine, endLine int, opts InlayHintOptions) []InlayHint {
	if !opts.Enabled || len(tokens) == 0 {
		return nil
	}

	// Find all function calls in the range
	calls := findFunctionCalls(tokens, startLine, endLine)
	if len(calls) == 0 {
		return nil
	}

	var hints []InlayHint

	for _, call := range calls {
		callHints := generateHintsForCall(call, procedures, opts)
		hints = append(hints, callHints...)
	}

	return hints
}

// findFunctionCalls scans tokens to find function calls within the specified range.
func findFunctionCalls(tokens []lexer.Token, startLine, endLine int) []FunctionCall {
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
							Line:   argToken.Line,
							Column: argToken.Column,
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
							Line:   argToken.Line,
							Column: argToken.Column,
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
func generateHintsForCall(call FunctionCall, procedures []parser.ProcedureInfo, opts InlayHintOptions) []InlayHint {
	var hints []InlayHint

	// Check if it's DoProc/ExecFunction
	if inlayDoProcPattern.MatchString(call.Name) {
		return generateDoProcHints(call, procedures, opts)
	}

	// Check built-in function signatures
	sig, ok := constants.GetFunctionSignature(call.Name)
	if !ok {
		return nil
	}

	// Check minimum parameter threshold
	if len(sig.Parameters) < opts.MinParameterCount {
		return nil
	}

	// Generate hints for each argument
	for i, arg := range call.Arguments {
		if i >= len(sig.Parameters) {
			break
		}
		param := sig.Parameters[i]
		hints = append(hints, InlayHint{
			Line:      arg.Line,
			Character: arg.Column,
			Label:     param.Name,
			Kind:      InlayHintKindParameter,
		})
	}

	return hints
}

// generateDoProcHints creates hints for DoProc/ExecFunction calls.
func generateDoProcHints(call FunctionCall, procedures []parser.ProcedureInfo, opts InlayHintOptions) []InlayHint {
	var hints []InlayHint

	// DoProc always has at least sProcName parameter hint
	// DoProc(sProcName, aParams)
	doProcParams := []string{"sProcName", "aParams"}

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
		procName := extractProcedureNameFromFirstArg(call)
		if procName != "" {
			// Find matching procedure
			for _, proc := range procedures {
				if strings.EqualFold(proc.Name, procName) {
					// Found the procedure - generate hints for array elements
					innerHints := generateArrayParamHints(call, proc, opts)
					hints = append(hints, innerHints...)
					break
				}
			}
		}
	}

	return hints
}

// extractProcedureNameFromFirstArg attempts to extract a string literal procedure name
// from the first argument of a DoProc/ExecFunction call.
// This is a simplified approach - in practice, the tokens would need to be passed in.
func extractProcedureNameFromFirstArg(call FunctionCall) string {
	// For now, we can't easily extract this without token access
	// This would need to be enhanced to pass tokens through
	// Return empty to indicate we couldn't resolve it
	return ""
}

// generateArrayParamHints generates hints for parameters inside a DoProc array argument.
// This is called when we know the target procedure and its parameters.
func generateArrayParamHints(call FunctionCall, proc parser.ProcedureInfo, opts InlayHintOptions) []InlayHint {
	// This would require parsing the array literal to find individual elements
	// For now, we only support DoProc-level hints
	// Full implementation would scan tokens inside the {} to find argument positions
	return nil
}
