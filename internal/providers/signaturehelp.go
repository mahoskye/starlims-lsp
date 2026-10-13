package providers

import (
	"fmt"
	"strings"

	"starlims-lsp/internal/constants"
	"starlims-lsp/internal/lexer"
	"starlims-lsp/internal/parser"
)

// SignatureHelp represents signature help information.
type SignatureHelp struct {
	Signatures      []SignatureInformation
	ActiveSignature int
	ActiveParameter int
}

// SignatureInformation represents a function signature.
type SignatureInformation struct {
	Label         string
	Documentation string
	Parameters    []ParameterInformation
}

// ParameterInformation represents a parameter in a signature.
type ParameterInformation struct {
	Label         string
	Documentation string
}

func buildSignatureHelp(funcName string, activeParam int) *SignatureHelp {
	sig, ok := constants.GetFunctionSignature(funcName)
	if !ok {
		return nil
	}

	docInfo := buildFunctionDoc(sig)

	return &SignatureHelp{
		Signatures: []SignatureInformation{
			{
				Label:         docInfo.Label,
				Documentation: docInfo.Documentation,
				Parameters:    docInfo.Parameters,
			},
		},
		ActiveSignature: 0,
		ActiveParameter: activeParam,
	}
}

// buildConstructorSignatureHelp returns signature help describing all
// constructor forms of a built-in SSL class. Each constructor appears as a
// separate SignatureInformation; the active signature is the first one whose
// arity matches the active parameter index.
func buildConstructorSignatureHelp(className string, activeParam int) *SignatureHelp {
	det, ok := constants.GeneratedClassDetails[strings.ToLower(className)]
	if !ok || len(det.Constructors) == 0 {
		return nil
	}

	infos := make([]SignatureInformation, 0, len(det.Constructors))
	activeSig := 0
	bestMatchOk := false

	for i, c := range det.Constructors {
		params := make([]ParameterInformation, 0, len(c.Parameters))
		for _, p := range c.Parameters {
			params = append(params, ParameterInformation{
				Label:         p.Name,
				Documentation: p.Description,
			})
		}
		doc := c.Description
		if doc == "" {
			doc = fmt.Sprintf("`%s` constructor.", className)
		}
		infos = append(infos, SignatureInformation{
			Label:         c.Signature,
			Documentation: doc,
			Parameters:    params,
		})
		// Pick the first signature that has enough parameters for the active
		// param index. Falls back to the first signature if none match.
		if !bestMatchOk && activeParam < len(params) {
			activeSig = i
			bestMatchOk = true
		}
	}

	return &SignatureHelp{
		Signatures:      infos,
		ActiveSignature: activeSig,
		ActiveParameter: activeParam,
	}
}

func GetSignatureHelpFromTokens(tokens []lexer.Token, line, column int) *SignatureHelp {
	return GetSignatureHelpWithProcedures(tokens, nil, line, column)
}

// GetSignatureHelpWithProcedures returns signature help for built-in SSL functions.
// The procedures parameter is accepted for API compatibility, but user-defined
// procedures are intentionally excluded because SSL runtime dispatch uses
// DoProc/ExecFunction rather than direct custom procedure calls.
func GetSignatureHelpWithProcedures(tokens []lexer.Token, procedures []parser.ProcedureInfo, line, column int) *SignatureHelp {
	_ = procedures

	// Find the call context from tokens: a function call `Foo(...)` or a
	// built-in class instantiation `Email{...}` (issue #40).
	funcName, activeParam, isConstructor := findFunctionContextFromTokens(tokens, line, column)
	if funcName == "" {
		return nil
	}

	if isConstructor {
		return buildConstructorSignatureHelp(funcName, activeParam)
	}

	// First, try to find in built-in functions
	if sig, ok := constants.GetFunctionSignature(funcName); ok {
		docInfo := buildFunctionDoc(sig)
		return &SignatureHelp{
			Signatures: []SignatureInformation{
				{
					Label:         docInfo.Label,
					Documentation: docInfo.Documentation,
					Parameters:    docInfo.Parameters,
				},
			},
			ActiveSignature: 0,
			ActiveParameter: activeParam,
		}
	}

	return nil
}

// findFunctionContextFromTokens finds the enclosing call context using
// tokens. It returns the callee name, the active (comma-counted) parameter
// index, and whether the context is a built-in class instantiation
// (`Email{...}`) rather than a function call.
func findFunctionContextFromTokens(tokens []lexer.Token, line, column int) (string, int, bool) {
	// Find the token index at the current position
	currentIdx := -1
	for i, token := range tokens {
		if token.Line == line && column >= token.Column && column <= token.Column+len(token.Text) {
			currentIdx = i
			break
		}
		if token.Line == line && token.Column > column {
			currentIdx = i - 1
			break
		}
		if token.Line > line {
			currentIdx = i - 1
			break
		}
	}

	if currentIdx < 0 {
		currentIdx = len(tokens) - 1
	}

	// Scan backwards to find the enclosing call context
	parenDepth := 0
	braceDepth := 0
	commaCount := 0

	for i := currentIdx; i >= 0; i-- {
		token := tokens[i]
		if token.Type == lexer.TokenWhitespace || token.Type == lexer.TokenComment {
			continue
		}

		if token.Type == lexer.TokenPunctuation {
			switch token.Text {
			case ")":
				parenDepth++
			case "(":
				if parenDepth == 0 && braceDepth == 0 {
					// Found the opening paren, look for the function name before it
					for j := i - 1; j >= 0; j-- {
						prev := tokens[j]
						if prev.Type == lexer.TokenWhitespace || prev.Type == lexer.TokenComment {
							continue
						}
						if prev.Type == lexer.TokenIdentifier {
							return prev.Text, commaCount, false
						}
						break
					}
					return "", 0, false
				}
				parenDepth--
			case "}":
				braceDepth++
			case "{":
				if braceDepth == 0 && parenDepth == 0 {
					// Unmatched '{': a built-in class instantiation if the
					// preceding significant token names a built-in class;
					// otherwise an array literal — the commas counted so far
					// belong to the literal, not an enclosing call.
					for j := i - 1; j >= 0; j-- {
						prev := tokens[j]
						if prev.Type == lexer.TokenWhitespace || prev.Type == lexer.TokenComment {
							continue
						}
						if prev.Type == lexer.TokenIdentifier && constants.IsSSLClass(prev.Text) {
							return prev.Text, commaCount, true
						}
						break
					}
					commaCount = 0
					continue
				}
				braceDepth--
			case ",":
				if parenDepth == 0 && braceDepth == 0 {
					commaCount++
				}
			}
		}
	}

	return "", 0, false
}
