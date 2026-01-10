package providers

import (
	"strings"

	"starlims-lsp/internal/constants"
	"starlims-lsp/internal/lexer"
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

// GetSignatureHelp returns signature help for the current position.
func GetSignatureHelp(text string, line, column int) *SignatureHelp {
	// Find the function call context at the current position
	funcName, activeParam := findFunctionContext(text, line, column)
	if funcName == "" {
		return nil
	}

	// Look up the function signature
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

// findFunctionContext finds the function name and active parameter index at the given position.
func findFunctionContext(text string, line, column int) (string, int) {
	lines := strings.Split(text, "\n")
	if line < 1 || line > len(lines) {
		return "", 0
	}

	// Get the text up to the cursor position
	var textBefore strings.Builder
	for i := 0; i < line-1; i++ {
		textBefore.WriteString(lines[i])
		textBefore.WriteString("\n")
	}
	lineText := lines[line-1]
	if column > len(lineText)+1 {
		column = len(lineText) + 1
	}
	textBefore.WriteString(lineText[:column-1])

	content := textBefore.String()

	// Find the innermost function call by scanning backwards
	parenDepth := 0
	commaCount := 0
	funcStart := -1
	funcEnd := -1

	runes := []rune(content)
	for i := len(runes) - 1; i >= 0; i-- {
		ch := runes[i]
		switch ch {
		case ')':
			parenDepth++
		case '(':
			if parenDepth == 0 {
				// Found the opening paren of our function call
				funcEnd = i
				// Find the function name before the paren
				funcStart = i - 1
				for funcStart >= 0 && (isIdentChar(runes[funcStart])) {
					funcStart--
				}
				funcStart++
				if funcStart < funcEnd {
					funcName := string(runes[funcStart:funcEnd])
					return funcName, commaCount
				}
				return "", 0
			}
			parenDepth--
		case ',':
			if parenDepth == 0 {
				commaCount++
			}
		}
	}

	return "", 0
}

// isIdentChar checks if a rune is valid in an identifier.
func isIdentChar(r rune) bool {
	return (r >= 'a' && r <= 'z') || (r >= 'A' && r <= 'Z') || (r >= '0' && r <= '9') || r == '_'
}

// GetSignatureHelpFromTokens returns signature help using tokenized input.
func GetSignatureHelpFromTokens(tokens []lexer.Token, line, column int) *SignatureHelp {
	// Find the function call context from tokens
	funcName, activeParam := findFunctionContextFromTokens(tokens, line, column)
	if funcName == "" {
		return nil
	}

	// Look up the function signature
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

// findFunctionContextFromTokens finds function context using tokens.
func findFunctionContextFromTokens(tokens []lexer.Token, line, column int) (string, int) {
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

	// Scan backwards to find function call context
	parenDepth := 0
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
				if parenDepth == 0 {
					// Found the opening paren, look for the function name before it
					for j := i - 1; j >= 0; j-- {
						prev := tokens[j]
						if prev.Type == lexer.TokenWhitespace || prev.Type == lexer.TokenComment {
							continue
						}
						if prev.Type == lexer.TokenIdentifier {
							return prev.Text, commaCount
						}
						break
					}
					return "", 0
				}
				parenDepth--
			case ",":
				if parenDepth == 0 {
					commaCount++
				}
			}
		}
	}

	return "", 0
}
