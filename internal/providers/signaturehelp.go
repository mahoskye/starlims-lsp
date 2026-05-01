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

// GetSignatureHelp returns signature help for the current position. It
// recognizes both function calls (`Foo(...)`) and built-in class
// instantiation (`Email{...}`), dispatching to the appropriate signature
// list. Class constructors fall back to the function-call form on the same
// line if no constructor context is detected.
func GetSignatureHelp(text string, line, column int) *SignatureHelp {
	if name, activeParam, ok := findClassInstantiationContext(text, line, column); ok {
		if help := buildConstructorSignatureHelp(name, activeParam); help != nil {
			return help
		}
	}

	funcName, activeParam := findFunctionContext(text, line, column)
	if funcName == "" {
		return nil
	}

	return buildSignatureHelp(funcName, activeParam)
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

// findClassInstantiationContext looks for an open `{` preceded by a built-in
// class name. Returns (className, activeParam, true) if found.
func findClassInstantiationContext(text string, line, column int) (string, int, bool) {
	lines := strings.Split(text, "\n")
	if line < 1 || line > len(lines) {
		return "", 0, false
	}

	var sb strings.Builder
	for i := 0; i < line-1; i++ {
		sb.WriteString(lines[i])
		sb.WriteString("\n")
	}
	lineText := lines[line-1]
	if column > len(lineText)+1 {
		column = len(lineText) + 1
	}
	sb.WriteString(lineText[:column-1])

	runes := []rune(sb.String())

	braceDepth := 0
	parenDepth := 0
	commaCount := 0

	for i := len(runes) - 1; i >= 0; i-- {
		ch := runes[i]
		switch ch {
		case '}':
			braceDepth++
		case ')':
			parenDepth++
		case '(':
			if parenDepth == 0 {
				return "", 0, false
			}
			parenDepth--
		case '{':
			if braceDepth == 0 && parenDepth == 0 {
				// Look for class name before `{`.
				j := i - 1
				for j >= 0 && (runes[j] == ' ' || runes[j] == '\t') {
					j--
				}
				end := j + 1
				for j >= 0 && isIdentChar(runes[j]) {
					j--
				}
				name := string(runes[j+1 : end])
				if name == "" {
					return "", 0, false
				}
				if _, ok := constants.GeneratedClassDetails[strings.ToLower(name)]; !ok {
					return "", 0, false
				}
				return name, commaCount, true
			}
			braceDepth--
		case ',':
			if braceDepth == 0 && parenDepth == 0 {
				commaCount++
			}
		}
	}

	return "", 0, false
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
	return GetSignatureHelpWithProcedures(tokens, nil, line, column)
}

// GetSignatureHelpWithProcedures returns signature help for built-in SSL functions.
// The procedures parameter is accepted for API compatibility, but user-defined
// procedures are intentionally excluded because SSL runtime dispatch uses
// DoProc/ExecFunction rather than direct custom procedure calls.
func GetSignatureHelpWithProcedures(tokens []lexer.Token, procedures []parser.ProcedureInfo, line, column int) *SignatureHelp {
	_ = procedures

	// Find the function call context from tokens
	funcName, activeParam := findFunctionContextFromTokens(tokens, line, column)
	if funcName == "" {
		return nil
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
