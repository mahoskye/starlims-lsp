package providers

import (
	"fmt"
	"strings"

	"starlims-lsp/internal/constants"
	"starlims-lsp/internal/lexer"
	"starlims-lsp/internal/parser"
)

// Hover represents hover information.
type Hover struct {
	Contents string
}

// GetHover returns hover information for a word at a position.
func GetHover(text string, line, column int, procedures []parser.ProcedureInfo, variables []parser.VariableInfo) *Hover {
	word := lexer.GetWordAtPosition(text, line, column)

	if word == "" {
		return nil
	}

	// Try each hover provider in order
	if hover := getKeywordHover(word); hover != nil {
		return hover
	}
	if hover := getFunctionHover(word); hover != nil {
		return hover
	}
	if hover := getClassHover(word); hover != nil {
		return hover
	}
	if hover := getLiteralHover(word); hover != nil {
		return hover
	}
	if hover := getOperatorHover(word); hover != nil {
		return hover
	}
	if hover := getProcedureHover(word, procedures); hover != nil {
		return hover
	}
	if hover := getVariableHover(word, variables); hover != nil {
		return hover
	}

	return nil
}

// getKeywordHover returns hover information for a keyword.
func getKeywordHover(word string) *Hover {
	upper := strings.ToUpper(strings.TrimPrefix(word, ":"))

	if constants.IsKeyword(upper) {
		description := constants.SSLKeywordDescriptions[upper]
		if description == "" {
			description = fmt.Sprintf("SSL keyword: %s", upper)
		}
		return &Hover{
			Contents: fmt.Sprintf("**:%s**\n\n%s", upper, description),
		}
	}

	return nil
}

// getFunctionHover returns hover information for a built-in function.
func getFunctionHover(word string) *Hover {
	wordLower := strings.ToLower(word)

	for _, fnName := range constants.SSLFunctionNames {
		if strings.ToLower(fnName) == wordLower {
			if sig, ok := constants.GetFunctionSignature(fnName); ok {
				docInfo := buildFunctionDoc(sig)
				return &Hover{
					Contents: formatFunctionHover(docInfo),
				}
			}
			return &Hover{
				Contents: fmt.Sprintf("**%s**\n\nBuilt-in SSL function", fnName),
			}
		}
	}

	return nil
}

func formatFunctionHover(docInfo functionDoc) string {
	sections := []string{
		fmt.Sprintf("**%s**", docInfo.Label),
		"Built-in SSL function",
	}

	if docInfo.Detail != "" {
		sections = append(sections, fmt.Sprintf("`%s`", docInfo.Detail))
	}

	if docInfo.Documentation != "" {
		sections = append(sections, docInfo.Documentation)
	}

	if len(docInfo.Parameters) > 0 {
		sections = append(sections, formatFunctionParameters(docInfo.Parameters))
	}

	return strings.Join(sections, "\n\n")
}

func formatFunctionParameters(params []ParameterInformation) string {
	var builder strings.Builder
	builder.WriteString("**Parameters:**")

	for _, param := range params {
		builder.WriteString("\n- `")
		builder.WriteString(param.Label)
		builder.WriteString("`")
		doc := strings.TrimSpace(param.Documentation)
		if doc != "" {
			builder.WriteString(": ")
			builder.WriteString(doc)
		}
	}

	return builder.String()
}

// getClassHover returns hover information for a built-in class.
func getClassHover(word string) *Hover {
	wordLower := strings.ToLower(word)

	for _, className := range constants.SSLClassNames {
		if strings.ToLower(className) == wordLower {
			return &Hover{
				Contents: fmt.Sprintf("**%s**\n\nBuilt-in SSL class", className),
			}
		}
	}

	return nil
}

// getLiteralHover returns hover information for a literal.
func getLiteralHover(word string) *Hover {
	upper := strings.ToUpper(word)

	if constants.IsSSLLiteral(upper) {
		description := constants.SSLLiteralDescriptions[upper]
		if description == "" {
			description = fmt.Sprintf("SSL literal: %s", upper)
		}
		return &Hover{
			Contents: fmt.Sprintf("**%s**\n\n%s", upper, description),
		}
	}

	return nil
}

// getOperatorHover returns hover information for an operator.
func getOperatorHover(word string) *Hover {
	upper := strings.ToUpper(word)

	if constants.IsSSLOperator(upper) {
		description := constants.SSLOperatorDescriptions[upper]
		if description == "" {
			description = fmt.Sprintf("SSL operator: %s", upper)
		}
		return &Hover{
			Contents: fmt.Sprintf("**%s**\n\n%s", upper, description),
		}
	}

	return nil
}

// getProcedureHover returns hover information for a procedure defined in the document.
func getProcedureHover(word string, procedures []parser.ProcedureInfo) *Hover {
	wordLower := strings.ToLower(word)

	for _, proc := range procedures {
		if strings.ToLower(proc.Name) == wordLower {
			paramsDoc := "*No parameters*"
			if len(proc.Parameters) > 0 {
				paramsDoc = fmt.Sprintf("**Parameters:** %s", strings.Join(proc.Parameters, ", "))
			}

			return &Hover{
				Contents: fmt.Sprintf("**%s**\n\n*Procedure defined in this file*\n\n%s\n\n**Location:** Line %d-%d",
					proc.Name, paramsDoc, proc.StartLine, proc.EndLine),
			}
		}
	}

	return nil
}

// getVariableHover returns hover information for a variable defined in the document.
func getVariableHover(word string, variables []parser.VariableInfo) *Hover {
	wordLower := strings.ToLower(word)

	for _, v := range variables {
		if strings.ToLower(v.Name) == wordLower {
			return &Hover{
				Contents: fmt.Sprintf("**%s**\n\n*%s variable*\n\n**Declared at:** Line %d, Column %d",
					v.Name, v.Scope, v.Line, v.Column),
			}
		}
	}

	return nil
}

// GetHoverForToken returns hover for a specific token.
func GetHoverForToken(token *lexer.Token, procedures []parser.ProcedureInfo, variables []parser.VariableInfo) *Hover {
	word := token.Text

	switch token.Type {
	case lexer.TokenKeyword:
		return getKeywordHover(word)
	case lexer.TokenIdentifier:
		if hover := getFunctionHover(word); hover != nil {
			return hover
		}
		if hover := getClassHover(word); hover != nil {
			return hover
		}
		if hover := getProcedureHover(word, procedures); hover != nil {
			return hover
		}
		return getVariableHover(word, variables)
	case lexer.TokenOperator:
		return getOperatorHover(word)
	default:
		return nil
	}
}
