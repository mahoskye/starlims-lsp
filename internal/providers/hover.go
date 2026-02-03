package providers

import (
	"fmt"
	"regexp"
	"sort"
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
	if hover := getMeKeywordHover(word); hover != nil {
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

// getMeKeywordHover returns hover information for the 'Me' keyword.
func getMeKeywordHover(word string) *Hover {
	if strings.EqualFold(word, "Me") {
		return &Hover{
			Contents: "**Me**\n\n" +
				"*Self-reference keyword*\n\n" +
				"Refers to the current object instance within a class definition.\n\n" +
				"**Usage:**\n" +
				"- Access class members: `Me:PropertyName`\n" +
				"- Call class methods: `Me:MethodName()`\n" +
				"- Pass self to other functions: `DoSomething(Me)`",
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

// SQLPlaceholder represents a SQL parameter placeholder.
type SQLPlaceholder struct {
	Name     string // Parameter name (empty for positional)
	Position int    // Position index (1-based) for positional parameters
	Start    int    // Start position within the string (0-based, relative to string content)
	End      int    // End position within the string (exclusive)
	IsNamed  bool   // True if this is a named parameter (?name?)
}

// namedParamPattern matches named SQL parameters like ?paramName?
var namedParamPattern = regexp.MustCompile(`\?([a-zA-Z_][a-zA-Z0-9_]*)\?`)

// ParseSQLPlaceholders extracts all SQL placeholders from a string.
// It handles both named parameters (?paramName?) and positional parameters (?).
func ParseSQLPlaceholders(sqlString string) []SQLPlaceholder {
	var placeholders []SQLPlaceholder
	positionalIndex := 0

	// First, find all named parameters and their positions
	namedMatches := namedParamPattern.FindAllStringSubmatchIndex(sqlString, -1)
	namedPositions := make(map[int]bool)
	for _, match := range namedMatches {
		if len(match) >= 4 {
			namedPositions[match[0]] = true
			placeholders = append(placeholders, SQLPlaceholder{
				Name:    sqlString[match[2]:match[3]], // Capture group 1
				Start:   match[0],
				End:     match[1],
				IsNamed: true,
			})
		}
	}

	// Then find positional parameters (? not followed by alphanumeric)
	// We need to manually scan to avoid matching the ? in ?name?
	for i := 0; i < len(sqlString); i++ {
		if sqlString[i] == '?' {
			// Check if this is the start of a named parameter
			if namedPositions[i] {
				continue
			}
			// Check if this is the ending ? of a named parameter
			if i > 0 {
				// Look back to see if this is closing a named param
				isClosing := false
				for j := i - 1; j >= 0; j-- {
					if sqlString[j] == '?' {
						// Check if there's a valid name between j and i
						between := sqlString[j+1 : i]
						if len(between) > 0 && namedParamPattern.MatchString("?"+between+"?") {
							isClosing = true
						}
						break
					}
					// If we hit a non-identifier char, stop looking
					c := sqlString[j]
					if !((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c == '_') {
						break
					}
				}
				if isClosing {
					continue
				}
			}
			// This is a positional parameter
			positionalIndex++
			placeholders = append(placeholders, SQLPlaceholder{
				Position: positionalIndex,
				Start:    i,
				End:      i + 1,
				IsNamed:  false,
			})
		}
	}

	// Sort placeholders by their position in the string
	sort.Slice(placeholders, func(i, j int) bool {
		return placeholders[i].Start < placeholders[j].Start
	})

	return placeholders
}

// GetSQLPlaceholderHover returns hover information for a SQL placeholder at the given position.
// The column is relative to the start of the string content (after the opening quote).
func GetSQLPlaceholderHover(stringContent string, columnInString int) *Hover {
	placeholders := ParseSQLPlaceholders(stringContent)

	for _, p := range placeholders {
		// Check if the cursor position is within this placeholder
		if columnInString >= p.Start && columnInString < p.End {
			if p.IsNamed {
				return &Hover{
					Contents: fmt.Sprintf("**SQL Parameter: %s**\n\n"+
						"*Named parameter placeholder*\n\n"+
						"This placeholder will be replaced with the value of `%s` at runtime.\n\n"+
						"**Syntax:** `?parameterName?`",
						p.Name, p.Name),
				}
			}
			// Positional parameter
			ordinal := getOrdinal(p.Position)
			return &Hover{
				Contents: fmt.Sprintf("**SQL Parameter #%d**\n\n"+
					"*Positional parameter placeholder*\n\n"+
					"This is the %s parameter in the query. It will be replaced with "+
					"the corresponding value from the parameters array at runtime.\n\n"+
					"**Syntax:** `?`",
					p.Position, ordinal),
			}
		}
	}

	return nil
}

// GetSQLPlaceholderHoverFromToken returns hover for a SQL placeholder when the cursor is inside a string token.
// line and column are 1-based positions in the document.
func GetSQLPlaceholderHoverFromToken(tokens []lexer.Token, line, column int) *Hover {
	// Find the string token containing the cursor
	for _, token := range tokens {
		if token.Type != lexer.TokenString {
			continue
		}

		// Check if position is within this token
		tokenEnd := token.Column + len(token.Text)
		if token.Line == line && column >= token.Column && column < tokenEnd {
			// Found the string token - extract content without quotes
			content := token.Text
			if len(content) >= 2 {
				// Remove surrounding quotes
				content = content[1 : len(content)-1]
			}

			// Calculate position within the string content
			// column is 1-based, token.Column is 1-based
			// We need to account for the opening quote
			columnInString := column - token.Column - 1

			if columnInString < 0 || columnInString >= len(content) {
				return nil
			}

			return GetSQLPlaceholderHover(content, columnInString)
		}
	}

	return nil
}

// getOrdinal returns the ordinal string for a number (1st, 2nd, 3rd, etc.)
func getOrdinal(n int) string {
	suffix := "th"
	if n%100 < 10 || n%100 > 20 {
		switch n % 10 {
		case 1:
			suffix = "st"
		case 2:
			suffix = "nd"
		case 3:
			suffix = "rd"
		}
	}
	return fmt.Sprintf("%d%s", n, suffix)
}
