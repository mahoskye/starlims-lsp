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
	if hover := getBaseKeywordHover(word); hover != nil {
		return hover
	}
	if hover := getConstructorHover(word); hover != nil {
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

func getBaseKeywordHover(word string) *Hover {
	if strings.EqualFold(word, "Base") {
		return &Hover{
			Contents: "**Base**\n\n" +
				"*Parent-class reference*\n\n" +
				"Used inside a `:CLASS` method to access inherited members.\n\n" +
				"**Usage:**\n" +
				"- Call a parent method: `Base:MethodName()`\n" +
				"- Access an inherited member: `Base:PropertyName`\n\n" +
				"`Base` must be followed by `:MemberName` and is only meaningful in class context.",
		}
	}
	return nil
}

func getConstructorHover(word string) *Hover {
	if strings.EqualFold(word, "Constructor") {
		return &Hover{
			Contents: "**Constructor**\n\n" +
				"*Reserved class constructor name*\n\n" +
				"Inside a `:CLASS`, define the constructor with `:PROCEDURE Constructor;`.\n\n" +
				"**Rules:**\n" +
				"- Constructors belong inside a `:CLASS`\n" +
				"- Class member order must be `:INHERIT`, `:DECLARE`, regular methods, then `Constructor`\n" +
				"- `:RETURN` cannot return a value from a constructor",
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
	canonical, ok := constants.CanonicalSSLLiteral(strings.ToUpper(word))
	if ok {
		description := constants.SSLLiteralDescriptions[canonical]
		if description == "" {
			description = fmt.Sprintf("SSL literal: %s", canonical)
		}
		return &Hover{
			Contents: fmt.Sprintf("**%s**\n\n%s", canonical, description),
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

var simpleNamedParamPattern = regexp.MustCompile(`^[a-zA-Z_][a-zA-Z0-9_]*$`)

// ParseSQLPlaceholders extracts all SQL placeholders from a string.
// It handles both named parameters (?paramName?) and positional parameters (?).
func ParseSQLPlaceholders(sqlString string) []SQLPlaceholder {
	var placeholders []SQLPlaceholder
	positionalIndex := 0

	for i := 0; i < len(sqlString); i++ {
		if sqlString[i] != '?' {
			continue
		}

		if end, name, ok := parseNamedPlaceholder(sqlString, i); ok {
			placeholders = append(placeholders, SQLPlaceholder{
				Name:    name,
				Start:   i,
				End:     end + 1,
				IsNamed: true,
			})
			i = end
			continue
		}

		positionalIndex++
		placeholders = append(placeholders, SQLPlaceholder{
			Position: positionalIndex,
			Start:    i,
			End:      i + 1,
			IsNamed:  false,
		})
	}

	// Sort placeholders by their position in the string
	sort.Slice(placeholders, func(i, j int) bool {
		return placeholders[i].Start < placeholders[j].Start
	})

	return placeholders
}

func parseNamedPlaceholder(sqlString string, start int) (int, string, bool) {
	if start < 0 || start >= len(sqlString) || sqlString[start] != '?' {
		return 0, "", false
	}

	for end := start + 1; end < len(sqlString); end++ {
		if sqlString[end] != '?' {
			continue
		}

		name := sqlString[start+1 : end]
		if isNamedPlaceholderContent(name) {
			return end, name, true
		}
		break
	}

	return 0, "", false
}

func isNamedPlaceholderContent(name string) bool {
	if name == "" {
		return false
	}

	first := rune(name[0])
	if !((first >= 'a' && first <= 'z') || (first >= 'A' && first <= 'Z') || first == '_') {
		return false
	}

	for _, ch := range name {
		switch {
		case ch >= 'a' && ch <= 'z':
		case ch >= 'A' && ch <= 'Z':
		case ch >= '0' && ch <= '9':
		case ch == '_', ch == ':', ch == '[', ch == ']', ch == '(', ch == ')', ch == '.':
		default:
			return false
		}
	}

	return true
}

func isSimpleNamedPlaceholder(name string) bool {
	return simpleNamedParamPattern.MatchString(name)
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
