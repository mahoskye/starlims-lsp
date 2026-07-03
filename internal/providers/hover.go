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
	if hover := getTypeHover(word); hover != nil {
		return hover
	}
	if hover := getSpecialFormHover(word); hover != nil {
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

// GetEndpointAmbientHover returns hover information for `Request` or
// `Response` when the cursor is inside an endpoint script. These are
// pre-injected runtime objects available only in endpoint scripts; in
// other contexts the identifiers are unbound. Returns nil if the word
// isn't one of the ambients.
func GetEndpointAmbientHover(word string) *Hover {
	switch strings.ToUpper(word) {
	case "REQUEST":
		return &Hover{
			Contents: "**Request** *(endpoint ambient)*\n\n" +
				"The incoming HTTP request, pre-injected in endpoint scripts. " +
				"Access URL, headers, body, and query/form values via `Request:` members. " +
				"Not declared with `:DECLARE` — do not qualify with `Me:`. Available only in endpoint scripts.",
		}
	case "RESPONSE":
		return &Hover{
			Contents: "**Response** *(endpoint ambient)*\n\n" +
				"The outgoing HTTP reply being built, pre-injected in endpoint scripts. " +
				"Shape the response via `Response:` members (status, headers, body). " +
				"Not declared with `:DECLARE` — do not qualify with `Me:`. Available only in endpoint scripts.",
		}
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

	if constants.IsBuilderDirective(upper) {
		description := constants.DataSourceBuilderDirectiveDescriptions[upper]
		if description == "" {
			description = fmt.Sprintf("SQL data source builder directive: %s", upper)
		}
		return &Hover{
			Contents: fmt.Sprintf("**:%s** *(builder directive)*\n\n%s\n\nOnly valid in data source files (`.ds`, `.ds.txt`). Preprocessed by `SqlDataSourceBuilder` before SSL compilation.", upper, description),
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
				"- Pass self as argument: `ExecFunction(\"Module.Script\", {Me})`",
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
					Contents: formatFunctionHoverWithMeta(docInfo, fnName),
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

// formatFunctionHoverWithMeta is formatFunctionHover plus a layered meta
// section. Kept separate so the bare formatter is still callable from
// completion.go's documentation builder, where the meta is folded in
// elsewhere if at all.
func formatFunctionHoverWithMeta(docInfo functionDoc, name string) string {
	body := formatFunctionHover(docInfo)
	if meta, ok := constants.LookupMeta(name); ok {
		if extra := formatElementMeta(meta); extra != "" {
			return body + "\n\n" + extra
		}
	}
	return body
}

// formatElementMeta renders the documented exceptions, caveats, and Don't
// list (collapsed inline; the Do list is omitted because it tends to be
// generic). Returns an empty string when none of the lists carry content.
//
// Hover is read at-a-glance, so we keep this terse: bullet lists, no
// per-section divider lines. The Don't items are particularly valuable
// because they double as anti-pattern guidance the user gets without
// leaving the editor.
func formatElementMeta(meta constants.ElementMeta) string {
	var builder strings.Builder

	if len(meta.Exceptions) > 0 {
		builder.WriteString("**Documented exceptions:**")
		for _, ex := range meta.Exceptions {
			builder.WriteString("\n- ")
			builder.WriteString(ex.Trigger)
			if ex.Message != "" {
				builder.WriteString(" — `")
				builder.WriteString(ex.Message)
				builder.WriteString("`")
			}
		}
	}

	if len(meta.Caveats) > 0 {
		if builder.Len() > 0 {
			builder.WriteString("\n\n")
		}
		builder.WriteString("**Caveats:**")
		for _, c := range meta.Caveats {
			builder.WriteString("\n- ")
			builder.WriteString(c)
		}
	}

	if len(meta.BestPractices.Dont) > 0 {
		if builder.Len() > 0 {
			builder.WriteString("\n\n")
		}
		builder.WriteString("**Don't:**")
		for _, d := range meta.BestPractices.Dont {
			builder.WriteString("\n- ")
			builder.WriteString(d)
		}
	}

	return builder.String()
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

// getClassHover returns hover information for a built-in class. The hover
// surfaces the published summary, then enumerates constructors, properties,
// and methods drawn from the SSL element reference.
func getClassHover(word string) *Hover {
	wordLower := strings.ToLower(word)

	var canonical string
	for _, className := range constants.SSLClassNames {
		if strings.ToLower(className) == wordLower {
			canonical = className
			break
		}
	}
	if canonical == "" {
		return nil
	}

	det, ok := constants.GeneratedClassDetails[wordLower]
	if !ok {
		return &Hover{Contents: fmt.Sprintf("**%s**\n\nBuilt-in SSL class", canonical)}
	}

	var b strings.Builder
	fmt.Fprintf(&b, "**%s**", canonical)
	if det.BaseClass != "" {
		fmt.Fprintf(&b, "  *(inherits from `%s`)*", det.BaseClass)
	}
	b.WriteString("\n\n")
	if det.Summary != "" {
		b.WriteString(det.Summary)
		b.WriteString("\n")
	}

	if len(det.Constructors) > 0 {
		b.WriteString("\n**Constructors:**\n\n")
		for _, c := range det.Constructors {
			fmt.Fprintf(&b, "- `%s`", c.Signature)
			if c.Description != "" {
				fmt.Fprintf(&b, " — %s", c.Description)
			}
			b.WriteString("\n")
		}
	}

	if len(det.Properties) > 0 {
		b.WriteString("\n**Properties:**\n\n")
		for _, p := range det.Properties {
			fmt.Fprintf(&b, "- `%s`", p.Name)
			if p.Type != "" {
				fmt.Fprintf(&b, " *(%s", p.Type)
				if p.Access != "" {
					fmt.Fprintf(&b, ", %s", p.Access)
				}
				b.WriteString(")*")
			}
			if p.Description != "" {
				fmt.Fprintf(&b, " — %s", p.Description)
			}
			b.WriteString("\n")
		}
	}

	if len(det.Methods) > 0 {
		b.WriteString("\n**Methods:**\n\n")
		for _, m := range det.Methods {
			fmt.Fprintf(&b, "- `%s`", m.Name)
			if m.Returns != "" && m.Returns != "none" {
				fmt.Fprintf(&b, " → %s", m.Returns)
			}
			if m.Description != "" {
				fmt.Fprintf(&b, " — %s", m.Description)
			}
			b.WriteString("\n")
		}
	}

	if meta, ok := constants.LookupMeta(canonical); ok {
		extra := formatElementMeta(meta)
		if extra != "" {
			b.WriteString("\n")
			b.WriteString(extra)
		}
	}

	return &Hover{Contents: strings.TrimRight(b.String(), "\n")}
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

// getOperatorHover returns hover information for an operator. The hover
// includes the curated short description plus the type-behavior table from
// the published reference when one is available (e.g. for `+=`, `==`, `$`).
func getOperatorHover(word string) *Hover {
	upper := strings.ToUpper(word)

	if !constants.IsSSLOperator(upper) {
		return nil
	}

	description := constants.SSLOperatorDescriptions[upper]
	if description == "" {
		description = fmt.Sprintf("SSL operator: %s", upper)
	}

	var b strings.Builder
	fmt.Fprintf(&b, "**%s**\n\n%s", upper, description)

	// The operator description lookup is keyed by symbol; the JSON-derived
	// detail map is keyed by symbol too via GeneratedOperatorBySymbol.
	if det, ok := constants.GeneratedOperatorBySymbol[word]; ok && len(det.TypeBehavior) > 0 {
		b.WriteString("\n\n**Type behavior:**\n\n")
		b.WriteString("| Left | Right | Result | Behavior |\n")
		b.WriteString("|------|-------|--------|----------|\n")
		for _, row := range det.TypeBehavior {
			fmt.Fprintf(&b, "| %s | %s | %s | %s |\n",
				orDash(row.Left), orDash(row.Right), orDash(row.Result), row.Behavior)
		}
	} else if det, ok := constants.GeneratedOperatorBySymbol[upper]; ok && len(det.TypeBehavior) > 0 {
		b.WriteString("\n\n**Type behavior:**\n\n")
		b.WriteString("| Left | Right | Result | Behavior |\n")
		b.WriteString("|------|-------|--------|----------|\n")
		for _, row := range det.TypeBehavior {
			fmt.Fprintf(&b, "| %s | %s | %s | %s |\n",
				orDash(row.Left), orDash(row.Right), orDash(row.Result), row.Behavior)
		}
	}

	return &Hover{Contents: strings.TrimRight(b.String(), "\n")}
}

func orDash(s string) string {
	if s == "" {
		return "—"
	}
	return s
}

// getTypeHover returns hover information for one of the 8 core SSL value
// types (array, boolean, codeblock, date, netobject, number, object, string).
// Shows runtime type, supported operators, and members.
func getTypeHover(word string) *Hover {
	det, ok := constants.GeneratedTypeDetails[strings.ToLower(word)]
	if !ok {
		return nil
	}

	var b strings.Builder
	fmt.Fprintf(&b, "**%s**", det.Title)
	if det.RuntimeType != "" {
		fmt.Fprintf(&b, "  *(runtime type: `%s`)*", det.RuntimeType)
	}
	b.WriteString("\n\n")
	if det.Summary != "" {
		b.WriteString(det.Summary)
		b.WriteString("\n")
	}

	if len(det.Operators) > 0 {
		b.WriteString("\n**Operators:**\n\n")
		for _, op := range det.Operators {
			fmt.Fprintf(&b, "- `%s`", op.Symbol)
			if op.Operator != "" && op.Operator != op.Symbol {
				fmt.Fprintf(&b, " (`%s`)", op.Operator)
			}
			if op.Returns != "" {
				fmt.Fprintf(&b, " → %s", op.Returns)
			}
			if op.Behavior != "" {
				fmt.Fprintf(&b, " — %s", op.Behavior)
			}
			b.WriteString("\n")
		}
	}

	if len(det.Members) > 0 {
		b.WriteString("\n**Members:**\n\n")
		for _, m := range det.Members {
			fmt.Fprintf(&b, "- `%s`", m.Name)
			if m.Kind != "" {
				fmt.Fprintf(&b, " *(%s)*", m.Kind)
			}
			if m.Returns != "" && m.Returns != "none" {
				fmt.Fprintf(&b, " → %s", m.Returns)
			}
			if m.Description != "" {
				fmt.Fprintf(&b, " — %s", m.Description)
			}
			b.WriteString("\n")
		}
	}

	return &Hover{Contents: strings.TrimRight(b.String(), "\n")}
}

// getSpecialFormHover returns hover information for one of the 6 SSL special
// forms (access-modifiers, base, code-block, code-organization, constructor,
// me). Shows summary and canonical syntax block.
func getSpecialFormHover(word string) *Hover {
	det, ok := constants.GeneratedSpecialFormDetails[strings.ToLower(word)]
	if !ok {
		return nil
	}

	var b strings.Builder
	fmt.Fprintf(&b, "**%s**\n\n", det.Title)
	if det.Summary != "" {
		b.WriteString(det.Summary)
		b.WriteString("\n")
	}
	if det.Syntax != "" {
		b.WriteString("\n**Syntax:**\n\n```ssl\n")
		b.WriteString(det.Syntax)
		b.WriteString("\n```")
	}

	return &Hover{Contents: strings.TrimRight(b.String(), "\n")}
}

// getProcedureHover returns hover information for a procedure defined in the document.
func getProcedureHover(word string, procedures []parser.ProcedureInfo) *Hover {
	wordLower := strings.ToLower(word)

	for _, proc := range procedures {
		if strings.ToLower(proc.Name) == wordLower {
			return &Hover{Contents: renderProcedureHover(proc)}
		}
	}

	return nil
}

// renderProcedureHover formats a hover panel for a script procedure, weaving
// in any docblock-derived description / parameter docs / return doc.
func renderProcedureHover(proc parser.ProcedureInfo) string {
	return renderProcedureHoverWithOrigin(proc, "*Procedure defined in this file*")
}

// renderProcedureHoverWithOrigin is renderProcedureHover with the origin
// line parameterized so cross-file hovers can name the defining script.
func renderProcedureHoverWithOrigin(proc parser.ProcedureInfo, origin string) string {
	var b strings.Builder
	fmt.Fprintf(&b, "**%s**\n\n%s", proc.Name, origin)

	if proc.Doc.Description != "" {
		fmt.Fprintf(&b, "\n\n%s", proc.Doc.Description)
	}

	if len(proc.Parameters) > 0 {
		b.WriteString("\n\n**Parameters:**")
		for _, name := range proc.Parameters {
			if desc := lookupParamDoc(proc.Doc.ParameterDocs, name); desc != "" {
				fmt.Fprintf(&b, "\n- `%s` — %s", name, desc)
			} else {
				fmt.Fprintf(&b, "\n- `%s`", name)
			}
		}
	} else {
		b.WriteString("\n\n*No parameters*")
	}

	if proc.Doc.Returns != "" {
		fmt.Fprintf(&b, "\n\n**Returns:** %s", proc.Doc.Returns)
	}

	fmt.Fprintf(&b, "\n\n**Location:** Line %d-%d", proc.StartLine, proc.EndLine)
	return b.String()
}

// lookupParamDoc resolves a parameter name against the parsed doc map,
// case-insensitively (docblocks rarely match SSL's loose casing exactly).
func lookupParamDoc(docs map[string]string, name string) string {
	if docs == nil {
		return ""
	}
	if v, ok := docs[name]; ok {
		return v
	}
	lower := strings.ToLower(name)
	for k, v := range docs {
		if strings.ToLower(k) == lower {
			return v
		}
	}
	return ""
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

// simpleNamedParamPattern matches standard SQLExecute placeholder forms:
// simple variables (?sVar?), property access (?oObj:Prop?), array indexing (?aArr[i]?),
// and parameterless function calls (?Today()?). Complex expressions with arithmetic
// operators (?sPrefix + sSuffix?) do NOT match and trigger a performance warning.
var simpleNamedParamPattern = regexp.MustCompile(`^[a-zA-Z_][a-zA-Z0-9_.:[\](),']*$`)

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
		case ch == ' ', ch == '+', ch == '-', ch == '*', ch == '/', ch == ',', ch == '\'':
			// Allow complex expression characters so they can be parsed and flagged
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

// WorkspaceProcInfo is the provider-side view of an indexed procedure for
// cross-file hover/completion rendering (providers cannot see the server's
// index types).
type WorkspaceProcInfo struct {
	Name       string
	Parameters []string
	Doc        parser.ProcedureDoc
	StartLine  int
	EndLine    int
}

// RenderCrossFileProcedureHover renders hover for a procedure defined in
// another workspace script. scriptDisplay is "Category.Script" when the
// category is known, else the bare script name; extra is the count of
// additional ambiguous matches (0 for a unique resolution).
func RenderCrossFileProcedureHover(p WorkspaceProcInfo, scriptDisplay string, extra int) string {
	proc := parser.ProcedureInfo{
		Name:       p.Name,
		Parameters: p.Parameters,
		Doc:        p.Doc,
		StartLine:  p.StartLine,
		EndLine:    p.EndLine,
	}
	origin := fmt.Sprintf("*Procedure defined in `%s`*", scriptDisplay)
	out := renderProcedureHoverWithOrigin(proc, origin)
	if extra > 0 {
		out += fmt.Sprintf("\n\n*+%d other match(es) — use go-to-definition to choose*", extra)
	}
	return out
}

// RenderScriptEntryHover renders hover for a 2-part dispatch target or an
// :INCLUDE resolving to a script's entry point.
func RenderScriptEntryHover(scriptDisplay string, entryParams []string, isClass bool, procCount, extra int) string {
	var b strings.Builder
	fmt.Fprintf(&b, "**%s**\n\n", scriptDisplay)
	if isClass {
		b.WriteString("*Class script*")
	} else {
		b.WriteString("*Script entry point*")
	}
	if len(entryParams) > 0 {
		b.WriteString("\n\n**Parameters:**")
		for _, name := range entryParams {
			fmt.Fprintf(&b, "\n- `%s`", name)
		}
	} else {
		b.WriteString("\n\n*No entry parameters*")
	}
	if procCount > 0 {
		fmt.Fprintf(&b, "\n\n%d procedure(s)", procCount)
	}
	if extra > 0 {
		fmt.Fprintf(&b, "\n\n*+%d other match(es) — use go-to-definition to choose*", extra)
	}
	return b.String()
}
