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
	if hover := getClassContextHover(word); hover != nil {
		return hover
	}
	if hover := getFunctionHover(word); hover != nil {
		return hover
	}
	if hover := getClassHover(word); hover != nil {
		return hover
	}
	if hover := getReturnsObjectHover(word); hover != nil {
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
	// A member name is a property or method on the receiver's type, not
	// the file's like-named procedure or variable (issue #184). Reporting
	// the local `sName` for `oRec:sName` is a wrong answer; the earlier
	// providers above answer from published inventory data and stay.
	if roleAtTextPosition(text, line-1, column-1) == parser.RoleMember {
		return nil
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
//
// Since the issue #123 refresh, the hover renders from published data:
// the special-form summary/syntax plus the backing returns object's
// member tables (SSLRequest/SSLResponse) and element meta.
func GetEndpointAmbientHover(word string) *Hover {
	var slug, backing string
	switch strings.ToUpper(word) {
	case "REQUEST":
		slug, backing = "request", "SSLRequest"
	case "RESPONSE":
		slug, backing = "response", "SSLResponse"
	default:
		return nil
	}

	var b strings.Builder
	det, ok := constants.GeneratedSpecialFormDetails[slug]
	if !ok {
		return nil
	}
	fmt.Fprintf(&b, "**%s** *(endpoint ambient — `%s`)*\n\n", det.Title, backing)
	if det.Summary != "" {
		b.WriteString(det.Summary)
		b.WriteString("\n\n")
	}
	b.WriteString("Pre-set by the runtime in endpoint scripts; not declared with `:DECLARE`.\n")
	if det.Syntax != "" {
		b.WriteString("\n**Syntax:**\n\n```ssl\n")
		b.WriteString(det.Syntax)
		b.WriteString("\n```\n")
	}

	obj := constants.GeneratedReturnsObjectDetails[strings.ToLower(backing)]
	writeMemberTables(&b, obj.Properties, obj.Methods)

	// One set of sections: the ambient's own page (how Request/Response
	// behave in and outside an endpoint) followed by the backing object's.
	var merged constants.ElementMeta
	appendMeta := func(m constants.ElementMeta, ok bool) {
		if !ok {
			return
		}
		merged.Exceptions = append(merged.Exceptions, m.Exceptions...)
		merged.Caveats = append(merged.Caveats, m.Caveats...)
		merged.BestPractices.Dont = append(merged.BestPractices.Dont, m.BestPractices.Dont...)
	}
	appendMeta(constants.LookupMetaByID("ssl.special_form." + slug))
	appendMeta(constants.LookupMeta(backing))
	if extra := formatElementMeta(merged); extra != "" {
		b.WriteString("\n")
		b.WriteString(extra)
	}

	return &Hover{Contents: strings.TrimRight(b.String(), "\n")}
}

// getKeywordHover returns hover information for a keyword: the published
// summary and syntax block, then the reference caveats and don't-list
// (issue #252 — the hand-written one-liners this replaced had drifted).
func getKeywordHover(word string) *Hover {
	upper := strings.ToUpper(strings.TrimPrefix(word, ":"))

	if constants.IsKeyword(upper) {
		det := constants.GeneratedKeywordDetails[upper]
		summary := det.Summary
		if summary == "" {
			summary = fmt.Sprintf("SSL keyword: %s", upper)
		}
		return &Hover{Contents: renderReferenceHover(
			fmt.Sprintf("**:%s** *(keyword)*", upper), summary, det.Syntax, "ssl.keyword."+strings.ToLower(upper))}
	}

	if constants.IsBuilderDirective(upper) {
		description := constants.DataSourceBuilderDirectiveDescriptions[upper]
		if description == "" {
			description = fmt.Sprintf("SQL data source builder directive: %s", upper)
		}
		return &Hover{
			Contents: fmt.Sprintf("**:%s** *(data source directive)*\n\n%s\n\n"+
				"Only meaningful in SQL data source files (`.ds`, `.ds.txt`), where it is read before the query runs. "+
				"Not an SSL keyword; the name is case-sensitive, so write it in uppercase.", upper, description),
		}
	}

	return nil
}

// renderReferenceHover renders a reference element hover: a heading, the
// published summary, an optional SSL syntax block, and the element's
// caveats and don't-list looked up by reference ID.
func renderReferenceHover(heading, summary, syntax, metaID string) string {
	var b strings.Builder
	b.WriteString(heading)
	if summary != "" {
		b.WriteString("\n\n")
		b.WriteString(summary)
	}
	if syntax != "" {
		b.WriteString("\n\n```ssl\n")
		b.WriteString(syntax)
		b.WriteString("\n```")
	}
	if meta, ok := constants.LookupMetaByID(metaID); ok {
		if extra := formatElementMeta(meta); extra != "" {
			b.WriteString("\n\n")
			b.WriteString(extra)
		}
	}
	return b.String()
}

// getClassContextHover returns hover for the class-context forms `Me`,
// `Base`, and `Constructor`, rendered from their published special-form
// pages (issue #252).
func getClassContextHover(word string) *Hover {
	var slug, name string
	switch strings.ToLower(word) {
	case "me":
		slug, name = "me", "Me"
	case "base":
		slug, name = "base", "Base"
	case "constructor":
		slug, name = "constructor", "Constructor"
	default:
		return nil
	}
	det := constants.GeneratedSpecialFormDetails[slug]
	return &Hover{Contents: renderReferenceHover(
		fmt.Sprintf("**%s** *(class context)*", name), det.Summary, det.Syntax, "ssl.special_form."+slug)}
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

// formatFunctionHover renders a built-in function hover top to bottom: the
// call form(s) as SSL, what the function does, each parameter with its
// type, optionality, and default, then what it returns (issue #252).
func formatFunctionHover(docInfo functionDoc) string {
	forms := make([]string, 0, len(docInfo.Forms))
	for _, f := range docInfo.Forms {
		forms = append(forms, f.Label)
	}
	sections := []string{
		"```ssl\n" + strings.Join(forms, "\n") + "\n```",
		"*Built-in SSL function*",
	}

	if docInfo.Description != "" {
		sections = append(sections, docInfo.Description)
	}

	if len(docInfo.Parameters) > 0 {
		sections = append(sections, formatFunctionParameters(docInfo.Parameters))
	}

	if docInfo.ReturnLine != "" {
		sections = append(sections, docInfo.ReturnLine)
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
				builder.WriteString(" — ")
				builder.WriteString(exceptionMessage(ex.Message))
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

// exceptionMessage renders a documented exception message as inline code.
// A few published messages mix the runtime text with prose ("…, then
// <text> on the second line") and arrive with stray backticks from the
// extraction; wrapping those in a code span would break the span, so they
// render as plain text with the backticks removed.
func exceptionMessage(msg string) string {
	if strings.Contains(msg, "`") {
		return strings.ReplaceAll(msg, "`", "")
	}
	return "`" + msg + "`"
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
			for _, p := range c.Parameters {
				fmt.Fprintf(&b, "  - `%s`: %s\n", p.Name, parameterDoc(p))
			}
		}
	}

	writeMemberTables(&b, det.Properties, det.Methods)

	if meta, ok := constants.LookupMeta(canonical); ok {
		extra := formatElementMeta(meta)
		if extra != "" {
			b.WriteString("\n")
			b.WriteString(extra)
		}
	}

	return &Hover{Contents: strings.TrimRight(b.String(), "\n")}
}

// getReturnsObjectHover returns hover information for a returns-category
// object (issue #123): an object obtainable only as a built-in method's
// return value (HttpClient, SoapResponse, …) or ambient in endpoint
// scripts. Renders like a class hover — summary, properties, methods —
// plus the element meta the other categories already surface.
func getReturnsObjectHover(word string) *Hover {
	if !constants.IsReturnsObject(word) {
		return nil
	}
	canonical := CanonicalReceiverTypeName(word)
	det := constants.GeneratedReturnsObjectDetails[strings.ToLower(word)]

	var b strings.Builder
	fmt.Fprintf(&b, "**%s**  *(returns object)*\n\n", canonical)
	if det.Summary != "" {
		b.WriteString(det.Summary)
		b.WriteString("\n")
	}
	writeMemberTables(&b, det.Properties, det.Methods)

	if meta, ok := constants.LookupMeta(canonical); ok {
		extra := formatElementMeta(meta)
		if extra != "" {
			b.WriteString("\n")
			b.WriteString(extra)
		}
	}

	return &Hover{Contents: strings.TrimRight(b.String(), "\n")}
}

// writeMemberTables renders the Properties/Methods bullet lists shared by
// class, returns-object, and endpoint-ambient hovers.
func writeMemberTables(b *strings.Builder, properties []constants.ClassProperty, methods []constants.ClassMethod) {
	if len(properties) > 0 {
		b.WriteString("\n**Properties:**\n\n")
		for _, p := range properties {
			fmt.Fprintf(b, "- `%s`", p.Name)
			if p.Type != "" {
				fmt.Fprintf(b, " *(%s", p.Type)
				if p.Access != "" {
					fmt.Fprintf(b, ", %s", p.Access)
				}
				b.WriteString(")*")
			}
			if p.Description != "" {
				fmt.Fprintf(b, " — %s", p.Description)
			}
			b.WriteString("\n")
		}
	}
	if len(methods) > 0 {
		b.WriteString("\n**Methods:**\n\n")
		for _, m := range methods {
			fmt.Fprintf(b, "- `%s`", m.Name)
			if m.Returns != "" && m.Returns != "none" {
				fmt.Fprintf(b, " → %s", m.Returns)
			}
			if m.Description != "" {
				fmt.Fprintf(b, " — %s", m.Description)
			}
			b.WriteString("\n")
		}
	}
}

// getLiteralHover returns hover information for a literal from its
// published reference page.
func getLiteralHover(word string) *Hover {
	canonical, ok := constants.CanonicalSSLLiteral(strings.ToUpper(word))
	if !ok {
		return nil
	}
	description := constants.SSLLiteralDescriptions[canonical]
	if description == "" {
		description = fmt.Sprintf("SSL literal: %s", canonical)
	}
	return &Hover{Contents: renderReferenceHover(
		fmt.Sprintf("**%s** *(literal)*", canonical), description, "", "ssl.literal."+constants.LiteralSlug(canonical))}
}

// getOperatorHover returns hover information for an operator: the published
// summary and syntax, the type-behavior table, then the reference caveats
// and don't-list. For `=`, `==`, and `!=` the table and don't-list carry the
// string semantics (prefix match vs exact match) in full.
func getOperatorHover(word string) *Hover {
	upper := strings.ToUpper(word)

	if !constants.IsSSLOperator(upper) {
		return nil
	}

	slug := constants.OperatorSlug(upper)
	det := constants.GeneratedOperatorDetails[slug]
	summary := det.Summary
	if summary == "" {
		summary = fmt.Sprintf("SSL operator: %s", upper)
	}

	var b strings.Builder
	heading := fmt.Sprintf("**%s**", upper)
	if slug != "" {
		heading += fmt.Sprintf(" *(%s)*", slug)
	}
	b.WriteString(heading)
	b.WriteString("\n\n")
	b.WriteString(summary)
	if det.Syntax != "" {
		b.WriteString("\n\n```ssl\n")
		b.WriteString(det.Syntax)
		b.WriteString("\n```")
	}

	if len(det.TypeBehavior) > 0 {
		b.WriteString("\n\n**Type behavior:**\n\n")
		b.WriteString("| Left | Right | Result | Behavior |\n")
		b.WriteString("|------|-------|--------|----------|")
		for _, row := range det.TypeBehavior {
			fmt.Fprintf(&b, "\n| %s | %s | %s | %s |",
				orDash(row.Left), orDash(row.Right), orDash(row.Result), row.Behavior)
		}
	}

	if meta, ok := constants.LookupMetaByID("ssl.operator." + slug); ok && slug != "" {
		if extra := formatElementMeta(meta); extra != "" {
			b.WriteString("\n\n")
			b.WriteString(extra)
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

	// Value types with no SSL-defined `:` members publish a Task | Use
	// table instead; say so, so the hover never implies members exist.
	if len(det.Tasks) > 0 {
		b.WriteString("\n**No SSL-defined `:` members.** Use instead:\n\n")
		for _, t := range det.Tasks {
			// "see below" points at a section of the reference page that
			// the hover does not carry.
			use := strings.TrimSuffix(t.Use, "; see below")
			fmt.Fprintf(&b, "- %s: %s\n", t.Task, use)
		}
	}

	return &Hover{Contents: strings.TrimRight(b.String(), "\n")}
}

// getSpecialFormHover returns hover information for an SSL special form
// (access-modifiers, base, code-block, code-organization, constructor, me,
// request, response). Shows summary and canonical syntax block.
//
// The request/response slugs are excluded here: in endpoint files the
// ambient hover (GetEndpointAmbientHover) owns them with full member
// detail, and in any other file an identifier named Request or Response is
// an ordinary variable that must not receive special-form hover (issue
// #123 D1).
func getSpecialFormHover(word string) *Hover {
	slug := strings.ToLower(word)
	if slug == "request" || slug == "response" {
		return nil
	}
	det, ok := constants.GeneratedSpecialFormDetails[slug]
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

// ProcedureHoverMarkdown renders the same-file procedure hover for name, or
// "" when no procedure matches (case-insensitive). Bare 1-part dispatch
// targets keep same-file semantics, so the server routes them here
// (feature.hover A17-A18).
func ProcedureHoverMarkdown(name string, procedures []parser.ProcedureInfo) string {
	if hover := getProcedureHover(name, procedures); hover != nil {
		return hover.Contents
	}
	return ""
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
		if hover := getReturnsObjectHover(word); hover != nil {
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
					Contents: fmt.Sprintf("**SQL parameter `?%s?`** *(named placeholder)*\n\n"+
						"`SQLExecute` binds the current value of `%s` from the calling scope as a query parameter "+
						"when the statement runs; the value is not spliced into the SQL text. "+
						"An array variable expands to one `?` per element, for `IN (...)` lists; "+
						"it must be a local variable, not an object property.\n\n"+
						"Only `SQLExecute` substitutes `?name?`. `RunSQL`, `LSearch`, `LSelect1`, `GetDataSet` "+
						"and the other SQL functions take positional `?` with a values array.",
						p.Name, p.Name),
				}
			}
			// Positional parameter
			ordinal := getOrdinal(p.Position)
			return &Hover{
				Contents: fmt.Sprintf("**SQL parameter #%d** *(positional placeholder)*\n\n"+
					"Bound to the %s element of the values array passed with the query, "+
					"such as `RunSQL(sSQL,, {…})`. Values are bound as parameters, not spliced into the SQL text, "+
					"and the number of `?` placeholders must match the number of values.",
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

// RenderDataSourceHover renders hover for a RunDS target resolving to a
// workspace data source (feature.hover A14).
func RenderDataSourceHover(display string, entryParams []string, extra int) string {
	var b strings.Builder
	fmt.Fprintf(&b, "**%s**\n\n*Data source*", display)
	if len(entryParams) > 0 {
		b.WriteString("\n\n**Parameters:**")
		for _, name := range entryParams {
			fmt.Fprintf(&b, "\n- `%s`", name)
		}
	} else {
		b.WriteString("\n\n*No parameters*")
	}
	if extra > 0 {
		fmt.Fprintf(&b, "\n\n*+%d other match(es) — use go-to-definition to choose*", extra)
	}
	return b.String()
}
