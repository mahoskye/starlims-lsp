// Package providers implements LSP feature providers for SSL.
package providers

import (
	"fmt"
	"strings"
	"unicode"

	"starlims-lsp/internal/constants"
	"starlims-lsp/internal/lexer"
	"starlims-lsp/internal/parser"
)

// DiagnosticSeverity represents the severity of a diagnostic.
type DiagnosticSeverity int

const (
	SeverityError   DiagnosticSeverity = 1
	SeverityWarning DiagnosticSeverity = 2
	SeverityInfo    DiagnosticSeverity = 3
	SeverityHint    DiagnosticSeverity = 4
)

// Range represents a range in a text document.
type Range struct {
	Start Position
	End   Position
}

// Position represents a position in a text document.
type Position struct {
	Line      int
	Character int
}

// Diagnostic represents a diagnostic message.
type Diagnostic struct {
	Range    Range
	Severity DiagnosticSeverity
	Message  string
	Source   string
}

// DiagnosticOptions configures diagnostic checking.
type DiagnosticOptions struct {
	CheckUnclosedBlocks    bool
	CheckUnmatchedParens   bool
	CheckUndeclaredVars    bool
	CheckUnusedVars        bool
	CheckSQLParams         bool
	CheckHungarianNotation bool
	HungarianPrefixes      []string
	GlobalVariables        []string
	MaxBlockDepth          int
	IsDataSourceFile       bool
}

// DefaultDiagnosticOptions returns default diagnostic options.
func DefaultDiagnosticOptions() DiagnosticOptions {
	return DiagnosticOptions{
		CheckUnclosedBlocks:    true,
		CheckUnmatchedParens:   true,
		CheckUndeclaredVars:    false,
		CheckUnusedVars:        false,
		CheckSQLParams:         false,
		CheckHungarianNotation: false,
		HungarianPrefixes:      []string{"a", "b", "d", "fn", "n", "o", "s", "v"},
		MaxBlockDepth:          4,
	}
}

// GetDiagnostics returns all diagnostics for a document.
func GetDiagnostics(text string, opts DiagnosticOptions) []Diagnostic {
	lex := lexer.NewLexer(text)
	tokens := lex.Tokenize()
	p := parser.NewParser(tokens)
	ast := p.Parse()
	return collectDiagnostics(tokens, ast, p, opts)
}

// GetDiagnosticsFromTokens returns diagnostics using cached tokens/AST.
// Note: A parser instance is created even when AST is provided because
// ExtractVariables requires parser helper methods to traverse the AST.
// Parser creation is O(1) as it just stores a reference to the tokens.
func GetDiagnosticsFromTokens(tokens []lexer.Token, ast *parser.Node, opts DiagnosticOptions) []Diagnostic {
	if len(tokens) == 0 {
		return nil
	}

	p := parser.NewParser(tokens)
	if ast == nil {
		ast = p.Parse()
	}

	return collectDiagnostics(tokens, ast, p, opts)
}

func collectDiagnostics(tokens []lexer.Token, ast *parser.Node, p *parser.Parser, opts DiagnosticOptions) []Diagnostic {
	var diagnostics []Diagnostic

	// Check for lexer-level issues
	diagnostics = append(diagnostics, checkTokenErrors(tokens)...)
	diagnostics = append(diagnostics, checkCommentTermination(tokens)...)

	// Check for unmatched parentheses/brackets
	if opts.CheckUnmatchedParens {
		diagnostics = append(diagnostics, checkUnmatchedDelimiters(tokens)...)
	}

	// Check for unclosed blocks
	if opts.CheckUnclosedBlocks {
		diagnostics = append(diagnostics, checkUnclosedBlocks(tokens)...)
	}

	// Check block depth
	if opts.MaxBlockDepth > 0 {
		diagnostics = append(diagnostics, checkBlockDepth(ast, opts.MaxBlockDepth)...)
	}

	variables := p.ExtractVariables(ast)
	typeInfo := buildSimpleTypeInfo(tokens, variables)

	// Check for Hungarian notation (opt-in)
	if opts.CheckHungarianNotation {
		diagnostics = append(diagnostics, checkHungarianNotation(variables, opts.HungarianPrefixes)...)
	}

	// SSL language rule enforcement (always enabled)
	if opts.IsDataSourceFile {
		diagnostics = append(diagnostics, checkKeywordFormsDataSource(tokens)...)
		diagnostics = append(diagnostics, checkDataSourceDefaultUsage(tokens)...)
		diagnostics = append(diagnostics, checkDataSourceParameterDefaults(tokens)...)
	} else {
		diagnostics = append(diagnostics, checkKeywordForms(tokens)...)
		diagnostics = append(diagnostics, checkDefaultOnDeclareLine(tokens)...)
		diagnostics = append(diagnostics, checkParameterPlacement(tokens)...)
		diagnostics = append(diagnostics, checkDefaultPlacement(tokens)...)
	}
	diagnostics = append(diagnostics, checkMissingExitCase(tokens)...)
	diagnostics = append(diagnostics, checkMissingOtherwise(tokens)...)
	diagnostics = append(diagnostics, checkBareLogicalOperators(tokens)...)
	diagnostics = append(diagnostics, checkIncludePlacement(tokens)...)
	diagnostics = append(diagnostics, checkInlineCodeNaming(tokens)...)
	diagnostics = append(diagnostics, checkBeginCaseHasCase(tokens)...)
	diagnostics = append(diagnostics, checkTryStructure(tokens)...)
	diagnostics = append(diagnostics, checkErrorHandlerStructure(tokens)...)
	diagnostics = append(diagnostics, checkCatchClauseForm(tokens)...)
	diagnostics = append(diagnostics, checkForLoopNumericLiterals(tokens, typeInfo)...)
	diagnostics = append(diagnostics, checkLoopAndFinallyControl(tokens)...)
	diagnostics = append(diagnostics, checkDeprecatedKeywords(tokens)...)
	diagnostics = append(diagnostics, checkNotPreferredOperators(tokens)...)
	diagnostics = append(diagnostics, checkLiteralTypeSafety(tokens, typeInfo)...)
	diagnostics = append(diagnostics, checkEmptyOptionalParamArrays(tokens)...)
	diagnostics = append(diagnostics, checkPublicVariables(tokens)...)
	procedures := p.ExtractProcedures(ast)
	diagnostics = append(diagnostics, checkProcedureParameterCounts(procedures)...)
	diagnostics = append(diagnostics, checkNameLengths(variables, procedures, opts.HungarianPrefixes)...)
	diagnostics = append(diagnostics, checkRedeclaredVariables(tokens)...)
	diagnostics = append(diagnostics, checkNestedIIF(tokens)...)
	diagnostics = append(diagnostics, checkNegativeLogic(tokens)...)
	diagnostics = append(diagnostics, checkVisibilityAnnotations(tokens)...)
	diagnostics = append(diagnostics, checkNilMethodCalls(tokens)...)

	// Check for assignment to global variables.
	// Always runs to catch writes to built-in predefined globals (e.g. MYUSERNAME).
	// Also enforces user-configured globals when provided.
	diagnostics = append(diagnostics, checkGlobalAssignment(tokens, opts.GlobalVariables)...)

	// Check for undeclared variable usage (opt-in)
	if opts.CheckUndeclaredVars {
		diagnostics = append(diagnostics, checkUndeclaredVariables(tokens, ast, p, opts.GlobalVariables)...)
	}

	// Check for unused variable declarations (opt-in)
	if opts.CheckUnusedVars {
		diagnostics = append(diagnostics, checkUnusedVariables(tokens, ast, p)...)
	}

	// Check for SQL parameter validation (opt-in)
	if opts.CheckSQLParams {
		diagnostics = append(diagnostics, checkSQLParameterValidation(tokens, ast, p, opts.GlobalVariables)...)
	}

	// SSL gotcha detection (always enabled)
	diagnostics = append(diagnostics, checkAssignmentInCondition(tokens)...)
	diagnostics = append(diagnostics, checkDotPropertyAccess(tokens)...)
	diagnostics = append(diagnostics, checkClassInstantiationSyntax(tokens)...)
	diagnostics = append(diagnostics, checkCreateUdObjectBuiltinClassMisuse(tokens)...)
	diagnostics = append(diagnostics, checkZeroBasedArrayIndex(tokens)...)
	diagnostics = append(diagnostics, checkNamedSQLParamsWithWrongFunction(tokens)...)
	diagnostics = append(diagnostics, checkComplexSQLPlaceholders(tokens)...)
	diagnostics = append(diagnostics, checkDirectProcedureCalls(tokens, ast, p)...)
	diagnostics = append(diagnostics, checkMissingQuotesInExecFunction(tokens)...)
	diagnostics = append(diagnostics, checkBranchTargetLabels(tokens)...)
	diagnostics = append(diagnostics, checkClassContextRules(tokens, ast, p)...)
	diagnostics = append(diagnostics, checkClassReferenceForms(tokens)...)
	diagnostics = append(diagnostics, checkScientificNotation(tokens)...)
	diagnostics = append(diagnostics, checkStepSpacing(tokens)...)
	diagnostics = append(diagnostics, checkRegionLegacyWarning(tokens)...)
	diagnostics = append(diagnostics, checkCodeBlockStructure(tokens)...)
	diagnostics = append(diagnostics, checkSkippedParamSpacing(tokens)...)
	diagnostics = append(diagnostics, checkNotEqualsAsymmetry(tokens)...)
	diagnostics = append(diagnostics, checkSQLConcatenationInjection(tokens)...)

	return diagnostics
}

// checkKeywordForms enforces colon-prefixed uppercase keywords and flags unknown colon forms.
func checkKeywordForms(tokens []lexer.Token) []Diagnostic {
	var diagnostics []Diagnostic

	for _, token := range tokens {
		if token.Type != lexer.TokenKeyword {
			continue
		}

		text := token.Text
		normalized := strings.ToUpper(strings.TrimPrefix(text, ":"))

		if strings.HasPrefix(text, ":") {
			if isLegacyLabelKeywordForm(text) {
				if !strings.HasPrefix(text, ":LABEL") {
					diagnostics = append(diagnostics, Diagnostic{
						Severity: SeverityError,
						Range:    tokenToRange(token),
						Message:  "SSL label keyword forms are case-sensitive: use ':LABEL Name;' or ':LABELName;'",
						Source:   "ssl-lsp",
					})
				}
				continue
			}

			if !constants.IsKeyword(normalized) {
				// Special case: :ENDFOR is a recognized token but NOT usable — use :NEXT
				if normalized == "ENDFOR" {
					diagnostics = append(diagnostics, Diagnostic{
						Severity: SeverityError,
						Range:    tokenToRange(token),
						Message:  "':ENDFOR' is not valid — FOR loops must be terminated with ':NEXT'",
						Source:   "ssl-lsp",
					})
				} else {
					diagnostics = append(diagnostics, Diagnostic{
						Severity: SeverityWarning,
						Range:    tokenToRange(token),
						Message:  fmt.Sprintf("Unknown SSL keyword: '%s'", text),
						Source:   "ssl-lsp",
					})
				}
				continue
			}

			canonical := ":" + normalized
			if text != canonical {
				diagnostics = append(diagnostics, Diagnostic{
					Severity: SeverityError,
					Range:    tokenToRange(token),
					Message:  fmt.Sprintf("SSL keywords are case-sensitive and must be uppercase: use '%s'", canonical),
					Source:   "ssl-lsp",
				})
			}
			continue
		}

		if constants.IsKeyword(normalized) {
			diagnostics = append(diagnostics, Diagnostic{
				Severity: SeverityError,
				Range:    tokenToRange(token),
				Message:  fmt.Sprintf("SSL keywords must be colon-prefixed: use ':%s'", normalized),
				Source:   "ssl-lsp",
			})
		}
	}

	return diagnostics
}

func isLegacyLabelKeywordForm(text string) bool {
	if !strings.HasPrefix(text, ":") {
		return false
	}

	trimmed := strings.TrimPrefix(text, ":")
	return strings.HasPrefix(strings.ToUpper(trimmed), "LABEL") && len(trimmed) > len("LABEL")
}

// checkTokenErrors checks for token-level errors.
// Skips TokenUnknown that look like dot property access (handled by checkDotPropertyAccess).
func checkTokenErrors(tokens []lexer.Token) []Diagnostic {
	var diagnostics []Diagnostic

	for i, token := range tokens {
		if token.Type == lexer.TokenUnknown {
			// Skip dot property access patterns - they have their own diagnostic
			if strings.HasPrefix(token.Text, ".") && len(token.Text) > 1 {
				rest := token.Text[1:]
				// Check if at least the start of rest looks like an identifier
				if len(rest) > 0 && len(extractIdentifier(rest)) > 0 {
					// Check if preceded by identifier
					isPropAccess := false
					for j := i - 1; j >= 0; j-- {
						if tokens[j].Type == lexer.TokenWhitespace || tokens[j].Type == lexer.TokenComment {
							continue
						}
						if tokens[j].Type == lexer.TokenIdentifier {
							isPropAccess = true
						}
						break
					}
					if isPropAccess {
						continue // Skip - will be reported by checkDotPropertyAccess
					}
				}
			}

			diagnostics = append(diagnostics, Diagnostic{
				Severity: SeverityWarning,
				Range:    tokenToRange(token),
				Message:  fmt.Sprintf("Unknown token: '%s'", token.Text),
				Source:   "ssl-lsp",
			})
		}
	}

	return diagnostics
}

// checkCommentTermination detects block comments where a semicolon inside the
// text terminates the comment prematurely, causing the remaining text to become
// executable code. This is one of the most destructive errors in SSL — a single
// stray semicolon in a header comment can corrupt the entire file's parse.
func checkCommentTermination(tokens []lexer.Token) []Diagnostic {
	var diagnostics []Diagnostic

	for i, token := range tokens {
		if token.Type != lexer.TokenComment {
			continue
		}

		if !strings.HasSuffix(token.Text, ";") {
			diagnostics = append(diagnostics, Diagnostic{
				Severity: SeverityError,
				Range:    tokenToRange(token),
				Message:  "SSL comments must end with a semicolon ';'",
				Source:   "ssl-lsp",
			})
			continue
		}

		// Skip region marker comments — the semicolon after the region name
		// is intentional and does not indicate premature termination.
		trimmed := strings.TrimSpace(strings.TrimPrefix(token.Text, "/*"))
		trimmed = strings.TrimSpace(strings.TrimPrefix(trimmed, "//"))
		lower := strings.ToLower(trimmed)
		if strings.HasPrefix(lower, "region") || strings.HasPrefix(lower, "endregion") {
			continue
		}

		nextIdx := nextSignificantTokenIndex(tokens, i+1)
		if nextIdx < 0 {
			continue
		}

		nextToken := tokens[nextIdx]

		// Same-line continuation: the semicolon terminated the comment
		// before the line ended. The remaining text becomes executable code,
		// which may be intentional or may be hiding code accidentally.
		if nextToken.Line == token.Line {
			diagnostics = append(diagnostics, Diagnostic{
				Severity: SeverityWarning,
				Range:    tokenToRange(token),
				Message:  "Comment terminated early by semicolon. Text after the ';' becomes executable code and may be unintentionally hidden",
				Source:   "ssl-lsp",
			})
			continue
		}

		// Multi-line detection: if a /* comment spans multiple lines (contains
		// newlines in its token text) and the next token is a bare keyword
		// (without the required : prefix), the semicolon almost certainly
		// terminated the comment prematurely — normal code never has bare
		// keywords like "Parameters", "Default", "For", etc.
		if !strings.HasPrefix(token.Text, "/*") {
			continue
		}
		if !strings.Contains(token.Text, "\n") {
			continue
		}
		if nextToken.Type == lexer.TokenKeyword && !strings.HasPrefix(nextToken.Text, ":") {
			diagnostics = append(diagnostics, Diagnostic{
				Severity: SeverityError,
				Range:    tokenToRange(token),
				Message:  "Comment likely terminated early by semicolon. The text on the following lines may be intended as comment content but is being parsed as code. Rewrite the comment to avoid internal semicolons",
				Source:   "ssl-lsp",
			})
		}
	}

	return diagnostics
}

// checkAssignmentInCondition detects := assignment operator used in IF/WHILE/CASE conditions.
// This is usually a mistake - the developer likely meant = or == for comparison.
// Gotcha #9 in gotchas.md.
func checkAssignmentInCondition(tokens []lexer.Token) []Diagnostic {
	var diagnostics []Diagnostic

	// Track when we're inside a condition (between IF/WHILE/CASE and semicolon)
	inCondition := false
	var conditionKeyword *lexer.Token

	for i := range tokens {
		token := &tokens[i]

		// Skip whitespace and comments
		if token.Type == lexer.TokenWhitespace || token.Type == lexer.TokenComment {
			continue
		}

		// Detect condition-starting keywords
		if token.Type == lexer.TokenKeyword {
			normalized := strings.ToUpper(strings.TrimPrefix(token.Text, ":"))
			if normalized == "IF" || normalized == "WHILE" || normalized == "CASE" {
				inCondition = true
				conditionKeyword = token
				continue
			}
		}

		// End of condition
		if token.Type == lexer.TokenPunctuation && token.Text == ";" {
			inCondition = false
			conditionKeyword = nil
			continue
		}

		// Detect := in condition
		if inCondition && token.Type == lexer.TokenOperator && token.Text == ":=" {
			keywordName := "condition"
			if conditionKeyword != nil {
				keywordName = strings.ToUpper(strings.TrimPrefix(conditionKeyword.Text, ":"))
			}
			diagnostics = append(diagnostics, Diagnostic{
				Severity: SeverityWarning,
				Range:    tokenToRange(*token),
				Message:  fmt.Sprintf("Assignment ':=' used in %s condition - did you mean '=' or '=='?", keywordName),
				Source:   "ssl-lsp",
			})
		}
	}

	return diagnostics
}

// checkDotPropertyAccess detects identifier.identifier patterns that look like
// property access using dot notation (common in other languages).
// SSL uses colon notation: object:property instead of object.property.
// Gotcha #8 in gotchas.md.
func checkDotPropertyAccess(tokens []lexer.Token) []Diagnostic {
	var diagnostics []Diagnostic

	for i, token := range tokens {
		// Look for TokenUnknown that starts with a dot followed by identifier chars
		if token.Type != lexer.TokenUnknown {
			continue
		}

		// Check if it looks like .identifier
		if !strings.HasPrefix(token.Text, ".") {
			continue
		}

		rest := token.Text[1:]
		if len(rest) == 0 {
			continue
		}

		// Extract identifier portion from rest (may have trailing non-identifier chars like semicolons)
		propName := extractIdentifier(rest)
		if len(propName) == 0 {
			continue
		}

		// Look back to see if preceded by an identifier (skip whitespace)
		precedingIsIdent := false
		precedingIdentIdx := -1
		for j := i - 1; j >= 0; j-- {
			if tokens[j].Type == lexer.TokenWhitespace || tokens[j].Type == lexer.TokenComment {
				continue
			}
			if tokens[j].Type == lexer.TokenIdentifier {
				precedingIsIdent = true
				precedingIdentIdx = j
			}
			break
		}

		// Skip dots in :INCLUDE namespace paths (e.g. :INCLUDE File_Helpers.FileWork)
		if precedingIsIdent && precedingIdentIdx >= 0 {
			isInclude := false
			for j := precedingIdentIdx - 1; j >= 0; j-- {
				if tokens[j].Type == lexer.TokenWhitespace || tokens[j].Type == lexer.TokenComment {
					continue
				}
				if tokens[j].Type == lexer.TokenKeyword {
					kw := strings.ToUpper(strings.TrimPrefix(tokens[j].Text, ":"))
					if kw == "INCLUDE" {
						isInclude = true
					}
				}
				break
			}
			if isInclude {
				continue
			}
		}

		if precedingIsIdent {
			diagnostics = append(diagnostics, Diagnostic{
				Severity: SeverityError,
				Range:    tokenToRange(token),
				Message:  fmt.Sprintf("SSL uses colon ':' for property access, not dot '.'. Use 'object:%s' instead of 'object.%s'", propName, propName),
				Source:   "ssl-lsp",
			})
		}
	}

	return diagnostics
}

// isIdentifierPattern checks if a string looks like an identifier.
func isIdentifierPattern(s string) bool {
	if len(s) == 0 {
		return false
	}
	// First char must be letter or underscore
	first := rune(s[0])
	if !((first >= 'a' && first <= 'z') || (first >= 'A' && first <= 'Z') || first == '_') {
		return false
	}
	// Rest can be letter, digit, or underscore
	for _, ch := range s[1:] {
		if !((ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') || (ch >= '0' && ch <= '9') || ch == '_') {
			return false
		}
	}
	return true
}

// extractIdentifier extracts the identifier portion from the start of a string.
func extractIdentifier(s string) string {
	var result strings.Builder
	for i, ch := range s {
		if i == 0 {
			if (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') || ch == '_' {
				result.WriteRune(ch)
			} else {
				break
			}
		} else {
			if (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') || (ch >= '0' && ch <= '9') || ch == '_' {
				result.WriteRune(ch)
			} else {
				break
			}
		}
	}
	return result.String()
}

// checkClassInstantiationSyntax detects ClassName() patterns for SSL built-in classes.
// SSL uses curly braces for class instantiation: Email{}, SSLRegex{}, etc.
// Gotcha #15 in gotchas.md.
func checkClassInstantiationSyntax(tokens []lexer.Token) []Diagnostic {
	var diagnostics []Diagnostic

	// Build a case-insensitive set of class names
	classNames := make(map[string]string) // uppercase -> original
	for _, cls := range constants.SSLClassNames {
		classNames[strings.ToUpper(cls)] = cls
	}

	for i, token := range tokens {
		if token.Type != lexer.TokenIdentifier {
			continue
		}

		// Check if this identifier is a class name
		originalName, isClass := classNames[strings.ToUpper(token.Text)]
		if !isClass {
			continue
		}

		// Look ahead for '(' (skip whitespace)
		for j := i + 1; j < len(tokens); j++ {
			if tokens[j].Type == lexer.TokenWhitespace || tokens[j].Type == lexer.TokenComment {
				continue
			}
			if tokens[j].Type == lexer.TokenPunctuation && tokens[j].Text == "(" {
				diagnostics = append(diagnostics, Diagnostic{
					Severity: SeverityError,
					Range:    tokenToRange(token),
					Message:  fmt.Sprintf("SSL built-in class '%s' uses curly braces for instantiation: '%s{}' not '%s()'", originalName, originalName, originalName),
					Source:   "ssl-lsp",
				})
			}
			break
		}
	}

	return diagnostics
}

// checkCreateUdObjectBuiltinClassMisuse detects CreateUdObject("BuiltInClass")
// patterns. The source guide reserves CreateUdObject string dispatch for
// user-defined :CLASS objects; built-in classes must use curly braces.
func checkCreateUdObjectBuiltinClassMisuse(tokens []lexer.Token) []Diagnostic {
	var diagnostics []Diagnostic

	for i := 0; i < len(tokens); i++ {
		token := tokens[i]
		if token.Type != lexer.TokenIdentifier || !strings.EqualFold(token.Text, "CreateUdObject") {
			continue
		}

		openParenIdx := nextSignificantTokenIndex(tokens, i+1)
		if openParenIdx < 0 || tokens[openParenIdx].Type != lexer.TokenPunctuation || tokens[openParenIdx].Text != "(" {
			continue
		}

		argStarts, argEnds, closeIdx := parseTopLevelCallArguments(tokens, openParenIdx)
		if closeIdx < 0 || len(argStarts) == 0 || len(argEnds) == 0 {
			continue
		}

		argStart := argStarts[0]
		argEnd := argEnds[0]
		if argStart < 0 || argEnd != argStart || tokens[argStart].Type != lexer.TokenString {
			continue
		}

		className := strings.TrimSpace(unquoteSSLString(tokens[argStart].Text))
		if className == "" || !constants.IsSSLClass(className) {
			continue
		}

		diagnostics = append(diagnostics, Diagnostic{
			Severity: SeverityError,
			Range:    tokenToRange(tokens[argStart]),
			Message:  fmt.Sprintf("Built-in SSL class '%s' must use curly-brace construction ('%s{}'), not CreateUdObject(\"%s\")", className, className, className),
			Source:   "ssl-lsp",
		})
	}

	return diagnostics
}

// checkZeroBasedArrayIndex detects [0] array access patterns.
// SSL arrays are 1-based, so index 0 is invalid.
// Gotcha #5 in gotchas.md.
func checkZeroBasedArrayIndex(tokens []lexer.Token) []Diagnostic {
	var diagnostics []Diagnostic

	for i, token := range tokens {
		// Look for '[' punctuation
		if token.Type != lexer.TokenPunctuation || token.Text != "[" {
			continue
		}

		// Check if preceded by an identifier (array variable)
		hasPrecedingIdent := false
		for j := i - 1; j >= 0; j-- {
			if tokens[j].Type == lexer.TokenWhitespace || tokens[j].Type == lexer.TokenComment {
				continue
			}
			if tokens[j].Type == lexer.TokenIdentifier {
				hasPrecedingIdent = true
			}
			break
		}

		if !hasPrecedingIdent {
			continue
		}

		// Look ahead for pattern: 0 followed by ]
		foundZero := false
		var zeroToken *lexer.Token
		for j := i + 1; j < len(tokens); j++ {
			if tokens[j].Type == lexer.TokenWhitespace || tokens[j].Type == lexer.TokenComment {
				continue
			}
			if tokens[j].Type == lexer.TokenNumber && tokens[j].Text == "0" {
				foundZero = true
				zeroToken = &tokens[j]
				continue
			}
			if foundZero && tokens[j].Type == lexer.TokenPunctuation && tokens[j].Text == "]" {
				// Found [0] pattern
				diagnostics = append(diagnostics, Diagnostic{
					Severity: SeverityError,
					Range:    tokenToRange(*zeroToken),
					Message:  "SSL arrays are 1-based; index 0 is invalid. Use index 1 for the first element.",
					Source:   "ssl-lsp",
				})
			}
			break
		}
	}

	return diagnostics
}

// checkNamedSQLParamsWithWrongFunction detects ?varName? syntax used with
// functions that don't support named parameters.
// Per the SSL style guide, only SQLExecute supports ?varName? syntax.
// Gotcha #7 in gotchas.md.
func checkNamedSQLParamsWithWrongFunction(tokens []lexer.Token) []Diagnostic {
	var diagnostics []Diagnostic

	// Build set of functions that DON'T support named params
	parameterizedFuncs := make(map[string]bool)
	for _, fn := range constants.ParameterizedSQLFunctions {
		parameterizedFuncs[strings.ToUpper(fn)] = true
	}

	for i, token := range tokens {
		if token.Type != lexer.TokenIdentifier {
			continue
		}

		// Check if this is a parameterized SQL function
		if !parameterizedFuncs[strings.ToUpper(token.Text)] {
			continue
		}

		funcName := token.Text

		// Look ahead for '(' then find the first string argument
		inCall := false
		parenDepth := 0
		for j := i + 1; j < len(tokens); j++ {
			t := tokens[j]

			if t.Type == lexer.TokenWhitespace || t.Type == lexer.TokenComment {
				continue
			}

			if t.Type == lexer.TokenPunctuation && t.Text == "(" {
				if !inCall {
					inCall = true
				}
				parenDepth++
				continue
			}

			if t.Type == lexer.TokenPunctuation && t.Text == ")" {
				parenDepth--
				if parenDepth <= 0 {
					break
				}
				continue
			}

			// Found a string in the function call
			if inCall && parenDepth == 1 && t.Type == lexer.TokenString {
				// Check for named parameters in this string
				content := t.Text
				if len(content) >= 2 {
					content = content[1 : len(content)-1] // Remove quotes
				}
				placeholders := ParseSQLPlaceholders(content)
				for _, ph := range placeholders {
					if ph.IsNamed {
						diagnostics = append(diagnostics, Diagnostic{
							Severity: SeverityWarning,
							Range:    tokenToRange(t),
							Message:  fmt.Sprintf("Named SQL parameter '?%s?' not supported by '%s'. Use positional '?' with value array, or use 'SQLExecute' for named parameters.", ph.Name, funcName),
							Source:   "ssl-lsp",
						})
						break // One warning per string is enough
					}
				}
				break // Only check first string argument
			}
		}
	}

	return diagnostics
}

// checkComplexSQLPlaceholders warns when SQLExecute calls contain named placeholders
// with complex expressions (property access, array indexing, function calls).
// These are evaluated on every query execution and should be pre-computed into variables.
// Gotcha #20 in gotchas.md.
func checkComplexSQLPlaceholders(tokens []lexer.Token) []Diagnostic {
	var diagnostics []Diagnostic

	for i, token := range tokens {
		if token.Type != lexer.TokenIdentifier {
			continue
		}

		if !strings.EqualFold(token.Text, "SQLExecute") {
			continue
		}

		// Look ahead for '(' then find the first string argument
		inCall := false
		parenDepth := 0
		for j := i + 1; j < len(tokens); j++ {
			t := tokens[j]

			if t.Type == lexer.TokenWhitespace || t.Type == lexer.TokenComment {
				continue
			}

			if t.Type == lexer.TokenPunctuation && t.Text == "(" {
				if !inCall {
					inCall = true
				}
				parenDepth++
				continue
			}

			if t.Type == lexer.TokenPunctuation && t.Text == ")" {
				parenDepth--
				if parenDepth <= 0 {
					break
				}
				continue
			}

			// Found a string in the function call — check its placeholders
			if inCall && parenDepth == 1 && t.Type == lexer.TokenString {
				content := t.Text
				if len(content) >= 2 {
					content = content[1 : len(content)-1]
				}
				placeholders := ParseSQLPlaceholders(content)
				for _, ph := range placeholders {
					if ph.IsNamed && !isSimpleNamedPlaceholder(ph.Name) {
						paramColumn := t.Column + 1 + ph.Start
						diagnostics = append(diagnostics, Diagnostic{
							Severity: SeverityInfo,
							Range: Range{
								Start: Position{Line: t.Line - 1, Character: paramColumn - 1},
								End:   Position{Line: t.Line - 1, Character: paramColumn - 1 + len(ph.Name) + 2},
							},
							Message: fmt.Sprintf("Complex expression '?%s?' in SQLExecute placeholder is evaluated on every execution. Pre-compute into a variable for better performance.", ph.Name),
							Source:  "ssl-lsp",
						})
					}
				}
				break // Only check first string argument
			}
		}
	}

	return diagnostics
}

// checkDirectProcedureCalls detects attempts to call procedures directly.
// SSL requires DoProc("name", {params}) or ExecFunction("Module.name", {params}).
// Gotcha #1 in gotchas.md.
func checkDirectProcedureCalls(tokens []lexer.Token, ast *parser.Node, p *parser.Parser) []Diagnostic {
	var diagnostics []Diagnostic

	for i, token := range tokens {
		if token.Type != lexer.TokenIdentifier {
			continue
		}

		if constants.IsSSLFunction(token.Text) || constants.IsSSLClass(token.Text) {
			continue
		}

		previousIdx := previousSignificantTokenIndex(tokens, i-1)
		if previousIdx >= 0 {
			prev := tokens[previousIdx]
			if prev.Type == lexer.TokenPunctuation && prev.Text == ":" {
				continue
			}
		}

		upperName := strings.ToUpper(token.Text)
		if upperName == "DOPROC" || upperName == "EXECFUNCTION" || upperName == "EXECUDF" || upperName == "EVAL" {
			continue
		}

		for j := i + 1; j < len(tokens); j++ {
			if tokens[j].Type == lexer.TokenWhitespace || tokens[j].Type == lexer.TokenComment {
				continue
			}
			if tokens[j].Type == lexer.TokenPunctuation && tokens[j].Text == "(" {
				isDeclaration := false
				for k := i - 1; k >= 0; k-- {
					if tokens[k].Type == lexer.TokenWhitespace {
						if strings.Contains(tokens[k].Text, "\n") {
							break
						}
						continue
					}
					if tokens[k].Type == lexer.TokenKeyword {
						normalized := strings.ToUpper(strings.TrimPrefix(tokens[k].Text, ":"))
						if normalized == "PROCEDURE" {
							isDeclaration = true
						}
					}
					break
				}

				if !isDeclaration {
					diagnostics = append(diagnostics, Diagnostic{
						Severity: SeverityError,
						Range:    tokenToRange(token),
						Message:  fmt.Sprintf("Custom procedures cannot be called directly. Use DoProc(\"%s\", {args}) for same-file script procedures, ExecFunction(...) for external script procedures, or Me:/Base: inside classes.", token.Text),
						Source:   "ssl-lsp",
					})
				}
			}
			break
		}
	}

	return diagnostics
}

// checkMissingQuotesInExecFunction detects ExecFunction(Module.Proc, ...) patterns
// where the namespace path is not quoted.
// Related to Gotcha #8 (dot notation).
func checkMissingQuotesInExecFunction(tokens []lexer.Token) []Diagnostic {
	var diagnostics []Diagnostic

	for i, token := range tokens {
		if token.Type != lexer.TokenIdentifier {
			continue
		}

		// Check if this is ExecFunction or DoProc
		upper := strings.ToUpper(token.Text)
		if upper != "EXECFUNCTION" && upper != "DOPROC" {
			continue
		}

		funcName := token.Text

		// Look ahead for '(' then check first argument
		inCall := false
		for j := i + 1; j < len(tokens); j++ {
			t := tokens[j]

			if t.Type == lexer.TokenWhitespace || t.Type == lexer.TokenComment {
				continue
			}

			if t.Type == lexer.TokenPunctuation && t.Text == "(" {
				inCall = true
				continue
			}

			if inCall {
				// First non-whitespace token after '(' should be the first argument
				// If it's an identifier followed by TokenUnknown starting with '.', that's the error
				if t.Type == lexer.TokenIdentifier {
					// Look ahead for .identifier pattern (TokenUnknown)
					for k := j + 1; k < len(tokens); k++ {
						if tokens[k].Type == lexer.TokenWhitespace || tokens[k].Type == lexer.TokenComment {
							continue
						}
						if tokens[k].Type == lexer.TokenUnknown && strings.HasPrefix(tokens[k].Text, ".") {
							// Found identifier.something pattern without quotes
							diagnostics = append(diagnostics, Diagnostic{
								Severity: SeverityError,
								Range: Range{
									Start: Position{Line: t.Line - 1, Character: t.Column - 1},
									End:   Position{Line: tokens[k].Line - 1, Character: tokens[k].Column - 1 + len(tokens[k].Text)},
								},
								Message: fmt.Sprintf("Namespace path must be quoted: %s(\"Module.Procedure\", ...) not %s(Module.Procedure, ...)", funcName, funcName),
								Source:  "ssl-lsp",
							})
						}
						break
					}
				}
				break
			}
		}
	}

	return diagnostics
}

// checkClassContextRules validates class-specific constraints from the updated style guide.
func checkClassContextRules(tokens []lexer.Token, ast *parser.Node, p *parser.Parser) []Diagnostic {
	var diagnostics []Diagnostic

	var classTokens []lexer.Token
	for _, token := range tokens {
		if token.Type == lexer.TokenKeyword &&
			strings.ToUpper(strings.TrimPrefix(token.Text, ":")) == "CLASS" {
			classTokens = append(classTokens, token)
		}
	}

	if len(classTokens) == 0 {
		procedures := p.ExtractProcedures(ast)
		for _, proc := range procedures {
			if strings.EqualFold(proc.Name, "Constructor") {
				diagnostics = append(diagnostics, Diagnostic{
					Severity: SeverityWarning,
					Range: Range{
						Start: Position{Line: proc.StartLine - 1, Character: 0},
						End:   Position{Line: proc.StartLine - 1, Character: len(proc.Name)},
					},
					Message: "'Constructor' is only meaningful inside a ':CLASS' definition",
					Source:  "ssl-lsp",
				})
			}
		}
		return diagnostics
	}

	if len(classTokens) > 1 {
		for _, token := range classTokens[1:] {
			diagnostics = append(diagnostics, Diagnostic{
				Severity: SeverityError,
				Range:    tokenToRange(token),
				Message:  "Only one ':CLASS' definition is allowed per file",
				Source:   "ssl-lsp",
			})
		}
	}

	classStartLine := classTokens[0].Line
	if !isFirstSignificantStatementKeyword(tokens, "CLASS") {
		diagnostics = append(diagnostics, Diagnostic{
			Severity: SeverityError,
			Range:    tokenToRange(classTokens[0]),
			Message:  "A file is either a ':CLASS' definition or a script; ':CLASS' must be the first significant statement",
			Source:   "ssl-lsp",
		})
	}

	diagnostics = append(diagnostics, checkClassMemberOrder(tokens, classTokens[0])...)

	procedures := p.ExtractProcedures(ast)
	for _, proc := range procedures {
		isClassMethod := proc.StartLine > classStartLine
		if strings.EqualFold(proc.Name, "Constructor") && !isClassMethod {
			diagnostics = append(diagnostics, Diagnostic{
				Severity: SeverityWarning,
				Range: Range{
					Start: Position{Line: proc.StartLine - 1, Character: 0},
					End:   Position{Line: proc.StartLine - 1, Character: len(proc.Name)},
				},
				Message: "'Constructor' is only meaningful inside a ':CLASS' definition",
				Source:  "ssl-lsp",
			})
		}
	}

	classMethodRanges := make([]parser.ProcedureInfo, 0, len(procedures))
	for _, proc := range procedures {
		if proc.StartLine > classStartLine {
			classMethodRanges = append(classMethodRanges, proc)
		}
	}

	for i, token := range tokens {
		if token.Type != lexer.TokenIdentifier || !strings.EqualFold(token.Text, "DoProc") {
			continue
		}

		if !tokenInProcedureRange(token, classMethodRanges) {
			continue
		}

		for j := i + 1; j < len(tokens); j++ {
			if tokens[j].Type == lexer.TokenWhitespace || tokens[j].Type == lexer.TokenComment {
				continue
			}
			if tokens[j].Type == lexer.TokenPunctuation && tokens[j].Text == "(" {
				diagnostics = append(diagnostics, Diagnostic{
					Severity: SeverityError,
					Range:    tokenToRange(token),
					Message:  "DoProc is a compile-time error inside class methods — all forms are rejected. Use Me:MethodName() / Base:MethodName() instead.",
					Source:   "ssl-lsp",
				})
			}
			break
		}
	}

	for _, proc := range classMethodRanges {
		if !strings.EqualFold(proc.Name, "Constructor") {
			continue
		}

		for i := range tokens {
			token := tokens[i]
			if token.Line < proc.StartLine || token.Line > proc.EndLine {
				continue
			}
			if token.Type != lexer.TokenKeyword ||
				strings.ToUpper(strings.TrimPrefix(token.Text, ":")) != "RETURN" {
				continue
			}

			hasReturnValue := false
			for j := i + 1; j < len(tokens); j++ {
				next := tokens[j]
				if next.Line > proc.EndLine {
					break
				}
				if next.Type == lexer.TokenWhitespace || next.Type == lexer.TokenComment {
					continue
				}
				if next.Type == lexer.TokenPunctuation && next.Text == ";" {
					break
				}
				hasReturnValue = true
				break
			}

			if hasReturnValue {
				diagnostics = append(diagnostics, Diagnostic{
					Severity: SeverityError,
					Range:    tokenToRange(token),
					Message:  "':RETURN' inside a Constructor cannot return a value",
					Source:   "ssl-lsp",
				})
			}
		}
	}

	return diagnostics
}

// checkClassReferenceForms validates source-of-truth rules for Me and Base.
func checkClassReferenceForms(tokens []lexer.Token) []Diagnostic {
	var diagnostics []Diagnostic

	var classToken *lexer.Token
	hasInherit := false
	for i := range tokens {
		token := &tokens[i]
		if token.Type != lexer.TokenKeyword {
			continue
		}

		switch strings.ToUpper(strings.TrimPrefix(token.Text, ":")) {
		case "CLASS":
			if classToken == nil {
				classToken = token
			}
		case "INHERIT":
			if classToken != nil {
				hasInherit = true
			}
		}
	}

	for i := 0; i < len(tokens); i++ {
		token := tokens[i]
		if token.Type != lexer.TokenIdentifier || isDeclarationIdentifier(tokens, i) {
			continue
		}

		switch {
		case strings.EqualFold(token.Text, "Me"):
			if tokenInClassRange(token, classToken) {
				continue
			}
			diagnostics = append(diagnostics, Diagnostic{
				Severity: SeverityError,
				Range:    tokenToRange(token),
				Message:  "'Me' can only be used inside a ':CLASS' definition",
				Source:   "ssl-lsp",
			})

		case strings.EqualFold(token.Text, "Base"):
			nextIdx := nextSignificantTokenIndex(tokens, i+1)
			if nextIdx < 0 || tokens[nextIdx].Type != lexer.TokenPunctuation || tokens[nextIdx].Text != ":" {
				diagnostics = append(diagnostics, Diagnostic{
					Severity: SeverityError,
					Range:    tokenToRange(token),
					Message:  "'Base' must be used as 'Base:MemberName' and cannot stand alone",
					Source:   "ssl-lsp",
				})
				continue
			}

			if !tokenInClassRange(token, classToken) {
				diagnostics = append(diagnostics, Diagnostic{
					Severity: SeverityError,
					Range:    tokenToRange(token),
					Message:  "'Base:MemberName' can only be used inside a ':CLASS' definition",
					Source:   "ssl-lsp",
				})
				continue
			}

			if !hasInherit {
				diagnostics = append(diagnostics, Diagnostic{
					Severity: SeverityError,
					Range:    tokenToRange(token),
					Message:  "'Base:MemberName' requires ':INHERIT' in the current ':CLASS' definition",
					Source:   "ssl-lsp",
				})
			}
		}
	}

	return diagnostics
}

func checkClassMemberOrder(tokens []lexer.Token, classToken lexer.Token) []Diagnostic {
	var diagnostics []Diagnostic

	const orderMessage = "Class members must be ordered as ':INHERIT', ':DECLARE', regular methods, then 'Constructor'"

	const (
		classOrderInherit = 1
		classOrderDeclare = 2
		classOrderMethod  = 3
		classOrderCtor    = 4
	)

	seenClass := false
	startOfStatement := true
	inMethod := false
	maxOrder := 0

	for i := 0; i < len(tokens); i++ {
		token := tokens[i]

		if token.Type == lexer.TokenWhitespace {
			continue
		}
		if token.Type == lexer.TokenComment {
			startOfStatement = true
			continue
		}

		if !seenClass {
			if token.Line == classToken.Line && token.Column == classToken.Column && token.Text == classToken.Text {
				seenClass = true
			}
			if token.Type == lexer.TokenPunctuation && token.Text == ";" {
				startOfStatement = true
			}
			continue
		}

		if inMethod {
			if token.Type == lexer.TokenKeyword &&
				strings.ToUpper(strings.TrimPrefix(token.Text, ":")) == "ENDPROC" {
				inMethod = false
				startOfStatement = false
			}
			if token.Type == lexer.TokenPunctuation && token.Text == ";" {
				startOfStatement = true
			}
			continue
		}

		if !startOfStatement {
			if token.Type == lexer.TokenPunctuation && token.Text == ";" {
				startOfStatement = true
			}
			continue
		}

		startOfStatement = false
		if token.Type != lexer.TokenKeyword {
			continue
		}

		normalized := strings.ToUpper(strings.TrimPrefix(token.Text, ":"))
		order := 0

		switch normalized {
		case "INHERIT":
			order = classOrderInherit
		case "DECLARE":
			order = classOrderDeclare
		case "PROCEDURE":
			order = classOrderMethod
			nameIdx := nextSignificantTokenIndex(tokens, i+1)
			if nameIdx >= 0 && tokens[nameIdx].Type == lexer.TokenIdentifier && strings.EqualFold(tokens[nameIdx].Text, "Constructor") {
				order = classOrderCtor
			}
			inMethod = true
		default:
			continue
		}

		if order < maxOrder {
			diagnostics = append(diagnostics, Diagnostic{
				Severity: SeverityInfo,
				Range:    tokenToRange(token),
				Message:  orderMessage,
				Source:   "ssl-lsp",
			})
		}
		if order > maxOrder {
			maxOrder = order
		}
	}

	return diagnostics
}

// checkUnmatchedDelimiters checks for unmatched parentheses and brackets.
func checkUnmatchedDelimiters(tokens []lexer.Token) []Diagnostic {
	var diagnostics []Diagnostic

	type stackItem struct {
		char  string
		token lexer.Token
	}
	var stack []stackItem

	pairs := map[string]string{
		"(": ")",
		"[": "]",
		"{": "}",
	}

	closers := map[string]string{
		")": "(",
		"]": "[",
		"}": "{",
	}

	for _, token := range tokens {
		if token.Type == lexer.TokenPunctuation {
			if _, isOpener := pairs[token.Text]; isOpener {
				stack = append(stack, stackItem{char: token.Text, token: token})
			} else if expected, isCloser := closers[token.Text]; isCloser {
				if len(stack) == 0 {
					diagnostics = append(diagnostics, Diagnostic{
						Severity: SeverityError,
						Range:    tokenToRange(token),
						Message:  fmt.Sprintf("Unmatched '%s'", token.Text),
						Source:   "ssl-lsp",
					})
				} else if stack[len(stack)-1].char != expected {
					diagnostics = append(diagnostics, Diagnostic{
						Severity: SeverityError,
						Range:    tokenToRange(token),
						Message:  fmt.Sprintf("Expected '%s' but found '%s'", pairs[stack[len(stack)-1].char], token.Text),
						Source:   "ssl-lsp",
					})
					stack = stack[:len(stack)-1]
				} else {
					stack = stack[:len(stack)-1]
				}
			}
		}
	}

	// Report unclosed delimiters
	for _, item := range stack {
		diagnostics = append(diagnostics, Diagnostic{
			Severity: SeverityError,
			Range:    tokenToRange(item.token),
			Message:  fmt.Sprintf("Unclosed '%s'", item.char),
			Source:   "ssl-lsp",
		})
	}

	return diagnostics
}

// checkUnclosedBlocks checks for unclosed block statements.
func checkUnclosedBlocks(tokens []lexer.Token) []Diagnostic {
	var diagnostics []Diagnostic

	blockPairs := map[string][]string{
		"IF":              {"ENDIF"},
		"WHILE":           {"ENDWHILE"},
		"FOR":             {"NEXT"},
		"BEGINCASE":       {"ENDCASE"},
		"BEGININLINECODE": {"ENDINLINECODE"},
		"TRY":             {"ENDTRY"},
		"PROCEDURE":       {"ENDPROC"},
		"REGION":          {"ENDREGION"},
	}

	endToStart := make(map[string][]string)
	for start, ends := range blockPairs {
		for _, end := range ends {
			endToStart[end] = append(endToStart[end], start)
		}
	}

	type stackItem struct {
		keyword string
		token   lexer.Token
	}
	var stack []stackItem

	for _, token := range tokens {
		if token.Type == lexer.TokenKeyword {
			normalized := strings.ToUpper(strings.TrimPrefix(token.Text, ":"))

			if _, isStart := blockPairs[normalized]; isStart {
				stack = append(stack, stackItem{keyword: normalized, token: token})
			} else if validStarts, isEnd := endToStart[normalized]; isEnd {
				if len(stack) == 0 {
					diagnostics = append(diagnostics, Diagnostic{
						Severity: SeverityError,
						Range:    tokenToRange(token),
						Message:  fmt.Sprintf("Unexpected ':%s' without matching block start", normalized),
						Source:   "ssl-lsp",
					})
				} else {
					top := stack[len(stack)-1]
					if contains(validStarts, top.keyword) {
						stack = stack[:len(stack)-1]
					} else {
						// Try to find a matching opener further down the stack
						found := false
						for i := len(stack) - 1; i >= 0; i-- {
							if contains(validStarts, stack[i].keyword) {
								// Report missing closers for items above
								for j := len(stack) - 1; j > i; j-- {
									unclosed := stack[j]
									expectedEnd := blockPairs[unclosed.keyword][0]
									diagnostics = append(diagnostics, Diagnostic{
										Severity: SeverityError,
										Range:    tokenToRange(unclosed.token),
										Message:  fmt.Sprintf("Unclosed ':%s' - expected ':%s'", unclosed.keyword, expectedEnd),
										Source:   "ssl-lsp",
									})
								}
								stack = stack[:i]
								found = true
								break
							}
						}

						if !found {
							diagnostics = append(diagnostics, Diagnostic{
								Severity: SeverityError,
								Range:    tokenToRange(token),
								Message:  fmt.Sprintf("':%s' does not match ':%s'", normalized, top.keyword),
								Source:   "ssl-lsp",
							})
						}
					}
				}
			}
		}
	}

	// Report any remaining unclosed blocks
	for _, item := range stack {
		expectedEnd := blockPairs[item.keyword][0]
		diagnostics = append(diagnostics, Diagnostic{
			Severity: SeverityError,
			Range:    tokenToRange(item.token),
			Message:  fmt.Sprintf("Unclosed ':%s' - expected ':%s'", item.keyword, expectedEnd),
			Source:   "ssl-lsp",
		})
	}

	return diagnostics
}

// checkBlockDepth checks for excessive block nesting depth.
func checkBlockDepth(ast *parser.Node, maxDepth int) []Diagnostic {
	var diagnostics []Diagnostic

	var checkNode func(node *parser.Node, depth int)
	checkNode = func(node *parser.Node, depth int) {
		if node.Type == parser.NodeBlock && depth > maxDepth {
			// Guard against invalid line numbers
			line := node.StartLine - 1
			if line < 0 {
				line = 0
			}
			diagnostics = append(diagnostics, Diagnostic{
				Severity: SeverityWarning,
				Range: Range{
					Start: Position{Line: line, Character: 0},
					End:   Position{Line: line, Character: 0},
				},
				Message: fmt.Sprintf("Block nesting depth (%d) exceeds maximum (%d)", depth, maxDepth),
				Source:  "ssl-lsp",
			})
		}

		for _, child := range node.Children {
			newDepth := depth
			if child.Type == parser.NodeBlock {
				newDepth++
			}
			checkNode(child, newDepth)
		}
	}

	checkNode(ast, 0)
	return diagnostics
}

func checkHungarianNotation(variables []parser.VariableInfo, prefixes []string) []Diagnostic {
	var diagnostics []Diagnostic

	if len(prefixes) == 0 {
		return diagnostics
	}

	validPrefixes := strings.Join(prefixes, ", ")

	for _, variable := range variables {
		if isHungarianExemptName(variable.Name) {
			continue
		}
		if _, ok := hasHungarianPrefix(variable.Name, prefixes); ok {
			continue
		}

		diagnostics = append(diagnostics, Diagnostic{
			Severity: SeverityWarning,
			Range: Range{
				Start: Position{Line: variable.Line - 1, Character: variable.Column - 1},
				End:   Position{Line: variable.Line - 1, Character: variable.Column - 1 + len(variable.Name)},
			},
			Message: fmt.Sprintf("Variable '%s' should use a Hungarian notation prefix (%s)", variable.Name, validPrefixes),
			Source:  "ssl-lsp",
		})
	}

	return diagnostics
}

func hasHungarianPrefix(name string, prefixes []string) (string, bool) {
	trimmed := strings.TrimLeft(name, "_")
	if trimmed == "" {
		return "", false
	}

	lower := strings.ToLower(trimmed)
	for _, prefix := range prefixes {
		if !strings.HasPrefix(lower, prefix) {
			continue
		}

		remainder := trimmed[len(prefix):]
		remainder = strings.TrimLeft(remainder, "_")
		if remainder == "" {
			continue
		}
		firstRune := []rune(remainder)[0]
		if unicode.IsUpper(firstRune) {
			return prefix, true
		}
	}

	return "", false
}

func isHungarianExemptName(name string) bool {
	trimmed := strings.TrimLeft(name, "_")
	if trimmed == "" {
		return true
	}

	switch trimmed {
	case "i", "j", "k", "x", "y", "z":
		return true
	}

	if strings.ToUpper(trimmed) == trimmed && strings.Contains(trimmed, "_") {
		return true
	}

	return false
}

func tokenInProcedureRange(token lexer.Token, procedures []parser.ProcedureInfo) bool {
	for _, proc := range procedures {
		if token.Line >= proc.StartLine && token.Line <= proc.EndLine {
			return true
		}
	}
	return false
}

func tokenInClassRange(token lexer.Token, classToken *lexer.Token) bool {
	if classToken == nil {
		return false
	}
	return token.Line >= classToken.Line
}

func isDeclarationIdentifier(tokens []lexer.Token, idx int) bool {
	prevIdx := previousSignificantTokenIndex(tokens, idx-1)
	if prevIdx < 0 {
		return false
	}
	prev := tokens[prevIdx]
	if prev.Type != lexer.TokenKeyword {
		return false
	}

	switch strings.ToUpper(strings.TrimPrefix(prev.Text, ":")) {
	case "DECLARE", "PARAMETERS", "DEFAULT", "PUBLIC", "PROCEDURE", "CLASS", "INHERIT":
		return true
	default:
		return false
	}
}

func isFirstSignificantStatementKeyword(tokens []lexer.Token, keyword string) bool {
	startOfStatement := true

	for _, token := range tokens {
		if token.Type == lexer.TokenWhitespace {
			continue
		}

		if token.Type == lexer.TokenComment {
			if startOfStatement {
				continue
			}
			startOfStatement = true
			continue
		}

		if startOfStatement {
			if token.Type == lexer.TokenKeyword &&
				strings.ToUpper(strings.TrimPrefix(token.Text, ":")) == keyword {
				return true
			}
			return false
		}

		if token.Type == lexer.TokenPunctuation && token.Text == ";" {
			startOfStatement = true
		}
	}

	return false
}

// tokenToRange converts a token to an LSP range.
func tokenToRange(token lexer.Token) Range {
	return Range{
		Start: Position{
			Line:      token.Line - 1,
			Character: token.Column - 1,
		},
		End: Position{
			Line:      token.Line - 1,
			Character: token.Column - 1 + len(token.Text),
		},
	}
}

// contains checks if a string slice contains a value.
func contains(slice []string, val string) bool {
	for _, s := range slice {
		if s == val {
			return true
		}
	}
	return false
}

// checkMissingExitCase checks that every :CASE and :OTHERWISE block ends with :EXITCASE.
// The source guide recommends this unless multi-match CASE behavior is intentional.
func checkMissingExitCase(tokens []lexer.Token) []Diagnostic {
	var diagnostics []Diagnostic

	// Use a stack to handle nested BEGINCASE blocks correctly
	type caseState struct {
		currentCaseToken *lexer.Token
		hasExitCase      bool
	}
	var stack []caseState

	reportMissing := func(caseToken *lexer.Token) {
		if caseToken != nil {
			diagnostics = append(diagnostics, Diagnostic{
				Severity: SeverityWarning,
				Range:    tokenToRange(*caseToken),
				Message:  fmt.Sprintf("':%s' block should end with ':EXITCASE;'", strings.ToUpper(strings.TrimPrefix(caseToken.Text, ":"))),
				Source:   "ssl-lsp",
			})
		}
	}

	for i := range tokens {
		token := &tokens[i]
		if token.Type != lexer.TokenKeyword {
			continue
		}

		normalized := strings.ToUpper(strings.TrimPrefix(token.Text, ":"))

		switch normalized {
		case "BEGINCASE":
			stack = append(stack, caseState{})

		case "CASE", "OTHERWISE":
			if len(stack) > 0 {
				top := &stack[len(stack)-1]
				// If we had a previous CASE/OTHERWISE without EXITCASE, report it
				if !top.hasExitCase {
					reportMissing(top.currentCaseToken)
				}
				top.currentCaseToken = token
				top.hasExitCase = false
			}

		case "EXITCASE":
			if len(stack) > 0 {
				stack[len(stack)-1].hasExitCase = true
			}

		case "ENDCASE":
			if len(stack) > 0 {
				top := &stack[len(stack)-1]
				// Check the last CASE/OTHERWISE block
				if !top.hasExitCase {
					reportMissing(top.currentCaseToken)
				}
				stack = stack[:len(stack)-1]
			}
		}
	}

	return diagnostics
}

// checkMissingOtherwise warns when a :BEGINCASE block has no :OTHERWISE clause.
// Style guide recommends including :OTHERWISE for default handling (advisory).
func checkMissingOtherwise(tokens []lexer.Token) []Diagnostic {
	var diagnostics []Diagnostic

	var beginCaseToken *lexer.Token
	hasOtherwise := false
	depth := 0

	for i := range tokens {
		token := &tokens[i]
		if token.Type != lexer.TokenKeyword {
			continue
		}

		normalized := strings.ToUpper(strings.TrimPrefix(token.Text, ":"))

		switch normalized {
		case "BEGINCASE":
			if depth == 0 {
				beginCaseToken = token
				hasOtherwise = false
			}
			depth++
		case "OTHERWISE":
			if depth == 1 {
				hasOtherwise = true
			}
		case "ENDCASE":
			depth--
			if depth == 0 && beginCaseToken != nil && !hasOtherwise {
				diagnostics = append(diagnostics, Diagnostic{
					Severity: SeverityHint,
					Range:    tokenToRange(*beginCaseToken),
					Message:  "':BEGINCASE' has no ':OTHERWISE' clause; consider adding one for default handling",
					Source:   "ssl-lsp",
				})
			}
			if depth <= 0 {
				beginCaseToken = nil
				hasOtherwise = false
				depth = 0
			}
		}
	}

	return diagnostics
}

// checkBareLogicalOperators checks for AND, OR, NOT without enclosing periods.
// SSL requires .AND., .OR., .NOT. - bare operators are an error.
func checkBareLogicalOperators(tokens []lexer.Token) []Diagnostic {
	var diagnostics []Diagnostic

	// Bare logical operators that should be .AND., .OR., .NOT.
	bareOperators := map[string]string{
		"AND": ".AND.",
		"OR":  ".OR.",
		"NOT": ".NOT.",
	}

	for _, token := range tokens {
		// Only check identifiers - the lexer tokenizes bare AND/OR/NOT as identifiers
		if token.Type != lexer.TokenIdentifier {
			continue
		}

		upper := strings.ToUpper(token.Text)
		if correct, isBare := bareOperators[upper]; isBare {
			diagnostics = append(diagnostics, Diagnostic{
				Severity: SeverityError,
				Range:    tokenToRange(token),
				Message:  fmt.Sprintf("Use '%s' instead of '%s' for logical operations in SSL", correct, token.Text),
				Source:   "ssl-lsp",
			})
		}
	}

	return diagnostics
}

// checkIncludePlacement reports :INCLUDE directives that appear after other
// significant statements or inside procedure bodies.
// Recommended conventional order: :PARAMETERS, :DEFAULT, :INCLUDE, :PUBLIC, :DECLARE.
// :PARAMETERS and :DEFAULT are required to precede :INCLUDE, so they don't
// count as "non-include statements" for the late-placement warning.
func checkIncludePlacement(tokens []lexer.Token) []Diagnostic {
	var diagnostics []Diagnostic

	startOfStatement := true
	seenNonPreambleStatement := false
	procedureDepth := 0

	for _, token := range tokens {
		if token.Type == lexer.TokenWhitespace {
			continue
		}

		if token.Type == lexer.TokenComment {
			startOfStatement = true
			continue
		}

		// Track procedure nesting
		if token.Type == lexer.TokenKeyword {
			normalized := strings.ToUpper(strings.TrimPrefix(token.Text, ":"))
			if normalized == "PROCEDURE" {
				procedureDepth++
			} else if normalized == "ENDPROC" && procedureDepth > 0 {
				procedureDepth--
			}
		}

		if !startOfStatement {
			if token.Type == lexer.TokenPunctuation && token.Text == ";" {
				startOfStatement = true
			}
			continue
		}

		startOfStatement = false

		if token.Type == lexer.TokenKeyword {
			normalized := strings.ToUpper(strings.TrimPrefix(token.Text, ":"))

			if normalized == "INCLUDE" {
				if procedureDepth > 0 {
					diagnostics = append(diagnostics, Diagnostic{
						Severity: SeverityWarning,
						Range:    tokenToRange(token),
						Message:  "':INCLUDE' inside a procedure body is not supported",
						Source:   "ssl-lsp",
					})
				} else if seenNonPreambleStatement {
					diagnostics = append(diagnostics, Diagnostic{
						Severity: SeverityInfo,
						Range:    tokenToRange(token),
						Message:  "':INCLUDE' should appear early in the file. Recommended order: :PARAMETERS, :DEFAULT, :INCLUDE, :PUBLIC, :DECLARE",
						Source:   "ssl-lsp",
					})
				}
				continue
			}

			// :PARAMETERS and :DEFAULT are required to precede :INCLUDE,
			// so they don't trigger the late-placement warning.
			if normalized == "PARAMETERS" || normalized == "DEFAULT" {
				continue
			}
		}

		seenNonPreambleStatement = true
	}

	return diagnostics
}

// checkDefaultOnDeclareLine checks for :DEFAULT appearing on the same line as :DECLARE.
// Per ssl_agent_instructions.md (Gotcha #3), these must be separate statements.
func checkDefaultOnDeclareLine(tokens []lexer.Token) []Diagnostic {
	var diagnostics []Diagnostic

	// Track lines where :DECLARE appears
	declareLines := make(map[int]lexer.Token)

	for _, token := range tokens {
		if token.Type != lexer.TokenKeyword {
			continue
		}

		normalized := strings.ToUpper(strings.TrimPrefix(token.Text, ":"))

		if normalized == "DECLARE" {
			declareLines[token.Line] = token
		} else if normalized == "DEFAULT" {
			if declareToken, found := declareLines[token.Line]; found {
				diagnostics = append(diagnostics, Diagnostic{
					Severity: SeverityError,
					Range:    tokenToRange(declareToken),
					Message:  "':DEFAULT' cannot be used with ':DECLARE' - use ':PARAMETERS' with ':DEFAULT' instead",
					Source:   "ssl-lsp",
				})
			}
		}
	}

	return diagnostics
}

// checkParameterPlacement enforces that procedure-level :PARAMETERS statements
// appear immediately after :PROCEDURE and that script-level :PARAMETERS appears
// before top-level executable statements (leading procedures are allowed).
func checkParameterPlacement(tokens []lexer.Token) []Diagnostic {
	var diagnostics []Diagnostic

	startOfStatement := true
	procedureDepth := 0
	waitingForProcedureParameters := false
	seenProcedureBodyStatement := false
	seenTopLevelStatement := false

	for _, token := range tokens {
		if token.Type == lexer.TokenWhitespace {
			continue
		}

		if token.Type == lexer.TokenComment {
			// Comments are structurally transparent — they should NOT prevent
			// :PARAMETERS from being accepted after :PROCEDURE.
			startOfStatement = true
			continue
		}

		if !startOfStatement {
			if token.Type == lexer.TokenPunctuation && token.Text == ";" {
				startOfStatement = true
			}
			continue
		}

		startOfStatement = false

		if token.Type != lexer.TokenKeyword {
			if procedureDepth > 0 && waitingForProcedureParameters {
				seenProcedureBodyStatement = true
			} else if procedureDepth == 0 {
				seenTopLevelStatement = true
			}
			continue
		}

		normalized := strings.ToUpper(strings.TrimPrefix(token.Text, ":"))

		switch normalized {
		case "PROCEDURE":
			procedureDepth++
			waitingForProcedureParameters = true
			seenProcedureBodyStatement = false
		case "ENDPROC":
			if procedureDepth > 0 {
				procedureDepth--
			}
			if procedureDepth == 0 {
				waitingForProcedureParameters = false
				seenProcedureBodyStatement = false
			}
		case "PARAMETERS":
			if procedureDepth > 0 {
				if !waitingForProcedureParameters || seenProcedureBodyStatement {
					diagnostics = append(diagnostics, Diagnostic{
						Severity: SeverityError,
						Range:    tokenToRange(token),
						Message:  "':PARAMETERS' must appear immediately after ':PROCEDURE'",
						Source:   "ssl-lsp",
					})
				}
				waitingForProcedureParameters = false
				seenProcedureBodyStatement = false
			} else if seenTopLevelStatement {
				diagnostics = append(diagnostics, Diagnostic{
					Severity: SeverityError,
					Range:    tokenToRange(token),
					Message:  "Script-level ':PARAMETERS' must appear before top-level statements (leading ':PROCEDURE' blocks are allowed)",
					Source:   "ssl-lsp",
				})
			}
		default:
			if procedureDepth > 0 && waitingForProcedureParameters {
				seenProcedureBodyStatement = true
				waitingForProcedureParameters = false
			} else if procedureDepth == 0 {
				seenTopLevelStatement = true
			}
		}

		if token.Type == lexer.TokenPunctuation && token.Text == ";" {
			startOfStatement = true
		}
	}

	return diagnostics
}

// checkDefaultPlacement enforces that :DEFAULT statements immediately follow
// their corresponding :PARAMETERS statement with no intervening statements.
func checkDefaultPlacement(tokens []lexer.Token) []Diagnostic {
	var diagnostics []Diagnostic

	startOfStatement := true
	defaultsAllowed := false

	for i := 0; i < len(tokens); i++ {
		token := tokens[i]

		if token.Type == lexer.TokenWhitespace {
			continue
		}

		if token.Type == lexer.TokenComment {
			// Comments are structurally transparent — they should NOT break
			// the :PARAMETERS -> :DEFAULT sequence.
			startOfStatement = true
			continue
		}

		if startOfStatement {
			startOfStatement = false

			if token.Type == lexer.TokenKeyword {
				normalized := strings.ToUpper(strings.TrimPrefix(token.Text, ":"))
				switch normalized {
				case "PARAMETERS":
					defaultsAllowed = true
				case "DEFAULT":
					if !defaultsAllowed {
						diagnostics = append(diagnostics, Diagnostic{
							Severity: SeverityError,
							Range:    tokenToRange(token),
							Message:  "':DEFAULT' must appear immediately after ':PARAMETERS'",
							Source:   "ssl-lsp",
						})
					}
				default:
					defaultsAllowed = false
				}
			} else {
				defaultsAllowed = false
			}
		}

		if token.Type == lexer.TokenPunctuation && token.Text == ";" {
			startOfStatement = true
		}
	}

	return diagnostics
}

// checkInlineCodeNaming enforces the style-guide requirement that BEGININLINECODE
// blocks be named with either an identifier or a quoted string.
func checkInlineCodeNaming(tokens []lexer.Token) []Diagnostic {
	var diagnostics []Diagnostic

	for i := 0; i < len(tokens); i++ {
		token := tokens[i]
		if token.Type != lexer.TokenKeyword {
			continue
		}
		if strings.ToUpper(strings.TrimPrefix(token.Text, ":")) != "BEGININLINECODE" {
			continue
		}

		nextIdx := nextSignificantTokenIndex(tokens, i+1)
		if nextIdx < 0 {
			diagnostics = append(diagnostics, Diagnostic{
				Severity: SeverityError,
				Range:    tokenToRange(token),
				Message:  "':BEGININLINECODE' requires a name (identifier or quoted string)",
				Source:   "ssl-lsp",
			})
			continue
		}

		next := tokens[nextIdx]
		if next.Type == lexer.TokenPunctuation && next.Text == ";" {
			diagnostics = append(diagnostics, Diagnostic{
				Severity: SeverityError,
				Range:    tokenToRange(token),
				Message:  "':BEGININLINECODE' requires a name (identifier or quoted string)",
				Source:   "ssl-lsp",
			})
			continue
		}

		if next.Type != lexer.TokenIdentifier && next.Type != lexer.TokenString {
			diagnostics = append(diagnostics, Diagnostic{
				Severity: SeverityError,
				Range:    tokenToRange(next),
				Message:  "':BEGININLINECODE' name must be an identifier or quoted string",
				Source:   "ssl-lsp",
			})
		}
	}

	return diagnostics
}

// checkBeginCaseHasCase ensures each BEGINCASE contains at least one CASE block.
func checkBeginCaseHasCase(tokens []lexer.Token) []Diagnostic {
	var diagnostics []Diagnostic

	type caseState struct {
		token   lexer.Token
		sawCase bool
	}
	var stack []caseState

	for _, token := range tokens {
		if token.Type != lexer.TokenKeyword {
			continue
		}

		normalized := strings.ToUpper(strings.TrimPrefix(token.Text, ":"))
		switch normalized {
		case "BEGINCASE":
			stack = append(stack, caseState{token: token})
		case "CASE":
			if len(stack) > 0 {
				stack[len(stack)-1].sawCase = true
			}
		case "ENDCASE":
			if len(stack) == 0 {
				continue
			}
			state := stack[len(stack)-1]
			stack = stack[:len(stack)-1]
			if !state.sawCase {
				diagnostics = append(diagnostics, Diagnostic{
					Severity: SeverityError,
					Range:    tokenToRange(state.token),
					Message:  "':BEGINCASE' requires at least one ':CASE' block",
					Source:   "ssl-lsp",
				})
			}
		}
	}

	return diagnostics
}

// checkTryStructure enforces TRY/CATCH/FINALLY structure rules.
func checkTryStructure(tokens []lexer.Token) []Diagnostic {
	var diagnostics []Diagnostic

	type tryState struct {
		token             lexer.Token
		hasCatch          bool
		hasFinally        bool
		bodyHasStatements bool
		inFinally         bool
		finallyToken      lexer.Token
		finallyHasBody    bool
	}
	var stack []tryState

	markStatement := func() {
		if len(stack) == 0 {
			return
		}

		if stack[len(stack)-1].inFinally {
			stack[len(stack)-1].finallyHasBody = true
			return
		}

		stack[len(stack)-1].bodyHasStatements = true
	}

	for _, token := range tokens {
		if token.Type == lexer.TokenWhitespace || token.Type == lexer.TokenComment {
			continue
		}

		if token.Type != lexer.TokenKeyword {
			if token.Type != lexer.TokenPunctuation || token.Text != ";" {
				markStatement()
			}
			continue
		}

		normalized := strings.ToUpper(strings.TrimPrefix(token.Text, ":"))
		switch normalized {
		case "TRY":
			markStatement()
			stack = append(stack, tryState{token: token})
		case "CATCH":
			if len(stack) == 0 {
				continue
			}
			if !stack[len(stack)-1].bodyHasStatements {
				diagnostics = append(diagnostics, Diagnostic{
					Severity: SeverityError,
					Range:    tokenToRange(stack[len(stack)-1].token),
					Message:  "':TRY' requires at least one statement before ':CATCH' or ':FINALLY'",
					Source:   "ssl-lsp",
				})
			}
			if stack[len(stack)-1].hasFinally {
				diagnostics = append(diagnostics, Diagnostic{
					Severity: SeverityError,
					Range:    tokenToRange(token),
					Message:  "':CATCH' must appear before ':FINALLY' in a ':TRY' block",
					Source:   "ssl-lsp",
				})
				continue
			}
			if stack[len(stack)-1].hasCatch {
				diagnostics = append(diagnostics, Diagnostic{
					Severity: SeverityError,
					Range:    tokenToRange(token),
					Message:  "Only one ':CATCH' block is allowed per ':TRY'",
					Source:   "ssl-lsp",
				})
				continue
			}
			stack[len(stack)-1].hasCatch = true
			stack[len(stack)-1].inFinally = false
		case "FINALLY":
			if len(stack) == 0 {
				continue
			}
			if !stack[len(stack)-1].bodyHasStatements {
				diagnostics = append(diagnostics, Diagnostic{
					Severity: SeverityError,
					Range:    tokenToRange(stack[len(stack)-1].token),
					Message:  "':TRY' requires at least one statement before ':CATCH' or ':FINALLY'",
					Source:   "ssl-lsp",
				})
			}
			if stack[len(stack)-1].hasFinally {
				diagnostics = append(diagnostics, Diagnostic{
					Severity: SeverityError,
					Range:    tokenToRange(token),
					Message:  "Only one ':FINALLY' block is allowed per ':TRY'",
					Source:   "ssl-lsp",
				})
				continue
			}
			stack[len(stack)-1].hasFinally = true
			stack[len(stack)-1].inFinally = true
			stack[len(stack)-1].finallyToken = token
		case "ENDTRY":
			if len(stack) == 0 {
				continue
			}
			state := stack[len(stack)-1]
			stack = stack[:len(stack)-1]
			if !state.hasCatch && !state.hasFinally {
				diagnostics = append(diagnostics, Diagnostic{
					Severity: SeverityError,
					Range:    tokenToRange(state.token),
					Message:  "':TRY' requires at least one ':CATCH' or ':FINALLY' block",
					Source:   "ssl-lsp",
				})
			}
			if !state.bodyHasStatements && (state.hasCatch || state.hasFinally) {
				diagnostics = append(diagnostics, Diagnostic{
					Severity: SeverityError,
					Range:    tokenToRange(state.token),
					Message:  "':TRY' requires at least one statement before ':CATCH' or ':FINALLY'",
					Source:   "ssl-lsp",
				})
			}
			if state.hasFinally && !state.finallyHasBody {
				diagnostics = append(diagnostics, Diagnostic{
					Severity: SeverityError,
					Range:    tokenToRange(state.finallyToken),
					Message:  "':FINALLY' must contain at least one statement",
					Source:   "ssl-lsp",
				})
			}
		default:
			markStatement()
		}
	}

	return diagnostics
}

// checkErrorHandlerStructure enforces that :ERROR handlers contain at least one
// statement before :RESUME or the end of the current scope.
func checkErrorHandlerStructure(tokens []lexer.Token) []Diagnostic {
	var diagnostics []Diagnostic

	for i := 0; i < len(tokens); i++ {
		token := tokens[i]
		if token.Type != lexer.TokenKeyword || strings.ToUpper(strings.TrimPrefix(token.Text, ":")) != "ERROR" {
			continue
		}

		nextIdx := -1
		for j := i + 1; j < len(tokens); j++ {
			next := tokens[j]
			if next.Type == lexer.TokenWhitespace || next.Type == lexer.TokenComment {
				continue
			}
			if next.Type == lexer.TokenPunctuation && next.Text == ";" {
				continue
			}
			nextIdx = j
			break
		}
		if nextIdx < 0 {
			diagnostics = append(diagnostics, Diagnostic{
				Severity: SeverityError,
				Range:    tokenToRange(token),
				Message:  "':ERROR' must contain at least one statement before ':RESUME' or the end of the current scope",
				Source:   "ssl-lsp",
			})
			continue
		}

		next := tokens[nextIdx]
		if next.Type != lexer.TokenKeyword {
			continue
		}

		switch strings.ToUpper(strings.TrimPrefix(next.Text, ":")) {
		case "RESUME", "ENDPROC", "ENDTRY", "ENDWHILE", "NEXT", "ENDCASE", "ENDINLINECODE", "ENDREGION":
			diagnostics = append(diagnostics, Diagnostic{
				Severity: SeverityError,
				Range:    tokenToRange(token),
				Message:  "':ERROR' must contain at least one statement before ':RESUME' or the end of the current scope",
				Source:   "ssl-lsp",
			})
		}
	}

	return diagnostics
}

// checkCatchClauseForm enforces the source-of-truth rule that :CATCH does not
// take an exception variable or other clause content.
func checkCatchClauseForm(tokens []lexer.Token) []Diagnostic {
	var diagnostics []Diagnostic

	for i := 0; i < len(tokens); i++ {
		token := tokens[i]
		if token.Type != lexer.TokenKeyword || strings.ToUpper(strings.TrimPrefix(token.Text, ":")) != "CATCH" {
			continue
		}

		nextIdx := nextSignificantTokenIndex(tokens, i+1)
		if nextIdx < 0 {
			continue
		}

		next := tokens[nextIdx]
		if next.Type == lexer.TokenPunctuation && next.Text == ";" {
			continue
		}

		diagnostics = append(diagnostics, Diagnostic{
			Severity: SeverityError,
			Range:    tokenToRange(next),
			Message:  "':CATCH' does not take an exception variable; call 'GetLastSSLError()' inside the block instead",
			Source:   "ssl-lsp",
		})
	}

	return diagnostics
}

// checkForLoopNumericLiterals flags non-numeric :FOR values when their types can
// be inferred from local declarations, assignments, constructors, or known
// built-in function returns.
func checkForLoopNumericLiterals(tokens []lexer.Token, typeInfo map[string]string) []Diagnostic {
	var diagnostics []Diagnostic

	for i := 0; i < len(tokens); i++ {
		token := tokens[i]
		if token.Type != lexer.TokenKeyword || strings.ToUpper(strings.TrimPrefix(token.Text, ":")) != "FOR" {
			continue
		}

		stmtEnd := -1
		for j := i + 1; j < len(tokens); j++ {
			if tokens[j].Type == lexer.TokenPunctuation && tokens[j].Text == ";" {
				stmtEnd = j
				break
			}
		}
		if stmtEnd < 0 {
			continue
		}

		assignIdx := -1
		toIdx := -1
		stepIdx := -1

		for j := i + 1; j < stmtEnd; j++ {
			current := tokens[j]
			if current.Type == lexer.TokenWhitespace || current.Type == lexer.TokenComment {
				continue
			}

			if current.Type == lexer.TokenOperator && current.Text == ":=" && assignIdx < 0 {
				assignIdx = j
				continue
			}
			if current.Type == lexer.TokenKeyword {
				switch strings.ToUpper(strings.TrimPrefix(current.Text, ":")) {
				case "TO":
					toIdx = j
				case "STEP":
					stepIdx = j
				}
			}
		}

		loopVarIdx := nextSignificantTokenIndex(tokens, i+1)
		if loopVarIdx >= 0 && loopVarIdx < stmtEnd && tokens[loopVarIdx].Type == lexer.TokenIdentifier {
			if inferred := inferSimpleType(tokens, loopVarIdx, typeInfo); inferred != "" && inferred != "numeric" {
				diagnostics = append(diagnostics, Diagnostic{
					Severity: SeverityWarning,
					Range:    tokenToRange(tokens[loopVarIdx]),
					Message:  "':FOR' loop variable should be numeric",
					Source:   "ssl-lsp",
				})
			}
		}

		checkValue := func(idx int, role string, upperBound int) {
			if idx < 0 {
				return
			}
			valueIdx := nextSignificantTokenIndex(tokens, idx+1)
			if valueIdx < 0 || valueIdx >= stmtEnd || (upperBound >= 0 && valueIdx >= upperBound) {
				return
			}

			inferred := inferExpressionType(tokens, valueIdx, expressionEnd(tokens, valueIdx, upperBound, stmtEnd), typeInfo)
			if inferred == "" || inferred == "numeric" {
				return
			}

			diagnostics = append(diagnostics, Diagnostic{
				Severity: SeverityWarning,
				Range:    tokenToRange(tokens[valueIdx]),
				Message:  fmt.Sprintf("':FOR' %s value should be numeric", role),
				Source:   "ssl-lsp",
			})
		}

		checkValue(assignIdx, "start", toIdx)
		checkValue(toIdx, "limit", stepIdx)
		checkValue(stepIdx, "step", -1)
	}

	return diagnostics
}

// checkLoopAndFinallyControl validates loop control placement and FINALLY restrictions.
func checkLoopAndFinallyControl(tokens []lexer.Token) []Diagnostic {
	var diagnostics []Diagnostic

	type tryState struct {
		inFinally bool
	}
	var tryStack []tryState
	var loopStack []string

	inFinally := func() bool {
		for i := len(tryStack) - 1; i >= 0; i-- {
			if tryStack[i].inFinally {
				return true
			}
		}
		return false
	}

	hasLoop := func(kind string) bool {
		for i := len(loopStack) - 1; i >= 0; i-- {
			if kind == "" || loopStack[i] == kind {
				return true
			}
		}
		return false
	}

	popLoop := func(kind string) {
		for i := len(loopStack) - 1; i >= 0; i-- {
			if loopStack[i] == kind {
				loopStack = append(loopStack[:i], loopStack[i+1:]...)
				return
			}
		}
	}

	for _, token := range tokens {
		if token.Type != lexer.TokenKeyword {
			continue
		}

		normalized := strings.ToUpper(strings.TrimPrefix(token.Text, ":"))
		switch normalized {
		case "TRY":
			tryStack = append(tryStack, tryState{})
		case "CATCH":
			if len(tryStack) > 0 {
				tryStack[len(tryStack)-1].inFinally = false
			}
		case "FINALLY":
			if len(tryStack) > 0 {
				tryStack[len(tryStack)-1].inFinally = true
			}
		case "ENDTRY":
			if len(tryStack) > 0 {
				tryStack = tryStack[:len(tryStack)-1]
			}
		case "FOR":
			loopStack = append(loopStack, "FOR")
		case "WHILE":
			loopStack = append(loopStack, "WHILE")
		case "NEXT":
			popLoop("FOR")
		case "ENDWHILE":
			popLoop("WHILE")
		case "EXITFOR":
			if inFinally() {
				diagnostics = append(diagnostics, Diagnostic{
					Severity: SeverityError,
					Range:    tokenToRange(token),
					Message:  "':EXITFOR' inside a ':FINALLY' block is a compile-time error",
					Source:   "ssl-lsp",
				})
			}
			if !hasLoop("FOR") {
				diagnostics = append(diagnostics, Diagnostic{
					Severity: SeverityError,
					Range:    tokenToRange(token),
					Message:  "':EXITFOR' must be inside a ':FOR' loop",
					Source:   "ssl-lsp",
				})
			}
		case "EXITWHILE":
			if inFinally() {
				diagnostics = append(diagnostics, Diagnostic{
					Severity: SeverityError,
					Range:    tokenToRange(token),
					Message:  "':EXITWHILE' inside a ':FINALLY' block is a compile-time error",
					Source:   "ssl-lsp",
				})
			}
			if !hasLoop("WHILE") {
				diagnostics = append(diagnostics, Diagnostic{
					Severity: SeverityError,
					Range:    tokenToRange(token),
					Message:  "':EXITWHILE' must be inside a ':WHILE' loop",
					Source:   "ssl-lsp",
				})
			}
		case "LOOP":
			if inFinally() {
				diagnostics = append(diagnostics, Diagnostic{
					Severity: SeverityError,
					Range:    tokenToRange(token),
					Message:  "':LOOP' inside a ':FINALLY' block is a compile-time error",
					Source:   "ssl-lsp",
				})
			}
			if !hasLoop("") {
				diagnostics = append(diagnostics, Diagnostic{
					Severity: SeverityError,
					Range:    tokenToRange(token),
					Message:  "':LOOP' must be inside a ':WHILE' or ':FOR' loop",
					Source:   "ssl-lsp",
				})
			}
		case "RETURN":
			if inFinally() {
				diagnostics = append(diagnostics, Diagnostic{
					Severity: SeverityError,
					Range:    tokenToRange(token),
					Message:  "':RETURN' inside a ':FINALLY' block is a compile-time error",
					Source:   "ssl-lsp",
				})
			}
		}
	}

	return diagnostics
}

// checkDeprecatedKeywords reports legacy keywords that the style guide discourages in new code.
func checkDeprecatedKeywords(tokens []lexer.Token) []Diagnostic {
	var diagnostics []Diagnostic

	for _, token := range tokens {
		if token.Type != lexer.TokenKeyword {
			continue
		}

		normalized := strings.ToUpper(strings.TrimPrefix(token.Text, ":"))
		var message string
		switch {
		case normalized == "ERROR":
			message = "':ERROR' is legacy error handling. Prefer ':TRY' / ':CATCH' / ':FINALLY'"
		case normalized == "RESUME":
			message = "':RESUME' is legacy error handling. Prefer ':TRY' / ':CATCH' / ':FINALLY'"
		case normalized == "LABEL" || strings.HasPrefix(normalized, "LABEL"):
			message = "':LABEL' is legacy flow control used with Branch(); prefer structured control flow in new code"
		}

		if message != "" {
			diagnostics = append(diagnostics, Diagnostic{
				Severity: SeverityWarning,
				Range:    tokenToRange(token),
				Message:  message,
				Source:   "ssl-lsp",
			})
		}
	}

	return diagnostics
}

// checkNotPreferredOperators reports valid but discouraged operator forms.
func checkNotPreferredOperators(tokens []lexer.Token) []Diagnostic {
	var diagnostics []Diagnostic

	for _, token := range tokens {
		if token.Type != lexer.TokenOperator {
			continue
		}

		switch token.Text {
		case "#":
			diagnostics = append(diagnostics, Diagnostic{
				Severity: SeverityInfo,
				Range:    tokenToRange(token),
				Message:  "Use '!=' instead of '#' for inequality",
				Source:   "ssl-lsp",
			})
		case "<>":
			diagnostics = append(diagnostics, Diagnostic{
				Severity: SeverityInfo,
				Range:    tokenToRange(token),
				Message:  "Use '!=' instead of '<>' for inequality",
				Source:   "ssl-lsp",
			})
		}
	}

	return diagnostics
}

// checkScientificNotation detects numbers immediately followed by an identifier
// starting with 'e' or 'E', which suggests the user intended scientific notation
// but omitted the required decimal point (e.g., 7e2 should be 7.0e2).
func checkScientificNotation(tokens []lexer.Token) []Diagnostic {
	var diagnostics []Diagnostic

	for i := 0; i < len(tokens)-1; i++ {
		if tokens[i].Type != lexer.TokenNumber {
			continue
		}
		num := tokens[i].Text

		next := tokens[i+1]
		if next.Type != lexer.TokenIdentifier {
			continue
		}
		upper := strings.ToUpper(next.Text)

		// Case 1: number WITHOUT decimal followed by eN, e-N, e+N identifier
		// e.g., 7e2 -> tokens: "7" + "e2"; 1e-3 -> tokens: "1" + "e" + "-" + "3"
		if !strings.Contains(num, ".") {
			if len(upper) >= 2 && upper[0] == 'E' && (upper[1] >= '0' && upper[1] <= '9' || upper[1] == '+' || upper[1] == '-') {
				diagnostics = append(diagnostics, Diagnostic{
					Severity: SeverityWarning,
					Range:    tokenToRange(tokens[i]),
					Message:  fmt.Sprintf("SSL scientific notation requires a decimal point: use '%s.0%s' instead of '%s%s'", num, next.Text, num, next.Text),
					Source:   "ssl-lsp",
				})
			}
			// Case 1b: 9E+1 -> tokens: "9" + "E" (single char) + "+" + "1"
			if upper == "E" && i+2 < len(tokens) {
				afterE := tokens[i+2]
				if afterE.Type == lexer.TokenOperator && (afterE.Text == "+" || afterE.Text == "-") {
					diagnostics = append(diagnostics, Diagnostic{
						Severity: SeverityWarning,
						Range:    tokenToRange(tokens[i]),
						Message:  fmt.Sprintf("SSL scientific notation requires a decimal point: use '%s.0%s%s...' instead of '%s%s%s...'", num, next.Text, afterE.Text, num, next.Text, afterE.Text),
						Source:   "ssl-lsp",
					})
				}
			}
		}

		// Case 2: number WITH decimal but no digit before decimal (e.g., .5e1)
		// The lexer produces ".5" as a number token followed by "e1" identifier
		if strings.HasPrefix(num, ".") && upper[0] == 'E' {
			diagnostics = append(diagnostics, Diagnostic{
				Severity: SeverityWarning,
				Range:    tokenToRange(tokens[i]),
				Message:  fmt.Sprintf("SSL scientific notation requires a digit before the decimal point: use '0%s%s' instead of '%s%s'", num, next.Text, num, next.Text),
				Source:   "ssl-lsp",
			})
		}
	}

	return diagnostics
}

// checkLiteralTypeSafety reports type-safety gotchas from the style guide using
// conservative local type inference.
func checkLiteralTypeSafety(tokens []lexer.Token, typeInfo map[string]string) []Diagnostic {
	var diagnostics []Diagnostic

	for i := 0; i < len(tokens); i++ {
		token := tokens[i]
		if token.Type != lexer.TokenOperator {
			continue
		}

		prevIdx := previousSignificantTokenIndex(tokens, i-1)
		nextIdx := nextSignificantTokenIndex(tokens, i+1)
		if prevIdx < 0 || nextIdx < 0 {
			continue
		}

		left := tokens[prevIdx]
		right := tokens[nextIdx]

		switch token.Text {
		case "=":
			if isNilDefaultValueComparison(left, right) {
				diagnostics = append(diagnostics, Diagnostic{
					Severity: SeverityInfo,
					Range:    tokenToRange(token),
					Message:  "NIL is not the same as empty string, zero, or .F. Declared variables initialize to empty string, not NIL",
					Source:   "ssl-lsp",
				})
				continue
			}
			if inferSimpleType(tokens, prevIdx, typeInfo) == "codeblock" || inferSimpleType(tokens, nextIdx, typeInfo) == "codeblock" {
				diagnostics = append(diagnostics, Diagnostic{
					Severity: SeverityWarning,
					Range:    tokenToRange(token),
					Message:  "Code blocks (lambdas) cannot be compared with '=' or '=='. This causes an error",
					Source:   "ssl-lsp",
				})
				continue
			}
			if left.Type == lexer.TokenString || right.Type == lexer.TokenString {
				diagnostics = append(diagnostics, Diagnostic{
					Severity: SeverityInfo,
					Range:    tokenToRange(token),
					Message:  "For strings, '=' does prefix matching. Use '==' for exact string comparisons",
					Source:   "ssl-lsp",
				})
			}
		case "==":
			if isNilDefaultValueComparison(left, right) {
				diagnostics = append(diagnostics, Diagnostic{
					Severity: SeverityInfo,
					Range:    tokenToRange(token),
					Message:  "NIL is not the same as empty string, zero, or .F. Declared variables initialize to empty string, not NIL",
					Source:   "ssl-lsp",
				})
			}
			if inferSimpleType(tokens, prevIdx, typeInfo) == "codeblock" || inferSimpleType(tokens, nextIdx, typeInfo) == "codeblock" {
				diagnostics = append(diagnostics, Diagnostic{
					Severity: SeverityWarning,
					Range:    tokenToRange(token),
					Message:  "Code blocks (lambdas) cannot be compared with '=' or '=='. This causes an error",
					Source:   "ssl-lsp",
				})
			}
		case "$":
			leftType := inferSimpleType(tokens, prevIdx, typeInfo)
			rightType := inferSimpleType(tokens, nextIdx, typeInfo)
			if (leftType != "" && leftType != "string") || (rightType != "" && rightType != "string") {
				diagnostics = append(diagnostics, Diagnostic{
					Severity: SeverityWarning,
					Range:    tokenToRange(token),
					Message:  "The '$' containment operator only works on strings. Non-string operands cause error",
					Source:   "ssl-lsp",
				})
			}
		case "+", "-", "*", "/":
			if isNilLiteral(left) || isNilLiteral(right) {
				diagnostics = append(diagnostics, Diagnostic{
					Severity: SeverityWarning,
					Range:    tokenToRange(token),
					Message:  "Using NIL in arithmetic or string operations causes error. Use Empty() to check for NIL first",
					Source:   "ssl-lsp",
				})
			}
		}
	}

	return diagnostics
}

func isNilDefaultValueComparison(left, right lexer.Token) bool {
	return (isNilLiteral(left) && isDefaultValueLiteral(right)) || (isNilLiteral(right) && isDefaultValueLiteral(left))
}

func isNilLiteral(token lexer.Token) bool {
	if token.Type != lexer.TokenKeyword {
		return false
	}

	canonical, ok := constants.CanonicalSSLLiteral(token.Text)
	return ok && canonical == "NIL"
}

func isBooleanLiteral(token lexer.Token) bool {
	if token.Type != lexer.TokenKeyword {
		return false
	}

	canonical, ok := constants.CanonicalSSLLiteral(token.Text)
	return ok && (canonical == ".T." || canonical == ".F.")
}

func isEmptyStringLiteral(token lexer.Token) bool {
	if token.Type != lexer.TokenString || len(token.Text) < 2 {
		return false
	}

	switch {
	case strings.HasPrefix(token.Text, "\"") && strings.HasSuffix(token.Text, "\""):
		return len(token.Text) == 2
	case strings.HasPrefix(token.Text, "'") && strings.HasSuffix(token.Text, "'"):
		return len(token.Text) == 2
	case strings.HasPrefix(token.Text, "[") && strings.HasSuffix(token.Text, "]"):
		return len(token.Text) == 2
	default:
		return false
	}
}

func isZeroNumberLiteral(token lexer.Token) bool {
	return token.Type == lexer.TokenNumber && token.Text == "0"
}

func isFalseLiteral(token lexer.Token) bool {
	if token.Type != lexer.TokenKeyword {
		return false
	}

	canonical, ok := constants.CanonicalSSLLiteral(token.Text)
	return ok && canonical == ".F."
}

func isDefaultValueLiteral(token lexer.Token) bool {
	return isEmptyStringLiteral(token) || isZeroNumberLiteral(token) || isFalseLiteral(token)
}

func buildSimpleTypeInfo(tokens []lexer.Token, variables []parser.VariableInfo) map[string]string {
	typeInfo := make(map[string]string, len(variables))

	for _, variable := range variables {
		if inferred := inferTypeFromName(variable.Name); inferred != "" {
			typeInfo[strings.ToLower(variable.Name)] = inferred
		}
	}

	for i := 0; i < len(tokens); i++ {
		if tokens[i].Type != lexer.TokenIdentifier {
			continue
		}

		prevIdx := previousSignificantTokenIndex(tokens, i-1)
		if prevIdx >= 0 && tokens[prevIdx].Type == lexer.TokenPunctuation && tokens[prevIdx].Text == ":" {
			continue
		}

		assignIdx := nextSignificantTokenIndex(tokens, i+1)
		if assignIdx < 0 || tokens[assignIdx].Type != lexer.TokenOperator || tokens[assignIdx].Text != ":=" {
			continue
		}

		stmtEnd := -1
		for j := assignIdx + 1; j < len(tokens); j++ {
			if tokens[j].Type == lexer.TokenPunctuation && tokens[j].Text == ";" {
				stmtEnd = j
				break
			}
		}
		if stmtEnd < 0 {
			continue
		}

		exprStart := nextSignificantTokenIndex(tokens, assignIdx+1)
		if exprStart < 0 || exprStart >= stmtEnd {
			continue
		}

		exprEnd := expressionEnd(tokens, exprStart, -1, stmtEnd)
		inferred := inferExpressionType(tokens, exprStart, exprEnd, typeInfo)
		if inferred == "" || inferred == "nil" {
			continue
		}

		typeInfo[strings.ToLower(tokens[i].Text)] = inferred
	}

	return typeInfo
}

func inferTypeFromName(name string) string {
	switch {
	case isHungarianExemptName(name):
		return "numeric"
	case hasSpecificHungarianPrefix(name, "fn"):
		return "codeblock"
	case hasSpecificHungarianPrefix(name, "s"):
		return "string"
	case hasSpecificHungarianPrefix(name, "n"):
		return "numeric"
	case hasSpecificHungarianPrefix(name, "b"):
		return "boolean"
	case hasSpecificHungarianPrefix(name, "a"):
		return "array"
	case hasSpecificHungarianPrefix(name, "o"):
		return "object"
	case hasSpecificHungarianPrefix(name, "d"):
		return "date"
	default:
		return ""
	}
}

func hasSpecificHungarianPrefix(name string, prefix string) bool {
	_, ok := hasHungarianPrefix(name, []string{prefix})
	return ok
}

func inferExpressionType(tokens []lexer.Token, startIdx, endIdx int, typeInfo map[string]string) string {
	if startIdx < 0 || endIdx < startIdx || endIdx >= len(tokens) {
		return ""
	}

	startIdx = nextSignificantTokenIndex(tokens, startIdx)
	if startIdx < 0 || startIdx > endIdx {
		return ""
	}
	endIdx = previousSignificantTokenIndex(tokens, endIdx)
	if endIdx < startIdx {
		return ""
	}

	for i := startIdx; i <= endIdx; i++ {
		token := tokens[i]
		if token.Type == lexer.TokenWhitespace || token.Type == lexer.TokenComment {
			continue
		}
		if token.Type == lexer.TokenOperator {
			switch token.Text {
			case "=", "==", "!=", "<>", "<", ">", "<=", ">=", "$", ".AND.", ".OR.", ".NOT.":
				return "boolean"
			}
		}
	}

	for i := startIdx; i <= endIdx; i++ {
		token := tokens[i]
		if token.Type != lexer.TokenOperator {
			continue
		}

		switch token.Text {
		case "+", "-", "*", "/":
			leftType := inferExpressionType(tokens, startIdx, previousSignificantTokenIndex(tokens, i-1), typeInfo)
			rightType := inferExpressionType(tokens, nextSignificantTokenIndex(tokens, i+1), endIdx, typeInfo)
			if token.Text == "+" && (leftType == "string" || rightType == "string") {
				return "string"
			}
			return "numeric"
		}
	}

	if startIdx == endIdx {
		return inferSimpleType(tokens, startIdx, typeInfo)
	}

	startToken := tokens[startIdx]
	if startToken.Type == lexer.TokenPunctuation && startToken.Text == "{" {
		if isCodeBlockLiteralStart(tokens, startIdx) {
			return "codeblock"
		}
		return "array"
	}

	if startToken.Type == lexer.TokenIdentifier {
		nextIdx := nextSignificantTokenIndex(tokens, startIdx+1)
		if nextIdx >= 0 && nextIdx <= endIdx {
			if tokens[nextIdx].Type == lexer.TokenPunctuation && tokens[nextIdx].Text == "(" {
				return inferFunctionReturnType(startToken.Text)
			}
			if tokens[nextIdx].Type == lexer.TokenPunctuation && tokens[nextIdx].Text == "{" && constants.IsSSLClass(startToken.Text) {
				return "object"
			}
		}
	}

	return inferSimpleType(tokens, startIdx, typeInfo)
}

func inferFunctionReturnType(name string) string {
	sig, ok := constants.GetFunctionSignature(name)
	if !ok {
		return ""
	}

	switch strings.ToLower(sig.ReturnType) {
	case "string":
		return "string"
	case "double", "numeric", "number", "integer":
		return "numeric"
	case "boolean", "bool":
		return "boolean"
	case "array":
		return "array"
	case "date":
		return "date"
	case "object", "sslexpando", "ssldataset", "sslnetobject":
		return "object"
	case "sslfunction", "codeblock":
		return "codeblock"
	default:
		return ""
	}
}

func expressionEnd(tokens []lexer.Token, startIdx, upperBound, stmtEnd int) int {
	limit := stmtEnd
	if upperBound >= 0 && upperBound < limit {
		limit = upperBound
	}
	return previousSignificantTokenIndex(tokens, limit-1)
}

func inferSimpleType(tokens []lexer.Token, idx int, typeInfo map[string]string) string {
	if idx < 0 || idx >= len(tokens) {
		return ""
	}

	token := tokens[idx]

	switch token.Type {
	case lexer.TokenString:
		return "string"
	case lexer.TokenNumber:
		return "numeric"
	case lexer.TokenKeyword:
		if isNilLiteral(token) {
			return "nil"
		}
		if isBooleanLiteral(token) {
			return "boolean"
		}
	case lexer.TokenIdentifier:
		if inferred := typeInfo[strings.ToLower(token.Text)]; inferred != "" {
			return inferred
		}
		if inferred := inferTypeFromName(token.Text); inferred != "" {
			return inferred
		}

		nextIdx := nextSignificantTokenIndex(tokens, idx+1)
		if nextIdx >= 0 {
			if tokens[nextIdx].Type == lexer.TokenPunctuation && tokens[nextIdx].Text == "(" {
				return inferFunctionReturnType(token.Text)
			}
			if tokens[nextIdx].Type == lexer.TokenPunctuation && tokens[nextIdx].Text == "{" && constants.IsSSLClass(token.Text) {
				return "object"
			}
		}
	case lexer.TokenPunctuation:
		if token.Text == "{" && isCodeBlockLiteralStart(tokens, idx) {
			return "codeblock"
		}
		if token.Text == "{" {
			return "array"
		}
		if token.Text == "}" && isCodeBlockLiteralEnd(tokens, idx) {
			return "codeblock"
		}
	}

	return ""
}

func isCodeBlockLiteralStart(tokens []lexer.Token, idx int) bool {
	if idx < 0 || idx >= len(tokens) || tokens[idx].Type != lexer.TokenPunctuation || tokens[idx].Text != "{" {
		return false
	}

	nextIdx := nextSignificantTokenIndex(tokens, idx+1)
	return nextIdx >= 0 && tokens[nextIdx].Type == lexer.TokenUnknown && tokens[nextIdx].Text == "|"
}

func isCodeBlockLiteralEnd(tokens []lexer.Token, idx int) bool {
	if idx < 0 || idx >= len(tokens) || tokens[idx].Type != lexer.TokenPunctuation || tokens[idx].Text != "}" {
		return false
	}

	depth := 0
	for i := idx; i >= 0; i-- {
		token := tokens[i]
		if token.Type == lexer.TokenWhitespace || token.Type == lexer.TokenComment {
			continue
		}

		if token.Type == lexer.TokenPunctuation {
			switch token.Text {
			case "}":
				depth++
			case "{":
				depth--
				if depth == 0 {
					return isCodeBlockLiteralStart(tokens, i)
				}
			}
		}
	}

	return false
}

// checkEmptyOptionalParamArrays warns when callers pass {} for trailing optional array args
// that the style guide recommends omitting entirely.
func checkEmptyOptionalParamArrays(tokens []lexer.Token) []Diagnostic {
	var diagnostics []Diagnostic

	targets := map[string]int{
		"DOPROC":       2,
		"EXECFUNCTION": 2,
		"GETDATASET":   2,
	}

	for i := 0; i < len(tokens); i++ {
		token := tokens[i]
		if token.Type != lexer.TokenIdentifier {
			continue
		}

		targetArgIndex, ok := targets[strings.ToUpper(token.Text)]
		if !ok {
			continue
		}

		callStart := nextSignificantTokenIndex(tokens, i+1)
		if callStart < 0 || tokens[callStart].Type != lexer.TokenPunctuation || tokens[callStart].Text != "(" {
			continue
		}

		argStarts, argEnds, closeIdx := parseTopLevelCallArguments(tokens, callStart)
		if closeIdx < 0 || len(argStarts) < targetArgIndex {
			continue
		}

		argPos := targetArgIndex - 1
		if !isEmptyArrayLiteral(tokens, argStarts[argPos], argEnds[argPos]) {
			continue
		}

		hasLaterArgs := false
		for j := argPos + 1; j < len(argStarts); j++ {
			if argStarts[j] >= 0 && argEnds[j] >= argStarts[j] {
				hasLaterArgs = true
				break
			}
		}
		if hasLaterArgs {
			continue
		}

		diagnostics = append(diagnostics, Diagnostic{
			Severity: SeverityInfo,
			Range: Range{
				Start: Position{Line: tokens[argStarts[argPos]].Line - 1, Character: tokens[argStarts[argPos]].Column - 1},
				End:   Position{Line: tokens[argEnds[argPos]].Line - 1, Character: tokens[argEnds[argPos]].Column - 1 + len(tokens[argEnds[argPos]].Text)},
			},
			Message: fmt.Sprintf("Omit the trailing empty array for '%s' instead of passing '{}'", token.Text),
			Source:  "ssl-lsp",
		})
	}

	return diagnostics
}

// checkBranchTargetLabels validates literal Branch() targets against the source
// rule that label token text must include the word LABEL (for example
// "LABEL SKIP" or "LABELSKIP").
func checkBranchTargetLabels(tokens []lexer.Token) []Diagnostic {
	var diagnostics []Diagnostic

	for i := 0; i < len(tokens); i++ {
		token := tokens[i]
		if token.Type != lexer.TokenIdentifier || !strings.EqualFold(token.Text, "Branch") {
			continue
		}

		openParenIdx := nextSignificantTokenIndex(tokens, i+1)
		if openParenIdx < 0 || tokens[openParenIdx].Type != lexer.TokenPunctuation || tokens[openParenIdx].Text != "(" {
			continue
		}

		argStarts, argEnds, closeIdx := parseTopLevelCallArguments(tokens, openParenIdx)
		if closeIdx < 0 || len(argStarts) == 0 || len(argEnds) == 0 {
			continue
		}

		argStart := argStarts[0]
		argEnd := argEnds[0]
		if argStart < 0 || argEnd < argStart || tokens[argStart].Type != lexer.TokenString || argStart != argEnd {
			continue
		}

		target := strings.TrimSpace(unquoteSSLString(tokens[argStart].Text))
		if target == "" || strings.HasPrefix(strings.ToUpper(target), "LABEL") {
			continue
		}

		diagnostics = append(diagnostics, Diagnostic{
			Severity: SeverityError,
			Range:    tokenToRange(tokens[argStart]),
			Message:  "Branch target string must include the label token text, such as \"LABEL SKIP\" or \"LABELSKIP\"",
			Source:   "ssl-lsp",
		})
	}

	return diagnostics
}

func unquoteSSLString(text string) string {
	if len(text) >= 2 {
		switch {
		case text[0] == '"' && text[len(text)-1] == '"':
			return text[1 : len(text)-1]
		case text[0] == '\'' && text[len(text)-1] == '\'':
			return text[1 : len(text)-1]
		case text[0] == '[' && text[len(text)-1] == ']':
			return text[1 : len(text)-1]
		}
	}
	return text
}

// checkPublicVariables warns on :PUBLIC usage because the style guide treats it
// as risky shared state.
func checkPublicVariables(tokens []lexer.Token) []Diagnostic {
	var diagnostics []Diagnostic

	for _, token := range tokens {
		if token.Type != lexer.TokenKeyword {
			continue
		}
		if strings.ToUpper(strings.TrimPrefix(token.Text, ":")) != "PUBLIC" {
			continue
		}

		diagnostics = append(diagnostics, Diagnostic{
			Severity: SeverityWarning,
			Range:    tokenToRange(token),
			Message:  "':PUBLIC' variables persist across procedures and risk namespace pollution. Prefer ':DECLARE' with parameter passing",
			Source:   "ssl-lsp",
		})
	}

	return diagnostics
}

// checkProcedureParameterCounts warns when procedures exceed the parameter count
// threshold documented in the style guide.
func checkProcedureParameterCounts(procedures []parser.ProcedureInfo) []Diagnostic {
	var diagnostics []Diagnostic

	for _, proc := range procedures {
		count := len(proc.Parameters)
		if count > 20 {
			diagnostics = append(diagnostics, Diagnostic{
				Severity: SeverityWarning,
				Range: Range{
					Start: Position{Line: proc.StartLine - 1, Character: 0},
					End:   Position{Line: proc.StartLine - 1, Character: len(proc.Name)},
				},
				Message: fmt.Sprintf("Procedure '%s' has %d parameters; procedures with more than 20 parameters should be refactored", proc.Name, count),
				Source:  "ssl-lsp",
			})
		} else if count > 8 {
			diagnostics = append(diagnostics, Diagnostic{
				Severity: SeverityHint,
				Range: Range{
					Start: Position{Line: proc.StartLine - 1, Character: 0},
					End:   Position{Line: proc.StartLine - 1, Character: len(proc.Name)},
				},
				Message: fmt.Sprintf("Procedure '%s' has %d parameters; style guide recommends at most 8 per procedure", proc.Name, count),
				Source:  "ssl-lsp",
			})
		}
	}

	return diagnostics
}

// checkNameLengths warns when variable or procedure names exceed style guide limits.
// Style guide: variable names max 20 characters (excluding Hungarian prefix),
// procedure names max 30 characters.
func checkNameLengths(variables []parser.VariableInfo, procedures []parser.ProcedureInfo, prefixes []string) []Diagnostic {
	var diagnostics []Diagnostic

	for _, v := range variables {
		// Strip Hungarian prefix to get the effective name length
		effectiveName := v.Name
		trimmed := strings.TrimLeft(v.Name, "_")
		if trimmed != "" {
			lower := strings.ToLower(trimmed)
			for _, prefix := range prefixes {
				if strings.HasPrefix(lower, prefix) {
					rest := trimmed[len(prefix):]
					if len(rest) > 0 && unicode.IsUpper([]rune(rest)[0]) {
						effectiveName = rest
						break
					}
				}
			}
		}

		if len(effectiveName) > 20 {
			diagnostics = append(diagnostics, Diagnostic{
				Severity: SeverityInfo,
				Range: Range{
					Start: Position{Line: v.Line - 1, Character: v.Column - 1},
					End:   Position{Line: v.Line - 1, Character: v.Column - 1 + len(v.Name)},
				},
				Message: fmt.Sprintf("Variable name '%s' exceeds 20-character limit (effective length %d excluding prefix)", v.Name, len(effectiveName)),
				Source:  "ssl-lsp",
			})
		}
	}

	for _, proc := range procedures {
		if len(proc.Name) > 30 {
			diagnostics = append(diagnostics, Diagnostic{
				Severity: SeverityInfo,
				Range: Range{
					Start: Position{Line: proc.StartLine - 1, Character: 0},
					End:   Position{Line: proc.StartLine - 1, Character: len(proc.Name)},
				},
				Message: fmt.Sprintf("Procedure name '%s' exceeds 30-character limit (length %d)", proc.Name, len(proc.Name)),
				Source:  "ssl-lsp",
			})
		}
	}

	return diagnostics
}

// checkVisibilityAnnotations validates /*@private; and /*@protected; annotations.
// These annotations must appear on their own line before :PROCEDURE.
// Per the style guide, they have NO effect on class methods (only script procedures).
func checkVisibilityAnnotations(tokens []lexer.Token) []Diagnostic {
	var diagnostics []Diagnostic

	inClass := false

	for i, token := range tokens {
		if token.Type == lexer.TokenKeyword {
			normalized := strings.ToUpper(strings.TrimPrefix(token.Text, ":"))
			if normalized == "CLASS" {
				inClass = true
			}
		}

		if token.Type != lexer.TokenComment {
			continue
		}

		text := strings.TrimSpace(token.Text)
		// Check for visibility annotation pattern
		if !strings.HasPrefix(text, "/*@") {
			continue
		}

		// Extract the annotation
		content := strings.TrimSpace(strings.TrimSuffix(text[3:], ";"))
		lower := strings.ToLower(content)

		if lower != "private" && lower != "protected" {
			continue
		}

		// Valid annotation found - check context
		if inClass {
			diagnostics = append(diagnostics, Diagnostic{
				Severity: SeverityWarning,
				Range:    tokenToRange(token),
				Message:  fmt.Sprintf("Visibility annotation '/*@%s;' has no effect on class methods — class methods are always public/virtual", content),
				Source:   "ssl-lsp",
			})
			continue
		}

		// Check that it's followed by :PROCEDURE
		nextIdx := nextSignificantTokenIndex(tokens, i+1)
		if nextIdx >= 0 {
			nextToken := tokens[nextIdx]
			if nextToken.Type == lexer.TokenKeyword {
				normalized := strings.ToUpper(strings.TrimPrefix(nextToken.Text, ":"))
				if normalized != "PROCEDURE" {
					diagnostics = append(diagnostics, Diagnostic{
						Severity: SeverityWarning,
						Range:    tokenToRange(token),
						Message:  fmt.Sprintf("Visibility annotation '/*@%s;' should appear on its own line immediately before ':PROCEDURE'", content),
						Source:   "ssl-lsp",
					})
				}
			}
		}
	}

	return diagnostics
}

// checkNilMethodCalls detects patterns where methods are called on NIL values.
// Style guide: "Do NOT call instance methods on NIL (raises error)".
// This uses conservative analysis — only flags cases where a variable is
// compared to NIL or known to be NIL from assignment context.
func checkNilMethodCalls(tokens []lexer.Token) []Diagnostic {
	var diagnostics []Diagnostic

	// Track variables assigned NIL
	nilVars := make(map[string]bool)

	for i := 0; i < len(tokens); i++ {
		token := tokens[i]

		// Track NIL assignments: x := NIL;
		if token.Type == lexer.TokenOperator && token.Text == ":=" {
			prevIdx := previousSignificantTokenIndex(tokens, i-1)
			nextIdx := nextSignificantTokenIndex(tokens, i+1)
			if prevIdx >= 0 && nextIdx >= 0 {
				nextTok := tokens[nextIdx]
				isNilAssign := strings.EqualFold(nextTok.Text, "NIL") &&
					(nextTok.Type == lexer.TokenIdentifier || nextTok.Type == lexer.TokenKeyword)
				if tokens[prevIdx].Type == lexer.TokenIdentifier && isNilAssign {
					nilVars[strings.ToUpper(tokens[prevIdx].Text)] = true
				} else if tokens[prevIdx].Type == lexer.TokenIdentifier {
					// Any non-NIL assignment clears the flag
					delete(nilVars, strings.ToUpper(tokens[prevIdx].Text))
				}
			}
		}

		// Check for method calls on NIL literal: NIL:Method()
		isNilToken := strings.EqualFold(token.Text, "NIL") &&
			(token.Type == lexer.TokenIdentifier || token.Type == lexer.TokenKeyword)
		if isNilToken {
			nextIdx := nextSignificantTokenIndex(tokens, i+1)
			if nextIdx >= 0 {
				nextTok := tokens[nextIdx]
				// Pattern 1: NIL : Method (colon as punctuation)
				isMemberAccess := nextTok.Type == lexer.TokenPunctuation && nextTok.Text == ":"
				// Pattern 2: NIL:Method (colon consumed into keyword token like :ToString)
				if !isMemberAccess && nextTok.Type == lexer.TokenKeyword && strings.HasPrefix(nextTok.Text, ":") {
					normalized := strings.ToUpper(strings.TrimPrefix(nextTok.Text, ":"))
					if !constants.IsKeyword(normalized) {
						isMemberAccess = true
					}
				}
				if isMemberAccess {
					diagnostics = append(diagnostics, Diagnostic{
						Severity: SeverityWarning,
						Range:    tokenToRange(token),
						Message:  "Calling a method on NIL raises an error. Check for NIL before accessing members.",
						Source:   "ssl-lsp",
					})
				}
			}
		}

		// Check for method calls on variables known to be NIL
		if token.Type == lexer.TokenIdentifier && nilVars[strings.ToUpper(token.Text)] {
			nextIdx := nextSignificantTokenIndex(tokens, i+1)
			if nextIdx >= 0 && tokens[nextIdx].Type == lexer.TokenPunctuation && tokens[nextIdx].Text == ":" {
				// Check it's a member access, not assignment
				memberIdx := nextSignificantTokenIndex(tokens, nextIdx+1)
				if memberIdx >= 0 && tokens[memberIdx].Type == lexer.TokenIdentifier {
					diagnostics = append(diagnostics, Diagnostic{
						Severity: SeverityWarning,
						Range:    tokenToRange(token),
						Message:  fmt.Sprintf("Variable '%s' may be NIL at this point. Calling methods on NIL raises an error.", token.Text),
						Source:   "ssl-lsp",
					})
				}
			}
		}
	}

	return diagnostics
}

func nextSignificantTokenIndex(tokens []lexer.Token, start int) int {
	for i := start; i < len(tokens); i++ {
		if tokens[i].Type == lexer.TokenWhitespace || tokens[i].Type == lexer.TokenComment {
			continue
		}
		return i
	}
	return -1
}

func parseTopLevelCallArguments(tokens []lexer.Token, openParenIdx int) ([]int, []int, int) {
	var argStarts []int
	var argEnds []int

	parenDepth := 0
	bracketDepth := 0
	braceDepth := 0
	argStart := -1

	for i := openParenIdx; i < len(tokens); i++ {
		token := tokens[i]
		if token.Type == lexer.TokenComment || token.Type == lexer.TokenWhitespace {
			continue
		}

		if token.Type == lexer.TokenPunctuation {
			switch token.Text {
			case "(":
				parenDepth++
				if parenDepth == 1 {
					continue
				}
			case ")":
				if parenDepth == 1 {
					if argStart >= 0 {
						argStarts = append(argStarts, argStart)
						argEnds = append(argEnds, previousSignificantTokenIndex(tokens, i-1))
					}
					return argStarts, argEnds, i
				}
				parenDepth--
			case "[":
				bracketDepth++
			case "]":
				bracketDepth--
			case "{":
				braceDepth++
			case "}":
				braceDepth--
			case ",":
				if parenDepth == 1 && bracketDepth == 0 && braceDepth == 0 {
					if argStart >= 0 {
						argStarts = append(argStarts, argStart)
						argEnds = append(argEnds, previousSignificantTokenIndex(tokens, i-1))
						argStart = -1
					} else {
						argStarts = append(argStarts, -1)
						argEnds = append(argEnds, -1)
					}
					continue
				}
			}
		}

		if parenDepth == 1 && argStart < 0 {
			argStart = i
		}
	}

	return nil, nil, -1
}

func previousSignificantTokenIndex(tokens []lexer.Token, start int) int {
	for i := start; i >= 0; i-- {
		if tokens[i].Type == lexer.TokenWhitespace || tokens[i].Type == lexer.TokenComment {
			continue
		}
		return i
	}
	return -1
}

func isEmptyArrayLiteral(tokens []lexer.Token, startIdx, endIdx int) bool {
	if startIdx < 0 || endIdx < startIdx {
		return false
	}

	significant := make([]lexer.Token, 0, endIdx-startIdx+1)
	for i := startIdx; i <= endIdx; i++ {
		if tokens[i].Type == lexer.TokenWhitespace || tokens[i].Type == lexer.TokenComment {
			continue
		}
		significant = append(significant, tokens[i])
	}

	return len(significant) == 2 &&
		significant[0].Type == lexer.TokenPunctuation && significant[0].Text == "{" &&
		significant[1].Type == lexer.TokenPunctuation && significant[1].Text == "}"
}

// checkGlobalAssignment checks for assignment to global variables.
// Global variables are pre-declared and should not be assigned to.
// Always checks SSLPredefinedGlobals (e.g. MYUSERNAME); also checks user-configured globals.
func checkGlobalAssignment(tokens []lexer.Token, globals []string) []Diagnostic {
	var diagnostics []Diagnostic

	// Build a case-insensitive set of global variable names.
	// Always include built-in predefined globals (MYUSERNAME, etc.).
	globalSet := make(map[string]bool)
	for _, g := range constants.SSLPredefinedGlobals {
		globalSet[strings.ToUpper(g)] = true
	}
	for _, g := range globals {
		globalSet[strings.ToUpper(g)] = true
	}

	// Look for assignment patterns: identifier := value
	for i := 0; i < len(tokens); i++ {
		token := tokens[i]

		// Skip non-identifiers
		if token.Type != lexer.TokenIdentifier {
			continue
		}

		// Check if this identifier is a global
		if !globalSet[strings.ToUpper(token.Text)] {
			continue
		}

		// Look ahead for := assignment operator
		j := i + 1
		for j < len(tokens) && tokens[j].Type == lexer.TokenWhitespace {
			j++
		}

		if j < len(tokens) && tokens[j].Type == lexer.TokenOperator && tokens[j].Text == ":=" {
			diagnostics = append(diagnostics, Diagnostic{
				Severity: SeverityError,
				Range:    tokenToRange(token),
				Message:  fmt.Sprintf("Cannot assign to global variable '%s'", token.Text),
				Source:   "ssl-lsp",
			})
		}
	}

	return diagnostics
}

// checkUndeclaredVariables checks for usage of undeclared variables.
// This implements the logic specified in DIAGNOSTICS_SPECIFICATION.md Section 5.
// It addresses GitHub issues:
//   - Issue #55: Globals config should recognize variables as pre-declared
//   - Issue #56: :INCLUDE paths should be skipped from checking
//   - Issue #2: 'Me' should be recognized as a built-in identifier
//   - Issue #53: Function calls (identifier followed by '(') should be skipped
func checkUndeclaredVariables(tokens []lexer.Token, ast *parser.Node, p *parser.Parser, globals []string) []Diagnostic {
	var diagnostics []Diagnostic

	// Build set of declared variables from the AST
	declaredVars := make(map[string]bool)
	variables := p.ExtractVariables(ast)
	for _, v := range variables {
		declaredVars[strings.ToUpper(v.Name)] = true
	}

	// Add configured globals to declared variables (Issue #55)
	for _, g := range globals {
		declaredVars[strings.ToUpper(g)] = true
	}

	// Build set of built-in identifiers to skip
	builtins := buildBuiltinSet()

	// Track which undeclared variables we've already reported (once per scope)
	reported := make(map[string]bool)

	// Track if we're inside an :INCLUDE statement (Issue #56)
	inInclude := false

	// Process tokens
	for i := 0; i < len(tokens); i++ {
		token := tokens[i]

		// Skip whitespace and comments
		if token.Type == lexer.TokenWhitespace || token.Type == lexer.TokenComment {
			continue
		}

		// Detect :INCLUDE keyword and skip until semicolon (Issue #56)
		if token.Type == lexer.TokenKeyword {
			normalized := strings.ToUpper(strings.TrimPrefix(token.Text, ":"))
			if normalized == "INCLUDE" {
				inInclude = true
				continue
			}
			// Other keywords are not variables
			continue
		}

		// End of :INCLUDE statement
		if inInclude {
			if token.Type == lexer.TokenPunctuation && token.Text == ";" {
				inInclude = false
			}
			// Skip all tokens in :INCLUDE path
			continue
		}

		// Only check identifiers
		if token.Type != lexer.TokenIdentifier {
			continue
		}

		upperName := strings.ToUpper(token.Text)

		// Skip built-in identifiers (functions, classes, literals, operators)
		if builtins[upperName] {
			continue
		}

		// Skip 'Me' - class self-reference (Issue #2)
		if upperName == "ME" {
			continue
		}

		// Check if preceded by ':' (property access, e.g., object:property)
		if i > 0 {
			prevIdx := i - 1
			for prevIdx > 0 && tokens[prevIdx].Type == lexer.TokenWhitespace {
				prevIdx--
			}
			if prevIdx >= 0 && tokens[prevIdx].Type == lexer.TokenPunctuation && tokens[prevIdx].Text == ":" {
				continue
			}
		}

		// Check if followed by '(' (function call) (Issue #53)
		nextIdx := i + 1
		for nextIdx < len(tokens) && tokens[nextIdx].Type == lexer.TokenWhitespace {
			nextIdx++
		}
		if nextIdx < len(tokens) && tokens[nextIdx].Type == lexer.TokenPunctuation && tokens[nextIdx].Text == "(" {
			continue
		}

		// Check if on left side of ':=' (assignment target - this declares the variable)
		if nextIdx < len(tokens) && tokens[nextIdx].Type == lexer.TokenOperator && tokens[nextIdx].Text == ":=" {
			// This is a dynamic declaration, add to declared set
			declaredVars[upperName] = true
			continue
		}

		// Check if on a declaration line (DECLARE, PARAMETERS, PUBLIC)
		if isOnDeclarationLine(tokens, i) {
			continue
		}

		// Check if declared
		if declaredVars[upperName] {
			continue
		}

		// Report undeclared variable (once per name)
		if !reported[upperName] {
			reported[upperName] = true
			diagnostics = append(diagnostics, Diagnostic{
				Severity: SeverityWarning,
				Range:    tokenToRange(token),
				Message:  fmt.Sprintf("Variable '%s' is not declared", token.Text),
				Source:   "ssl-lsp",
			})
		}
	}

	return diagnostics
}

// buildBuiltinSet creates a case-insensitive set of all built-in identifiers.
func buildBuiltinSet() map[string]bool {
	builtins := make(map[string]bool)

	// Add all SSL function names
	for _, fn := range constants.SSLFunctionNames {
		builtins[strings.ToUpper(fn)] = true
	}

	// Add all SSL class names
	for _, cls := range constants.SSLClassNames {
		builtins[strings.ToUpper(cls)] = true
	}

	// Add SSL literals
	for _, lit := range constants.SSLLiterals {
		builtins[strings.ToUpper(lit)] = true
	}

	// Add SSL operators (the text form)
	for _, op := range constants.SSLLogicalOperators {
		builtins[strings.ToUpper(op)] = true
	}

	// Add predefined read-only globals (always recognized, never flagged as undeclared)
	for _, g := range constants.SSLPredefinedGlobals {
		builtins[strings.ToUpper(g)] = true
	}

	// Add special identifiers
	builtins["ME"] = true          // Class self-reference
	builtins["BASE"] = true        // Parent-class reference
	builtins["CONSTRUCTOR"] = true // Reserved constructor identifier
	builtins["NIL"] = true         // Null value

	return builtins
}

// isOnDeclarationLine checks if a token at position i is on a declaration line.
func isOnDeclarationLine(tokens []lexer.Token, pos int) bool {
	if pos < 0 || pos >= len(tokens) {
		return false
	}

	line := tokens[pos].Line

	// Search backward to find the first keyword on this line
	for i := pos - 1; i >= 0; i-- {
		if tokens[i].Line != line {
			break
		}
		if tokens[i].Type == lexer.TokenKeyword {
			normalized := strings.ToUpper(strings.TrimPrefix(tokens[i].Text, ":"))
			if normalized == "DECLARE" || normalized == "PARAMETERS" || normalized == "PUBLIC" || normalized == "PROCEDURE" {
				return true
			}
		}
	}

	// Also check forward in case the keyword comes after position
	for i := pos; i < len(tokens) && tokens[i].Line == line; i++ {
		if tokens[i].Type == lexer.TokenKeyword {
			normalized := strings.ToUpper(strings.TrimPrefix(tokens[i].Text, ":"))
			if normalized == "DECLARE" || normalized == "PARAMETERS" || normalized == "PUBLIC" || normalized == "PROCEDURE" {
				return true
			}
		}
	}

	return false
}

// checkUnusedVariables checks for declared variables that are never used.
func checkUnusedVariables(tokens []lexer.Token, ast *parser.Node, p *parser.Parser) []Diagnostic {
	var diagnostics []Diagnostic

	// Extract all declared variables
	variables := p.ExtractVariables(ast)
	if len(variables) == 0 {
		return diagnostics
	}

	// Extract procedures for scope awareness
	procedures := p.ExtractProcedures(ast)

	// Count usages for each declared variable
	for _, v := range variables {
		usageCount := countVariableUsages(tokens, v, procedures)

		if usageCount == 0 {
			diagnostics = append(diagnostics, Diagnostic{
				Severity: SeverityHint,
				Range: Range{
					Start: Position{Line: v.Line - 1, Character: v.Column - 1},
					End:   Position{Line: v.Line - 1, Character: v.Column - 1 + len(v.Name)},
				},
				Message: fmt.Sprintf("Variable '%s' is declared but never used", v.Name),
				Source:  "ssl-lsp",
			})
		}
	}

	return diagnostics
}

// countVariableUsages counts how many times a variable is used in the code.
// For local/parameter variables, only counts usages within the same procedure.
// Returns the number of usages (excluding the declaration itself).
func countVariableUsages(tokens []lexer.Token, v parser.VariableInfo, procedures []parser.ProcedureInfo) int {
	usageCount := 0
	varNameUpper := strings.ToUpper(v.Name)

	// Determine scope for local/parameter variables
	var scopeProc *parser.ProcedureInfo
	if v.Scope == parser.ScopeLocal || v.Scope == parser.ScopeParameter {
		// Find the procedure that contains this variable
		for i := range procedures {
			if v.Line >= procedures[i].StartLine && v.Line <= procedures[i].EndLine {
				scopeProc = &procedures[i]
				break
			}
		}
	}

	for _, token := range tokens {
		// Only check identifiers
		if token.Type != lexer.TokenIdentifier {
			continue
		}

		// Check if name matches (case-insensitive)
		if strings.ToUpper(token.Text) != varNameUpper {
			continue
		}

		// Skip if this is the declaration line and column
		if token.Line == v.Line && token.Column == v.Column {
			continue
		}

		// For scoped variables, only count usages within the procedure
		if scopeProc != nil {
			if token.Line < scopeProc.StartLine || token.Line > scopeProc.EndLine {
				continue
			}
		}

		// Check if this is a property access (preceded by ':')
		// We should count these as usages even though they're properties
		// Actually, if preceded by ':' it's accessing the property on an object,
		// not our variable, so we should skip these
		// But we need to find the preceding token...
		// For simplicity, we'll count all identifier matches as usages

		usageCount++
	}

	return usageCount
}

// checkSQLParameterValidation checks that SQL parameters (?param?) match declared variables.
// This validation ensures that named parameters in SQL strings reference variables
// that are actually declared in the current scope (case-insensitive).
func checkSQLParameterValidation(tokens []lexer.Token, ast *parser.Node, p *parser.Parser, globals []string) []Diagnostic {
	var diagnostics []Diagnostic

	// Build set of all declared variables (case-insensitive)
	declaredVars := make(map[string]bool)
	variables := p.ExtractVariables(ast)
	for _, v := range variables {
		declaredVars[strings.ToUpper(v.Name)] = true
	}

	// Add built-in predefined globals (MYUSERNAME, etc.)
	for _, g := range constants.SSLPredefinedGlobals {
		declaredVars[strings.ToUpper(g)] = true
	}

	// Add configured globals
	for _, g := range globals {
		declaredVars[strings.ToUpper(g)] = true
	}

	// Add procedure parameters to declared vars
	procedures := p.ExtractProcedures(ast)
	for _, proc := range procedures {
		for _, param := range proc.Parameters {
			declaredVars[strings.ToUpper(param)] = true
		}
	}

	// Track reported parameters to avoid duplicate warnings
	reported := make(map[string]map[int]bool) // paramName -> line -> reported

	// Scan all string tokens for SQL parameters
	for _, token := range tokens {
		if token.Type != lexer.TokenString {
			continue
		}

		// Extract string content (remove quotes)
		content := token.Text
		if len(content) < 2 {
			continue
		}
		content = content[1 : len(content)-1]

		// Parse SQL placeholders from the string
		placeholders := ParseSQLPlaceholders(content)

		for _, ph := range placeholders {
			// Only validate named parameters (skip complex expressions with operators)
			if !ph.IsNamed || !isSimpleNamedPlaceholder(ph.Name) {
				continue
			}

			// Extract base variable name from property/array/function access
			// e.g., oUser:ID -> oUser, aArr[1] -> aArr, Today() -> Today
			baseName := extractBaseVarName(ph.Name)
			paramUpper := strings.ToUpper(baseName)

			// Skip function calls (Today(), etc.) — they're not variables
			if strings.Contains(ph.Name, "(") {
				continue
			}

			// Initialize reported map for this parameter if needed
			if reported[paramUpper] == nil {
				reported[paramUpper] = make(map[int]bool)
			}

			// Skip if already reported on this line
			if reported[paramUpper][token.Line] {
				continue
			}

			// Check if the parameter matches a declared variable
			if !declaredVars[paramUpper] {
				reported[paramUpper][token.Line] = true

				// Calculate the position of the parameter within the string token
				// token.Column is 1-based, ph.Start is 0-based offset in content
				// +1 for the opening quote
				paramColumn := token.Column + 1 + ph.Start

				diagnostics = append(diagnostics, Diagnostic{
					Severity: SeverityWarning,
					Range: Range{
						Start: Position{Line: token.Line - 1, Character: paramColumn - 1},
						End:   Position{Line: token.Line - 1, Character: paramColumn - 1 + len(ph.Name) + 2}, // +2 for surrounding ?
					},
					Message: fmt.Sprintf("SQL parameter '%s' does not match any declared variable", ph.Name),
					Source:  "ssl-lsp",
				})
			}
		}
	}

	return diagnostics
}

// extractBaseVarName extracts the root variable name from a SQL placeholder.
// For example: "oUser:ID" -> "oUser", "aArr[1]" -> "aArr", "Today()" -> "Today".
func extractBaseVarName(name string) string {
	for i, ch := range name {
		if ch == ':' || ch == '[' || ch == '(' {
			return name[:i]
		}
	}
	return name
}

// checkRedeclaredVariables warns when the same variable is declared more than once
// in the same scope. Per the schema, re-declaring is silently ignored by the runtime
// but is almost always a mistake.
func checkRedeclaredVariables(tokens []lexer.Token) []Diagnostic {
	var diagnostics []Diagnostic

	// Track declared variables per scope (procedure or global)
	type scopeInfo struct {
		declared map[string]lexer.Token // variable name -> first declaration token
	}

	currentScope := &scopeInfo{declared: make(map[string]lexer.Token)}
	scopeStack := []*scopeInfo{currentScope}

	for i, token := range tokens {
		if token.Type == lexer.TokenKeyword {
			normalized := strings.ToUpper(strings.TrimPrefix(token.Text, ":"))
			if normalized == "PROCEDURE" {
				// New scope
				currentScope = &scopeInfo{declared: make(map[string]lexer.Token)}
				scopeStack = append(scopeStack, currentScope)
			} else if normalized == "ENDPROC" {
				// Pop scope
				if len(scopeStack) > 1 {
					scopeStack = scopeStack[:len(scopeStack)-1]
					currentScope = scopeStack[len(scopeStack)-1]
				}
			} else if normalized == "DECLARE" || normalized == "PARAMETERS" {
				// Collect the identifiers on this line until semicolon
				for j := i + 1; j < len(tokens); j++ {
					t := tokens[j]
					if t.Type == lexer.TokenPunctuation && t.Text == ";" {
						break
					}
					if t.Type == lexer.TokenIdentifier {
						upper := strings.ToUpper(t.Text)
						if firstDecl, exists := currentScope.declared[upper]; exists {
							diagnostics = append(diagnostics, Diagnostic{
								Severity: SeverityHint,
								Range:    tokenToRange(t),
								Message:  fmt.Sprintf("Variable '%s' is already declared (first declared at line %d). Re-declaration is silently ignored at runtime.", t.Text, firstDecl.Line),
								Source:   "ssl-lsp",
							})
						} else {
							currentScope.declared[upper] = t
						}
					}
				}
			}
		}
	}

	return diagnostics
}

// checkNestedIIF detects nested IIF() calls which reduce readability.
// Schema: no_nested_ternaries: true
func checkNestedIIF(tokens []lexer.Token) []Diagnostic {
	var diagnostics []Diagnostic

	for i, token := range tokens {
		if token.Type != lexer.TokenIdentifier {
			continue
		}
		if !strings.EqualFold(token.Text, "IIF") {
			continue
		}
		// Check this is a function call (followed by `(`)
		nextIdx := nextSignificantTokenIndex(tokens, i+1)
		if nextIdx < 0 || tokens[nextIdx].Text != "(" {
			continue
		}

		// Scan inside the IIF(...) for nested IIF calls
		parenDepth := 0
		for j := nextIdx; j < len(tokens); j++ {
			if tokens[j].Text == "(" {
				parenDepth++
			} else if tokens[j].Text == ")" {
				parenDepth--
				if parenDepth == 0 {
					break
				}
			}
			if parenDepth > 0 && tokens[j].Type == lexer.TokenIdentifier && strings.EqualFold(tokens[j].Text, "IIF") {
				// Check it's a call
				nIdx := nextSignificantTokenIndex(tokens, j+1)
				if nIdx >= 0 && tokens[nIdx].Text == "(" {
					diagnostics = append(diagnostics, Diagnostic{
						Severity: SeverityInfo,
						Range:    tokenToRange(tokens[j]),
						Message:  "Nested IIF() reduces readability. Consider using :BEGINCASE/:CASE or :IF/:ELSE instead.",
						Source:   "ssl-lsp",
					})
				}
			}
		}
	}

	return diagnostics
}

// checkNegativeLogic flags :IF blocks with negated conditions that have :ELSE blocks,
// suggesting the logic be inverted for readability.
// Schema: prefer_positive_logic: true
func checkNegativeLogic(tokens []lexer.Token) []Diagnostic {
	var diagnostics []Diagnostic

	for i, token := range tokens {
		if token.Type != lexer.TokenKeyword {
			continue
		}
		normalized := strings.ToUpper(strings.TrimPrefix(token.Text, ":"))
		if normalized != "IF" {
			continue
		}

		// Check if the condition starts with a negation
		nextIdx := nextSignificantTokenIndex(tokens, i+1)
		if nextIdx < 0 {
			continue
		}

		isNegated := false
		negToken := tokens[nextIdx]
		if negToken.Type == lexer.TokenOperator {
			upper := strings.ToUpper(negToken.Text)
			if upper == ".NOT." || upper == "!" {
				isNegated = true
			}
		}

		if !isNegated {
			continue
		}

		// Look for matching :ELSE — scan forward tracking IF/ENDIF depth
		depth := 1
		hasElse := false
		for j := nextIdx + 1; j < len(tokens); j++ {
			if tokens[j].Type != lexer.TokenKeyword {
				continue
			}
			kw := strings.ToUpper(strings.TrimPrefix(tokens[j].Text, ":"))
			if kw == "IF" {
				depth++
			} else if kw == "ENDIF" {
				depth--
				if depth == 0 {
					break
				}
			} else if kw == "ELSE" && depth == 1 {
				hasElse = true
				break
			}
		}

		if hasElse {
			diagnostics = append(diagnostics, Diagnostic{
				Severity: SeverityHint,
				Range:    tokenToRange(negToken),
				Message:  "Consider inverting this condition to use positive logic: swap the :IF and :ELSE branches and remove the negation.",
				Source:   "ssl-lsp",
			})
		}
	}

	return diagnostics
}

// checkStepSpacing warns when :STEP has no space before it in FOR loops.
// Source of truth: ssl_agent_instructions.md gotcha #16.
func checkStepSpacing(tokens []lexer.Token) []Diagnostic {
	var diagnostics []Diagnostic

	for i, token := range tokens {
		if token.Type != lexer.TokenKeyword {
			continue
		}
		normalized := strings.ToUpper(strings.TrimPrefix(token.Text, ":"))
		if normalized != "STEP" {
			continue
		}
		// Check the preceding token — it should be whitespace
		if i > 0 && tokens[i-1].Type != lexer.TokenWhitespace {
			diagnostics = append(diagnostics, Diagnostic{
				Severity: SeverityWarning,
				Range:    tokenToRange(token),
				Message:  "':STEP' should have a space before it: ':FOR i := 1 :TO 10 :STEP 2;'",
				Source:   "ssl-lsp",
			})
		}
	}

	return diagnostics
}

// checkRegionLegacyWarning warns when :REGION/:ENDREGION is used (legacy functional construct).
// Source of truth: ssl_agent_instructions.md gotcha #22.
func checkRegionLegacyWarning(tokens []lexer.Token) []Diagnostic {
	var diagnostics []Diagnostic

	for _, token := range tokens {
		if token.Type != lexer.TokenKeyword {
			continue
		}
		normalized := strings.ToUpper(strings.TrimPrefix(token.Text, ":"))
		if normalized == "REGION" || normalized == "ENDREGION" {
			diagnostics = append(diagnostics, Diagnostic{
				Severity: SeverityInfo,
				Range:    tokenToRange(token),
				Message:  fmt.Sprintf("':%s' is a legacy functional construct that captures body text for GetRegion(). For IDE code folding and grouping, prefer '/* region' / '/* endregion' comments instead.", normalized),
				Source:   "ssl-lsp",
			})
		}
	}

	return diagnostics
}

// checkCodeBlockStructure validates code block literals {|params| expr}.
// Source of truth: ssl_agent_instructions.md — code blocks require at least one bound variable.
func checkCodeBlockStructure(tokens []lexer.Token) []Diagnostic {
	var diagnostics []Diagnostic

	for _, token := range tokens {
		if token.Type != lexer.TokenCodeBlock {
			continue
		}
		text := token.Text
		// Check for empty parameter list: {|| expr} or {| | expr} (with whitespace)
		// Source of truth: ssl-ebnf-grammar.md — at least one parameter required between pipes.
		if len(text) >= 3 && text[0] == '{' && text[1] == '|' {
			// Find closing pipe and check if anything non-whitespace exists between pipes
			hasParam := false
			for ci := 2; ci < len(text); ci++ {
				if text[ci] == '|' {
					break
				}
				if text[ci] != ' ' && text[ci] != '\t' {
					hasParam = true
					break
				}
			}
			if !hasParam {
				diagnostics = append(diagnostics, Diagnostic{
					Severity: SeverityWarning,
					Range:    tokenToRange(token),
					Message:  "Code blocks require at least one bound variable between the pipes: {|x| expr}",
					Source:   "ssl-lsp",
				})
			}
		}
	}

	return diagnostics
}

// checkSkippedParamSpacing flags spaces between adjacent commas in skipped parameters.
// Source of truth: ssl-style-guide.schema.yaml parameter_skipping_style — {p1,,p3} valid, {p1, , p3} invalid.
func checkSkippedParamSpacing(tokens []lexer.Token) []Diagnostic {
	var diagnostics []Diagnostic

	for i := 0; i < len(tokens); i++ {
		if tokens[i].Text != "," {
			continue
		}
		// Look for pattern: comma, whitespace with spaces (no newline), comma
		if i+2 < len(tokens) &&
			tokens[i+1].Type == lexer.TokenWhitespace &&
			!strings.Contains(tokens[i+1].Text, "\n") &&
			tokens[i+2].Text == "," {
			diagnostics = append(diagnostics, Diagnostic{
				Severity: SeverityInfo,
				Range:    tokenToRange(tokens[i+1]),
				Message:  "Skipped parameters should use adjacent commas with no space: {a,,b} not {a, , b}",
				Source:   "ssl-lsp",
			})
		}
	}

	return diagnostics
}

// checkNotEqualsAsymmetry warns when != is used with string literals, since != negates == (exact),
// not = (prefix). This means = and != are NOT logical opposites for strings.
// Source of truth: ssl_agent_instructions.md gotcha #18.
func checkNotEqualsAsymmetry(tokens []lexer.Token) []Diagnostic {
	var diagnostics []Diagnostic

	for i := 0; i < len(tokens); i++ {
		token := tokens[i]
		if token.Type != lexer.TokenOperator || token.Text != "!=" {
			continue
		}

		prevIdx := previousSignificantTokenIndex(tokens, i-1)
		nextIdx := nextSignificantTokenIndex(tokens, i+1)
		if prevIdx < 0 || nextIdx < 0 {
			continue
		}

		left := tokens[prevIdx]
		right := tokens[nextIdx]

		if left.Type == lexer.TokenString || right.Type == lexer.TokenString {
			diagnostics = append(diagnostics, Diagnostic{
				Severity: SeverityInfo,
				Range:    tokenToRange(token),
				Message:  "'!=' negates '==' (exact match), not '=' (prefix match). For strings, '=' and '!=' are NOT logical opposites",
				Source:   "ssl-lsp",
			})
		}
	}

	return diagnostics
}

// checkKeywordFormsDataSource is the data-source variant of checkKeywordForms.
// It accepts builder directives (:DSN, :TABLENAME, :NULLASBLANK, :INVARIANTDATECOLUMNS)
// as valid colon-prefixed forms instead of flagging them as unknown keywords.
func checkKeywordFormsDataSource(tokens []lexer.Token) []Diagnostic {
	var diagnostics []Diagnostic

	for _, token := range tokens {
		if token.Type != lexer.TokenKeyword {
			continue
		}

		text := token.Text
		normalized := strings.ToUpper(strings.TrimPrefix(text, ":"))

		if strings.HasPrefix(text, ":") {
			if isLegacyLabelKeywordForm(text) {
				if !strings.HasPrefix(text, ":LABEL") {
					diagnostics = append(diagnostics, Diagnostic{
						Severity: SeverityError,
						Range:    tokenToRange(token),
						Message:  "SSL label keyword forms are case-sensitive: use ':LABEL Name;' or ':LABELName;'",
						Source:   "ssl-lsp",
					})
				}
				continue
			}

			// Builder directives are valid in data source files
			if constants.IsBuilderDirective(normalized) {
				canonical := ":" + normalized
				if text != canonical {
					diagnostics = append(diagnostics, Diagnostic{
						Severity: SeverityError,
						Range:    tokenToRange(token),
						Message:  fmt.Sprintf("Builder directives must be uppercase: use '%s'", canonical),
						Source:   "ssl-lsp",
					})
				}
				continue
			}

			if !constants.IsKeyword(normalized) {
				if normalized == "ENDFOR" {
					diagnostics = append(diagnostics, Diagnostic{
						Severity: SeverityError,
						Range:    tokenToRange(token),
						Message:  "':ENDFOR' is not valid — FOR loops must be terminated with ':NEXT'",
						Source:   "ssl-lsp",
					})
				} else {
					diagnostics = append(diagnostics, Diagnostic{
						Severity: SeverityWarning,
						Range:    tokenToRange(token),
						Message:  fmt.Sprintf("Unknown SSL keyword: '%s'", text),
						Source:   "ssl-lsp",
					})
				}
				continue
			}

			canonical := ":" + normalized
			if text != canonical {
				diagnostics = append(diagnostics, Diagnostic{
					Severity: SeverityError,
					Range:    tokenToRange(token),
					Message:  fmt.Sprintf("SSL keywords are case-sensitive and must be uppercase: use '%s'", canonical),
					Source:   "ssl-lsp",
				})
			}
			continue
		}

		if constants.IsKeyword(normalized) {
			diagnostics = append(diagnostics, Diagnostic{
				Severity: SeverityError,
				Range:    tokenToRange(token),
				Message:  fmt.Sprintf("SSL keywords must be colon-prefixed: use ':%s'", normalized),
				Source:   "ssl-lsp",
			})
		}
	}

	return diagnostics
}

// checkDataSourceDefaultUsage flags :DEFAULT statements in data source files.
// Data sources use inline := defaults in :PARAMETERS, not separate :DEFAULT statements.
func checkDataSourceDefaultUsage(tokens []lexer.Token) []Diagnostic {
	var diagnostics []Diagnostic

	for _, token := range tokens {
		if token.Type != lexer.TokenKeyword {
			continue
		}
		normalized := strings.ToUpper(strings.TrimPrefix(token.Text, ":"))
		if normalized == "DEFAULT" {
			diagnostics = append(diagnostics, Diagnostic{
				Severity: SeverityError,
				Range:    tokenToRange(token),
				Message:  "Data source files use inline ':=' defaults in ':PARAMETERS', not separate ':DEFAULT' statements",
				Source:   "ssl-lsp",
			})
		}
	}

	return diagnostics
}

// checkDataSourceParameterDefaults checks that every parameter in a data source
// :PARAMETERS declaration has an inline := default value.
// Expected syntax: :PARAMETERS p1 := val1, p2 := val2;
//
// The scanner uses a state machine: after finding a parameter name (identifier),
// it expects := followed by a default value. The default value is consumed
// (skipped until , or ;) so that identifiers within default values are not
// mistaken for parameter names.
func checkDataSourceParameterDefaults(tokens []lexer.Token) []Diagnostic {
	var diagnostics []Diagnostic

	for i := 0; i < len(tokens); i++ {
		token := tokens[i]
		if token.Type != lexer.TokenKeyword {
			continue
		}
		normalized := strings.ToUpper(strings.TrimPrefix(token.Text, ":"))
		if normalized != "PARAMETERS" {
			continue
		}

		// State machine scanning the :PARAMETERS statement.
		// expectParam: true when we expect the next identifier to be a parameter name.
		j := i + 1
		expectParam := true
		for j < len(tokens) {
			t := tokens[j]
			if t.Type == lexer.TokenPunctuation && t.Text == ";" {
				break
			}
			if t.Type == lexer.TokenWhitespace || t.Type == lexer.TokenComment {
				j++
				continue
			}

			if expectParam && t.Type == lexer.TokenIdentifier {
				// This is a parameter name. Look ahead for :=
				k := j + 1
				for k < len(tokens) && tokens[k].Type == lexer.TokenWhitespace {
					k++
				}
				if k >= len(tokens) || tokens[k].Type != lexer.TokenOperator || tokens[k].Text != ":=" {
					diagnostics = append(diagnostics, Diagnostic{
						Severity: SeverityError,
						Range:    tokenToRange(t),
						Message:  fmt.Sprintf("Data source parameter '%s' must have an inline ':=' default value", t.Text),
						Source:   "ssl-lsp",
					})
					// Skip to next comma or semicolon
					for j < len(tokens) && !(tokens[j].Type == lexer.TokenPunctuation && (tokens[j].Text == "," || tokens[j].Text == ";")) {
						j++
					}
				} else {
					// Skip past := and consume the default value until , or ;
					j = k + 1
					for j < len(tokens) && !(tokens[j].Type == lexer.TokenPunctuation && (tokens[j].Text == "," || tokens[j].Text == ";")) {
						j++
					}
				}
				expectParam = false
				continue
			}

			if t.Type == lexer.TokenPunctuation && t.Text == "," {
				expectParam = true
			}
			j++
		}
	}

	return diagnostics
}

// safeSQLBuilderFunctions lists functions that produce safe SQL fragments
// (e.g. properly escaped IN-clause value lists). Concatenating their return
// value into a SQL string is not an injection risk.
var safeSQLBuilderFunctions = map[string]bool{
	"BUILDSTRINGFORIN": true,
}

// isSafeSQLBuilderCall checks if the token at idx is the start of a call to a
// known-safe SQL builder function (e.g. BuildStringForIn(...)).
func isSafeSQLBuilderCall(tokens []lexer.Token, idx int) bool {
	if idx >= len(tokens) || tokens[idx].Type != lexer.TokenIdentifier {
		return false
	}
	if !safeSQLBuilderFunctions[strings.ToUpper(tokens[idx].Text)] {
		return false
	}
	// Verify it's actually a call — next significant token should be "("
	nextIdx := nextSignificantTokenIndex(tokens, idx+1)
	return nextIdx >= 0 && tokens[nextIdx].Text == "("
}

// checkSQLConcatenationInjection detects string concatenation in SQL function arguments,
// which may indicate SQL injection vulnerability.
// Source of truth: ssl-style-guide.schema.yaml lints.security.prevent_sql_injection.
func checkSQLConcatenationInjection(tokens []lexer.Token) []Diagnostic {
	var diagnostics []Diagnostic

	for i, token := range tokens {
		if token.Type != lexer.TokenIdentifier {
			continue
		}

		upper := strings.ToUpper(token.Text)
		if !constants.IsSQLFunction(upper) {
			continue
		}

		// Walk into the function call to find the first string argument
		parenDepth := 0
		inCall := false
		for j := i + 1; j < len(tokens); j++ {
			t := tokens[j]
			if t.Type == lexer.TokenWhitespace || t.Type == lexer.TokenComment {
				continue
			}

			if t.Text == "(" {
				if !inCall {
					inCall = true
				}
				parenDepth++
				continue
			}

			if t.Text == ")" {
				parenDepth--
				if parenDepth <= 0 {
					break
				}
				continue
			}

			// Check if first arg has concatenation with + operator
			if inCall && parenDepth == 1 {
				if t.Type == lexer.TokenString {
					nextIdx := nextSignificantTokenIndex(tokens, j+1)
					if nextIdx >= 0 && tokens[nextIdx].Text == "+" {
						// Skip if concatenating with a known-safe builder function
						afterPlusIdx := nextSignificantTokenIndex(tokens, nextIdx+1)
						if afterPlusIdx >= 0 && isSafeSQLBuilderCall(tokens, afterPlusIdx) {
							break
						}
						diagnostics = append(diagnostics, Diagnostic{
							Severity: SeverityWarning,
							Range:    tokenToRange(tokens[nextIdx]),
							Message:  fmt.Sprintf("String concatenation in '%s' argument may cause SQL injection. Use parameterized queries instead.", token.Text),
							Source:   "ssl-lsp",
						})
					}
					break
				}
				if t.Type == lexer.TokenIdentifier {
					nextIdx := nextSignificantTokenIndex(tokens, j+1)
					if nextIdx >= 0 && tokens[nextIdx].Text == "+" {
						afterPlusIdx := nextSignificantTokenIndex(tokens, nextIdx+1)
						if afterPlusIdx >= 0 && tokens[afterPlusIdx].Type == lexer.TokenString {
							diagnostics = append(diagnostics, Diagnostic{
								Severity: SeverityWarning,
								Range:    tokenToRange(tokens[nextIdx]),
								Message:  fmt.Sprintf("String concatenation in '%s' argument may cause SQL injection. Use parameterized queries instead.", token.Text),
								Source:   "ssl-lsp",
							})
						}
					}
					break
				}
				break
			}
		}
	}

	return diagnostics
}
