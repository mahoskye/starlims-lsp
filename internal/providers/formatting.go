// Package providers implements LSP feature providers for SSL.
package providers

import (
	"strings"

	"starlims-lsp/internal/constants"
	"starlims-lsp/internal/lexer"
)

// FormattingOptions configures document formatting.
type FormattingOptions struct {
	IndentStyle            string // "tab" or "space"
	IndentSize             int    // spaces per indent level (when using spaces)
	MaxLineLength          int    // max line length (0 = unlimited)
	OperatorSpacing        bool   // space around operators
	CommaSpacing           bool   // space after commas
	SemicolonEnforcement   bool   // ensure statements end with semicolon
	BlankLinesBetweenProcs int    // blank lines between procedures
	// TrimTrailingWhitespace removes trailing space/tab characters from
	// every formatted line. Default true.
	TrimTrailingWhitespace bool
	// MaxConsecutiveBlankLines collapses runs of blank lines longer than
	// this threshold. 0 disables the cap.
	MaxConsecutiveBlankLines int
	// BuiltinFunctionCase controls casing of built-in function names.
	// "preserve" (default) keeps the user's casing; "PascalCase" rewrites
	// each call site to the canonical inventory casing.
	BuiltinFunctionCase string
	SQL                 SQLFormattingOptions
}

// DefaultFormattingOptions returns default formatting options.
func DefaultFormattingOptions() FormattingOptions {
	return FormattingOptions{
		IndentStyle:              "tab",
		IndentSize:               4,
		MaxLineLength:            90,
		OperatorSpacing:          true,
		CommaSpacing:             true,
		SemicolonEnforcement:     true,
		BlankLinesBetweenProcs:   1,
		TrimTrailingWhitespace:   true,
		MaxConsecutiveBlankLines: 0,
		BuiltinFunctionCase:      "preserve",
		SQL:                      DefaultSQLFormattingOptions(),
	}
}

// TextEdit represents a text edit to apply to a document.
type TextEdit struct {
	Range   Range
	NewText string
}

// FormatDocument formats an entire SSL document.
func FormatDocument(text string, opts FormattingOptions) []TextEdit {
	lex := lexer.NewLexer(text)
	tokens := lex.Tokenize()

	formatted := formatTokens(tokens, opts)
	formatted = applyPostFormatPasses(formatted, opts)

	// Return a single edit replacing the entire document
	lines := strings.Split(text, "\n")
	endLine := len(lines) - 1
	endChar := 0
	if endLine >= 0 && len(lines[endLine]) > 0 {
		endChar = len(lines[endLine])
	}

	return []TextEdit{
		{
			Range: Range{
				Start: Position{Line: 0, Character: 0},
				End:   Position{Line: endLine, Character: endChar},
			},
			NewText: formatted,
		},
	}
}

// FormatDocumentRange formats a specific range of an SSL document.
// The range is specified by start and end positions (0-based line and character).
func FormatDocumentRange(text string, startLine, startChar, endLine, endChar int, opts FormattingOptions) []TextEdit {
	lines := strings.Split(text, "\n")

	// Validate and clamp range
	if startLine < 0 {
		startLine = 0
	}
	if endLine >= len(lines) {
		endLine = len(lines) - 1
	}
	if startLine > endLine {
		return nil
	}

	// Expand range to include complete lines for proper formatting
	// This ensures we format complete statements
	startChar = 0
	if endLine < len(lines) {
		endChar = len(lines[endLine])
	}

	// Extract the text in range
	var rangeLines []string
	for i := startLine; i <= endLine && i < len(lines); i++ {
		rangeLines = append(rangeLines, lines[i])
	}
	rangeText := strings.Join(rangeLines, "\n")

	// Detect the base indentation of the first non-empty line in the range
	baseIndent := detectBaseIndent(rangeLines)

	// Remove the base indentation for formatting
	dedentedLines := make([]string, len(rangeLines))
	for i, line := range rangeLines {
		dedentedLines[i] = removeIndent(line, baseIndent)
	}
	dedentedText := strings.Join(dedentedLines, "\n")

	// Format the dedented text
	lex := lexer.NewLexer(dedentedText)
	tokens := lex.Tokenize()
	formatted := formatTokens(tokens, opts)

	// Re-apply the base indentation to each line
	formattedLines := strings.Split(formatted, "\n")
	reindentedLines := make([]string, 0, len(formattedLines))
	for _, line := range formattedLines {
		if strings.TrimSpace(line) == "" {
			reindentedLines = append(reindentedLines, "")
		} else {
			reindentedLines = append(reindentedLines, baseIndent+line)
		}
	}

	// Remove trailing empty line if the original didn't have one
	if len(reindentedLines) > 0 && reindentedLines[len(reindentedLines)-1] == "" {
		if !strings.HasSuffix(rangeText, "\n") {
			reindentedLines = reindentedLines[:len(reindentedLines)-1]
		}
	}

	reindentedText := strings.Join(reindentedLines, "\n")

	return []TextEdit{
		{
			Range: Range{
				Start: Position{Line: startLine, Character: startChar},
				End:   Position{Line: endLine, Character: endChar},
			},
			NewText: reindentedText,
		},
	}
}

// detectBaseIndent detects the common base indentation of the given lines.
func detectBaseIndent(lines []string) string {
	var baseIndent string
	first := true

	for _, line := range lines {
		// Skip empty lines
		if strings.TrimSpace(line) == "" {
			continue
		}

		// Extract leading whitespace
		indent := ""
		for _, r := range line {
			if r == ' ' || r == '\t' {
				indent += string(r)
			} else {
				break
			}
		}

		if first {
			baseIndent = indent
			first = false
		} else {
			// Find common prefix
			baseIndent = commonPrefix(baseIndent, indent)
		}
	}

	return baseIndent
}

// commonPrefix returns the common prefix of two strings.
func commonPrefix(a, b string) string {
	minLen := len(a)
	if len(b) < minLen {
		minLen = len(b)
	}

	for i := 0; i < minLen; i++ {
		if a[i] != b[i] {
			return a[:i]
		}
	}
	return a[:minLen]
}

// removeIndent removes the specified indentation prefix from a line.
func removeIndent(line, indent string) string {
	if strings.HasPrefix(line, indent) {
		return line[len(indent):]
	}
	return line
}

type formatState struct {
	builder               *strings.Builder
	opts                  FormattingOptions
	indent                int
	lineStart             bool
	prevToken             lexer.Token
	lastNonWSToken        lexer.Token
	prevKeyword           string
	inProcedure           bool
	afterEndProc          bool
	afterEndRegion        bool
	currentLineLen        int
	parenDepth            int
	continuationIndent    int // Additional indent for continuation lines inside parens
	inSQLFunction         bool
	sqlFunctionParenDepth int
	sqlArgCount           int
	sqlFormatter          *SQLFormatter
	pendingComment        *lexer.Token // End-of-line comment to write before newline
	pendingStatementBreak bool
	inErrorHandler        bool // Tracks :ERROR scope-based handler for indent
}

func newFormatState(opts FormattingOptions) *formatState {
	return &formatState{
		builder:      &strings.Builder{},
		opts:         opts,
		lineStart:    true,
		sqlFormatter: NewSQLFormatter(opts.SQL),
	}
}

func (s *formatState) updateForKeyword(token lexer.Token) {
	if token.Type != lexer.TokenKeyword {
		return
	}

	normalized := strings.ToUpper(strings.TrimPrefix(token.Text, ":"))

	// Handle block end keywords - dedent before.
	// If we're in an :ERROR handler, the scope-based handler ends too,
	// so we need an extra dedent to close it.
	if constants.IsBlockEndKeyword(normalized) {
		if s.inErrorHandler {
			s.indent--
			s.inErrorHandler = false
		}
		s.indent--
		if s.indent < 0 {
			s.indent = 0
		}
	}

	// Handle middle keywords (ELSE, CASE, CATCH, etc.) - dedent before
	// They will re-indent in finalizeToken if they're in BlockStartKeywords
	// Note: EXITCASE is NOT in BlockMiddleKeywords — it stays at content level
	// like a break statement, causing no indent change at all.
	if constants.IsBlockMiddleKeyword(normalized) {
		s.indent--
		if s.indent < 0 {
			s.indent = 0
		}
	}

	switch normalized {
	case "PROCEDURE":
		if s.afterEndProc && s.opts.BlankLinesBetweenProcs > 0 {
			for j := 0; j < s.opts.BlankLinesBetweenProcs; j++ {
				s.builder.WriteString("\n")
			}
		}
		s.inProcedure = true
		s.afterEndProc = false
		s.afterEndRegion = false
	case "ENDPROC":
		s.inProcedure = false
		s.afterEndProc = true
	case "REGION":
		if s.afterEndRegion && s.opts.BlankLinesBetweenProcs > 0 {
			for j := 0; j < s.opts.BlankLinesBetweenProcs; j++ {
				s.builder.WriteString("\n")
			}
		}
		s.afterEndRegion = false
		s.afterEndProc = false
	case "ENDREGION":
		s.afterEndRegion = true
	case "ERROR":
		// :ERROR is scope-based (no explicit closer). Indent its body.
		s.inErrorHandler = true
		s.afterEndProc = false
		s.afterEndRegion = false
	case "RESUME":
		// :RESUME is a middle keyword within :ERROR — dedent before, indent after.
		if s.inErrorHandler {
			s.indent--
			if s.indent < 0 {
				s.indent = 0
			}
		}
		s.afterEndProc = false
		s.afterEndRegion = false
	default:
		s.afterEndProc = false
		s.afterEndRegion = false
	}
}

func (s *formatState) updateParenDepth(token lexer.Token) {
	if isOpenParen(token) {
		s.parenDepth++
		return
	}
	if isCloseParen(token) {
		s.parenDepth--
		if s.parenDepth < 0 {
			s.parenDepth = 0
		}
	}
}

func (s *formatState) writeIndentIfNeeded(token lexer.Token) {
	if s.lineStart && token.Type != lexer.TokenWhitespace && token.Type != lexer.TokenComment {
		// Calculate continuation indent for lines inside parens
		contIndent := s.continuationIndent
		// If the first token on this line is a closing paren/bracket, dedent by one
		// since it should align with the opening paren's level
		if isCloseParen(token) && contIndent > 0 {
			contIndent--
		}
		// Calculate total indent: base indent + continuation indent
		totalIndent := s.indent + contIndent
		s.currentLineLen = writeIndentLen(s.builder, totalIndent, s.opts)
		s.lineStart = false
	}
}

func (s *formatState) flushPendingStatementBreak(token lexer.Token) {
	if !s.pendingStatementBreak || token.Type == lexer.TokenWhitespace || token.Type == lexer.TokenComment {
		return
	}

	if s.pendingComment != nil {
		s.builder.WriteString("  ")
		s.builder.WriteString(s.pendingComment.Text)
		s.pendingComment = nil
	}

	s.builder.WriteString("\n")
	s.lineStart = true
	s.currentLineLen = 0
	s.pendingStatementBreak = false
}

func (s *formatState) handleWhitespace(token lexer.Token, tokens []lexer.Token, index int) bool {
	if token.Type != lexer.TokenWhitespace {
		return false
	}

	if s.pendingStatementBreak && !strings.Contains(token.Text, "\n") {
		s.prevToken = token
		return true
	}

	if strings.Contains(token.Text, "\n") {
		if s.opts.SemicolonEnforcement && needsSemicolonAtLineEnd(s.lastNonWSToken, tokens, index) {
			s.builder.WriteString(";")
			s.currentLineLen++
		}

		// Write any pending end-of-line comment before the newline
		if s.pendingComment != nil {
			s.builder.WriteString("  ") // Two spaces before comment
			s.builder.WriteString(s.pendingComment.Text)
			s.pendingComment = nil
		}

		newlineCount := strings.Count(token.Text, "\n")
		if newlineCount > 2 {
			newlineCount = 2
		}
		for j := 0; j < newlineCount; j++ {
			s.builder.WriteString("\n")
		}
		s.lineStart = true
		s.currentLineLen = 0
		s.pendingStatementBreak = false

		// Set continuation indent for lines inside parentheses.
		// Schema specifies continuation_indent: 1 (fixed, not proportional to depth).
		if s.parenDepth > 0 {
			s.continuationIndent = 1
		} else {
			s.continuationIndent = 0
		}
	} else if !s.lineStart {
		// Suppress space before ( in function calls: "MyFunc (" -> "MyFunc("
		if s.lastNonWSToken.Type == lexer.TokenIdentifier {
			if next := findNextNonWS(tokens, index); next != nil && next.Text == "(" {
				s.prevToken = token
				return true
			}
		}
		// Suppress space around : in member access: "obj : prop" -> "obj:prop"
		// (colon as TokenPunctuation is member access; := is TokenOperator, :IF is TokenKeyword)
		if s.lastNonWSToken.Text == ":" && s.lastNonWSToken.Type == lexer.TokenPunctuation {
			s.prevToken = token
			return true
		}
		// Also suppress before : when preceded by ), ], or identifier (chained access)
		if s.lastNonWSToken.Type == lexer.TokenIdentifier || s.lastNonWSToken.Text == ")" || s.lastNonWSToken.Text == "]" {
			if next := findNextNonWS(tokens, index); next != nil && next.Text == ":" && next.Type == lexer.TokenPunctuation {
				s.prevToken = token
				return true
			}
		}
		// Suppress space after opening delimiters: "( x" -> "(x", "{ x" -> "{x"
		if isOpenParen(s.lastNonWSToken) {
			s.prevToken = token
			return true
		}
		// Suppress space before closing delimiters: "x )" -> "x)", "x }" -> "x}"
		if next := findNextNonWS(tokens, index); next != nil && isCloseParen(*next) {
			s.prevToken = token
			return true
		}
		// Suppress space before semicolons: "x ;" -> "x;"
		if next := findNextNonWS(tokens, index); next != nil && next.Text == ";" {
			s.prevToken = token
			return true
		}
		s.builder.WriteString(" ")
		s.currentLineLen++
	}

	s.prevToken = token
	return true
}

// findNextNonWS returns the next non-whitespace token starting from startIdx+1, or nil.
func findNextNonWS(tokens []lexer.Token, startIdx int) *lexer.Token {
	for i := startIdx + 1; i < len(tokens); i++ {
		if tokens[i].Type != lexer.TokenWhitespace {
			return &tokens[i]
		}
	}
	return nil
}

func (s *formatState) applyLineWrap(token lexer.Token) {
	if s.opts.MaxLineLength <= 0 || s.lineStart {
		return
	}

	tokenLen := len(token.Text)
	if s.currentLineLen+tokenLen <= s.opts.MaxLineLength {
		return
	}

	if !canWrapBefore(token, s.lastNonWSToken, s.parenDepth) {
		return
	}

	// Rule F: when the next token is a string that will be reflowed as
	// multi-line SQL, don't wrap before it. The SQL formatter handles its
	// own line breaks, and the opening '"' should stay on the previous
	// line (attached to ':=', a named-arg, etc.).
	if s.willFormatAsMultilineSQL(token) {
		return
	}

	s.builder.WriteString("\n")
	contIndent := s.indent + 1 // Fixed 1-level continuation indent per schema
	s.currentLineLen = writeIndentLen(s.builder, contIndent, s.opts)
	s.lineStart = false
}

func (s *formatState) writeOperatorOrComma(token lexer.Token, tokens []lexer.Token, index int) bool {
	if s.opts.OperatorSpacing && isOperator(token) {
		// Detect unary minus/plus: no space between operator and operand
		// when preceded by another operator, open paren, comma, assignment, or at line start.
		if (token.Text == "-" || token.Text == "+") && isUnaryContext(s.lastNonWSToken, s.lineStart) {
			s.builder.WriteString(token.Text)
			s.currentLineLen += len(token.Text)
			return true
		}
		if !s.lineStart && s.prevToken.Type != lexer.TokenWhitespace && !isOpenParen(s.prevToken) {
			s.builder.WriteString(" ")
			s.currentLineLen++
		}
		s.builder.WriteString(token.Text)
		s.currentLineLen += len(token.Text)
		if index+1 < len(tokens) {
			next := tokens[index+1]
			if next.Type != lexer.TokenWhitespace && !isCloseParen(next) && next.Text != ";" {
				s.builder.WriteString(" ")
				s.currentLineLen++
			}
		}
		return true
	}

	if s.opts.CommaSpacing && token.Text == "," {
		s.builder.WriteString(",")
		s.currentLineLen++
		// Skipped parameters: adjacent commas get no space (e.g., DoProc("P", {a,,b}))
		if index+1 < len(tokens) && tokens[index+1].Text == "," {
			return true
		}
		if s.opts.MaxLineLength > 0 && index+1 < len(tokens) {
			next := tokens[index+1]
			if next.Type != lexer.TokenWhitespace && next.Type != lexer.TokenEOF {
				remainingLen := estimateRemainingLineLen(tokens, index+1)
				if s.currentLineLen+1+remainingLen > s.opts.MaxLineLength && s.parenDepth > 0 {
					s.builder.WriteString("\n")
					contIndent := s.indent + 1 // Fixed 1-level continuation indent
					s.currentLineLen = writeIndentLen(s.builder, contIndent, s.opts)
					s.lineStart = false
				} else {
					s.builder.WriteString(" ")
					s.currentLineLen++
				}
			}
		} else if index+1 < len(tokens) {
			next := tokens[index+1]
			if next.Type != lexer.TokenWhitespace && next.Text != "," {
				s.builder.WriteString(" ")
				s.currentLineLen++
			}
		}
		return true
	}

	return false
}

func (s *formatState) updateSQLFunctionState(token lexer.Token) {
	if token.Type == lexer.TokenIdentifier {
		upper := strings.ToUpper(token.Text)
		if SQLFunctions[upper] {
			s.inSQLFunction = true
			s.sqlFunctionParenDepth = s.parenDepth + 1
			s.sqlArgCount = 0
		}
	}

	if s.inSQLFunction {
		if token.Text == "(" && s.parenDepth == s.sqlFunctionParenDepth-1 {
			s.sqlArgCount = 0
		} else if token.Text == ")" && s.parenDepth == s.sqlFunctionParenDepth {
			s.inSQLFunction = false
			s.sqlFunctionParenDepth = 0
		} else if token.Text == "," && s.parenDepth == s.sqlFunctionParenDepth {
			s.sqlArgCount++
		}
	}
}

// willFormatAsMultilineSQL returns true if writeTokenWithSQLFormatting will
// format this token as multi-line SQL. Used by applyLineWrap to suppress a
// pre-string newline so the opening quote stays on the prior line.
func (s *formatState) willFormatAsMultilineSQL(token lexer.Token) bool {
	if token.Type != lexer.TokenString || !s.opts.SQL.Enabled {
		return false
	}
	if len(token.Text) < 2 {
		return false
	}
	if !s.opts.SQL.DetectSQLStrings && !s.inSQLFunction {
		return false
	}
	inner := token.Text[1 : len(token.Text)-1]
	return IsSQLString(inner)
}

func (s *formatState) writeTokenWithSQLFormatting(token lexer.Token) bool {
	if token.Type != lexer.TokenString {
		return false
	}

	content := token.Text
	if len(content) < 2 {
		s.builder.WriteString(token.Text)
		s.currentLineLen += len(token.Text)
		return true
	}

	quoteChar := content[0]
	innerContent := content[1 : len(content)-1]

	// When DetectSQLStrings is disabled, only format strings inside SQL
	// function calls (e.g. SQLExecute). Skip regular string literals.
	if !s.opts.SQL.DetectSQLStrings && !s.inSQLFunction {
		return false
	}

	// Only format as SQL if the string actually looks like a SQL statement.
	// Being inside a SQL function call (first argument) is not sufficient —
	// the argument could be an error message or other non-SQL string.
	shouldFormat := IsSQLString(innerContent)

	if !shouldFormat {
		return false
	}

	baseIndent := strings.Repeat("\t", s.indent)
	if s.opts.IndentStyle == "space" {
		baseIndent = strings.Repeat(" ", s.opts.IndentSize*s.indent)
	}

	formattedSQL := s.sqlFormatter.FormatSQLInString(innerContent, quoteChar, baseIndent)
	s.builder.WriteString(formattedSQL)
	s.currentLineLen += len(formattedSQL)
	return true
}

func (s *formatState) finalizeToken(token lexer.Token) {
	if token.Type == lexer.TokenKeyword {
		s.prevKeyword = strings.ToUpper(strings.TrimPrefix(token.Text, ":"))
		if constants.IsBlockStartKeyword(s.prevKeyword) {
			s.indent++
		}
		// :ERROR and :RESUME are scope-based — indent body after them
		if s.prevKeyword == "ERROR" || s.prevKeyword == "RESUME" {
			s.indent++
		}
	}

	if token.Text == ";" {
		s.pendingStatementBreak = true
		s.prevKeyword = ""
	}

	s.prevToken = token
	s.lastNonWSToken = token
}

// formatTokens formats tokens according to options.
func formatTokens(tokens []lexer.Token, opts FormattingOptions) string {
	state := newFormatState(opts)

	for i, token := range tokens {
		if token.Type == lexer.TokenEOF {
			break
		}

		// Check if this is an end-of-line comment (comment on same line as code)
		if token.Type == lexer.TokenComment {
			if isEndOfLineComment(token, state.lastNonWSToken, tokens, i) {
				// Store this comment to be written before the next newline
				commentCopy := token
				state.pendingComment = &commentCopy
				continue
			}
		}

		state.updateForKeyword(token)
		state.updateParenDepth(token)
		state.flushPendingStatementBreak(token)
		state.writeIndentIfNeeded(token)
		if state.handleWhitespace(token, tokens, i) {
			continue
		}

		state.applyLineWrap(token)
		tokenWritten := state.writeOperatorOrComma(token, tokens, i)
		if !tokenWritten {
			state.updateSQLFunctionState(token)
			tokenWritten = state.writeTokenWithSQLFormatting(token)
		}
		if !tokenWritten {
			// Normalize mashed :LABELName -> :LABEL Name
			if token.Type == lexer.TokenKeyword && isLabelKeywordMashed(token.Text) {
				labelName := token.Text[1+len("LABEL"):] // skip ":" + "LABEL"
				out := ":LABEL " + labelName
				state.builder.WriteString(out)
				state.currentLineLen += len(out)
			} else if token.Type == lexer.TokenKeyword && strings.HasPrefix(token.Text, ".") {
				// Dot-wrapped literals (.T., .F., .AND., .OR., .NOT.) — uppercase but no colon
				normalized := strings.ToUpper(token.Text)
				state.builder.WriteString(normalized)
				state.currentLineLen += len(normalized)
			} else if token.Type == lexer.TokenKeyword && !strings.HasPrefix(token.Text, ":") {
				// Bare keyword (e.g. NIL) — uppercase, no colon added
				normalized := strings.ToUpper(token.Text)
				state.builder.WriteString(normalized)
				state.currentLineLen += len(normalized)
			} else if token.Type == lexer.TokenKeyword {
				// Colon-prefixed keyword: :if -> :IF, :endIf -> :ENDIF
				normalized := ":" + strings.ToUpper(strings.TrimPrefix(token.Text, ":"))
				state.builder.WriteString(normalized)
				state.currentLineLen += len(normalized)
			} else {
				state.builder.WriteString(token.Text)
				state.currentLineLen += len(token.Text)
			}
		}

		state.finalizeToken(token)
	}

	// Write any remaining pending comment at end of file
	if state.pendingComment != nil {
		state.builder.WriteString("  ")
		state.builder.WriteString(state.pendingComment.Text)
	}

	formatted := state.builder.String()

	// Trim trailing whitespace from each line
	lines := strings.Split(formatted, "\n")
	for i, line := range lines {
		lines[i] = strings.TrimRight(line, " \t")
	}
	formatted = strings.Join(lines, "\n")

	if len(formatted) > 0 && !strings.HasSuffix(formatted, "\n") {
		formatted += "\n"
	}

	return formatted
}

// isEndOfLineComment checks if a comment token is an end-of-line comment
// (a comment on the same line as code, following code).
func isEndOfLineComment(comment lexer.Token, lastNonWSToken lexer.Token, tokens []lexer.Token, commentIndex int) bool {
	// If no code was written yet, this is not an end-of-line comment
	if lastNonWSToken.Type == 0 {
		return false
	}

	// If the comment is on the same line as the last non-whitespace token, it's an end-of-line comment
	if comment.Line == lastNonWSToken.Line {
		// Also check that the comment doesn't contain newlines (is single-line)
		if !strings.Contains(comment.Text, "\n") {
			return true
		}
	}

	return false
}

// writeIndentLen writes indentation and returns the visual length.
func writeIndentLen(b *strings.Builder, level int, opts FormattingOptions) int {
	if level <= 0 {
		return 0
	}

	if opts.IndentStyle == "space" {
		spaces := strings.Repeat(" ", opts.IndentSize*level)
		b.WriteString(spaces)
		return opts.IndentSize * level
	}
	tabs := strings.Repeat("\t", level)
	b.WriteString(tabs)
	// Assume tab width equals IndentSize for length calculation
	return opts.IndentSize * level
}

// canWrapBefore checks if we can wrap before a token.
func canWrapBefore(token, prevToken lexer.Token, parenDepth int) bool {
	// Don't wrap at the very start
	if prevToken.Type == 0 {
		return false
	}
	// Good wrap points: after comma, after assignment, inside parens
	if prevToken.Text == "," {
		return true
	}
	if prevToken.Text == ":=" {
		return true
	}
	// Wrap before identifiers/keywords inside parentheses
	if parenDepth > 0 && (token.Type == lexer.TokenIdentifier || token.Type == lexer.TokenKeyword) {
		return true
	}
	// Schema: wrap_long_lines.break_on includes "before_operator".
	// Only wrap before operators that are natural statement-level break points.
	// Comparison operators (=, ==, !=, <, >, <=, >=, <>) bind tightly to their
	// operands (e.g. "a = 1") and should not be separated from them.
	if token.Type == lexer.TokenOperator {
		upper := strings.ToUpper(token.Text)
		switch upper {
		case ".AND.", ".OR.", ".NOT.":
			return true
		case "+", "-", "*", "/", "%", "^", "**":
			return true
		case "+=", "-=", "*=", "/=", "%=", "^=":
			return true
		case "$":
			return true
		}
	}
	return false
}

// needsSemicolonAtLineEnd checks if a semicolon should be added at the end of a line.
func needsSemicolonAtLineEnd(lastToken lexer.Token, tokens []lexer.Token, wsIndex int) bool {
	// Don't add semicolon at the very start
	if lastToken.Type == 0 {
		return false
	}

	// Already have a semicolon
	if lastToken.Text == ";" {
		return false
	}

	// Don't add after opening delimiters (incomplete expression)
	if isOpenParen(lastToken) {
		return false
	}

	// Don't add after operators (incomplete expression)
	if lastToken.Type == lexer.TokenOperator || lastToken.Text == ":=" {
		return false
	}

	// Don't add after comma (in parameter list)
	if lastToken.Text == "," {
		return false
	}

	// Don't add after keywords that don't end statements (like :TO, :STEP)
	if lastToken.Type == lexer.TokenKeyword {
		keyword := strings.ToUpper(strings.TrimPrefix(lastToken.Text, ":"))
		nonStatementEndingKeywords := map[string]bool{
			"TO":   true,
			"STEP": true,
		}
		if nonStatementEndingKeywords[keyword] {
			return false
		}
	}

	// Look at the next non-whitespace token to decide
	for j := wsIndex + 1; j < len(tokens); j++ {
		nextTok := tokens[j]
		if nextTok.Type == lexer.TokenEOF {
			// End of file - add semicolon if we have statement content
			return isStatementContent(lastToken)
		}
		if nextTok.Type == lexer.TokenWhitespace {
			continue
		}
		if nextTok.Type == lexer.TokenComment {
			continue
		}

		// Check if next token is a keyword that starts a new statement
		if nextTok.Type == lexer.TokenKeyword {
			keyword := strings.ToUpper(strings.TrimPrefix(nextTok.Text, ":"))

			// Keywords that are continuations (don't need semicolon before)
			continuationKeywords := map[string]bool{
				"ELSE":      true,
				"CATCH":     true,
				"FINALLY":   true,
				"CASE":      true,
				"OTHERWISE": true,
				"TO":        true,
				"STEP":      true,
			}

			if continuationKeywords[keyword] {
				return false
			}

			// It's a statement-starting keyword, need semicolon
			return isStatementContent(lastToken)
		}

		// If next is identifier, we have an assignment/call starting - need semicolon
		if nextTok.Type == lexer.TokenIdentifier {
			return isStatementContent(lastToken)
		}

		// Other cases - no semicolon needed
		return false
	}

	return false
}

// isStatementContent checks if a token is valid statement content that should end with semicolon.
func isStatementContent(token lexer.Token) bool {
	// Identifiers, numbers, strings, closing parens are valid statement endings
	if token.Type == lexer.TokenIdentifier {
		return true
	}
	if token.Type == lexer.TokenNumber {
		return true
	}
	if token.Type == lexer.TokenString || token.Type == lexer.TokenCodeBlock {
		return true
	}
	if isCloseParen(token) {
		return true
	}
	// Keywords that end statements
	if token.Type == lexer.TokenKeyword {
		return true
	}
	return false
}

// estimateRemainingLineLen estimates length until next newline or semicolon.
func estimateRemainingLineLen(tokens []lexer.Token, startIdx int) int {
	length := 0
	for i := startIdx; i < len(tokens); i++ {
		tok := tokens[i]
		if tok.Type == lexer.TokenEOF {
			break
		}
		if tok.Type == lexer.TokenWhitespace && strings.Contains(tok.Text, "\n") {
			break
		}
		if tok.Text == ";" || tok.Text == ")" {
			length += len(tok.Text)
			break
		}
		if tok.Type == lexer.TokenWhitespace {
			length++ // Single space
		} else {
			length += len(tok.Text)
		}
	}
	return length
}

// isUnaryContext returns true if the previous token context indicates that
// a - or + is unary (sign) rather than binary (subtraction/addition).
func isUnaryContext(lastNonWS lexer.Token, lineStart bool) bool {
	if lineStart || lastNonWS.Type == 0 {
		return true
	}
	// After operators, assignment, open parens, commas, keywords — it's unary
	if lastNonWS.Type == lexer.TokenOperator {
		return true
	}
	if isOpenParen(lastNonWS) || lastNonWS.Text == "," {
		return true
	}
	if lastNonWS.Text == ":=" {
		return true
	}
	if lastNonWS.Type == lexer.TokenKeyword {
		return true
	}
	return false
}

// isBlockEndKeyword checks if a keyword ends a block.
func isBlockEndKeyword(keyword string) bool {
	return constants.IsBlockEndKeyword(keyword)
}

// isOperator checks if a token is an operator that needs spacing.
// Increment (++) and decrement (--) are excluded — they attach to their operand.
func isOperator(token lexer.Token) bool {
	if token.Type == lexer.TokenOperator {
		// Exclude unary operators — no spaces around these
		if token.Text == "++" || token.Text == "--" || token.Text == "!" {
			return false
		}
		return true
	}
	// Assignment operator
	if token.Text == ":=" {
		return true
	}
	// Comparison operators
	ops := map[string]bool{
		"=": true, "<>": true, "!=": true,
		"<": true, ">": true, "<=": true, ">=": true,
		"+": true, "-": true, "*": true, "/": true,
	}
	return ops[token.Text]
}

// isOpenParen checks if token is an opening delimiter.
// isLabelKeywordMashed detects :LABELName (mashed form without space).
func isLabelKeywordMashed(text string) bool {
	if !strings.HasPrefix(text, ":") {
		return false
	}
	trimmed := text[1:]
	return len(trimmed) > len("LABEL") && strings.HasPrefix(strings.ToUpper(trimmed), "LABEL")
}

func isOpenParen(token lexer.Token) bool {
	return token.Text == "(" || token.Text == "[" || token.Text == "{"
}

// isCloseParen checks if token is a closing delimiter.
func isCloseParen(token lexer.Token) bool {
	return token.Text == ")" || token.Text == "]" || token.Text == "}"
}

// applyPostFormatPasses runs whole-document post-processing on the formatter
// output. These are line-oriented passes that don't need token information,
// so they're easier to express here than inside the token-stream formatter.
func applyPostFormatPasses(text string, opts FormattingOptions) string {
	if opts.BuiltinFunctionCase == "PascalCase" {
		text = canonicalizeBuiltinCasing(text)
	}
	if opts.TrimTrailingWhitespace {
		text = trimTrailingWhitespacePerLine(text)
	}
	if opts.MaxConsecutiveBlankLines > 0 {
		text = capConsecutiveBlankLines(text, opts.MaxConsecutiveBlankLines)
	}
	return text
}

// trimTrailingWhitespacePerLine removes trailing space/tab characters from
// every line. Final newline (if any) is preserved.
func trimTrailingWhitespacePerLine(text string) string {
	hadTrailingNewline := strings.HasSuffix(text, "\n")
	lines := strings.Split(text, "\n")
	for i, line := range lines {
		lines[i] = strings.TrimRight(line, " \t")
	}
	out := strings.Join(lines, "\n")
	if hadTrailingNewline && !strings.HasSuffix(out, "\n") {
		out += "\n"
	}
	return out
}

// capConsecutiveBlankLines collapses runs of N+1 or more blank lines (whitespace
// only) into exactly `max` blank lines. Lines that contain non-whitespace text
// are unaffected.
func capConsecutiveBlankLines(text string, max int) string {
	lines := strings.Split(text, "\n")
	out := make([]string, 0, len(lines))
	blankRun := 0
	for _, line := range lines {
		if strings.TrimSpace(line) == "" {
			blankRun++
			if blankRun <= max {
				out = append(out, line)
			}
			continue
		}
		blankRun = 0
		out = append(out, line)
	}
	return strings.Join(out, "\n")
}

// canonicalizeBuiltinCasing rewrites built-in function call sites to their
// canonical PascalCase form. A "call site" is a bare identifier immediately
// followed by `(`. We only rewrite identifiers whose lowercased form matches
// a published built-in; user-defined functions and identifiers used as
// arguments are left alone.
func canonicalizeBuiltinCasing(text string) string {
	canonical := constants.CanonicalFunctionNames()
	if len(canonical) == 0 {
		return text
	}
	var b strings.Builder
	b.Grow(len(text))
	i := 0
	n := len(text)
	for i < n {
		// Find next identifier start.
		c := text[i]
		if !isIdentStart(c) {
			b.WriteByte(c)
			i++
			continue
		}
		j := i + 1
		for j < n && isIdentCont(text[j]) {
			j++
		}
		ident := text[i:j]
		// Skip over any whitespace following ident; only rewrite if the
		// next non-whitespace character is '('.
		k := j
		for k < n && (text[k] == ' ' || text[k] == '\t') {
			k++
		}
		if k < n && text[k] == '(' {
			if pascal, ok := canonical[strings.ToLower(ident)]; ok && pascal != ident {
				b.WriteString(pascal)
				b.WriteString(text[j:k])
				b.WriteByte('(')
				i = k + 1
				continue
			}
		}
		b.WriteString(ident)
		i = j
	}
	return b.String()
}

func isIdentStart(c byte) bool {
	return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_'
}

func isIdentCont(c byte) bool {
	return isIdentStart(c) || (c >= '0' && c <= '9')
}
