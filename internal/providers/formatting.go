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
	// BlankLineBetweenBlocks inserts a blank line between sibling control-flow
	// blocks (:IF / :WHILE / :FOR / :BEGINCASE / :TRY) at the same indent so
	// they read as distinct units rather than one wall of code. The blank line
	// is inserted between the closing keyword of the previous block and the
	// opening keyword of the next block. Default true.
	BlankLineBetweenBlocks bool
	// TrimTrailingWhitespace removes trailing space/tab characters from
	// every formatted line. Default true.
	TrimTrailingWhitespace bool
	// MaxConsecutiveBlankLines collapses runs of blank lines longer than
	// this threshold. 0 disables the cap.
	MaxConsecutiveBlankLines int
	// BuiltinFunctionCase controls casing of built-in function names.
	// "PascalCase" (default — the style guide's exact documented casing is
	// authoritative, issue #92) rewrites each call site to the canonical
	// inventory casing; "preserve" keeps the user's casing.
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
		BlankLineBetweenBlocks:   true,
		TrimTrailingWhitespace:   true,
		MaxConsecutiveBlankLines: 0,
		BuiltinFunctionCase:      "PascalCase",
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

	// A document ending in an unterminated string cannot be formatted
	// safely: the string has swallowed the rest of the file and each pass
	// appended another semicolon (issue #87). Per feature.formatting, when
	// the formatter cannot proceed it leaves the text unchanged.
	if hasUnterminatedString(tokens) {
		return nil
	}

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
	if hasUnterminatedString(tokens) {
		return nil // issue #87 — see FormatDocument
	}
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
	reindentedText = wrapLongLines(reindentedText, opts)

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
// When the lines mix tabs and spaces the common byte prefix collapses to ""
// and the whole selection used to be re-anchored at column 0 (issue #98);
// the first non-blank line's indent is the fallback anchor so the block
// stays where it sits (feature.formatting A2).
func detectBaseIndent(lines []string) string {
	var baseIndent, firstIndent string
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
			firstIndent = indent
			first = false
		} else {
			// Find common prefix
			baseIndent = commonPrefix(baseIndent, indent)
		}
	}

	if baseIndent == "" {
		return firstIndent
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
	lastLineIndent        int // indent level of the last non-continuation line
	parenDepth            int
	continuationIndent    int // Additional indent for continuation lines inside parens
	inSQLFunction         bool
	sqlFunctionParenDepth int
	sqlArgCount           int
	sqlFormatter          *SQLFormatter
	pendingComment        *lexer.Token // End-of-line comment to write before newline
	pendingStatementBreak bool
	inErrorHandler        bool // Tracks :ERROR scope-based handler for indent
	// commentBlockStart is the builder offset where the current run of
	// standalone comment lines began, or -1 when the output does not
	// currently end in such a run. A blank line or any non-comment content
	// breaks the run. Used by normalizeProcBoundary (issue #33) to place
	// procedure-separating blank lines above an attached doc comment.
	commentBlockStart int
}

func newFormatState(opts FormattingOptions) *formatState {
	return &formatState{
		builder:           &strings.Builder{},
		opts:              opts,
		lineStart:         true,
		sqlFormatter:      NewSQLFormatter(opts.SQL),
		commentBlockStart: -1,
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
		if s.afterEndProc {
			s.normalizeProcBoundary()
		}
		s.inProcedure = true
		s.afterEndProc = false
		s.afterEndRegion = false
	case "ENDPROC":
		s.inProcedure = false
		s.afterEndProc = true
	case "REGION":
		if s.afterEndRegion {
			s.normalizeProcBoundary()
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

// normalizeProcBoundary rewrites the blank-line run between the previous
// :ENDPROC;/:ENDREGION; and the upcoming :PROCEDURE/:REGION keyword so that
// exactly BlankLinesBetweenProcs blank lines separate them (issue #33). The
// count is normalized, not additive: blank lines surviving from the source
// are replaced, never stacked on. When a standalone comment block is attached
// to the upcoming keyword (no blank line between the comments and the
// keyword), the blank lines are placed above the comment block so
// documentation stays with the procedure it documents. A setting of 0
// disables normalization entirely.
func (s *formatState) normalizeProcBoundary() {
	n := s.opts.BlankLinesBetweenProcs
	if n <= 0 {
		return
	}

	// Complete a pending statement line (e.g. mashed ":ENDPROC;:PROCEDURE")
	// so the boundary sits after a finished line.
	if s.pendingStatementBreak {
		if s.pendingComment != nil {
			s.builder.WriteString("  ")
			s.builder.WriteString(s.pendingComment.Text)
			s.pendingComment = nil
		}
		s.builder.WriteString("\n")
		s.pendingStatementBreak = false
	}

	built := s.builder.String()
	insertPos := len(built)
	if s.commentBlockStart >= 0 && s.commentBlockStart <= insertPos {
		insertPos = s.commentBlockStart
	}
	head, tail := built[:insertPos], built[insertPos:]
	trimmed := strings.TrimRight(head, "\n")
	if trimmed == "" {
		return
	}

	rebuilt := trimmed + "\n" + strings.Repeat("\n", n) + tail
	if rebuilt != built {
		s.builder.Reset()
		s.builder.WriteString(rebuilt)
		if s.commentBlockStart >= 0 {
			s.commentBlockStart += len(rebuilt) - len(built)
		}
	}
	if strings.HasSuffix(rebuilt, "\n") {
		s.lineStart = true
		s.currentLineLen = 0
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
	// Standalone comments are indented like statements at the enclosing
	// block depth (issue #36); only the first line of a multi-line comment
	// is indented — its interior lines are part of the token text and stay
	// verbatim.
	if s.lineStart && token.Type != lexer.TokenWhitespace {
		// Issues #86/#89: every expression continuation — inside an open
		// delimiter, starting with a binary operator, or following a line
		// that ended in ':=' or a binary operator — sits exactly one level
		// past the line that opened the statement (lexical, not block
		// depth: an :IF has already incremented s.indent for its body, but
		// its condition's continuation belongs one past the :IF line).
		isContinuation := s.continuationIndent > 0 ||
			isContinuationOperator(token) ||
			s.lastNonWSToken.Text == ":=" ||
			isContinuationOperator(s.lastNonWSToken)
		var totalIndent int
		if isContinuation {
			totalIndent = s.lastLineIndent + 1
			// A closing paren/bracket leading the line aligns with the
			// statement line instead.
			if isCloseParen(token) {
				totalIndent--
			}
		} else {
			totalIndent = s.indent
			s.lastLineIndent = totalIndent
		}
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

	// A split SQL-string assignment rejoins (fmt.sql_in_strings, issue
	// #140): a single line break between ':=' and a detected SQL string, or
	// between that string and its terminating ';', is treated as a plain
	// space so the statement converges on the canonical layout (inline when
	// it fits, rule F when it doesn't) instead of preserving whatever split
	// the source happened to have. Blank-line runs and lines carrying an
	// end-of-line comment are left alone.
	if strings.Count(token.Text, "\n") == 1 && s.pendingComment == nil &&
		s.shouldJoinSQLStringBreak(tokens, index) {
		token.Text = " "
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

		// Source blank-line runs are preserved as written (issue #37); when
		// ssl.format.maxConsecutiveBlankLines is > 0, the post-format pass
		// caps them afterwards.
		newlineCount := strings.Count(token.Text, "\n")
		for j := 0; j < newlineCount; j++ {
			s.builder.WriteString("\n")
		}
		if newlineCount >= 2 {
			// A blank line detaches any preceding standalone comment run.
			s.commentBlockStart = -1
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
		// Suppress space before commas when comma spacing is managed:
		// "x ," -> "x," (issue #35)
		if s.opts.CommaSpacing {
			if next := findNextNonWS(tokens, index); next != nil && next.Text == "," {
				s.prevToken = token
				return true
			}
		}
		s.builder.WriteString(" ")
		s.currentLineLen++
	}

	s.prevToken = token
	return true
}

// shouldJoinSQLStringBreak reports whether a newline whitespace token sits at
// one of the two seams of a split SQL-string assignment (issue #140):
// directly after ':=' with a detected SQL string next, or directly after the
// SQL string with its ';' next. Detection mirrors writeTokenWithSQLFormatting:
// outside SQL function calls it requires DetectSQLStrings, and only argument
// 0 of a SQL function call is ever a SQL candidate.
func (s *formatState) shouldJoinSQLStringBreak(tokens []lexer.Token, index int) bool {
	next := findNextNonWS(tokens, index)
	if next == nil {
		return false
	}
	if s.lastNonWSToken.Type == lexer.TokenOperator && s.lastNonWSToken.Text == ":=" &&
		next.Type == lexer.TokenString {
		return s.isDetectedSQLString(*next)
	}
	if s.lastNonWSToken.Type == lexer.TokenString &&
		next.Type == lexer.TokenPunctuation && next.Text == ";" {
		return s.isDetectedSQLString(s.lastNonWSToken)
	}
	return false
}

// isDetectedSQLString applies the fmt.sql_in_strings candidacy gates to a
// string token: quote-delimited, allowed by DetectSQLStrings / SQL-function
// argument position, and structurally SQL (IsSQLString).
func (s *formatState) isDetectedSQLString(tok lexer.Token) bool {
	if len(tok.Text) < 2 {
		return false
	}
	if !s.opts.SQL.DetectSQLStrings && !s.inSQLFunction {
		return false
	}
	if s.inSQLFunction && s.sqlArgCount > 0 {
		return false
	}
	return IsReformattableSQLString(tok.Text[1 : len(tok.Text)-1])
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

func (s *formatState) writeOperatorOrComma(token lexer.Token, tokens []lexer.Token, index int) bool {
	if s.opts.OperatorSpacing && isOperator(token) {
		// Detect unary minus/plus: no space between operator and operand
		// when preceded by another operator, open paren, comma, assignment, or at line start.
		if (token.Text == "-" || token.Text == "+") && isUnaryContext(s.lastNonWSToken, s.lineStart) {
			s.builder.WriteString(token.Text)
			s.currentLineLen += len(token.Text)
			return true
		}
		// Issue #88: an operator directly after another operator (glued
		// input like `:=.not.` or `**=` lexed as `**` `=`) already has the
		// previous operator's trailing space — adding a leading one printed
		// a double space.
		if !s.lineStart && s.prevToken.Type != lexer.TokenWhitespace &&
			s.prevToken.Type != lexer.TokenOperator && !isOpenParen(s.prevToken) {
			s.builder.WriteString(" ")
			s.currentLineLen++
		}
		opText := canonicalDotOperator(token.Text)
		s.builder.WriteString(opText)
		s.currentLineLen += len(opText)
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
		if index+1 < len(tokens) {
			next := tokens[index+1]
			if next.Type != lexer.TokenWhitespace && next.Type != lexer.TokenEOF && next.Text != "," {
				s.builder.WriteString(" ")
				s.currentLineLen++
			}
		}
		return true
	}

	return false
}

// updateSQLFunctionState tracks whether the formatter is inside a known SQL
// function call and which argument position it is at. Must run for every
// token (including commas — issue #82: it used to run only for tokens the
// operator/comma writer didn't handle, so sqlArgCount never advanced).
func (s *formatState) updateSQLFunctionState(token lexer.Token) {
	if token.Type == lexer.TokenIdentifier {
		upper := strings.ToUpper(token.Text)
		if SQLFunctions[upper] {
			s.inSQLFunction = true
			s.sqlFunctionParenDepth = s.parenDepth + 1
			s.sqlArgCount = 0
		}
	}

	if !s.inSQLFunction {
		return
	}
	switch {
	case token.Text == ")" && s.parenDepth < s.sqlFunctionParenDepth:
		// The call's own closing paren (parenDepth is already decremented).
		// Nested closers land at >= sqlFunctionParenDepth and must not end
		// the call early.
		s.inSQLFunction = false
		s.sqlFunctionParenDepth = 0
	case token.Text == "," && s.parenDepth == s.sqlFunctionParenDepth:
		s.sqlArgCount++
	}
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

	// Only argument 0 of a SQL function is the SQL string; later arguments
	// (friendly names, LSearch default values) must never be reformatted
	// even when they look like SQL (issue #82).
	if s.inSQLFunction && s.sqlArgCount > 0 {
		return false
	}

	// Only format as SQL if the string actually looks like a SQL statement.
	// Being inside a SQL function call (first argument) is not sufficient —
	// the argument could be an error message or other non-SQL string.
	shouldFormat := IsReformattableSQLString(innerContent)

	if !shouldFormat {
		return false
	}

	// Issue #64: don't reformat SQL that's already single-line and would
	// fit on the current line. Reformatting a short query like
	// `sX := "SELECT * FROM DUAL";` into a 5-line block breaks the
	// surrounding SSL syntax and is undesirable when the original already
	// fits. Only reflow when wrapping is genuinely necessary.
	if !strings.ContainsRune(innerContent, '\n') {
		fits := s.opts.MaxLineLength <= 0 ||
			s.currentLineLen+len(token.Text) <= s.opts.MaxLineLength
		if fits {
			return false
		}
	}

	baseIndent := strings.Repeat("\t", s.indent)
	if s.opts.IndentStyle == "space" {
		baseIndent = strings.Repeat(" ", s.opts.IndentSize*s.indent)
	}

	formattedSQL := s.sqlFormatter.FormatSQLInString(innerContent, quoteChar, baseIndent)
	s.builder.WriteString(formattedSQL)
	// Rule E: after a multi-line SQL string, currentLineLen must reflect only
	// the *last* physical line of the output (typically just the closing
	// quote at baseIndent). Otherwise the next token's wrap logic sees a
	// huge length and forces a newline between the closing '"' and a
	// trailing ',' / remaining call args.
	if idx := strings.LastIndex(formattedSQL, "\n"); idx >= 0 {
		s.currentLineLen = len(formattedSQL) - idx - 1
	} else {
		s.currentLineLen += len(formattedSQL)
	}
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

	// A statement following a standalone comment on the same source line
	// moves to its own line — code must not hide behind a comment
	// (one_statement_per_line, issue #101). End-of-line comments never
	// reach finalizeToken (they ride the pendingComment path).
	if token.Type == lexer.TokenComment {
		s.pendingStatementBreak = true
	}

	// Any non-comment content ends the current standalone-comment block.
	if token.Type != lexer.TokenComment {
		s.commentBlockStart = -1
	}

	s.prevToken = token
	s.lastNonWSToken = token
}

// formatTokens formats tokens according to options.
func formatTokens(tokens []lexer.Token, opts FormattingOptions) string {
	state := newFormatState(opts)

	for i, token := range tokens {
		if token.Type == lexer.TokenEOF {
			// A final statement with no trailing newline still gets its
			// semicolon (issue #38) — mirror the newline-triggered check.
			if opts.SemicolonEnforcement && !state.lineStart &&
				needsSemicolonAtLineEnd(state.lastNonWSToken, tokens, i-1) {
				state.builder.WriteString(";")
			}
			break
		}

		// Check if this is an end-of-line comment (comment on same line as code)
		if token.Type == lexer.TokenComment {
			if isEndOfLineComment(token, state.lastNonWSToken, tokens, i) {
				// Store this comment to be written before the next newline.
				// A line can carry several consecutive EOL comments
				// (`x := 1; /*old value; /*explanation;`) — merge rather
				// than clobber, or the earlier comment is silently deleted
				// (issue #215).
				commentCopy := token
				if state.pendingComment != nil {
					commentCopy.Text = state.pendingComment.Text + "  " + token.Text
				}
				state.pendingComment = &commentCopy
				continue
			}
			// A standalone comment at line start opens (or continues) a
			// comment block; remember where it began for issue #33.
			if state.lineStart && state.commentBlockStart < 0 {
				state.commentBlockStart = state.builder.Len()
			}
		}

		// Region bodies are opaque payload (issue #164): pass through
		// verbatim — no reindent, no semicolon enforcement, no SQL reflow.
		// The token carries its own leading newline from the header line.
		if token.Type == lexer.TokenRegionBody {
			if state.pendingComment != nil {
				state.builder.WriteString("  ")
				state.builder.WriteString(state.pendingComment.Text)
				state.pendingComment = nil
			}
			state.pendingStatementBreak = false
			state.builder.WriteString(token.Text)
			if idx := strings.LastIndex(token.Text, "\n"); idx >= 0 {
				state.currentLineLen = len(token.Text) - idx - 1
			} else {
				state.currentLineLen += len(token.Text)
			}
			state.lineStart = strings.HasSuffix(token.Text, "\n")
			state.prevToken = token
			state.lastNonWSToken = token
			continue
		}

		state.updateForKeyword(token)
		state.updateParenDepth(token)
		state.flushPendingStatementBreak(token)
		state.writeIndentIfNeeded(token)
		if state.handleWhitespace(token, tokens, i) {
			continue
		}

		state.updateSQLFunctionState(token)
		tokenWritten := state.writeOperatorOrComma(token, tokens, i)
		if !tokenWritten {
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
			} else if canonical, ok := canonicalReceiver(token, tokens, i); ok {
				// Me/Base as member-access receivers take their canonical
				// casing (issue #90, schema R41).
				state.builder.WriteString(canonical)
				state.currentLineLen += len(canonical)
			} else if token.Type == lexer.TokenCodeBlock {
				// Code-block literals canonicalize to `{|params| expr}`
				// (schema R42, issue #91).
				normalized := normalizeCodeBlockLiteral(token.Text, opts)
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

	// Trim trailing whitespace from each line — but only when the option is
	// on (issue #39): with trimTrailingWhitespace disabled, line-end
	// whitespace (in practice, inside multi-line comment content) survives.
	if opts.TrimTrailingWhitespace {
		formatted = trimTrailingWhitespacePerLine(formatted)
	}

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

	// Never append a semicolon to raw region-body text (issue #164).
	if lastToken.Type == lexer.TokenRegionBody {
		return false
	}

	// Don't add after opening delimiters (incomplete expression)
	if isOpenParen(lastToken) {
		return false
	}

	// Don't add after operators (incomplete expression) — except postfix
	// increment/decrement, which end a complete statement (issue #99).
	if lastToken.Type == lexer.TokenOperator && lastToken.Text != "++" && lastToken.Text != "--" {
		return false
	}
	if lastToken.Text == ":=" {
		return false
	}

	// Don't add after comma (in parameter list)
	if lastToken.Text == "," {
		return false
	}

	// Don't add after keywords that don't end statements. :TO/:STEP are
	// mid-:FOR continuations; the declaration keywords take an operand
	// list that legitimately starts on the next line (`:PARAMETERS ⏎
	// name1, name2;`, corpus-observed) — a forced semicolon truncates the
	// statement and orphans the list (issue #216 review residual).
	if lastToken.Type == lexer.TokenKeyword {
		keyword := strings.ToUpper(strings.TrimPrefix(lastToken.Text, ":"))
		nonStatementEndingKeywords := map[string]bool{
			"TO":         true,
			"STEP":       true,
			"PARAMETERS": true,
			"DECLARE":    true,
			"PUBLIC":     true,
			"DEFAULT":    true,
			"INCLUDE":    true,
			"INHERIT":    true,
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
	// Postfix increment/decrement end a statement (issue #99).
	if token.Text == "++" || token.Text == "--" {
		return true
	}
	// Keywords that end statements
	if token.Type == lexer.TokenKeyword {
		return true
	}
	return false
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
	text = wrapLongLines(text, opts)
	if opts.BuiltinFunctionCase == "PascalCase" {
		text = canonicalizeBuiltinCasing(text)
	}
	if opts.TrimTrailingWhitespace {
		text = trimTrailingWhitespacePerLine(text)
	}
	if opts.BlankLineBetweenBlocks {
		text = blankLineBetweenSiblingBlocks(text)
	}
	if opts.MaxConsecutiveBlankLines > 0 {
		text = capConsecutiveBlankLines(text, opts.MaxConsecutiveBlankLines)
	}
	return text
}

// innerBlockOpeners and innerBlockClosers cover the control-flow constructs
// that need vertical separation. :PROCEDURE/:REGION are excluded — they're
// handled by BlankLinesBetweenProcs in the streaming formatter.
var innerBlockOpeners = map[string]bool{
	"IF":        true,
	"WHILE":     true,
	"FOR":       true,
	"BEGINCASE": true,
	"TRY":       true,
}

var innerBlockClosers = map[string]bool{
	"ENDIF":    true,
	"ENDWHILE": true,
	"NEXT":     true,
	"ENDCASE":  true,
	"ENDTRY":   true,
}

// blankLineBetweenSiblingBlocks inserts a blank line between adjacent
// control-flow blocks at the same indent, so that two `:IF / :ENDIF` siblings
// read as separate units. A blank line is inserted only when the previous
// non-blank line is the closer of an inner block and the current non-blank
// line is the opener of another inner block at the *same* leading indent, and
// no blank line already separates them.
func blankLineBetweenSiblingBlocks(text string) string {
	lines := strings.Split(text, "\n")
	out := make([]string, 0, len(lines)+8)

	prevContent := ""
	prevContentIdx := -1 // index in `out` where the previous non-blank line was written
	blankSinceLastContent := true

	for _, line := range lines {
		if strings.TrimSpace(line) == "" {
			out = append(out, line)
			blankSinceLastContent = true
			continue
		}

		if prevContentIdx >= 0 && !blankSinceLastContent {
			if leadingIndentString(prevContent) == leadingIndentString(line) {
				prevKey := firstKeyword(prevContent)
				currKey := firstKeyword(line)
				if innerBlockClosers[prevKey] && innerBlockOpeners[currKey] {
					out = append(out, "")
				}
			}
		}

		out = append(out, line)
		prevContent = line
		prevContentIdx = len(out) - 1
		blankSinceLastContent = false
	}

	return strings.Join(out, "\n")
}

// leadingIndentString returns the leading whitespace of a line.
func leadingIndentString(line string) string {
	for i := 0; i < len(line); i++ {
		c := line[i]
		if c != ' ' && c != '\t' {
			return line[:i]
		}
	}
	return line
}

// firstKeyword returns the uppercase keyword that starts the line (without the
// leading colon), or "" if the line doesn't start with a colon-prefixed
// keyword. Trailing characters (parameters, semicolons) are ignored.
func firstKeyword(line string) string {
	trimmed := strings.TrimLeft(line, " \t")
	if !strings.HasPrefix(trimmed, ":") {
		return ""
	}
	rest := trimmed[1:]
	end := 0
	for end < len(rest) {
		c := rest[end]
		isAlpha := (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
		if !isAlpha {
			break
		}
		end++
	}
	if end == 0 {
		return ""
	}
	return strings.ToUpper(rest[:end])
}

// trimTrailingWhitespacePerLine removes trailing space/tab characters from
// every line. Final newline (if any) is preserved.
func trimTrailingWhitespacePerLine(text string) string {
	// String literals are user content (feature.formatting A3): a
	// line-end that falls inside a multi-line string token must keep its
	// trailing whitespace — trimming there rewrites string bytes (issue
	// #216 review residual: 343 corpus files). Lex the output once and
	// only trim line-ends outside string/code-block/region-body spans.
	inLiteral := make(map[int]bool) // newline offset -> inside a literal
	for _, tok := range lexer.NewLexer(text).Tokenize() {
		switch tok.Type {
		case lexer.TokenString, lexer.TokenCodeBlock, lexer.TokenRegionBody:
			for j, r := range tok.Text {
				if r == '\n' {
					inLiteral[tok.Offset+j] = true
				}
			}
		}
	}

	var b strings.Builder
	b.Grow(len(text))
	lineStart := 0
	runes := []rune(text)
	// The lexer counts offsets in runes; walk runes so offsets line up.
	for i := 0; i <= len(runes); i++ {
		atEnd := i == len(runes)
		if !atEnd && runes[i] != '\n' {
			continue
		}
		line := string(runes[lineStart:i])
		if atEnd || !inLiteral[i] {
			line = strings.TrimRight(line, " \t")
		}
		b.WriteString(line)
		if !atEnd {
			b.WriteByte('\n')
		}
		lineStart = i + 1
	}
	return b.String()
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
// canonical PascalCase form. A "call site" is an identifier followed by `(`,
// optionally with intervening spaces/tabs. We only rewrite identifiers whose
// lowercased form matches a published built-in; user-defined functions and
// identifiers used as arguments are left alone. The pass re-lexes the
// formatted text so string literals and comments are literal text and never
// rewritten (issue #34).
func canonicalizeBuiltinCasing(text string) string {
	canonical := constants.CanonicalFunctionNames()
	if len(canonical) == 0 {
		return text
	}
	tokens := lexer.NewLexer(text).Tokenize()
	var b strings.Builder
	b.Grow(len(text))
	for i, tok := range tokens {
		if tok.Type == lexer.TokenEOF {
			break
		}
		if tok.Type == lexer.TokenIdentifier && isCallSite(tokens, i) {
			if pascal, ok := canonical[strings.ToLower(tok.Text)]; ok {
				b.WriteString(pascal)
				continue
			}
		}
		b.WriteString(tok.Text)
	}
	return b.String()
}

// isCallSite reports whether the identifier at index i is immediately
// followed by `(`, allowing only same-line spaces/tabs in between.
func isCallSite(tokens []lexer.Token, i int) bool {
	for j := i + 1; j < len(tokens); j++ {
		t := tokens[j]
		if t.Type == lexer.TokenWhitespace {
			if strings.ContainsAny(t.Text, "\n\r") {
				return false
			}
			continue
		}
		return t.Text == "("
	}
	return false
}

// isContinuationOperator reports whether a token at line start continues the
// previous expression: the binary operators the wrapper breaks before
// (fmt.max_line_length). Statements never begin with these (issue #86).
func isContinuationOperator(token lexer.Token) bool {
	if token.Type != lexer.TokenOperator {
		return false
	}
	switch strings.ToUpper(token.Text) {
	case ".AND.", ".OR.", ".NOT.",
		"+", "-", "*", "/", "%", "^", "**",
		"+=", "-=", "*=", "/=", "%=", "^=",
		"$":
		return true
	}
	return false
}

// hasUnterminatedString reports whether the token stream ends in a string
// literal missing its closing delimiter — only the final string token can be
// unterminated, since an unclosed string consumes to end of file (issue #87).
func hasUnterminatedString(tokens []lexer.Token) bool {
	for i := len(tokens) - 1; i >= 0; i-- {
		t := tokens[i]
		if t.Type == lexer.TokenEOF || t.Type == lexer.TokenWhitespace {
			continue
		}
		if t.Type != lexer.TokenString {
			return false
		}
		if len(t.Text) < 2 {
			return true
		}
		open := t.Text[0]
		close := byte('"')
		switch open {
		case '\'':
			close = '\''
		case '[':
			close = ']'
		}
		return t.Text[len(t.Text)-1] != close
	}
	return false
}

// canonicalDotOperator uppercases the dot-wrapped logical operators
// (.and. -> .AND.), which lex as TokenOperator and so never reached the
// keyword-casing branch (issue #90, schema R38). Other operators are
// returned unchanged.
func canonicalDotOperator(text string) string {
	switch strings.ToUpper(text) {
	case ".AND.", ".OR.", ".NOT.":
		return strings.ToUpper(text)
	}
	return text
}

// canonicalReceiver returns the canonical casing for the special receivers
// Me and Base when the identifier is immediately followed by a member-access
// ':' (issue #90). Ordinary identifiers — including variables that merely
// share the name — are only rewritten in receiver position, where the names
// are reserved.
func canonicalReceiver(token lexer.Token, tokens []lexer.Token, i int) (string, bool) {
	if token.Type != lexer.TokenIdentifier {
		return "", false
	}
	var canonical string
	switch strings.ToLower(token.Text) {
	case "me":
		canonical = "Me"
	case "base":
		canonical = "Base"
	default:
		return "", false
	}
	next := findNextNonWS(tokens, i)
	if next == nil || next.Type != lexer.TokenPunctuation || next.Text != ":" {
		return "", false
	}
	return canonical, true
}
