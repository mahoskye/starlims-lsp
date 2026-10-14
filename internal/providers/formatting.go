// Package providers implements LSP feature providers for SSL.
package providers

import (
	"strings"
	"unicode/utf8"

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

	formatted, ok := formatToFixedPoint(tokens, opts)
	if !ok {
		return nil
	}

	// Return a single edit replacing the entire document. Its end is the
	// last line's length in runes, the unit every provider position
	// counts in (issue #307).
	lines := strings.Split(text, "\n")
	endLine := len(lines) - 1
	endChar := 0
	if endLine >= 0 {
		endChar = utf8.RuneCountInString(lines[endLine])
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

// maxFormatPasses bounds formatToFixedPoint. Well-formed input is stable
// after one pass; the extra passes absorb a malformed shape that settles
// on the second.
const maxFormatPasses = 3

// formatToFixedPoint formats until the output formats to itself, so the
// result always satisfies feature.formatting A6 (formatting it again
// changes nothing). When no fixed point is reached within maxFormatPasses
// — malformed input the formatter keeps rewriting, such as a code block
// whose string never closes (issue #311) — it reports false and the
// caller leaves the text unchanged, as for an unterminated string.
// formatTokensToFixedPoint is formatToFixedPoint for range formatting,
// which runs the token formatter without the whole-document post passes
// (feature.formatting).
func formatTokensToFixedPoint(tokens []lexer.Token, opts FormattingOptions) (string, bool) {
	formatted := formatTokens(tokens, opts)
	for pass := 1; pass < maxFormatPasses; pass++ {
		again := lexer.NewLexer(formatted).Tokenize()
		if hasUnterminatedString(again) {
			return "", false
		}
		next := formatTokens(again, opts)
		if next == formatted {
			return formatted, true
		}
		formatted = next
	}
	return "", false
}

func formatToFixedPoint(tokens []lexer.Token, opts FormattingOptions) (string, bool) {
	formatted := applyPostFormatPasses(formatTokens(tokens, opts), opts)
	for pass := 1; pass < maxFormatPasses; pass++ {
		again := lexer.NewLexer(formatted).Tokenize()
		if hasUnterminatedString(again) {
			return "", false
		}
		next := applyPostFormatPasses(formatTokens(again, opts), opts)
		if next == formatted {
			return formatted, true
		}
		formatted = next
	}
	return "", false
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

	// A range boundary must never cut through a multi-line token: lexed on
	// its own, a line from the middle of a comment or string reads as code,
	// and formatting it (spacing, a forced ';') rewrites the literal — a ';'
	// even ends the comment early and turns the rest of it into code (issue
	// #258). Lex the whole document and widen the range to cover every
	// token that crosses either boundary.
	docTokens := lexer.NewLexer(text).Tokenize()
	if hasUnterminatedString(docTokens) {
		return nil // issue #87 — see FormatDocument
	}
	startLine, endLine = widenRangeToStatements(docTokens, startLine, endLine)

	// Expand range to include complete lines for proper formatting
	// This ensures we format complete statements
	startChar = 0
	if endLine < len(lines) {
		endChar = utf8.RuneCountInString(lines[endLine])
	}

	// Extract the text in range
	var rangeLines []string
	for i := startLine; i <= endLine && i < len(lines); i++ {
		rangeLines = append(rangeLines, lines[i])
	}
	rangeText := strings.Join(rangeLines, "\n")

	// Lines inside a multi-line token (string, comment, code block, region
	// body) are literal text: they neither set nor take the base indent,
	// or re-indenting would change a string's value (issue #325, A13).
	docMask := literalInteriorMask(docTokens, len(lines), nil)
	var codeLines []string
	for i, line := range rangeLines {
		if !docMask[startLine+i] {
			codeLines = append(codeLines, line)
		}
	}
	baseIndent := detectBaseIndent(codeLines)
	// The one line widening cannot anchor: the document's first code line
	// led by a binary operator or a comma. The formatter gives it a continuation
	// level of its own, which the base indent already holds (issue #325,
	// found by FuzzFormatRangeStable).
	if first := firstSignificant(docTokens); first != nil && first.Line-1 == startLine &&
		(isContinuationOperator(*first) || first.Text == ",") {
		baseIndent = strings.TrimSuffix(baseIndent, oneIndentLevel(opts))
	}

	// Remove the base indentation for formatting
	dedentedLines := make([]string, len(rangeLines))
	for i, line := range rangeLines {
		if docMask[startLine+i] {
			dedentedLines[i] = line
		} else {
			dedentedLines[i] = removeIndent(line, baseIndent)
		}
	}
	dedentedText := strings.Join(dedentedLines, "\n")

	// Format the dedented text. The range is whole lines, so it is lexed
	// with its line break, as the full document is: a token that runs to
	// end of input (an unclosed code block or comment) then carries the
	// same newline it does there (issue #325).
	toLex := dedentedText
	if !strings.HasSuffix(toLex, "\n") {
		toLex += "\n"
	}
	lex := lexer.NewLexer(toLex)
	tokens := lex.Tokenize()
	if hasUnterminatedString(tokens) {
		return nil // issue #87 — see FormatDocument
	}
	// Like a full format, a range converges or is declined: an input whose
	// formatting never settles (each pass appends another ';' after a code
	// block left open at end of input) is left unchanged (issue #325).
	// The range is formatted dedented, so every width decision the token
	// formatter makes would measure from column 0 and pack lines wider than
	// a full format does. Narrow each budget by the base indent the lines
	// get back, in that budget's own unit: the SSL fit test counts a tab as
	// indentSize columns, the SQL engine counts its indent string in bytes
	// (a tab as one — the SQL width defect in issue #327; change this
	// offset with it) (issue #325).
	rangeOpts := opts
	if rangeOpts.MaxLineLength > 0 {
		rangeOpts.MaxLineLength = max(1, rangeOpts.MaxLineLength-visualWidth(baseIndent, opts))
	}
	if rangeOpts.SQL.MaxLineLength > 0 {
		rangeOpts.SQL.MaxLineLength = max(1, rangeOpts.SQL.MaxLineLength-len(baseIndent))
	}
	formatted, ok := formatTokensToFixedPoint(tokens, rangeOpts)
	if !ok {
		return nil
	}

	// Re-apply the base indentation to each line
	formattedLines := strings.Split(formatted, "\n")
	outMask := verbatimInteriorMask(toLex, formatted)
	reindentedLines := make([]string, 0, len(formattedLines))
	for i, line := range formattedLines {
		if outMask[i] {
			reindentedLines = append(reindentedLines, line)
		} else if strings.TrimSpace(line) == "" {
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
	// The wrap pass sees the range's line break too, so a token running to
	// end of input is multi-line here as in the document, and freezes its
	// line the same way (issue #325, found by FuzzFormatRangeStable).
	reindentedText = strings.TrimSuffix(wrapLongLines(reindentedText+"\n", opts), "\n")

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

// firstSignificant returns the first token that is neither whitespace nor
// a comment, or nil.
func firstSignificant(tokens []lexer.Token) *lexer.Token {
	for i, t := range tokens {
		if t.Type == lexer.TokenEOF {
			return nil
		}
		if t.Type != lexer.TokenWhitespace && t.Type != lexer.TokenComment {
			return &tokens[i]
		}
	}
	return nil
}

// verbatimInteriorMask marks the lines of output that are interior lines of
// a multi-line literal token (string, comment, code block, region body)
// the formatter passed through unchanged from input. Those lines are
// literal text and must not take the range's base indent; the interior of
// a token the formatter rewrote — reflowed SQL, laid out at the range's
// local depth — still does.
func verbatimInteriorMask(input, output string) []bool {
	kept := map[string]bool{}
	for _, t := range lexer.NewLexer(input).Tokenize() {
		if isLiteralToken(t) {
			kept[t.Text] = true
		}
	}
	return literalInteriorMask(lexer.NewLexer(output).Tokenize(), strings.Count(output, "\n")+1, kept)
}

// literalInteriorMask marks the 0-based lines that begin inside a
// multi-line literal token — every line after the token's first that holds
// some of its text (a region body's closing newline leaves the
// :ENDREGION line outside it). With only non-nil, only tokens whose text
// is in only count.
func literalInteriorMask(tokens []lexer.Token, nLines int, only map[string]bool) []bool {
	mask := make([]bool, nLines)
	for _, t := range tokens {
		if !isLiteralToken(t) || (only != nil && !only[t.Text]) {
			continue
		}
		first := t.Line - 1
		last := first + strings.Count(strings.TrimSuffix(t.Text, "\n"), "\n")
		for l := first + 1; l <= last && l < nLines; l++ {
			mask[l] = true
		}
	}
	return mask
}

func isLiteralToken(t lexer.Token) bool {
	for _, typ := range literalTokenTypes {
		if t.Type == typ {
			return true
		}
	}
	return false
}

// widenRangeToStatements grows the 0-based line range [startLine, endLine]
// to whole statements, alternating with widenRangeToTokens until neither
// moves a boundary. Formatted on its own, a line from the middle of a
// statement reads as a statement: the line before a continuation gets a
// ';', an operator-led line loses its continuation anchor, and a leading
// `:Member` lexes as a keyword (issue #325, feature.formatting A2).
func widenRangeToStatements(tokens []lexer.Token, startLine, endLine int) (int, int) {
	cont, prev := statementContinuations(tokens)
	for changed := true; changed; {
		changed = false
		startLine, endLine = widenRangeToTokens(tokens, startLine, endLine)
		for {
			p, ok := prev[startLine]
			if !ok || !cont[startLine] {
				break
			}
			startLine, changed = p, true
		}
		// The first code line after the range continues it: take it in.
		next := -1
		for l := range cont {
			if l > endLine && (next < 0 || l < next) {
				next = l
			}
		}
		if next >= 0 && cont[next] {
			endLine, changed = next, true
		}
	}
	return startLine, endLine
}

// statementContinuations reports, for each 0-based line whose first token
// starts a line, whether that line continues the statement of the code
// before it (cont), and the line on which the preceding token ends (prev).
// The decision mirrors formatTokens: a line led by a binary operator or a
// member-access ':', inside an open delimiter, or after code ending in
// ':=', ',' or a binary operator always continues; otherwise a line led by
// any colon keyword but :TO/:STEP starts a statement;
// otherwise the line continues unless the last code token before it is
// ';' or is one the formatter terminates there (needsSemicolonAtLineEnd,
// which looks past comments to the next code token). Standalone comments
// sit on lines of their own and get the same decision, so widening walks
// through them; the semicolon decision ignores them, as formatTokens'
// lookahead does.
func statementContinuations(tokens []lexer.Token) (cont map[int]bool, prev map[int]int) {
	cont, prev = map[int]bool{}, map[int]int{}
	var last, lastCode lexer.Token
	haveLast, haveCode := false, false
	lastEnd := -1   // 0-based line on which `last` ends
	codeWsIdx := -1 // first newline whitespace token after lastCode
	depth := 0      // open ( { [ delimiters
	for i, t := range tokens {
		if t.Type == lexer.TokenEOF {
			break
		}
		if t.Type == lexer.TokenWhitespace {
			if haveCode && codeWsIdx < 0 && strings.Contains(t.Text, "\n") {
				codeWsIdx = i
			}
			continue
		}
		line := t.Line - 1
		if haveLast && t.Type == lexer.TokenComment && isEndOfLineComment(t, last) {
			continue // rides its code line, as in formatTokens
		}
		if line > lastEnd && haveLast {
			prev[line] = lastEnd
			// A line led by a keyword starts a statement or clause; only
			// :TO / :STEP continue one (a mid-:FOR header).
			kw := t.Type == lexer.TokenKeyword && strings.HasPrefix(t.Text, ":") &&
				!isLabelKeywordMashed(t.Text)
			if kw {
				switch strings.ToUpper(strings.TrimPrefix(t.Text, ":")) {
				case "TO", "STEP":
					kw = false
				}
			}
			switch {
			case depth > 0 || isContinuationOperator(t) || isMemberAccessColon(t) || t.Text == ",":
				cont[line] = true
			case haveCode && (lastCode.Text == ":=" || lastCode.Text == "," ||
				isContinuationOperator(lastCode) || (awaitsOperand(lastCode) && !kw)):
				// The code before ends mid-expression: writeIndentIfNeeded
				// indents this line as a continuation, keyword or not.
				cont[line] = true
			case kw || !haveCode:
				cont[line] = false
			default:
				ended := lastCode.Text == ";" ||
					(codeWsIdx >= 0 && needsSemicolonAtLineEnd(lastCode, tokens, codeWsIdx))
				cont[line] = !ended
			}
		} else if line > lastEnd {
			cont[line] = false
		}
		switch {
		case isOpenParen(t):
			depth++
		case isCloseParen(t) && depth > 0:
			depth--
		}
		last, haveLast = t, true
		lastEnd = line + strings.Count(strings.TrimSuffix(t.Text, "\n"), "\n")
		if t.Type != lexer.TokenComment {
			lastCode, haveCode = t, true
			codeWsIdx = -1
		}
	}
	return cont, prev
}

// widenRangeToTokens grows the 0-based line range [startLine, endLine] until
// no multi-line token (string, comment, code block, region body) straddles
// either boundary.
func widenRangeToTokens(tokens []lexer.Token, startLine, endLine int) (int, int) {
	for changed := true; changed; {
		changed = false
		for _, t := range tokens {
			if t.Type == lexer.TokenWhitespace || t.Type == lexer.TokenEOF ||
				!strings.Contains(t.Text, "\n") {
				continue
			}
			// A token ending in a newline (one that runs to end of input
			// takes the final line break) does not occupy the next line.
			first := t.Line - 1
			last := first + strings.Count(strings.TrimSuffix(t.Text, "\n"), "\n")
			if last < startLine || first > endLine {
				continue
			}
			if first < startLine {
				startLine, changed = first, true
			}
			if last > endLine {
				endLine, changed = last, true
			}
		}
	}
	return startLine, endLine
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
	beforeLastNonWS       lexer.Token // the code token before lastNonWSToken
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
	// orphanColon is true while the last code token is a `:` punctuation
	// token with no member-access receiver before it. The space
	// after such a colon is kept: `: a` -> `:a` would re-lex as a keyword
	// on the next pass (issue #256, found by FuzzFormat).
	orphanColon bool
	// tokens and tokenIndex expose the token stream and the position of
	// the token being formatted to helpers called without them.
	tokens                []lexer.Token
	tokenIndex            int
	pendingStatementBreak bool
	inErrorHandler        bool // Tracks :ERROR scope-based handler for indent
	errorBodyIndent       int  // indent level of the :ERROR / :RESUME body
	// commentBlockStart is the builder offset where the current run of
	// standalone comment lines began, or -1 when the output does not
	// currently end in such a run. A blank line or any non-comment content
	// breaks the run. Used by normalizeProcBoundary (issue #33) to place
	// procedure-separating blank lines above an attached doc comment.
	commentBlockStart int
	// stmtIndentWidth is the visual width of the indent written for the
	// current statement's first line.
	stmtIndentWidth int
	// declPending is true from a declaration keyword (:DECLARE,
	// :PARAMETERS, :PUBLIC) that began its line until the next token, which
	// is the list's first item when it shares that line.
	declPending bool
	// declListAlign is the column, counted from the statement's indent, of
	// the first item of the declaration list being written; its hanging
	// lines align there (wrapped_list_alignment). 0 when not in such a list.
	declListAlign int
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
	// Only the block end that closes the block holding the :ERROR —
	// reached at the handler body's own depth — ends the handler; a closer
	// of a block nested in the handler body does not (issue #326).
	// :ENDFOR is not SSL (endfor_invalid reports it), but the validator
	// pairs it with its :FOR; indenting it as a closer keeps the rest of
	// the file at its depth instead of one level too deep (issue #326).
	if constants.IsBlockEndKeyword(normalized) || normalized == "ENDFOR" {
		if s.inErrorHandler && s.indent == s.errorBodyIndent {
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
		// Any top-level unit boundary — :ENDPROC; or :ENDREGION; followed by
		// :PROCEDURE or :REGION — is normalized; mixed pairs used to be
		// left unseparated (issue #255; schema blank_lines.before_procedure
		// and before_region are both 1).
		if s.afterEndProc || s.afterEndRegion {
			s.normalizeProcBoundary()
		}
		s.inProcedure = true
		s.afterEndProc = false
		s.afterEndRegion = false
	case "ENDPROC":
		s.inProcedure = false
		s.afterEndProc = true
	case "REGION":
		if s.afterEndRegion || s.afterEndProc {
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

	// A glued boundary with no separator (":ENDPROC:PROCEDURE") becomes a
	// line end here, so it takes the semicolon a source newline would have
	// given it — otherwise the next pass adds it (issue #256, found by
	// FuzzFormat).
	if !s.pendingStatementBreak && !s.lineStart && s.opts.SemicolonEnforcement &&
		needsSemicolonAtLineEnd(s.lastNonWSToken, s.tokens, s.tokenIndex-1) {
		// Drop the separator space already written (`:ENDPROC :PROCEDURE`).
		built := strings.TrimRight(s.builder.String(), " \t")
		s.builder.Reset()
		s.builder.WriteString(built)
		s.builder.WriteString(";")
		if s.commentBlockStart > len(built) {
			s.commentBlockStart = len(built)
		}
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
		// As after a source newline: an unclosed delimiter continues
		// (issue #256, found by FuzzFormat).
		if s.parenDepth > 0 {
			s.continuationIndent = 1
		} else {
			s.continuationIndent = 0
		}
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
		// A trailing comma also continues the statement — declaration
		// lists (`:DECLARE a, b, ⏎ c;`) have no delimiters, so without
		// this the wrap engine's +1 fragment flattened to block indent on
		// the second pass and oscillated (issue #218).
		isContinuation := s.continuationIndent > 0 ||
			isContinuationOperator(token) ||
			token.Text == "," || // a comma-led line continues its list (issue #326)
			(isMemberAccessColon(token) && s.colonHasReceiver()) ||
			s.lastNonWSToken.Text == ":=" ||
			s.lastNonWSToken.Text == "," ||
			isContinuationOperator(s.lastNonWSToken) ||
			(awaitsOperand(s.lastNonWSToken) && token.Type != lexer.TokenKeyword)
		s.declPending = false
		if isContinuation && s.declListAlign > 0 {
			// A hanging line of a wrapped declaration list aligns with the
			// list's first item: the statement's indent, then spaces
			// (style guide wrapped_list_alignment, ssl-style-guide#120).
			s.currentLineLen = writeIndentLen(s.builder, s.lastLineIndent, s.opts)
			s.builder.WriteString(strings.Repeat(" ", s.declListAlign))
			s.currentLineLen += s.declListAlign
			s.lineStart = false
			return
		}
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
			s.declListAlign = 0
		}
		s.currentLineLen = writeIndentLen(s.builder, totalIndent, s.opts)
		if !isContinuation {
			s.stmtIndentWidth = s.currentLineLen
		}
		s.lineStart = false
	}
}

// noteDeclListStart records where a declaration list's first item sits,
// called for each written token before it is written. Only an item on the
// keyword's own line fixes the column; a list that starts on the next line
// (`:DECLARE ⏎ a, b;`) has nothing to align with and keeps the ordinary
// one-level continuation indent.
func (s *formatState) noteDeclListStart(token lexer.Token) {
	if !s.declPending {
		return
	}
	s.declPending = false
	if token.Type != lexer.TokenComment {
		s.declListAlign = s.currentLineLen - s.stmtIndentWidth
	}
}

// declListKeywords open a comma-separated declaration list whose wrapped
// lines align under the first item (wrapped_list_alignment).
var declListKeywords = map[string]bool{
	"DECLARE": true, "PARAMETERS": true, "PUBLIC": true,
}

// isDeclListKeyword reports whether tok is a colon-prefixed declaration-list
// keyword.
func isDeclListKeyword(tok lexer.Token) bool {
	return tok.Type == lexer.TokenKeyword && strings.HasPrefix(tok.Text, ":") &&
		declListKeywords[strings.ToUpper(strings.TrimPrefix(tok.Text, ":"))]
}

func (s *formatState) flushPendingStatementBreak(token lexer.Token) {
	if !s.pendingStatementBreak || token.Type == lexer.TokenWhitespace || token.Type == lexer.TokenComment {
		return
	}
	// An empty statement — a stray ';' after a ';' — stays on its
	// statement's line (`:IF .T.;;`); breaking before each one turned a
	// typo into lines holding a lone ';' (issue #326).
	if token.Text == ";" && s.lastNonWSToken.Text == ";" {
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
	// Mirror the source-newline path: the next pass sees a real newline
	// here and indents an unclosed delimiter's continuation (issue #256,
	// found by FuzzFormat).
	if s.parenDepth > 0 {
		s.continuationIndent = 1
	} else {
		s.continuationIndent = 0
	}
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
		// Unary operators attach to their operand (fmt.operator_spacing):
		// a source space after `!`, a unary sign or a prefix `++`/`--`,
		// or before a postfix `++`/`--`, is dropped (issue #326).
		if next := findNextNonWS(tokens, index); next != nil && s.attachesUnary(*next) {
			s.prevToken = token
			return true
		}
		// Suppress space before ( in function calls: "MyFunc (" -> "MyFunc("
		if s.lastNonWSToken.Type == lexer.TokenIdentifier {
			if next := findNextNonWS(tokens, index); next != nil && next.Text == "(" {
				s.prevToken = token
				return true
			}
		}
		// Suppress space around : in member access: "obj : prop" -> "obj:prop"
		// (colon as TokenPunctuation is member access; := is TokenOperator, :IF is TokenKeyword)
		// Keep the space when the next token starts with `=`: `: =` glued
		// re-lexes as `:=` (issue #256, found by FuzzFormat).
		if isMemberAccessColon(s.lastNonWSToken) && !s.orphanColon {
			if next := findNextNonWS(tokens, index); next == nil || !strings.HasPrefix(next.Text, "=") {
				s.prevToken = token
				return true
			}
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
		// Suppress trailing space at end of input: the EOF semicolon
		// insertion (issue #38) would otherwise land after it ("x ;"), and
		// the next pass collapses that to "x;" — not idempotent (A6).
		if next := findNextNonWS(tokens, index); next == nil || next.Type == lexer.TokenEOF {
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
		// Suppress space before a same-line single-line comment: the
		// pending-EOL-comment flush writes the canonical two-space
		// separator itself, so source whitespace here compounded into a
		// third space on the next pass (issue #218).
		if next := findNextNonWS(tokens, index); next != nil &&
			next.Type == lexer.TokenComment && isSingleLineComment(next.Text) {
			s.prevToken = token
			return true
		}
		s.builder.WriteString(" ")
		s.currentLineLen++
	}

	s.prevToken = token
	return true
}

// gluedBefore reports whether a colon keyword follows code on its line
// with no space between them.
func (s *formatState) gluedBefore(kw lexer.Token) bool {
	p := s.prevToken
	return !s.lineStart && !s.lineHasOnlyIndent() && p.Type != 0 &&
		p.Type != lexer.TokenWhitespace && !isOpenParen(p) && p.Line == kw.Line
}

// gluedAfter reports whether the colon keyword at tokens[i] is directly
// followed by an operand on its line — anything but whitespace, ';', a
// closing delimiter or a comma.
func gluedAfter(tokens []lexer.Token, i int) bool {
	if i+1 >= len(tokens) {
		return false
	}
	n := tokens[i+1]
	return n.Type != lexer.TokenWhitespace && n.Type != lexer.TokenEOF &&
		n.Type != lexer.TokenComment && n.Text != ";" && n.Text != "," && !isCloseParen(n)
}

// attachesUnary reports whether the source space between the last code
// token and next separates a unary operator from its operand, so it is
// dropped. The space stays wherever gluing would re-lex as a different
// token (`! =` -> `!=`, `- -5` -> `--5`).
func (s *formatState) attachesUnary(next lexer.Token) bool {
	if !s.opts.OperatorSpacing || next.Type == lexer.TokenComment || next.Type == lexer.TokenEOF {
		return false
	}
	last, before := s.lastNonWSToken, s.beforeLastNonWS
	operandEnd := func(t lexer.Token) bool {
		return t.Type == lexer.TokenIdentifier || t.Text == ")" || t.Text == "]"
	}
	switch {
	case (next.Text == "++" || next.Text == "--") && operandEnd(last):
		return true // postfix: `nA ++` -> `nA++`
	case last.Type == lexer.TokenOperator && last.Text == "!":
		return !strings.HasPrefix(next.Text, "=")
	case (last.Text == "-" || last.Text == "+") && before.Type != 0 &&
		before.Line == last.Line && isUnaryContext(before, false) &&
		// A value keyword (NIL, .T.) is an operand: the sign after it is binary.
		!(before.Type == lexer.TokenKeyword && !strings.HasPrefix(before.Text, ":")):
		return !strings.ContainsAny(next.Text[:1], "+-=")
	case (last.Text == "++" || last.Text == "--") && !operandEnd(before):
		return !strings.ContainsAny(next.Text[:1], "+-=") // prefix: `++ nA`
	}
	return false
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

// colonHasReceiver reports whether a `:` punctuation token written next
// follows something that can end a member-access receiver (an identifier,
// `)` or `]`) — the lexer's own test for member access versus a keyword.
// Without a receiver, removing the space after the colon (`: a` -> `:a`)
// or giving it continuation indent makes the next pass lex a keyword
// instead, and the two passes disagree (issue #256, found by FuzzFormat).
func (s *formatState) colonHasReceiver() bool {
	t := s.lastNonWSToken
	return t.Type == lexer.TokenIdentifier ||
		(t.Type == lexer.TokenPunctuation && (t.Text == ")" || t.Text == "]"))
}

// lineHasOnlyIndent reports whether the output line being built holds
// nothing but indentation so far.
func (s *formatState) lineHasOnlyIndent() bool {
	built := s.builder.String()
	tail := built[strings.LastIndex(built, "\n")+1:]
	return strings.TrimLeft(tail, " \t") == ""
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
		// A comment ends the line's content, so an operator glued to one
		// gets no leading space here. The same token reached on the next
		// pass has source whitespace before it and goes through the
		// whitespace handler, which emits indentation instead — the two
		// paths must agree or the forms oscillate between passes, the
		// same failure issue #218 fixed for commas. The shape that hits
		// this is an orphan `*/` after a comment SSL already terminated
		// at its `;` (`/* note;*/`), which lexes as `*` then `/`
		// (issue #249).
		// A comma glued to the operator (`,#`) already wrote its trailing
		// space under CommaSpacing, for the same reason (issue #256, found
		// by FuzzFormat).
		gluedComma := s.opts.CommaSpacing && s.prevToken.Text == ","
		// Nor after a member-access colon (`:#`): the whitespace handler
		// drops the space after `:`, so adding one here oscillated.
		// Nor does an operator that opens a line after a statement break
		// (`;#` splits to two lines): only indentation precedes it.
		if !s.lineStart && !s.lineHasOnlyIndent() && s.prevToken.Type != lexer.TokenWhitespace &&
			s.prevToken.Type != lexer.TokenOperator && !gluedComma &&
			s.prevToken.Type != lexer.TokenComment && !isOpenParen(s.prevToken) &&
			!(isMemberAccessColon(s.prevToken) && !s.orphanColon) {
			s.builder.WriteString(" ")
			s.currentLineLen++
		}
		opText := canonicalDotOperator(token.Text)
		s.builder.WriteString(opText)
		s.currentLineLen += len(opText)
		if index+1 < len(tokens) {
			next := tokens[index+1]
			// No space before a comma either when the whitespace handler
			// would drop it (issue #256, found by FuzzFormat).
			// The invalid `===` / `!==` lex as `==`/`!=` then `=`; spacing
			// them apart turns invalid_operator_sequence's one operator
			// into two and adds an unexpected_token error, so a glued pair
			// stays glued (issue #326).
			gluedStrictEquals := (token.Text == "==" || token.Text == "!=") && next.Text == "="
			if next.Type != lexer.TokenWhitespace && !isCloseParen(next) && next.Text != ";" &&
				!(s.opts.CommaSpacing && next.Text == ",") && !trailingComment(tokens, index+1) &&
				next.Type != lexer.TokenEOF && !gluedStrictEquals {
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
			// No space before closing delimiters or ';' — the whitespace
			// handler suppresses these when source whitespace exists, so
			// the no-whitespace path must agree or the two forms oscillate
			// between passes (`,,)` <-> `,, )`, issue #218).
			// Nor before a same-line comment: the pending-EOL-comment flush
			// writes the canonical two-space separator itself, so a space
			// here became a third one that the next pass dropped (issue #258).
			next := tokens[index+1]
			if next.Type != lexer.TokenWhitespace && next.Type != lexer.TokenEOF &&
				next.Text != "," && !isCloseParen(next) && next.Text != ";" && !trailingComment(tokens, index+1) {
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
	// It is measured in columns, a tab as indentSize, like the indent
	// writeIndentLen counts; counting bytes made a range format, which
	// narrows its budget by its base indent in columns, disagree with a
	// full format on the next string's fit (issue #325 follow-up, found
	// by FuzzFormatRangeStable).
	if idx := strings.LastIndex(formattedSQL, "\n"); idx >= 0 {
		s.currentLineLen = visualWidth(formattedSQL[idx+1:], s.opts)
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
			s.errorBodyIndent = s.indent
		}
	}

	if token.Text == ";" {
		s.pendingStatementBreak = true
		s.prevKeyword = ""
		s.declListAlign = 0
	}

	// A declaration keyword that began its line opens an alignable list.
	if isDeclListKeyword(token) && s.currentLineLen-len(token.Text) == s.stmtIndentWidth {
		s.declPending = true
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

	s.orphanColon = isMemberAccessColon(token) && !s.colonHasReceiver()
	s.prevToken = token
	s.beforeLastNonWS = s.lastNonWSToken
	s.lastNonWSToken = token
}

// formatTokens formats tokens according to options.
func formatTokens(tokens []lexer.Token, opts FormattingOptions) string {
	state := newFormatState(opts)

	state.tokens = tokens
	for i, token := range tokens {
		state.tokenIndex = i
		// A leading byte-order mark is an encoding marker the lexer keeps
		// as zero-width whitespace; write it back untouched (issue #255).
		if i == 0 && token.Type == lexer.TokenWhitespace && token.Text == "\uFEFF" {
			state.builder.WriteString(token.Text)
			continue
		}
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
			if isEndOfLineComment(token, state.lastNonWSToken) {
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
				// The comment moves to the line end; when it was the only
				// separator between two words (`A/*;B`, `A/*;:IF`), keep
				// one space or the words fuse into different tokens on the
				// next pass (issue #256, found by FuzzFormat).
				if i+1 < len(tokens) && wordLike(state.lastNonWSToken) && wordLike(tokens[i+1]) {
					state.builder.WriteString(" ")
					state.currentLineLen++
				}
				continue
			}
			// A multi-line comment starting on the same line as a pending
			// end-of-line comment is written now, so flush the pending one
			// first: deferring it put it after the later comment, swapping
			// their order (issue #256, found by FuzzFormat).
			if state.pendingComment != nil {
				state.builder.WriteString("  ")
				state.builder.WriteString(state.pendingComment.Text)
				state.pendingComment = nil
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
		// The statement break is flushed before this token's own delimiter
		// depth is counted, as a source newline would be.
		state.flushPendingStatementBreak(token)
		state.updateParenDepth(token)
		state.writeIndentIfNeeded(token)
		if state.handleWhitespace(token, tokens, i) {
			continue
		}

		state.noteDeclListStart(token)
		state.updateSQLFunctionState(token)
		tokenWritten := state.writeOperatorOrComma(token, tokens, i)
		if !tokenWritten {
			tokenWritten = state.writeTokenWithSQLFormatting(token)
		}
		if !tokenWritten {
			// A joined `:LABELName` is written verbatim. It stores the label
			// text `LABELName`, which Branch() matches exactly; `:LABEL Name`
			// would store `LABEL Name` and send every Branch("LABELName") to a
			// label that no longer exists (reference keywords/LABEL.md).
			if token.Type == lexer.TokenKeyword && isLabelKeywordMashed(token.Text) {
				state.builder.WriteString(token.Text)
				state.currentLineLen += len(token.Text)
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
				// Colon-prefixed keyword: :if -> :IF, :endIf -> :ENDIF.
				// A keyword glued to the code around it is spaced apart
				// (`1:TO 5`, `:STEP-1`, `:IF(j > 1)`; schema
				// spaces.before_step_keyword, issue #326).
				known := constants.IsKeyword(strings.ToUpper(strings.TrimPrefix(token.Text, ":")))
				if known && state.gluedBefore(token) {
					state.builder.WriteString(" ")
					state.currentLineLen++
				}
				normalized := ":" + strings.ToUpper(strings.TrimPrefix(token.Text, ":"))
				state.builder.WriteString(normalized)
				state.currentLineLen += len(normalized)
				if known && gluedAfter(tokens, i) {
					state.builder.WriteString(" ")
					state.currentLineLen++
				}
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

// trailingComment reports whether tokens[idx] is a single-line comment that
// ends its line (only a newline or end of input follows). Glued to the
// token before it, such a comment rides the end-of-line path, whose flush
// writes the canonical two-space separator itself — a space written there
// as well became three, then two on the next pass (issue #256, found by
// FuzzFormat; the whitespace-handler side is issue #218). A comment with
// code after it on the line still needs the space, for that code.
func trailingComment(tokens []lexer.Token, idx int) bool {
	if idx >= len(tokens) || tokens[idx].Type != lexer.TokenComment || !isSingleLineComment(tokens[idx].Text) {
		return false
	}
	if idx+1 >= len(tokens) {
		return true
	}
	after := tokens[idx+1]
	if after.Type == lexer.TokenWhitespace && strings.Contains(after.Text, "\n") {
		return true
	}
	next := findNextNonWS(tokens, idx)
	return next == nil || next.Type == lexer.TokenEOF
}

// wordLike reports whether a token is an identifier, number or keyword —
// a token that fuses with an adjacent word when nothing separates them.
func wordLike(t lexer.Token) bool {
	return t.Type == lexer.TokenIdentifier || t.Type == lexer.TokenNumber || t.Type == lexer.TokenKeyword
}

// isEndOfLineComment checks if a comment token is an end-of-line comment
// (a comment on the same line as code, following code).
func isEndOfLineComment(comment lexer.Token, lastNonWSToken lexer.Token) bool {
	// If no code was written yet, this is not an end-of-line comment
	if lastNonWSToken.Type == 0 {
		return false
	}

	// If the comment is on the same line as the last non-whitespace token, it's an end-of-line comment
	if comment.Line == lastNonWSToken.Line {
		// Also check that the comment doesn't contain newlines (is single-line)
		if isSingleLineComment(comment.Text) {
			return true
		}
	}

	return false
}

// isSingleLineComment reports whether a comment's text spans one line. An
// unterminated comment runs to end of input and so carries the file's
// final newline (`/*` then EOF, once the formatter has added the newline);
// that trailing newline does not make it multi-line, or a same-line
// unterminated comment flipped between end-of-line and standalone layout
// across passes (issue #256, found by FuzzFormat).
func isSingleLineComment(text string) bool {
	return !strings.Contains(strings.TrimRight(text, "\r\n"), "\n")
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

		// :BEGINCASE takes no operand, so it is a complete statement on its
		// own and ends with a semicolon regardless of what follows. Without
		// this it is the one block opener that never gets one: the token
		// after it is always :CASE or :OTHERWISE, both continuation
		// keywords, so the lookahead below returns false. Every sibling
		// opener (:IF, :TRY, :WHILE, :PROCEDURE) already gets a semicolon
		// because nothing continues it.
		if keyword == "BEGINCASE" {
			return true
		}

		// A keyword that takes an operand, ending its line, has that
		// operand on the next line (`:IF ⏎ bReady;`, `:RETURN ⏎ nValue;`).
		// The compiler reads the next line as the operand, so a forced ';'
		// would turn `:IF` into a compile error and `:RETURN` into a bare
		// return — the same truncation issue #216 fixed for the
		// declaration keywords (issue #255). A keyword on the next line
		// means the operand really is absent, and the lookahead below
		// still terminates the statement.
		if operandKeywords[keyword] {
			if next := findNextSignificant(tokens, wsIndex); next != nil && next.Type != lexer.TokenKeyword {
				return false
			}
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

			// Only :TO / :STEP continue a statement (a :FOR header split
			// across lines). The middle keywords :ELSE, :CATCH, :FINALLY,
			// :CASE and :OTHERWISE begin a clause of their own, so the
			// statement before them ends here; treating them as
			// continuations left it unterminated, a compile error the
			// formatter kept (issue #326).
			if keyword == "TO" || keyword == "STEP" {
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

// operandKeywords take an operand that may legitimately start on the next
// line. The declaration keywords are handled separately (no ';' at all).
var operandKeywords = map[string]bool{
	"IF": true, "WHILE": true, "CASE": true, "FOR": true, "RETURN": true,
	"PROCEDURE": true, "CLASS": true, "LABEL": true, "REGION": true,
	"BEGININLINECODE": true,
}

// awaitsOperand reports whether tok is an operand-taking keyword that ended
// its line without its operand, so the next line (when it does not itself
// start with a keyword) is that operand — a continuation, not a statement.
func awaitsOperand(tok lexer.Token) bool {
	if tok.Type != lexer.TokenKeyword || !strings.HasPrefix(tok.Text, ":") {
		return false
	}
	k := strings.ToUpper(strings.TrimPrefix(tok.Text, ":"))
	return operandKeywords[k] || declarationOperandKeywords[k]
}

// declarationOperandKeywords take an operand list that may start on the
// next line (`:PARAMETERS ⏎ sA, sB;`); that list is a continuation, one
// level deep throughout, rather than its first line sitting at the
// keyword's depth and the rest one deeper (issue #326). Their ';' handling
// is needsSemicolonAtLineEnd's.
var declarationOperandKeywords = map[string]bool{
	"PARAMETERS": true, "DECLARE": true, "PUBLIC": true, "DEFAULT": true,
	"INCLUDE": true, "INHERIT": true,
}

// findNextSignificant returns the next token after startIdx that is neither
// whitespace nor a comment, or nil.
func findNextSignificant(tokens []lexer.Token, startIdx int) *lexer.Token {
	for i := startIdx + 1; i < len(tokens); i++ {
		if tokens[i].Type != lexer.TokenWhitespace && tokens[i].Type != lexer.TokenComment {
			return &tokens[i]
		}
	}
	return nil
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
	// A joined `:LABELName` ends with its label name, an operand, so an
	// operator after it is not awaiting one (issue #256, found by
	// FuzzFormat).
	if lastNonWS.Type == lexer.TokenKeyword && isLabelKeywordMashed(lastNonWS.Text) {
		return false
	}
	if lastNonWS.Type == lexer.TokenKeyword {
		return true
	}
	return false
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
	mask := literalLineMask(text, literalTokenTypes...)
	out := make([]string, 0, len(lines)+8)

	prevContent := ""
	prevContentIdx := -1 // index in `out` where the previous non-blank line was written
	blankSinceLastContent := true

	for i, line := range lines {
		// A line inside a multi-line string, comment, code block or region
		// body is literal text (D7): it is neither a closer nor an opener,
		// and nothing is inserted before it (issue #255).
		if mask[i] {
			out = append(out, line)
			prevContent = ""
			prevContentIdx = -1
			blankSinceLastContent = false
			continue
		}
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

// literalLineMask lexes text and reports, per line index, whether that line
// begins inside a multi-line token of one of the given types — i.e. the
// line is a continuation of that token's content rather than a line of
// code. The line-oriented post-passes use it so they never edit literal
// text (D7, issue #255): a line inside a multi-line string or comment is
// not a block closer/opener and its blank lines are not blank-line runs.
func literalLineMask(text string, types ...lexer.TokenType) []bool {
	want := make(map[lexer.TokenType]bool, len(types))
	for _, t := range types {
		want[t] = true
	}
	// The lexer counts offsets in runes; record which rune offsets of
	// newlines fall inside a wanted token.
	inside := make(map[int]bool)
	for _, tok := range lexer.NewLexer(text).Tokenize() {
		if !want[tok.Type] {
			continue
		}
		j := 0
		for _, r := range tok.Text {
			if r == '\n' {
				inside[tok.Offset+j] = true
			}
			j++
		}
	}
	mask := make([]bool, strings.Count(text, "\n")+1)
	line := 0
	for i, r := range []rune(text) {
		if r == '\n' {
			line++
			mask[line] = inside[i]
		}
	}
	return mask
}

// literalTokenTypes are the tokens whose content is the author's literal
// bytes: strings, code blocks, :REGION bodies, and comments.
var literalTokenTypes = []lexer.TokenType{
	lexer.TokenString, lexer.TokenCodeBlock, lexer.TokenRegionBody, lexer.TokenComment,
}

// trimTrailingWhitespacePerLine removes trailing space/tab characters from
// every line. Final newline (if any) is preserved.
func trimTrailingWhitespacePerLine(text string) string {
	// String literals are user content (feature.formatting A3): a
	// line-end that falls inside a multi-line string token must keep its
	// trailing whitespace — trimming there rewrites string bytes (issue
	// #216 review residual: 343 corpus files). Comment interiors are
	// deliberately trimmed (the D7 exception for line-end whitespace).
	mask := literalLineMask(text, lexer.TokenString, lexer.TokenCodeBlock, lexer.TokenRegionBody)
	lines := strings.Split(text, "\n")
	for i := range lines {
		// Line i's end is inside a literal exactly when line i+1 starts
		// inside it; the last line always ends outside.
		if i+1 < len(lines) && mask[i+1] {
			continue
		}
		lines[i] = strings.TrimRight(lines[i], " \t")
	}
	return strings.Join(lines, "\n")
}

// capConsecutiveBlankLines collapses runs of N+1 or more blank lines (whitespace
// only) into exactly `max` blank lines. Lines that contain non-whitespace text
// are unaffected.
func capConsecutiveBlankLines(text string, max int) string {
	lines := strings.Split(text, "\n")
	mask := literalLineMask(text, literalTokenTypes...)
	out := make([]string, 0, len(lines))
	blankRun := 0
	for i, line := range lines {
		// Blank lines inside a multi-line string, comment, code block or
		// region body are literal content, never a run to cap (issue #255).
		if mask[i] {
			blankRun = 0
			out = append(out, line)
			continue
		}
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
		if tok.Type == lexer.TokenIdentifier && isCallSite(tokens, i) && !isMemberName(tokens, i) {
			if pascal, ok := canonical[strings.ToLower(tok.Text)]; ok {
				b.WriteString(pascal)
				continue
			}
		}
		b.WriteString(tok.Text)
	}
	return b.String()
}

// isMemberName reports whether the identifier at index i is the member of a
// `:` access (`oVar:upper(...)`). A member is not a built-in function call,
// and the members of built-in value types are case-sensitive native .NET
// members at runtime, so their casing is never rewritten (issue #255).
func isMemberName(tokens []lexer.Token, i int) bool {
	for j := i - 1; j >= 0; j-- {
		t := tokens[j]
		if t.Type == lexer.TokenWhitespace || t.Type == lexer.TokenComment {
			continue
		}
		return t.Type == lexer.TokenPunctuation && t.Text == ":"
	}
	return false
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

// isMemberAccessColon reports whether a token at line start is the
// member-access `:` continuing a call chain from the previous line
// (`txt:ToString()` then `:Replace(...)`); the lexer only emits the
// punctuation colon there when the previous significant token ends a
// receiver, so a statement never begins with one.
func isMemberAccessColon(token lexer.Token) bool {
	return token.Type == lexer.TokenPunctuation && token.Text == ":"
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
// The lexer's own Unterminated flag is authoritative: checking the last
// character missed a nested bracket string such as `[[a]`, which ends in
// `]` without being closed, and the formatter then wrote its `;` inside
// the string (issue #258).
func hasUnterminatedString(tokens []lexer.Token) bool {
	for i := len(tokens) - 1; i >= 0; i-- {
		t := tokens[i]
		if t.Type == lexer.TokenEOF || t.Type == lexer.TokenWhitespace {
			continue
		}
		return t.Type == lexer.TokenString && t.Unterminated
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
