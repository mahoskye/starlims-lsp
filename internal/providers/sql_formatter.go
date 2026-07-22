// Package providers implements LSP feature providers for SSL.
package providers

import (
	"strings"
)

// SQLFormattingOptions configures SQL formatting.
type SQLFormattingOptions struct {
	Enabled          bool   // Enable SQL formatting
	Style            string // "standard", "canonicalCompact", "compact", "expanded"
	KeywordCase      string // "upper", "lower", "preserve"
	IndentSize       int    // Spaces per indent level
	MaxLineLength    int    // Max line length for wrapping
	DetectSQLStrings bool   // Auto-detect SQL in any string literal
}

// SQL formatting styles:
// - "standard": Simple clause breaks (FROM, WHERE, JOIN on new lines).
// - "canonicalCompact": Balanced formatting with indented AND/OR and smart column wrapping.
// - "compact": Minimal formatting, single line where possible.
// - "expanded": Each column/condition on its own line.

// DefaultSQLFormattingOptions returns default SQL formatting options.
// Default follows the official STARLIMS Style Guide.
func DefaultSQLFormattingOptions() SQLFormattingOptions {
	return SQLFormattingOptions{
		Enabled:          true,
		Style:            "canonicalCompact",
		KeywordCase:      "upper",
		IndentSize:       4,
		MaxLineLength:    90,
		DetectSQLStrings: true,
	}
}

// SQLFormatter formats SQL strings.
type SQLFormatter struct {
	opts         SQLFormattingOptions
	indentString string
}

// NewSQLFormatter creates a new SQL formatter.
func NewSQLFormatter(opts SQLFormattingOptions) *SQLFormatter {
	return &SQLFormatter{
		opts:         opts,
		indentString: strings.Repeat(" ", opts.IndentSize),
	}
}

// FormatSQL formats a SQL string.
func (f *SQLFormatter) FormatSQL(sql string, baseIndent string) string {
	if !f.opts.Enabled {
		return sql
	}

	lexer := NewSQLLexer(sql)
	tokens := lexer.Tokenize()

	// Filter out whitespace tokens for formatting (preserve hints and comments)
	var nonWSTokens []SQLToken
	for _, t := range tokens {
		if t.Type != SQLTokenWhitespace {
			nonWSTokens = append(nonWSTokens, t)
		}
	}

	if len(nonWSTokens) == 0 {
		return sql
	}

	// Check if SQL is complex enough to need formatting
	isComplex := f.isComplexSQL(nonWSTokens)

	var result strings.Builder
	currentLineLen := len(baseIndent)
	isFirstToken := true

	// State tracking
	var currentClause string
	var rootCommand string
	parenDepth := 0
	inSelectColumns := false
	addBlankLineBeforeNextBreak := false
	subqueryParenDepths := make(map[int]bool)
	afterBetween := false // tracks BETWEEN...AND to suppress AND line break
	// Rule D: stack of column positions immediately after each open '('.
	// Used to hang-indent wrapped argument lists / IN lists under their
	// opening paren. Subquery parens (subqueryParenDepths[d]==true) are
	// excluded from hang-indent — the subquery has its own formatting.
	var parenOpenCols []int

	// CASE in SELECT tracking (Gap 6)
	caseInSelectColumns := false
	caseDepth := 0

	// OVER() clause tracking (Gap 8)
	inOverClause := false
	overParenDepth := 0

	// DECODE function tracking (Gap 7)
	inDecodeCall := false
	decodeParenDepth := 0
	decodeArgCount := 0

	for i := 0; i < len(nonWSTokens); i++ {
		t := nonWSTokens[i]
		upperText := strings.ToUpper(t.Text)

		// Track state
		if t.Type == SQLTokenKeyword {
			if rootCommand == "" && SQLCommandKeywords[upperText] {
				rootCommand = upperText
			}
			switch upperText {
			case "SELECT":
				currentClause = "SELECT"
				inSelectColumns = true
			case "UPDATE":
				currentClause = "UPDATE"
			case "INSERT":
				currentClause = "INSERT"
			case "MERGE":
				currentClause = "MERGE"
			case "FROM":
				inSelectColumns = false
				currentClause = "FROM"
			case "WHERE":
				currentClause = "WHERE"
			case "SET":
				currentClause = "SET"
			case "VALUES":
				currentClause = "VALUES"
			case "ORDER", "GROUP":
				currentClause = upperText
			case "RETURNING":
				currentClause = "RETURNING"
			}
		}

		if t.Text == "(" {
			parenDepth++
		} else if t.Text == ")" {
			parenDepth--
			if parenDepth < 0 {
				parenDepth = 0
			}
		}

		// Track BETWEEN for BETWEEN...AND suppression
		if upperText == "BETWEEN" && t.Type == SQLTokenKeyword {
			afterBetween = true
		}

		// Track CASE blocks in SELECT columns (Gap 6)
		// Note: END tracking must happen AFTER formatting decisions (below)
		// so caseInSelectColumns is still true when END is being formatted.
		if upperText == "CASE" && t.Type == SQLTokenKeyword {
			if caseDepth == 0 && inSelectColumns {
				caseInSelectColumns = true
			}
			caseDepth++
		}

		// Track OVER() clause (Gap 8)
		if upperText == "OVER" && (t.Type == SQLTokenKeyword || t.Type == SQLTokenFunction) {
			inOverClause = true
			overParenDepth = parenDepth + 1
		}
		if t.Text == ")" && inOverClause && parenDepth == overParenDepth-1 {
			inOverClause = false
		}

		// Track DECODE function (Gap 7)
		if upperText == "DECODE" && t.Type == SQLTokenFunction {
			inDecodeCall = true
			decodeParenDepth = parenDepth + 1
			decodeArgCount = 0
		}
		if t.Text == ")" && inDecodeCall && parenDepth == decodeParenDepth-1 {
			inDecodeCall = false
		}

		// Apply casing
		tokenText := f.applyKeywordCasing(t)

		// Determine if we need a line break
		needsBreak := false
		extraIndent := ""
		prev := getPrevNonWS(nonWSTokens, i)
		var prevPrevToken *SQLToken
		if i >= 2 {
			prevPrevToken = &nonWSTokens[i-2]
		}

		if !isFirstToken && isComplex {
			prevUpper := ""
			if prev != nil {
				prevUpper = strings.ToUpper(prev.Text)
			}

			style := f.opts.Style
			if style == "" {
				style = "standard"
			}

			// Break conditions for major clauses (all styles except compact)
			if style != "compact" && SQLBreakBeforeKeywords[upperText] && t.Type == SQLTokenKeyword {
				// Don't break before JOIN if preceded by modifier
				if upperText == "JOIN" && prev != nil && SQLJoinModifiers[prevUpper] {
					needsBreak = false
				} else if upperText == "INTO" && (prevUpper == "INSERT" || prevUpper == "MERGE" || prevUpper == "RETURNING" || currentClause == "RETURNING") {
					// MERGE INTO, INSERT INTO, and RETURNING ... INTO stay on one line
					needsBreak = false
				} else if upperText == "FROM" && prevUpper == "DELETE" {
					// DELETE FROM stays on one line
					needsBreak = false
				} else if upperText == "UPDATE" && prevUpper == "FOR" {
					// FOR UPDATE is a compound clause — stays on one line
					needsBreak = false
				} else if upperText == "CASE" && prev != nil && prev.Type == SQLTokenOperator {
					// CASE after = in SET clause stays inline: fldsts = CASE WHEN ...
					needsBreak = false
				} else {
					needsBreak = true
					extraIndent = f.keywordIndent(style, upperText, currentClause, rootCommand, parenDepth)
				}
			}

			// Indented keywords (AND, OR) - only for canonicalCompact and expanded styles
			if (style == "canonicalCompact" || style == "expanded") &&
				SQLIndentedKeywords[upperText] && t.Type == SQLTokenKeyword {
				// Suppress line break for AND that is part of BETWEEN...AND
				if upperText == "AND" && afterBetween {
					afterBetween = false
				} else {
					needsBreak = true
					extraIndent = f.keywordIndent(style, upperText, currentClause, rootCommand, parenDepth)
				}
			}

			// MERGE sub-statement indentation: UPDATE SET / INSERT / DELETE indented under WHEN
			if rootCommand == "MERGE" && parenDepth == 0 &&
				(style == "canonicalCompact" || style == "expanded") {
				if upperText == "UPDATE" || upperText == "INSERT" || upperText == "DELETE" {
					needsBreak = true
					extraIndent = f.indentString
				}
			}

			// SET clause formatting
			if style != "compact" && prev != nil && prevUpper == "SET" && parenDepth == 0 {
				needsBreak = true
				if style == "canonicalCompact" || style == "expanded" {
					extraIndent = f.indentString
				}
			}
			if style != "compact" && prev != nil && prev.Text == "," && currentClause == "SET" && parenDepth == 0 {
				needsBreak = true
				if style == "canonicalCompact" || style == "expanded" {
					extraIndent = f.indentString
				}
			}

			// VALUES paren — opening paren stays on VALUES line, content indented inside
			if style != "compact" && t.Text == "(" && prevUpper == "VALUES" {
				// Don't break before the paren; it stays on the VALUES line.
				// The content inside gets indented via parenDepth.
				subqueryParenDepths[parenDepth] = true
			}

			// Subquery SELECT — parenDepth already provides one level of indent
			if style != "compact" && upperText == "SELECT" && prev != nil && prev.Text == "(" {
				needsBreak = true
				subqueryParenDepths[parenDepth] = true
			}

			// Rule C (whole-projection move): at the start of a new projection
			// in SELECT columns, look ahead to the full projection's rendered
			// length. If continuing on the current line would overflow, move
			// the whole projection to its own line — better than splitting
			// the projection later and stranding pieces of it.
			if !needsBreak && prev != nil && prev.Text == "," &&
				inSelectColumns && parenDepth == 0 && f.opts.MaxLineLength > 0 &&
				(style == "canonicalCompact" || style == "expanded") {
				end := f.projectionEndIndex(nonWSTokens, i)
				projLen := f.projectionRenderLen(nonWSTokens, i, end)
				spaceLen := 0
				if f.shouldAddSpace(prev, &t, prevPrevToken) {
					spaceLen = 1
				}
				if currentLineLen+spaceLen+projLen > f.opts.MaxLineLength {
					needsBreak = true
					extraIndent = strings.Repeat(" ", 7) // align with SELECT columns
				}
			}

			// Proactive line wrapping (only for canonicalCompact and expanded)
			if !needsBreak && prev != nil && f.opts.MaxLineLength > 0 &&
				(style == "canonicalCompact" || style == "expanded") {
				spaceLen := 0
				if f.shouldAddSpace(prev, &t, prevPrevToken) {
					spaceLen = 1
				}
				projectedLen := currentLineLen + spaceLen + len(tokenText)

				canBreak := prev.Text == "," ||
					(t.Type == SQLTokenKeyword || t.Type == SQLTokenIdentifier) &&
						prev.Text != "."

				// Rule C: never split a projection from its AS alias. Block
				// wrapping immediately before AS, and immediately after AS
				// (the alias identifier must stay attached).
				if t.Type == SQLTokenKeyword && upperText == "AS" {
					canBreak = false
				}
				if prev.Type == SQLTokenKeyword && strings.ToUpper(prev.Text) == "AS" {
					canBreak = false
				}

				if projectedLen > f.opts.MaxLineLength && canBreak && prev.Text != "(" {
					needsBreak = true
					// Rule D: when wrapping inside a non-subquery argument list,
					// hang-indent under the innermost opening '('.
					hangCol := -1
					if len(parenOpenCols) > 0 && !subqueryParenDepths[parenDepth] {
						hangCol = parenOpenCols[len(parenOpenCols)-1]
					}
					if hangCol >= 0 {
						baseLen := len(baseIndent)
						parenIndentLen := len(f.indentString) * parenDepth
						spaces := hangCol - baseLen - parenIndentLen
						if spaces < 0 {
							spaces = 0
						}
						extraIndent = strings.Repeat(" ", spaces)
					} else if inSelectColumns {
						extraIndent = strings.Repeat(" ", 7) // Align with SELECT columns
					} else {
						extraIndent = f.indentString
					}
				}
			}
		}

		// SELECT after set operation needs its own line
		if !needsBreak && upperText == "SELECT" && prev != nil {
			pUpper := strings.ToUpper(prev.Text)
			if pUpper == "ALL" || pUpper == "UNION" || pUpper == "INTERSECT" || pUpper == "MINUS" || pUpper == "EXCEPT" {
				needsBreak = true
			}
		}

		// CASE/END in SELECT columns: break with col-7 alignment instead of col 0
		if needsBreak && inSelectColumns {
			if upperText == "CASE" {
				extraIndent = strings.Repeat(" ", 7) // align with SELECT columns
			} else if upperText == "END" && caseInSelectColumns {
				extraIndent = strings.Repeat(" ", 7) // END aligns with CASE
			}
		}

		// WHEN/ELSE inside CASE in SELECT columns: align at col 11 (7 + 4)
		if caseInSelectColumns && (upperText == "WHEN" || upperText == "ELSE") && t.Type == SQLTokenKeyword {
			needsBreak = true
			extraIndent = strings.Repeat(" ", 11)
		}

		// Rule B: AND/OR continuing a WHEN's predicate inside CASE-in-SELECT
		// indents one step past the WHEN keyword (col 11 + indentSize).
		if needsBreak && caseInSelectColumns && parenDepth == 0 &&
			(upperText == "AND" || upperText == "OR") && t.Type == SQLTokenKeyword {
			extraIndent = strings.Repeat(" ", 11) + f.indentString
		}

		// OVER() internal formatting: PARTITION BY / ORDER BY on their own lines (Gap 8)
		if inOverClause && parenDepth >= overParenDepth && t.Type == SQLTokenKeyword {
			if upperText == "PARTITION" || upperText == "ORDER" || upperText == "ROWS" || upperText == "RANGE" {
				needsBreak = true
				extraIndent = strings.Repeat(" ", 11)
			}
		}
		// Closing ) of OVER on its own line aligned with OVER
		if t.Text == ")" && inOverClause && parenDepth+1 == overParenDepth {
			needsBreak = true
			extraIndent = strings.Repeat(" ", 7)
		}

		// DECODE argument alignment: break after first value pair when many args (Gap 7)
		if inDecodeCall && parenDepth == decodeParenDepth {
			if prev != nil && prev.Text == "," {
				decodeArgCount++
			}
			// Break after the first value pair (arg index 3+) when there are many args
			if prev != nil && prev.Text == "," && decodeArgCount >= 3 {
				needsBreak = true
				extraIndent = strings.Repeat(" ", 14) // align after "DECODE("
			}
		}

		// Closing paren on its own line for subqueries/VALUES blocks
		if t.Text == ")" && subqueryParenDepths[parenDepth+1] {
			needsBreak = true
			delete(subqueryParenDepths, parenDepth+1)
		}

		// Blank line before set operations (sql-canonical-compact-reference §2.8)
		isSetOp := needsBreak && t.Type == SQLTokenKeyword &&
			(upperText == "UNION" || upperText == "INTERSECT" || upperText == "MINUS" || upperText == "EXCEPT")
		if isSetOp {
			addBlankLineBeforeNextBreak = true // blank line after set-op line
		}

		// Preserve SQL comments — write inline with a preceding space
		if t.Type == SQLTokenComment {
			if !isFirstToken {
				result.WriteString(" ")
				currentLineLen++
			}
			result.WriteString(t.Text)
			currentLineLen += len(t.Text)
			isFirstToken = false
			continue
		}

		// Write output
		if needsBreak {
			if isSetOp {
				result.WriteString("\n") // blank line before set operation
			}
			if addBlankLineBeforeNextBreak && !isSetOp {
				result.WriteString("\n") // blank line after set operation
				addBlankLineBeforeNextBreak = false
			}
			result.WriteString("\n")
			parenIndent := strings.Repeat(f.indentString, parenDepth)
			result.WriteString(baseIndent)
			result.WriteString(parenIndent)
			result.WriteString(extraIndent)
			currentLineLen = len(baseIndent) + len(parenIndent) + len(extraIndent)
		} else if prev != nil && f.shouldAddSpace(prev, &t, prevPrevToken) {
			result.WriteString(" ")
			currentLineLen++
		}

		result.WriteString(tokenText)
		currentLineLen += len(tokenText)
		isFirstToken = false

		// Rule D: maintain stack of opening-paren columns for hang-indent.
		// Push the column where contents begin (i.e. currentLineLen after
		// '(' is written) on '(', pop on ')'.
		if t.Text == "(" {
			parenOpenCols = append(parenOpenCols, currentLineLen)
		} else if t.Text == ")" && len(parenOpenCols) > 0 {
			parenOpenCols = parenOpenCols[:len(parenOpenCols)-1]
		}

		// Post-token: decrement CASE depth after END has been formatted
		if upperText == "END" && t.Type == SQLTokenKeyword && caseDepth > 0 {
			caseDepth--
			if caseDepth == 0 {
				caseInSelectColumns = false
			}
		}
	}

	return result.String()
}

// projectionEndIndex returns the index (exclusive) of the end of the
// projection starting at start. A projection ends at the next top-level
// (parenDepth == 0 within the lookahead) comma or at FROM/INTO. If neither
// is found, len(tokens) is returned.
func (f *SQLFormatter) projectionEndIndex(tokens []SQLToken, start int) int {
	depth := 0
	for j := start; j < len(tokens); j++ {
		switch tokens[j].Text {
		case "(":
			depth++
			continue
		case ")":
			if depth > 0 {
				depth--
			}
			continue
		case ",":
			if depth == 0 {
				return j
			}
		}
		if depth == 0 && tokens[j].Type == SQLTokenKeyword {
			upper := strings.ToUpper(tokens[j].Text)
			if upper == "FROM" || upper == "INTO" {
				return j
			}
		}
	}
	return len(tokens)
}

// projectionRenderLen estimates the rendered length of tokens[start:end]
// when laid out on a single line, including the spaces shouldAddSpace would
// emit between adjacent tokens.
func (f *SQLFormatter) projectionRenderLen(tokens []SQLToken, start, end int) int {
	total := 0
	for j := start; j < end; j++ {
		text := f.applyKeywordCasing(tokens[j])
		if j > start {
			var pp *SQLToken
			if j >= 2 {
				pp = &tokens[j-2]
			}
			if f.shouldAddSpace(&tokens[j-1], &tokens[j], pp) {
				total++
			}
		}
		total += len(text)
	}
	return total
}

func (f *SQLFormatter) keywordIndent(style, keyword, currentClause, rootCommand string, parenDepth int) string {
	switch style {
	case "canonicalCompact":
		switch keyword {
		case "AND", "OR", "ON", "HAVING", "WHEN", "ELSE":
			if rootCommand == "MERGE" && parenDepth == 0 && (keyword == "ON" || keyword == "WHEN") {
				return ""
			}
			// MERGE ON conditions: AND/OR indented 4 spaces (aligned under first condition)
			if rootCommand == "MERGE" && parenDepth == 0 && (keyword == "AND" || keyword == "OR") {
				return f.indentString // 4 spaces
			}
			return "  "
		}
	case "expanded":
		switch keyword {
		case "AND", "OR", "ON", "WHEN", "ELSE":
			return f.indentString
		case "HAVING":
			if currentClause == "GROUP" {
				return f.indentString
			}
		}
	}

	return ""
}

// isComplexSQL checks if SQL needs multi-line formatting.
func (f *SQLFormatter) isComplexSQL(tokens []SQLToken) bool {
	for _, t := range tokens {
		upper := strings.ToUpper(t.Text)
		if upper == "FROM" || upper == "WHERE" || upper == "JOIN" ||
			upper == "GROUP" || upper == "ORDER" || upper == "UNION" ||
			upper == "VALUES" || upper == "SET" || upper == "MERGE" ||
			upper == "HAVING" || upper == "CASE" || upper == "PIVOT" ||
			upper == "START" || upper == "CONNECT" {
			return true
		}
		if upper == "SELECT" && len(tokens) > 5 {
			return true
		}
	}
	return false
}

// applyKeywordCasing applies keyword casing rules.
func (f *SQLFormatter) applyKeywordCasing(t SQLToken) string {
	// Preserve optimizer hints exactly as-is
	if t.Type == SQLTokenHint {
		return t.Text
	}

	// Apply casing to keywords and built-in functions
	if t.Type == SQLTokenKeyword || t.Type == SQLTokenFunction {
		switch f.opts.KeywordCase {
		case "lower":
			return strings.ToLower(t.Text)
		case "preserve":
			return t.Text
		default: // "upper"
			return strings.ToUpper(t.Text)
		}
	}

	// Identifiers (table names, column names) stay lowercase,
	// but preserve casing for double-quoted identifiers (external schema objects).
	if t.Type == SQLTokenIdentifier {
		if len(t.Text) >= 2 && t.Text[0] == '"' && t.Text[len(t.Text)-1] == '"' {
			return t.Text
		}
		return strings.ToLower(t.Text)
	}

	return t.Text
}

// shouldAddSpace checks if a space should be added between tokens.
func (f *SQLFormatter) shouldAddSpace(prev *SQLToken, curr *SQLToken, prevPrev ...*SQLToken) bool {
	if prev == nil {
		return false
	}

	// Keyword followed by ( - add space (e.g., "WHERE ("),
	// UNLESS the keyword is also a known built-in function (e.g., LEFT, RIGHT, REPLACE)
	if prev.Type == SQLTokenKeyword && curr.Text == "(" {
		upper := strings.ToUpper(prev.Text)
		if SQLBuiltinFunctions[upper] {
			return false // Function-like usage: LEFT(x, 3), REPLACE(s, 'a', 'b')
		}
		return true
	}

	// Function followed by ( - NO space (e.g., "COUNT(")
	if prev.Type == SQLTokenFunction && curr.Text == "(" {
		return false
	}

	// No space after ( or { or before ) or }
	if prev.Text == "(" || prev.Text == "{" || curr.Text == ")" || curr.Text == "}" {
		return false
	}

	// Space before { (ODBC escape sequences like {fn IFNULL(...)})
	if curr.Text == "{" {
		return true
	}

	// No space before comma
	if curr.Text == "," {
		return false
	}

	// Space after comma
	if prev.Text == "," {
		return true
	}

	// No space around .
	if prev.Text == "." || curr.Text == "." {
		return false
	}

	// Space around operators — but not between unary minus/plus and its operand.
	// Unary context: - or + after ( or , (no space before or after the sign).
	// After = or other operators, keep space before the sign but not after.
	if (curr.Text == "-" || curr.Text == "+") && curr.Type == SQLTokenOperator {
		// No space between ( or , and unary sign
		if prev.Text == "(" || prev.Text == "," {
			return false
		}
	}
	if (prev.Text == "-" || prev.Text == "+") && prev.Type == SQLTokenOperator {
		// No space between unary sign and its operand
		var pp *SQLToken
		if len(prevPrev) > 0 {
			pp = prevPrev[0]
		}
		if pp != nil && (pp.Text == "(" || pp.Text == "," || pp.Text == "=" ||
			pp.Type == SQLTokenOperator) {
			return false
		}
	}
	if prev.Type == SQLTokenOperator || curr.Type == SQLTokenOperator {
		return true
	}

	// Space between atoms (keywords, functions, identifiers, numbers, strings, placeholders, hints)
	isAtom := func(t *SQLToken) bool {
		return t.Type == SQLTokenKeyword ||
			t.Type == SQLTokenFunction ||
			t.Type == SQLTokenIdentifier ||
			t.Type == SQLTokenNumber ||
			t.Type == SQLTokenString ||
			t.Type == SQLTokenPlaceholder ||
			t.Type == SQLTokenHint
	}

	if isAtom(prev) && isAtom(curr) {
		return true
	}

	// Space after ) before atom
	if prev.Text == ")" && isAtom(curr) {
		return true
	}

	return false
}

// Helper to get previous non-whitespace token
func getPrevNonWS(tokens []SQLToken, i int) *SQLToken {
	if i > 0 {
		return &tokens[i-1]
	}
	return nil
}

// FormatSQLInString formats SQL within an SSL string literal.
// It handles the quote characters and maintains proper indentation.
func (f *SQLFormatter) FormatSQLInString(content string, quoteChar byte, baseIndent string) string {
	if !f.opts.Enabled || len(content) == 0 {
		return string(quoteChar) + content + string(quoteChar)
	}

	formatted := f.FormatSQL(content, baseIndent+f.indentString)

	// Check if formatting produced multi-line output
	if strings.Contains(formatted, "\n") {
		var result strings.Builder
		result.WriteByte(quoteChar)
		result.WriteString("\n")
		result.WriteString(baseIndent)
		result.WriteString(f.indentString)
		result.WriteString(formatted)
		result.WriteString("\n")
		result.WriteString(baseIndent)
		result.WriteByte(quoteChar)
		return result.String()
	}

	return string(quoteChar) + formatted + string(quoteChar)
}

// IsSQLString checks if a string content appears to be a complete SQL statement.
// It uses structural validation to distinguish SQL from English sentences.
func IsSQLString(content string) bool {
	if len(content) == 0 {
		return false
	}

	// Tokenize the content
	lexer := NewSQLLexer(content)
	tokens := lexer.Tokenize()

	// Filter out whitespace tokens
	var nonWSTokens []SQLToken
	for _, t := range tokens {
		if t.Type != SQLTokenWhitespace {
			nonWSTokens = append(nonWSTokens, t)
		}
	}

	return isSQLStatementTokens(nonWSTokens)
}

// IsSQLDocument reports whether an entire document's content is a plain SQL
// statement rather than SSL code — the data-source case
// (feature.diagnostics_pipeline A10-A12): STARLIMS data sources often hold
// raw SQL. Unlike IsSQLString it also ignores SQL comments and optimizer
// hints, which legitimately precede a stored statement. SSL content never
// classifies: an SSL leading comment (`/* text;`) has no `*/` terminator,
// so the SQL lexer consumes the rest of the file as one comment token, and
// SSL keywords/identifiers fail the command-keyword check.
func IsSQLDocument(content string) bool {
	lexer := NewSQLLexer(content)
	tokens := lexer.Tokenize()

	var significant []SQLToken
	for _, t := range tokens {
		if t.Type == SQLTokenWhitespace || t.Type == SQLTokenComment || t.Type == SQLTokenHint {
			continue
		}
		significant = append(significant, t)
	}

	return isSQLStatementTokens(significant)
}

// isSQLStatementTokens applies the command-keyword and structure validation
// shared by IsSQLString and IsSQLDocument to a pre-filtered token list.
func isSQLStatementTokens(tokens []SQLToken) bool {
	if len(tokens) == 0 {
		return false
	}

	firstUpper := strings.ToUpper(tokens[0].Text)
	if !SQLCommandKeywords[firstUpper] {
		return false
	}

	return validateSQLStructure(firstUpper, tokens)
}

// validateSQLStructure validates that tokens form a complete SQL statement.
func validateSQLStructure(command string, tokens []SQLToken) bool {
	switch command {
	case "SELECT":
		return validateSelectStatement(tokens)
	case "INSERT":
		return containsKeyword(tokens, "INTO")
	case "UPDATE":
		return containsKeyword(tokens, "SET")
	case "DELETE":
		return containsKeyword(tokens, "FROM")
	case "MERGE":
		return containsKeyword(tokens, "INTO")
	case "WITH":
		// CTE - must contain a DML statement
		return containsKeyword(tokens, "SELECT") ||
			containsKeyword(tokens, "INSERT") ||
			containsKeyword(tokens, "UPDATE") ||
			containsKeyword(tokens, "DELETE")
	case "CREATE", "ALTER", "DROP":
		return containsDDLObject(tokens)
	case "TRUNCATE":
		return containsKeyword(tokens, "TABLE")
	case "EXEC", "EXECUTE", "CALL":
		// Must have content after the keyword
		return len(tokens) > 1
	case "GRANT", "REVOKE":
		// Must have content after the keyword
		return len(tokens) > 1
	default:
		return false
	}
}

// validateSelectStatement checks if tokens form a valid SELECT statement.
func validateSelectStatement(tokens []SQLToken) bool {
	// Find SELECT and FROM positions
	selectIdx := -1
	fromIdx := -1

	for i, t := range tokens {
		upper := strings.ToUpper(t.Text)
		if upper == "SELECT" && selectIdx == -1 {
			selectIdx = i
		} else if upper == "FROM" && fromIdx == -1 {
			fromIdx = i
		}
	}

	// If there's a FROM, there must be at least one token between SELECT and FROM
	if fromIdx > 0 {
		return fromIdx > selectIdx+1
	}

	// SELECT without FROM is valid if there's a valid expression after SELECT
	// Examples: SELECT 1, SELECT GETDATE(), SELECT *, SELECT @variable
	if selectIdx >= 0 && len(tokens) > selectIdx+1 {
		nextToken := tokens[selectIdx+1]
		// Valid expressions: numbers, strings, functions, *, identifiers, placeholders
		validTypes := nextToken.Type == SQLTokenNumber ||
			nextToken.Type == SQLTokenString ||
			nextToken.Type == SQLTokenFunction ||
			nextToken.Type == SQLTokenIdentifier ||
			nextToken.Type == SQLTokenPlaceholder ||
			nextToken.Text == "*"
		return validTypes
	}

	return false
}

// containsKeyword checks if tokens contain a specific keyword.
func containsKeyword(tokens []SQLToken, keyword string) bool {
	for _, t := range tokens {
		if strings.ToUpper(t.Text) == keyword {
			return true
		}
	}
	return false
}

// containsDDLObject checks if tokens contain a DDL object type.
func containsDDLObject(tokens []SQLToken) bool {
	for _, t := range tokens {
		if SQLDDLObjects[strings.ToUpper(t.Text)] {
			return true
		}
	}
	return false
}
