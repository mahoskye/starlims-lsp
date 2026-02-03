// Package providers implements LSP feature providers for SSL.
package providers

import (
	"strings"
)

// SQLFormattingOptions configures SQL formatting.
type SQLFormattingOptions struct {
	Enabled       bool   // Enable SQL formatting
	Style         string // "standard", "canonicalCompact", "compact", "expanded"
	KeywordCase   string // "upper", "lower", "preserve"
	IndentSize    int    // Spaces per indent level
	MaxLineLength int    // Max line length for wrapping
}

// SQL formatting styles:
// - "standard": Simple clause breaks (FROM, WHERE, JOIN on new lines). Default per style guide.
// - "canonicalCompact": Balanced formatting with indented AND/OR and smart column wrapping.
// - "compact": Minimal formatting, single line where possible.
// - "expanded": Each column/condition on its own line.

// DefaultSQLFormattingOptions returns default SQL formatting options.
// Default follows the official STARLIMS Style Guide.
func DefaultSQLFormattingOptions() SQLFormattingOptions {
	return SQLFormattingOptions{
		Enabled:       true,
		Style:         "standard",
		KeywordCase:   "upper",
		IndentSize:    4,
		MaxLineLength: 90,
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

	// Filter out whitespace tokens for formatting
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
	parenDepth := 0
	inSelectColumns := false

	for i := 0; i < len(nonWSTokens); i++ {
		t := nonWSTokens[i]
		upperText := strings.ToUpper(t.Text)

		// Track state
		if t.Type == SQLTokenKeyword {
			switch upperText {
			case "SELECT":
				currentClause = "SELECT"
				inSelectColumns = true
			case "UPDATE":
				currentClause = "UPDATE"
			case "INSERT":
				currentClause = "INSERT"
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

		// Apply casing
		tokenText := f.applyKeywordCasing(t)

		// Determine if we need a line break
		needsBreak := false
		extraIndent := ""
		prev := getPrevNonWS(nonWSTokens, i)

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
				} else if upperText == "INTO" && prevUpper == "INSERT" {
					needsBreak = false
				} else {
					needsBreak = true
					// Only indent ON for canonicalCompact style
					if upperText == "ON" && style == "canonicalCompact" {
						extraIndent = f.indentString
					}
				}
			}

			// Indented keywords (AND, OR) - only for canonicalCompact and expanded styles
			if (style == "canonicalCompact" || style == "expanded") &&
				SQLIndentedKeywords[upperText] && t.Type == SQLTokenKeyword {
				needsBreak = true
				extraIndent = f.indentString
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

			// VALUES paren
			if style != "compact" && t.Text == "(" && prevUpper == "VALUES" {
				needsBreak = true
				if style == "canonicalCompact" || style == "expanded" {
					extraIndent = f.indentString
				}
			}

			// Subquery SELECT
			if style != "compact" && upperText == "SELECT" && prev != nil && prev.Text == "(" {
				needsBreak = true
				extraIndent = f.indentString
			}

			// Proactive line wrapping (only for canonicalCompact and expanded)
			if !needsBreak && prev != nil && f.opts.MaxLineLength > 0 &&
				(style == "canonicalCompact" || style == "expanded") {
				spaceLen := 0
				if f.shouldAddSpace(prev, &t) {
					spaceLen = 1
				}
				projectedLen := currentLineLen + spaceLen + len(tokenText)

				canBreak := prev.Text == "," ||
					(t.Type == SQLTokenKeyword || t.Type == SQLTokenIdentifier) &&
						prev.Text != "."

				if projectedLen > f.opts.MaxLineLength && canBreak && prev.Text != "(" {
					needsBreak = true
					if inSelectColumns {
						extraIndent = strings.Repeat(" ", 7) // Align with SELECT columns
					} else {
						extraIndent = f.indentString
					}
				}
			}
		}

		// Write output
		if needsBreak {
			result.WriteString("\n")
			parenIndent := strings.Repeat(f.indentString, parenDepth)
			result.WriteString(baseIndent)
			result.WriteString(parenIndent)
			result.WriteString(extraIndent)
			currentLineLen = len(baseIndent) + len(parenIndent) + len(extraIndent)
		} else if prev != nil && f.shouldAddSpace(prev, &t) {
			result.WriteString(" ")
			currentLineLen++
		}

		result.WriteString(tokenText)
		currentLineLen += len(tokenText)
		isFirstToken = false
	}

	return result.String()
}

// isComplexSQL checks if SQL needs multi-line formatting.
func (f *SQLFormatter) isComplexSQL(tokens []SQLToken) bool {
	for _, t := range tokens {
		upper := strings.ToUpper(t.Text)
		if upper == "FROM" || upper == "WHERE" || upper == "JOIN" ||
			upper == "GROUP" || upper == "ORDER" || upper == "UNION" ||
			upper == "VALUES" || upper == "SET" {
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

	// Identifiers (table names, column names) stay lowercase
	if t.Type == SQLTokenIdentifier {
		return strings.ToLower(t.Text)
	}

	return t.Text
}

// shouldAddSpace checks if a space should be added between tokens.
func (f *SQLFormatter) shouldAddSpace(prev *SQLToken, curr *SQLToken) bool {
	if prev == nil {
		return false
	}

	// Keyword followed by ( - add space (e.g., "WHERE (")
	if prev.Type == SQLTokenKeyword && curr.Text == "(" {
		return true
	}

	// Function followed by ( - NO space (e.g., "COUNT(")
	if prev.Type == SQLTokenFunction && curr.Text == "(" {
		return false
	}

	// No space after ( or before )
	if prev.Text == "(" || curr.Text == ")" {
		return false
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

	// Space around operators
	if prev.Type == SQLTokenOperator || curr.Type == SQLTokenOperator {
		// Exception: negative numbers
		if (prev.Text == "-" || prev.Text == "+") && curr.Type == SQLTokenNumber {
			return false
		}
		return true
	}

	// Space between atoms (keywords, functions, identifiers, numbers, strings, placeholders)
	isAtom := func(t *SQLToken) bool {
		return t.Type == SQLTokenKeyword ||
			t.Type == SQLTokenFunction ||
			t.Type == SQLTokenIdentifier ||
			t.Type == SQLTokenNumber ||
			t.Type == SQLTokenString ||
			t.Type == SQLTokenPlaceholder
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
