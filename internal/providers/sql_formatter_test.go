package providers

import (
	"strings"
	"testing"
)

func TestSQLLexer_SimpleSelect(t *testing.T) {
	input := "SELECT id, name FROM users WHERE id = 1"
	lexer := NewSQLLexer(input)
	tokens := lexer.Tokenize()

	// Filter out whitespace for counting
	nonWS := filterNonWSSQL(tokens)

	if len(nonWS) != 10 {
		t.Errorf("expected 10 non-WS tokens, got %d", len(nonWS))
		for _, tok := range nonWS {
			t.Logf("Token: %v %q", tok.Type, tok.Text)
		}
	}

	// Check first token is SELECT keyword
	if nonWS[0].Type != SQLTokenKeyword || strings.ToUpper(nonWS[0].Text) != "SELECT" {
		t.Errorf("expected SELECT keyword, got %v %q", nonWS[0].Type, nonWS[0].Text)
	}
}

func TestSQLLexer_SSLParameter(t *testing.T) {
	input := "SELECT * FROM users WHERE id = ?userId?"
	lexer := NewSQLLexer(input)
	tokens := lexer.Tokenize()

	// Find placeholder token
	var placeholder *SQLToken
	for i := range tokens {
		if tokens[i].Type == SQLTokenPlaceholder {
			placeholder = &tokens[i]
			break
		}
	}

	if placeholder == nil {
		t.Fatal("expected to find placeholder token")
	}

	if placeholder.Text != "?userId?" {
		t.Errorf("expected ?userId?, got %q", placeholder.Text)
	}
}

func TestSQLFormatter_SimpleSelect(t *testing.T) {
	sql := "SELECT id, name FROM users WHERE id = 1"

	opts := DefaultSQLFormattingOptions()
	formatter := NewSQLFormatter(opts)

	formatted := formatter.FormatSQL(sql, "")

	// Should contain newlines due to complexity
	if !strings.Contains(formatted, "\n") {
		t.Error("expected formatted SQL to contain newlines")
	}

	// Keywords should be uppercase
	if !strings.Contains(formatted, "SELECT") {
		t.Error("expected SELECT to be uppercase")
	}
	if !strings.Contains(formatted, "FROM") {
		t.Error("expected FROM to be uppercase")
	}
	if !strings.Contains(formatted, "WHERE") {
		t.Error("expected WHERE to be uppercase")
	}

	t.Logf("Formatted SQL:\n%s", formatted)
}

func TestSQLFormatter_KeywordCasingLower(t *testing.T) {
	sql := "SELECT id FROM users"

	opts := DefaultSQLFormattingOptions()
	opts.KeywordCase = "lower"
	formatter := NewSQLFormatter(opts)

	formatted := formatter.FormatSQL(sql, "")

	if !strings.Contains(formatted, "select") {
		t.Error("expected select to be lowercase")
	}
	if !strings.Contains(formatted, "from") {
		t.Error("expected from to be lowercase")
	}

	t.Logf("Formatted SQL:\n%s", formatted)
}

func TestSQLFormatter_KeywordCasingPreserve(t *testing.T) {
	sql := "Select id From users"

	opts := DefaultSQLFormattingOptions()
	opts.KeywordCase = "preserve"
	formatter := NewSQLFormatter(opts)

	formatted := formatter.FormatSQL(sql, "")

	if !strings.Contains(formatted, "Select") {
		t.Error("expected Select to be preserved")
	}
	if !strings.Contains(formatted, "From") {
		t.Error("expected From to be preserved")
	}

	t.Logf("Formatted SQL:\n%s", formatted)
}

func TestSQLFormatter_WhereClause_Standard(t *testing.T) {
	sql := "SELECT * FROM users WHERE status = 'active' AND role = 'admin'"

	opts := DefaultSQLFormattingOptions()
	// Default is "standard" style - AND stays on same line as WHERE
	formatter := NewSQLFormatter(opts)

	formatted := formatter.FormatSQL(sql, "")

	// In standard style, AND should stay on the same line as WHERE
	if !strings.Contains(formatted, "WHERE status = 'active' AND role = 'admin'") {
		t.Error("in standard style, AND should stay on same line as WHERE")
	}

	t.Logf("Formatted SQL (standard):\n%s", formatted)
}

func TestSQLFormatter_WhereClause_CanonicalCompact(t *testing.T) {
	sql := "SELECT * FROM users WHERE status = 'active' AND role = 'admin'"

	opts := DefaultSQLFormattingOptions()
	opts.Style = "canonicalCompact"
	formatter := NewSQLFormatter(opts)

	formatted := formatter.FormatSQL(sql, "")

	// In canonicalCompact style, AND should be on its own line with indentation
	lines := strings.Split(formatted, "\n")
	foundAND := false
	for _, line := range lines {
		if strings.Contains(line, "AND") {
			foundAND = true
			// Should have some indentation before AND
			trimmed := strings.TrimLeft(line, " \t")
			if len(trimmed) == len(line) {
				t.Error("expected AND to be indented in canonicalCompact style")
			}
		}
	}

	if !foundAND {
		t.Error("expected to find AND in output")
	}

	t.Logf("Formatted SQL (canonicalCompact):\n%s", formatted)
}

func TestSQLFormatter_JoinClause(t *testing.T) {
	sql := "SELECT u.name, o.total FROM users u INNER JOIN orders o ON u.id = o.user_id"

	opts := DefaultSQLFormattingOptions()
	formatter := NewSQLFormatter(opts)

	formatted := formatter.FormatSQL(sql, "")

	// FROM should be on its own line
	if !strings.Contains(formatted, "\nFROM") {
		t.Error("expected FROM on new line")
	}

	// INNER JOIN should NOT have break between INNER and JOIN
	if strings.Contains(formatted, "INNER\nJOIN") {
		t.Error("INNER and JOIN should not be separated by newline")
	}

	t.Logf("Formatted SQL:\n%s", formatted)
}

func TestSQLFormatter_InsertStatement(t *testing.T) {
	sql := "INSERT INTO users (id, name, email) VALUES (1, 'John', 'john@example.com')"

	opts := DefaultSQLFormattingOptions()
	formatter := NewSQLFormatter(opts)

	formatted := formatter.FormatSQL(sql, "")

	// VALUES should be on its own line
	if !strings.Contains(formatted, "\nVALUES") {
		t.Error("expected VALUES on new line")
	}

	t.Logf("Formatted SQL:\n%s", formatted)
}

func TestSQLFormatter_UpdateStatement(t *testing.T) {
	sql := "UPDATE users SET name = 'Jane', email = 'jane@example.com' WHERE id = 1"

	opts := DefaultSQLFormattingOptions()
	formatter := NewSQLFormatter(opts)

	formatted := formatter.FormatSQL(sql, "")

	// WHERE should be on its own line
	if !strings.Contains(formatted, "\nWHERE") {
		t.Error("expected WHERE on new line")
	}

	t.Logf("Formatted SQL:\n%s", formatted)
}

func TestSQLFormatter_SimpleQuery_NoBreaks(t *testing.T) {
	// A simple query that shouldn't need multi-line formatting
	sql := "SELECT 1"

	opts := DefaultSQLFormattingOptions()
	formatter := NewSQLFormatter(opts)

	formatted := formatter.FormatSQL(sql, "")

	// Should not contain newlines for simple query
	if strings.Contains(formatted, "\n") {
		t.Error("simple query should not contain newlines")
	}

	expected := "SELECT 1"
	if formatted != expected {
		t.Errorf("expected %q, got %q", expected, formatted)
	}
}

func TestSQLFormatter_Disabled(t *testing.T) {
	sql := "SELECT id FROM users"

	opts := DefaultSQLFormattingOptions()
	opts.Enabled = false
	formatter := NewSQLFormatter(opts)

	formatted := formatter.FormatSQL(sql, "")

	// Should return unchanged when disabled
	if formatted != sql {
		t.Errorf("expected unchanged SQL when disabled, got %q", formatted)
	}
}

func filterNonWSSQL(tokens []SQLToken) []SQLToken {
	var result []SQLToken
	for _, t := range tokens {
		if t.Type != SQLTokenWhitespace {
			result = append(result, t)
		}
	}
	return result
}
