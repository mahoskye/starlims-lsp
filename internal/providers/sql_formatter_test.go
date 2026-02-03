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

// ============================================================================
// SQL Function Casing Tests
// ============================================================================

func TestSQLLexer_RecognizesFunctions(t *testing.T) {
	input := "SELECT COUNT(*) FROM users"
	lexer := NewSQLLexer(input)
	tokens := lexer.Tokenize()

	// Find COUNT token
	var countToken *SQLToken
	for i := range tokens {
		if strings.ToUpper(tokens[i].Text) == "COUNT" {
			countToken = &tokens[i]
			break
		}
	}

	if countToken == nil {
		t.Fatal("expected to find COUNT token")
	}

	if countToken.Type != SQLTokenFunction {
		t.Errorf("expected COUNT to be SQLTokenFunction, got %v", countToken.Type)
	}
}

func TestSQLFormatter_FunctionCasingUpper(t *testing.T) {
	sql := "SELECT count(*), sum(amount), avg(price) FROM orders"

	opts := DefaultSQLFormattingOptions()
	opts.KeywordCase = "upper" // default
	formatter := NewSQLFormatter(opts)

	formatted := formatter.FormatSQL(sql, "")

	// Functions should be uppercase
	if !strings.Contains(formatted, "COUNT(") {
		t.Errorf("expected COUNT to be uppercase, got: %s", formatted)
	}
	if !strings.Contains(formatted, "SUM(") {
		t.Errorf("expected SUM to be uppercase, got: %s", formatted)
	}
	if !strings.Contains(formatted, "AVG(") {
		t.Errorf("expected AVG to be uppercase, got: %s", formatted)
	}

	t.Logf("Formatted SQL:\n%s", formatted)
}

func TestSQLFormatter_FunctionCasingLower(t *testing.T) {
	sql := "SELECT COUNT(*), SUM(amount) FROM orders"

	opts := DefaultSQLFormattingOptions()
	opts.KeywordCase = "lower"
	formatter := NewSQLFormatter(opts)

	formatted := formatter.FormatSQL(sql, "")

	// Functions should be lowercase
	if !strings.Contains(formatted, "count(") {
		t.Errorf("expected count to be lowercase, got: %s", formatted)
	}
	if !strings.Contains(formatted, "sum(") {
		t.Errorf("expected sum to be lowercase, got: %s", formatted)
	}

	t.Logf("Formatted SQL:\n%s", formatted)
}

func TestSQLFormatter_FunctionCasingPreserve(t *testing.T) {
	sql := "SELECT Count(*), Sum(amount) FROM orders"

	opts := DefaultSQLFormattingOptions()
	opts.KeywordCase = "preserve"
	formatter := NewSQLFormatter(opts)

	formatted := formatter.FormatSQL(sql, "")

	// Functions should preserve original casing
	if !strings.Contains(formatted, "Count(") {
		t.Errorf("expected Count to be preserved, got: %s", formatted)
	}
	if !strings.Contains(formatted, "Sum(") {
		t.Errorf("expected Sum to be preserved, got: %s", formatted)
	}

	t.Logf("Formatted SQL:\n%s", formatted)
}

func TestSQLFormatter_FunctionNoSpaceBeforeParen(t *testing.T) {
	sql := "SELECT COUNT(*) FROM users"

	opts := DefaultSQLFormattingOptions()
	formatter := NewSQLFormatter(opts)

	formatted := formatter.FormatSQL(sql, "")

	// Functions should NOT have space before parenthesis
	if strings.Contains(formatted, "COUNT (") {
		t.Errorf("expected no space between COUNT and (, got: %s", formatted)
	}
	if !strings.Contains(formatted, "COUNT(") {
		t.Errorf("expected COUNT( without space, got: %s", formatted)
	}

	t.Logf("Formatted SQL:\n%s", formatted)
}

func TestSQLFormatter_VariousFunctions(t *testing.T) {
	sql := "SELECT MAX(id), MIN(id), COALESCE(name, 'N/A'), UPPER(status) FROM users"

	opts := DefaultSQLFormattingOptions()
	formatter := NewSQLFormatter(opts)

	formatted := formatter.FormatSQL(sql, "")

	// All functions should be uppercase
	functions := []string{"MAX(", "MIN(", "COALESCE(", "UPPER("}
	for _, fn := range functions {
		if !strings.Contains(formatted, fn) {
			t.Errorf("expected %s to be present and uppercase, got: %s", fn, formatted)
		}
	}

	t.Logf("Formatted SQL:\n%s", formatted)
}

func TestSQLFormatter_DateFunctions(t *testing.T) {
	sql := "SELECT GETDATE(), DATEADD(day, 1, created_at), YEAR(created_at) FROM orders"

	opts := DefaultSQLFormattingOptions()
	formatter := NewSQLFormatter(opts)

	formatted := formatter.FormatSQL(sql, "")

	// Date functions should be uppercase
	functions := []string{"GETDATE(", "DATEADD(", "YEAR("}
	for _, fn := range functions {
		if !strings.Contains(formatted, fn) {
			t.Errorf("expected %s to be present and uppercase, got: %s", fn, formatted)
		}
	}

	t.Logf("Formatted SQL:\n%s", formatted)
}

func TestSQLFormatter_MixedKeywordsAndFunctions(t *testing.T) {
	sql := "select count(*) from users where status = 'active' group by role having count(*) > 1"

	opts := DefaultSQLFormattingOptions()
	opts.KeywordCase = "upper"
	formatter := NewSQLFormatter(opts)

	formatted := formatter.FormatSQL(sql, "")

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
	if !strings.Contains(formatted, "GROUP") {
		t.Error("expected GROUP to be uppercase")
	}
	if !strings.Contains(formatted, "HAVING") {
		t.Error("expected HAVING to be uppercase")
	}

	// Function should also be uppercase
	if !strings.Contains(formatted, "COUNT(") {
		t.Error("expected COUNT to be uppercase")
	}

	t.Logf("Formatted SQL:\n%s", formatted)
}

// ============================================================================
// SQL String Detection Tests
// ============================================================================

func TestIsSQLString_ValidSelectStatements(t *testing.T) {
	tests := []struct {
		name  string
		input string
		want  bool
	}{
		// Valid SELECT statements
		{"SELECT with FROM", "SELECT * FROM users", true},
		{"SELECT with columns and FROM", "SELECT id, name FROM users", true},
		{"SELECT with WHERE", "SELECT * FROM users WHERE id = 1", true},
		{"SELECT 1", "SELECT 1", true},
		{"SELECT expression", "SELECT GETDATE()", true},
		{"SELECT star only", "SELECT *", true},
		{"SELECT variable", "SELECT ?userId?", true},
		{"SELECT lowercase", "select * from users", true},
		{"SELECT mixed case", "Select * From Users", true},
		{"SELECT with JOIN", "SELECT u.name FROM users u INNER JOIN orders o ON u.id = o.user_id", true},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := IsSQLString(tt.input)
			if got != tt.want {
				t.Errorf("IsSQLString(%q) = %v, want %v", tt.input, got, tt.want)
			}
		})
	}
}

func TestIsSQLString_InvalidSelectStatements(t *testing.T) {
	tests := []struct {
		name  string
		input string
		want  bool
	}{
		// Invalid SELECT - nothing between SELECT and FROM
		{"SELECT FROM without columns", "SELECT FROM users", false},
		// SELECT without anything after
		{"SELECT alone", "SELECT", false},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := IsSQLString(tt.input)
			if got != tt.want {
				t.Errorf("IsSQLString(%q) = %v, want %v", tt.input, got, tt.want)
			}
		})
	}
}

func TestIsSQLString_ValidDMLStatements(t *testing.T) {
	tests := []struct {
		name  string
		input string
		want  bool
	}{
		// INSERT
		{"INSERT INTO", "INSERT INTO users VALUES (1)", true},
		{"INSERT with columns", "INSERT INTO users (id, name) VALUES (1, 'John')", true},
		// UPDATE
		{"UPDATE with SET", "UPDATE users SET name = 'Jane'", true},
		{"UPDATE with WHERE", "UPDATE users SET name = 'Jane' WHERE id = 1", true},
		// DELETE
		{"DELETE FROM", "DELETE FROM users", true},
		{"DELETE with WHERE", "DELETE FROM users WHERE id = 1", true},
		// MERGE
		{"MERGE INTO", "MERGE INTO target USING source ON condition", true},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := IsSQLString(tt.input)
			if got != tt.want {
				t.Errorf("IsSQLString(%q) = %v, want %v", tt.input, got, tt.want)
			}
		})
	}
}

func TestIsSQLString_InvalidDMLStatements(t *testing.T) {
	tests := []struct {
		name  string
		input string
		want  bool
	}{
		// Invalid INSERT - no INTO
		{"INSERT without INTO", "Insert the record", false},
		// Invalid UPDATE - no SET
		{"UPDATE without SET", "Update your settings", false},
		// Invalid DELETE - no FROM
		{"DELETE without FROM", "Delete this item", false},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := IsSQLString(tt.input)
			if got != tt.want {
				t.Errorf("IsSQLString(%q) = %v, want %v", tt.input, got, tt.want)
			}
		})
	}
}

func TestIsSQLString_ValidDDLStatements(t *testing.T) {
	tests := []struct {
		name  string
		input string
		want  bool
	}{
		// CREATE
		{"CREATE TABLE", "CREATE TABLE users (id INT)", true},
		{"CREATE VIEW", "CREATE VIEW active_users AS SELECT * FROM users", true},
		{"CREATE INDEX", "CREATE INDEX idx_users ON users(name)", true},
		{"CREATE PROCEDURE", "CREATE PROCEDURE sp_GetUsers AS SELECT * FROM users", true},
		// ALTER
		{"ALTER TABLE", "ALTER TABLE users ADD email VARCHAR(255)", true},
		// DROP
		{"DROP TABLE", "DROP TABLE users", true},
		{"DROP INDEX", "DROP INDEX idx_users", true},
		// TRUNCATE
		{"TRUNCATE TABLE", "TRUNCATE TABLE users", true},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := IsSQLString(tt.input)
			if got != tt.want {
				t.Errorf("IsSQLString(%q) = %v, want %v", tt.input, got, tt.want)
			}
		})
	}
}

func TestIsSQLString_InvalidDDLStatements(t *testing.T) {
	tests := []struct {
		name  string
		input string
		want  bool
	}{
		// Invalid CREATE - no DDL object
		{"CREATE without object", "Create a new report", false},
		// Invalid DROP - no DDL object
		{"DROP without object", "Drop the ball", false},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := IsSQLString(tt.input)
			if got != tt.want {
				t.Errorf("IsSQLString(%q) = %v, want %v", tt.input, got, tt.want)
			}
		})
	}
}

func TestIsSQLString_ValidOtherStatements(t *testing.T) {
	tests := []struct {
		name  string
		input string
		want  bool
	}{
		// WITH (CTE)
		{"WITH CTE", "WITH cte AS (SELECT * FROM users) SELECT * FROM cte", true},
		// EXEC/EXECUTE
		{"EXEC procedure", "EXEC sp_GetUsers", true},
		{"EXECUTE procedure", "EXECUTE sp_GetUsers @id = 1", true},
		// CALL
		{"CALL procedure", "CALL sp_GetUsers()", true},
		// GRANT/REVOKE
		{"GRANT", "GRANT SELECT ON users TO public", true},
		{"REVOKE", "REVOKE SELECT ON users FROM public", true},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := IsSQLString(tt.input)
			if got != tt.want {
				t.Errorf("IsSQLString(%q) = %v, want %v", tt.input, got, tt.want)
			}
		})
	}
}

func TestIsSQLString_NotSQL(t *testing.T) {
	tests := []struct {
		name  string
		input string
		want  bool
	}{
		// Regular English sentences
		{"Hello world", "Hello world", false},
		{"English sentence", "This is a regular sentence", false},
		// SQL fragments (not complete statements)
		{"WHERE clause only", "WHERE id = 1", false},
		{"FROM clause only", "FROM users", false},
		{"ORDER BY only", "ORDER BY name", false},
		// Empty
		{"Empty string", "", false},
		// Other non-SQL
		{"JSON", "{\"key\": \"value\"}", false},
		{"Path", "/path/to/file", false},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := IsSQLString(tt.input)
			if got != tt.want {
				t.Errorf("IsSQLString(%q) = %v, want %v", tt.input, got, tt.want)
			}
		})
	}
}
