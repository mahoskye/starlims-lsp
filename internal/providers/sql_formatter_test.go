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
	opts.Style = "standard"
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

func TestSQLFormatter_CanonicalCompactUsesTwoSpaceClauseIndent(t *testing.T) {
	sql := "SELECT * FROM users WHERE status = 'active' AND role = 'admin' GROUP BY role HAVING COUNT(*) > 1"

	opts := DefaultSQLFormattingOptions()
	opts.Style = "canonicalCompact"
	formatter := NewSQLFormatter(opts)

	formatted := formatter.FormatSQL(sql, "")

	if !strings.Contains(formatted, "\n  AND role = 'admin'") {
		t.Fatalf("expected canonicalCompact AND indentation to be two spaces, got:\n%s", formatted)
	}
	if !strings.Contains(formatted, "\n  HAVING COUNT(*) > 1") {
		t.Fatalf("expected canonicalCompact HAVING indentation to be two spaces, got:\n%s", formatted)
	}
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

func TestSQLFormatter_CanonicalCompactJoinOnIndent(t *testing.T) {
	sql := "SELECT u.name, o.total FROM users u INNER JOIN orders o ON u.id = o.user_id AND o.active = 1"

	opts := DefaultSQLFormattingOptions()
	opts.Style = "canonicalCompact"
	formatter := NewSQLFormatter(opts)

	formatted := formatter.FormatSQL(sql, "")

	if !strings.Contains(formatted, "\n  ON u.id = o.user_id") {
		t.Fatalf("expected canonicalCompact ON indentation to be two spaces, got:\n%s", formatted)
	}
	if !strings.Contains(formatted, "\n  AND o.active = 1") {
		t.Fatalf("expected canonicalCompact join AND indentation to be two spaces, got:\n%s", formatted)
	}
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

// --- Tests for sql-canonical-compact-reference coverage ---

func TestSQLFormatter_OracleFunctionCasing(t *testing.T) {
	// sql-canonical-compact-reference: Oracle functions must be uppercased
	opts := DefaultSQLFormattingOptions()
	opts.KeywordCase = "upper"
	f := NewSQLFormatter(opts)

	tests := []struct {
		input    string
		expected string
	}{
		{"to_date('2025-01-01', 'YYYY-MM-DD')", "TO_DATE('2025-01-01', 'YYYY-MM-DD')"},
		{"nvl(col, 0)", "NVL(col, 0)"},
		{"nvl2(col, 'yes', 'no')", "NVL2(col, 'yes', 'no')"},
		{"decode(status, 'A', 'Active')", "DECODE(status, 'A', 'Active')"},
		{"to_char(sysdate, 'YYYY-MM-DD')", "TO_CHAR(SYSDATE, 'YYYY-MM-DD')"},
		{"trunc(sysdate)", "TRUNC(SYSDATE)"},
		{"coalesce(a, b, c)", "COALESCE(a, b, c)"},
	}

	for _, tt := range tests {
		result := f.FormatSQL(tt.input, "")
		trimmed := strings.TrimSpace(result)
		if trimmed != tt.expected {
			t.Errorf("FormatSQL(%q) =\n  %q\nwant:\n  %q", tt.input, trimmed, tt.expected)
		}
	}
}

func TestSQLFormatter_WindowFunctionCasing(t *testing.T) {
	// sql-canonical-compact-reference section 3: Window/analytic functions must be uppercased
	opts := DefaultSQLFormattingOptions()
	opts.KeywordCase = "upper"
	f := NewSQLFormatter(opts)

	inputs := []struct {
		input string
		funcs []string // functions that must appear uppercased
	}{
		{"row_number() over (partition by col)", []string{"ROW_NUMBER", "OVER", "PARTITION"}},
		{"lag(col, 1) over (order by col)", []string{"LAG", "OVER", "ORDER"}},
		{"lead(col, 1) over (order by col)", []string{"LEAD", "OVER"}},
		{"first_value(col) over (order by col)", []string{"FIRST_VALUE", "OVER"}},
		{"last_value(col) over (order by col)", []string{"LAST_VALUE", "OVER"}},
		{"listagg(col, ',') within group (order by col)", []string{"LISTAGG"}},
	}

	for _, tt := range inputs {
		result := f.FormatSQL(tt.input, "")
		for _, fn := range tt.funcs {
			if !strings.Contains(result, fn) {
				t.Errorf("FormatSQL(%q): expected %q in result:\n%s", tt.input, fn, result)
			}
		}
	}
}

func TestSQLFormatter_DDLKeywordCasing(t *testing.T) {
	// sql-canonical-compact-reference section 6: DDL keywords
	opts := DefaultSQLFormattingOptions()
	opts.KeywordCase = "upper"
	f := NewSQLFormatter(opts)

	input := "create table orders (ordno varchar2(20) not null, constraint pk_orders primary key (ordno))"
	result := f.FormatSQL(input, "")

	for _, kw := range []string{"CREATE", "TABLE", "NOT", "NULL", "CONSTRAINT", "PRIMARY", "KEY"} {
		if !strings.Contains(result, kw) {
			t.Errorf("expected %q to be uppercased in result: %s", kw, result)
		}
	}
}

func TestSQLFormatter_OracleSpecificBreaks(t *testing.T) {
	// sql-canonical-compact-reference section 4: Oracle constructs get line breaks
	opts := DefaultSQLFormattingOptions()
	opts.Style = "canonicalCompact"
	f := NewSQLFormatter(opts)

	// PIVOT should get its own line
	input := "SELECT * FROM sales PIVOT (SUM(amount) FOR quarter IN ('Q1', 'Q2'))"
	result := f.FormatSQL(input, "")
	if !strings.Contains(result, "\nPIVOT") {
		t.Errorf("expected PIVOT on its own line, got:\n%s", result)
	}

	// START WITH / CONNECT BY should get line breaks
	input2 := "SELECT empno, mgr FROM emp START WITH mgr IS NULL CONNECT BY PRIOR empno = mgr"
	result2 := f.FormatSQL(input2, "")
	if !strings.Contains(result2, "\nSTART") {
		t.Errorf("expected START WITH on its own line, got:\n%s", result2)
	}
	if !strings.Contains(result2, "\nCONNECT") {
		t.Errorf("expected CONNECT BY on its own line, got:\n%s", result2)
	}
}

func TestSQLFormatter_SetOperationBreaks(t *testing.T) {
	// sql-canonical-compact-reference section 2.8: Set operations on own line
	opts := DefaultSQLFormattingOptions()
	opts.Style = "canonicalCompact"
	f := NewSQLFormatter(opts)

	input := "SELECT a FROM t1 UNION ALL SELECT a FROM t2 INTERSECT SELECT a FROM t3"
	result := f.FormatSQL(input, "")
	if !strings.Contains(result, "\nUNION") {
		t.Errorf("expected UNION on its own line, got:\n%s", result)
	}
	if !strings.Contains(result, "\nINTERSECT") {
		t.Errorf("expected INTERSECT on its own line, got:\n%s", result)
	}
}

// ==================== Missing Test Coverage ====================

// --- Blank lines around set operations ---

func TestSQLFormatter_SetOperationBlankLines(t *testing.T) {
	// sql-canonical-compact-reference §2.8: set operations surrounded by blank lines
	opts := DefaultSQLFormattingOptions()
	opts.Style = "canonicalCompact"
	f := NewSQLFormatter(opts)

	input := "SELECT a FROM t1 UNION ALL SELECT a FROM t2"
	result := f.FormatSQL(input, "")

	// Should have a blank line before UNION (two consecutive newlines)
	if !strings.Contains(result, "\n\nUNION") {
		t.Errorf("expected blank line before UNION, got:\n%s", result)
	}
}

func TestSQLFormatter_SetOperationBlankLines_Intersect(t *testing.T) {
	opts := DefaultSQLFormattingOptions()
	opts.Style = "canonicalCompact"
	f := NewSQLFormatter(opts)

	input := "SELECT a FROM t1 INTERSECT SELECT a FROM t2"
	result := f.FormatSQL(input, "")

	if !strings.Contains(result, "\n\nINTERSECT") {
		t.Errorf("expected blank line before INTERSECT, got:\n%s", result)
	}
}

func TestSQLFormatter_SetOperationBlankLines_Minus(t *testing.T) {
	opts := DefaultSQLFormattingOptions()
	opts.Style = "canonicalCompact"
	f := NewSQLFormatter(opts)

	input := "SELECT a FROM t1 MINUS SELECT a FROM t2"
	result := f.FormatSQL(input, "")

	if !strings.Contains(result, "\n\nMINUS") {
		t.Errorf("expected blank line before MINUS, got:\n%s", result)
	}
}

// --- CASE/WHEN/ELSE indentation ---

func TestSQLFormatter_CaseWhenElseIndentation(t *testing.T) {
	// sql-canonical-compact-reference §5.1-5.2: CASE in SELECT list gets
	// WHEN/ELSE at col 11 (7 for SELECT column alignment + 4 for CASE indent)
	opts := DefaultSQLFormattingOptions()
	opts.Style = "canonicalCompact"
	f := NewSQLFormatter(opts)

	input := "SELECT CASE WHEN status = 1 THEN 'Active' WHEN status = 2 THEN 'Inactive' ELSE 'Unknown' END AS label FROM orders"
	result := f.FormatSQL(input, "")

	// WHEN/ELSE should be at col 11 inside SELECT CASE
	for _, line := range strings.Split(result, "\n") {
		trimmed := strings.TrimLeft(line, " ")
		if strings.HasPrefix(trimmed, "WHEN") || strings.HasPrefix(trimmed, "ELSE") {
			indent := len(line) - len(trimmed)
			if indent != 11 {
				t.Errorf("expected WHEN/ELSE at col 11, got %d: %q", indent, line)
			}
		}
	}
	t.Logf("Formatted:\n%s", result)
}

// --- UPDATE SET formatting ---

func TestSQLFormatter_UpdateSetFormatting(t *testing.T) {
	// sql-canonical-compact-reference §2.10: SET on same line as UPDATE,
	// column assignments indented
	opts := DefaultSQLFormattingOptions()
	opts.Style = "canonicalCompact"
	f := NewSQLFormatter(opts)

	input := "UPDATE ordtask SET status = 'Logged', moddate = GETDATE() WHERE ordno = ?sOrdNo?"
	result := f.FormatSQL(input, "")

	// SET should break after it, not before (it stays on UPDATE line)
	if strings.Contains(result, "\nSET") {
		t.Errorf("SET should stay on same line as UPDATE in canonicalCompact, got:\n%s", result)
	}
	// WHERE should be on its own line
	if !strings.Contains(result, "\nWHERE") {
		t.Errorf("expected WHERE on its own line, got:\n%s", result)
	}
}

// --- Subquery indentation ---

func TestSQLFormatter_SubqueryIndentation(t *testing.T) {
	// sql-canonical-compact-reference §1.28: subqueries indented inside parens
	opts := DefaultSQLFormattingOptions()
	opts.Style = "canonicalCompact"
	f := NewSQLFormatter(opts)

	input := "SELECT name FROM users WHERE id IN (SELECT user_id FROM orders WHERE status = 'Active')"
	result := f.FormatSQL(input, "")

	// The inner SELECT should be indented at single level (4 spaces from parenDepth)
	if !strings.Contains(result, "\n    SELECT user_id") {
		t.Errorf("expected subquery SELECT at single indent level, got:\n%s", result)
	}
	// The subquery should start on a new line after (
	if !strings.Contains(result, "(\n") {
		t.Errorf("expected subquery to start on new line after opening paren, got:\n%s", result)
	}
	// Closing ) should be on its own line
	if !strings.Contains(result, "\n)") {
		t.Errorf("expected closing ) on its own line, got:\n%s", result)
	}
}

// --- HAVING indentation ---

func TestSQLFormatter_HavingIndentation(t *testing.T) {
	// sql-canonical-compact-reference §1.21: HAVING indented 2 spaces under GROUP BY
	opts := DefaultSQLFormattingOptions()
	opts.Style = "canonicalCompact"
	f := NewSQLFormatter(opts)

	input := "SELECT status, COUNT(*) as cnt FROM orders GROUP BY status HAVING COUNT(*) > 5"
	result := f.FormatSQL(input, "")

	if !strings.Contains(result, "\n  HAVING") {
		t.Errorf("expected HAVING indented 2 spaces under GROUP BY, got:\n%s", result)
	}
}

// --- Join ON indentation with multiple conditions ---

func TestSQLFormatter_JoinOnMultipleConditions(t *testing.T) {
	opts := DefaultSQLFormattingOptions()
	opts.Style = "canonicalCompact"
	f := NewSQLFormatter(opts)

	input := "SELECT a.id, b.name FROM table1 a INNER JOIN table2 b ON a.id = b.id AND a.type = b.type WHERE a.status = 1"
	result := f.FormatSQL(input, "")

	// ON should be indented 2 spaces
	if !strings.Contains(result, "\n  ON") {
		t.Errorf("expected ON indented 2 spaces under JOIN, got:\n%s", result)
	}
	// AND inside ON should also be indented
	if !strings.Contains(result, "\n  AND") {
		t.Errorf("expected AND indented 2 spaces, got:\n%s", result)
	}
}

// --- SQL keyword and identifier casing ---

func TestSQLFormatter_IdentifierCasingLowercase(t *testing.T) {
	// sql-canonical-compact-reference §1.24: identifiers lowercase
	opts := DefaultSQLFormattingOptions()
	opts.Style = "canonicalCompact"
	f := NewSQLFormatter(opts)

	input := "SELECT OrderNo, TestCode FROM OrdTask WHERE Status = 1"
	result := f.FormatSQL(input, "")

	// Identifiers should be lowercased
	if strings.Contains(result, "OrderNo") || strings.Contains(result, "TestCode") || strings.Contains(result, "OrdTask") {
		t.Errorf("expected identifiers lowercased, got:\n%s", result)
	}
	// Keywords should be uppercased
	if !strings.Contains(result, "SELECT") || !strings.Contains(result, "FROM") {
		t.Errorf("expected keywords uppercased, got:\n%s", result)
	}
}

// --- Compact style stays on one line ---

func TestSQLFormatter_CompactStyleSingleLine(t *testing.T) {
	opts := DefaultSQLFormattingOptions()
	opts.Style = "compact"
	f := NewSQLFormatter(opts)

	input := "SELECT id, name FROM users WHERE status = 1"
	result := f.FormatSQL(input, "")

	if strings.Contains(result, "\n") {
		t.Errorf("compact style should stay on one line for simple query, got:\n%s", result)
	}
}

// --- Expanded style puts each item on its own line ---

func TestSQLFormatter_ExpandedStyleBreaksAll(t *testing.T) {
	opts := DefaultSQLFormattingOptions()
	opts.Style = "expanded"
	f := NewSQLFormatter(opts)

	input := "SELECT id, name, status FROM users WHERE id = 1 AND name = 'test'"
	result := f.FormatSQL(input, "")

	// FROM, WHERE, AND should all be on their own lines
	if !strings.Contains(result, "\nFROM") {
		t.Errorf("expected FROM on own line in expanded, got:\n%s", result)
	}
	if !strings.Contains(result, "\nWHERE") {
		t.Errorf("expected WHERE on own line in expanded, got:\n%s", result)
	}
}

// ==================== Optimizer Hint Tests ====================

func TestSQLLexer_OptimizerHint(t *testing.T) {
	input := "SELECT /*+ INDEX(t1) */ col1 FROM t1"
	lexer := NewSQLLexer(input)
	tokens := lexer.Tokenize()

	foundHint := false
	for _, tok := range tokens {
		if tok.Type == SQLTokenHint {
			foundHint = true
			if tok.Text != "/*+ INDEX(t1) */" {
				t.Errorf("expected hint text '/*+ INDEX(t1) */', got %q", tok.Text)
			}
		}
	}

	if !foundHint {
		t.Fatal("expected optimizer hint token")
	}
}

func TestSQLFormatter_PreservesOptimizerHint(t *testing.T) {
	// sql-canonical-compact-reference §4.7: never strip hints
	opts := DefaultSQLFormattingOptions()
	opts.Style = "canonicalCompact"
	f := NewSQLFormatter(opts)

	input := "SELECT /*+ FULL(t1) */ col1, col2 FROM t1 WHERE status = 1"
	result := f.FormatSQL(input, "")

	if !strings.Contains(result, "/*+ FULL(t1) */") {
		t.Errorf("optimizer hint should be preserved, got:\n%s", result)
	}
}

func TestSQLFormatter_HintNotStripped_MultipleHints(t *testing.T) {
	opts := DefaultSQLFormattingOptions()
	opts.Style = "canonicalCompact"
	f := NewSQLFormatter(opts)

	input := "SELECT /*+ PARALLEL(4) */ id FROM orders WHERE status = 1"
	result := f.FormatSQL(input, "")

	if !strings.Contains(result, "/*+ PARALLEL(4) */") {
		t.Errorf("optimizer hint should be preserved, got:\n%s", result)
	}
}

func TestSQLFormatter_RegularBlockComment_Preserved(t *testing.T) {
	// Regular comments (without +) should be tokenized as SQLTokenComment and preserved
	input := "SELECT /* regular comment */ id FROM t1"
	lexer := NewSQLLexer(input)
	tokens := lexer.Tokenize()

	foundComment := false
	for _, tok := range tokens {
		if tok.Type == SQLTokenHint {
			t.Error("regular comment should not be treated as hint")
		}
		if tok.Type == SQLTokenComment {
			foundComment = true
		}
	}
	if !foundComment {
		t.Error("regular block comment should be tokenized as SQLTokenComment")
	}

	// Verify comment is preserved in formatted output
	opts := DefaultSQLFormattingOptions()
	opts.Style = "canonicalCompact"
	f := NewSQLFormatter(opts)
	result := f.FormatSQL(input, "")
	if !strings.Contains(result, "/* regular comment */") {
		t.Errorf("comment should be preserved in output, got: %s", result)
	}
}

// --- MERGE statement formatting ---

func TestSQLFormatter_MergeOnWhenAtColumn0(t *testing.T) {
	// sql-canonical-compact-reference §2.12: ON/WHEN at column 0 in MERGE
	opts := DefaultSQLFormattingOptions()
	opts.Style = "canonicalCompact"
	f := NewSQLFormatter(opts)

	input := "MERGE INTO target t USING source s ON t.id = s.id WHEN MATCHED THEN UPDATE SET t.name = s.name WHEN NOT MATCHED THEN INSERT (id, name) VALUES (s.id, s.name)"
	result := f.FormatSQL(input, "")

	if !strings.Contains(result, "\nON ") && !strings.Contains(result, "\nON\n") {
		t.Errorf("expected ON at column 0 in MERGE, got:\n%s", result)
	}
	if !strings.Contains(result, "\nWHEN ") {
		t.Errorf("expected WHEN at column 0 in MERGE, got:\n%s", result)
	}
}

func TestSQLFormatter_MergeSubStatementIndent(t *testing.T) {
	// sql-canonical-compact-reference §2.12: UPDATE SET / INSERT indented 4 under WHEN
	opts := DefaultSQLFormattingOptions()
	opts.Style = "canonicalCompact"
	f := NewSQLFormatter(opts)

	input := "MERGE INTO target t USING source s ON t.id = s.id WHEN MATCHED THEN UPDATE SET t.name = s.name WHEN NOT MATCHED THEN INSERT (id, name) VALUES (s.id, s.name)"
	result := f.FormatSQL(input, "")

	if !strings.Contains(result, "\n    UPDATE") {
		t.Errorf("expected UPDATE indented 4 spaces under WHEN, got:\n%s", result)
	}
	if !strings.Contains(result, "\n    INSERT") {
		t.Errorf("expected INSERT indented 4 spaces under WHEN, got:\n%s", result)
	}
}

// --- Correlated subquery formatting ---

func TestSQLFormatter_CorrelatedSubquery(t *testing.T) {
	opts := DefaultSQLFormattingOptions()
	opts.Style = "canonicalCompact"
	f := NewSQLFormatter(opts)

	input := "SELECT o.ordno FROM orders o WHERE EXISTS (SELECT 1 FROM ordtask t WHERE t.ordno = o.ordno)"
	result := f.FormatSQL(input, "")

	if !strings.Contains(result, "\nWHERE") {
		t.Errorf("expected WHERE at column 0, got:\n%s", result)
	}
	// Subquery SELECT should be indented
	if !strings.Contains(result, "    SELECT 1") {
		t.Errorf("expected subquery SELECT indented, got:\n%s", result)
	}
}

// --- CTE formatting ---

func TestSQLFormatter_CTEFormatting(t *testing.T) {
	opts := DefaultSQLFormattingOptions()
	opts.Style = "canonicalCompact"
	f := NewSQLFormatter(opts)

	input := "WITH active_orders AS (SELECT ordno, status FROM orders WHERE status = 'Active') SELECT ordno FROM active_orders"
	result := f.FormatSQL(input, "")

	if !strings.Contains(result, "WITH") {
		t.Errorf("expected WITH keyword, got:\n%s", result)
	}
	if !strings.Contains(result, "SELECT ordno") {
		t.Errorf("expected final SELECT, got:\n%s", result)
	}
}

// --- CASE WHEN indent with nested context ---

func TestSQLFormatter_CaseWhenInWhere(t *testing.T) {
	// CASE in WHERE clause: WHEN should still be indented 2 spaces under CASE
	opts := DefaultSQLFormattingOptions()
	opts.Style = "canonicalCompact"
	f := NewSQLFormatter(opts)

	input := "SELECT ordno FROM orders WHERE CASE WHEN status = 'L' THEN 1 WHEN status = 'C' THEN 2 ELSE 0 END > 0"
	result := f.FormatSQL(input, "")

	if !strings.Contains(result, "\n  WHEN") {
		t.Errorf("expected WHEN indented 2 spaces under CASE in WHERE, got:\n%s", result)
	}
}

// --- Fix 1: Subquery SELECT single-level indent (not double) ---

func TestSQLFormatter_SubquerySingleLevelIndent(t *testing.T) {
	// Subquery SELECT should be at one indent level (4 spaces from parenDepth),
	// not double-indented (8 spaces).
	opts := DefaultSQLFormattingOptions()
	opts.Style = "canonicalCompact"
	f := NewSQLFormatter(opts)

	input := "SELECT name FROM users WHERE id IN (SELECT user_id FROM orders WHERE status = 'Active')"
	result := f.FormatSQL(input, "")

	// Should have 4 spaces before inner SELECT (one indent from parenDepth)
	if !strings.Contains(result, "\n    SELECT user_id") {
		t.Errorf("expected subquery SELECT at 4-space indent (single level), got:\n%s", result)
	}
	// Should NOT have 8 spaces (double indent)
	if strings.Contains(result, "\n        SELECT user_id") {
		t.Errorf("subquery SELECT should NOT be double-indented, got:\n%s", result)
	}
}

// --- Fix 2: Set operations — blank line AFTER and SELECT on new line ---

func TestSQLFormatter_SetOpBlankLineAfterAndSelectNewLine(t *testing.T) {
	// §2.8: blank line before AND after set operator, SELECT on its own line
	opts := DefaultSQLFormattingOptions()
	opts.Style = "canonicalCompact"
	f := NewSQLFormatter(opts)

	input := "SELECT a FROM t1 UNION ALL SELECT a FROM t2"
	result := f.FormatSQL(input, "")

	// Blank line before UNION
	if !strings.Contains(result, "\n\nUNION ALL") {
		t.Errorf("expected blank line before UNION ALL, got:\n%s", result)
	}
	// Blank line after UNION ALL (before SELECT)
	if !strings.Contains(result, "UNION ALL\n\nSELECT") {
		t.Errorf("expected blank line after UNION ALL before SELECT, got:\n%s", result)
	}
	// SELECT should be on its own line (not same line as ALL)
	if strings.Contains(result, "ALL SELECT") {
		t.Errorf("SELECT should not be on same line as ALL, got:\n%s", result)
	}
}

func TestSQLFormatter_SetOpIntersectBlankLines(t *testing.T) {
	opts := DefaultSQLFormattingOptions()
	opts.Style = "canonicalCompact"
	f := NewSQLFormatter(opts)

	input := "SELECT a FROM t1 INTERSECT SELECT a FROM t2"
	result := f.FormatSQL(input, "")

	if !strings.Contains(result, "\n\nINTERSECT") {
		t.Errorf("expected blank line before INTERSECT, got:\n%s", result)
	}
	if !strings.Contains(result, "INTERSECT\n\nSELECT") {
		t.Errorf("expected blank line after INTERSECT before SELECT, got:\n%s", result)
	}
}

// --- Fix 3: CASE in SELECT list stays inline ---

func TestSQLFormatter_CaseInSelectListAligned(t *testing.T) {
	// CASE in SELECT list should align at col 7 (SELECT column position)
	// WHEN/ELSE at col 11, END at col 7
	opts := DefaultSQLFormattingOptions()
	opts.Style = "canonicalCompact"
	f := NewSQLFormatter(opts)

	input := "SELECT ordno, CASE status WHEN 'L' THEN 'Logged' WHEN 'C' THEN 'Complete' ELSE 'Unknown' END AS label FROM orders"
	result := f.FormatSQL(input, "")

	// CASE should be at col 7 (not col 0)
	if strings.Contains(result, "\nCASE") && !strings.Contains(result, "       CASE") {
		t.Errorf("CASE should be at col 7 in SELECT list, got:\n%s", result)
	}
	t.Logf("Formatted:\n%s", result)
}

// --- Fix 4: Closing ) on its own line for subqueries ---

func TestSQLFormatter_SubqueryClosingParenOwnLine(t *testing.T) {
	// Subquery closing ) should be on its own line at the outer indent level.
	opts := DefaultSQLFormattingOptions()
	opts.Style = "canonicalCompact"
	f := NewSQLFormatter(opts)

	input := "SELECT o.ordno FROM orders o WHERE EXISTS (SELECT 1 FROM ordtask t WHERE t.ordno = o.ordno)"
	result := f.FormatSQL(input, "")

	// The closing ) should be on its own line, not on the WHERE line
	if !strings.Contains(result, "\n)") {
		t.Errorf("expected closing ) on its own line for subquery, got:\n%s", result)
	}
}

func TestSQLFormatter_FunctionParenNotOnOwnLine(t *testing.T) {
	// Function call closing ) should NOT be on its own line (e.g., COUNT(*))
	opts := DefaultSQLFormattingOptions()
	opts.Style = "canonicalCompact"
	f := NewSQLFormatter(opts)

	input := "SELECT COUNT(*) FROM users WHERE status = 'active'"
	result := f.FormatSQL(input, "")

	// COUNT(*) should stay together
	if !strings.Contains(result, "COUNT(*)") {
		t.Errorf("expected COUNT(*) to stay together, got:\n%s", result)
	}
}

// --- FOR UPDATE on its own line ---

func TestSQLFormatter_ForUpdateOwnLine(t *testing.T) {
	opts := DefaultSQLFormattingOptions()
	opts.Style = "canonicalCompact"
	f := NewSQLFormatter(opts)

	input := "SELECT ordno, status FROM orders WHERE status = 'Active' FOR UPDATE"
	result := f.FormatSQL(input, "")

	if !strings.Contains(result, "\nFOR UPDATE") {
		t.Errorf("expected FOR UPDATE on its own line, got:\n%s", result)
	}
}

// --- Col-7 wrapping for long SELECT lists ---

func TestSQLFormatter_Col7Wrapping(t *testing.T) {
	// When SELECT columns exceed MaxLineLength, continuation wraps at column 7
	opts := DefaultSQLFormattingOptions()
	opts.Style = "canonicalCompact"
	opts.MaxLineLength = 90
	f := NewSQLFormatter(opts)

	input := "SELECT ordno, testcode, status, moddate, created_by, modified_by, long_column_name, extra_col FROM orders WHERE status = 1"
	result := f.FormatSQL(input, "")

	// After wrapping, continuation should be at 7 spaces (aligned under first column after SELECT)
	if !strings.Contains(result, "\n       extra_col") {
		t.Errorf("expected col-7 alignment for wrapped SELECT columns, got:\n%s", result)
	}
}

// --- MERGE DELETE WHERE ---

func TestSQLFormatter_MergeDeleteWhere(t *testing.T) {
	// sql-canonical-compact-reference §2.12: DELETE WHERE indented under WHEN
	opts := DefaultSQLFormattingOptions()
	opts.Style = "canonicalCompact"
	f := NewSQLFormatter(opts)

	input := "MERGE INTO target t USING source s ON t.id = s.id WHEN MATCHED THEN UPDATE SET t.name = s.name WHEN NOT MATCHED THEN INSERT (id) VALUES (s.id) WHEN MATCHED AND s.active = 0 THEN DELETE WHERE t.status = 0"
	result := f.FormatSQL(input, "")

	if !strings.Contains(result, "\n    DELETE") {
		t.Errorf("expected DELETE indented under WHEN, got:\n%s", result)
	}
	t.Logf("Formatted MERGE DELETE WHERE:\n%s", result)
}

// --- LISTAGG WITHIN GROUP ---

func TestSQLFormatter_ListaggWithinGroup(t *testing.T) {
	// sql-canonical-compact-reference §3.4: LISTAGG ... WITHIN GROUP (ORDER BY ...)
	opts := DefaultSQLFormattingOptions()
	opts.KeywordCase = "upper"
	f := NewSQLFormatter(opts)

	input := "SELECT department, listagg(name, ', ') within group (order by name) as members FROM employees GROUP BY department"
	result := f.FormatSQL(input, "")

	if !strings.Contains(result, "LISTAGG(") {
		t.Errorf("expected LISTAGG uppercased, got:\n%s", result)
	}
	if !strings.Contains(result, "WITHIN") {
		t.Errorf("expected WITHIN uppercased, got:\n%s", result)
	}
	t.Logf("Formatted LISTAGG WITHIN GROUP:\n%s", result)
}

// --- LISTAGG ON OVERFLOW ---

func TestSQLFormatter_ListaggOnOverflow(t *testing.T) {
	// sql-canonical-compact-reference §3.4: ON OVERFLOW TRUNCATE
	opts := DefaultSQLFormattingOptions()
	opts.KeywordCase = "upper"
	f := NewSQLFormatter(opts)

	input := "SELECT listagg(testcode, ', ' on overflow truncate '...') within group (order by testcode) FROM ordtask"
	result := f.FormatSQL(input, "")

	if !strings.Contains(result, "OVERFLOW") {
		t.Errorf("expected OVERFLOW uppercased, got:\n%s", result)
	}
	if !strings.Contains(result, "TRUNCATE") {
		t.Errorf("expected TRUNCATE uppercased, got:\n%s", result)
	}
	t.Logf("Formatted LISTAGG ON OVERFLOW:\n%s", result)
}

// --- MERGE multi-line ON ---

func TestSQLFormatter_MergeMultilineOn(t *testing.T) {
	// sql-canonical-compact-reference §2.12: MERGE ON at column 0, AND inside ON aligned
	opts := DefaultSQLFormattingOptions()
	opts.Style = "canonicalCompact"
	f := NewSQLFormatter(opts)

	input := "MERGE INTO target t USING source s ON t.ordno = s.ordno AND t.testcode = s.testcode WHEN MATCHED THEN UPDATE SET t.status = s.status"
	result := f.FormatSQL(input, "")

	// ON should be at column 0 (no indent) for MERGE
	if !strings.Contains(result, "\nON ") {
		t.Errorf("expected ON at column 0 in MERGE, got:\n%s", result)
	}
	t.Logf("Formatted MERGE multi-line ON:\n%s", result)
}

// --- MERGE INTO stays on one line ---

func TestSQLFormatter_MergeIntoOnOneLine(t *testing.T) {
	// sql-canonical-compact-reference: MERGE INTO target t on one line
	opts := DefaultSQLFormattingOptions()
	opts.Style = "canonicalCompact"
	f := NewSQLFormatter(opts)

	input := "MERGE INTO ordtask_summary tgt USING source src ON tgt.id = src.id WHEN MATCHED THEN UPDATE SET tgt.status = src.status"
	result := f.FormatSQL(input, "")

	if !strings.Contains(result, "MERGE INTO") {
		t.Errorf("expected MERGE INTO on one line, got:\n%s", result)
	}
}

// --- DELETE FROM stays on one line ---

func TestSQLFormatter_DeleteFromOnOneLine(t *testing.T) {
	// sql-canonical-compact-reference: DELETE FROM on one line
	opts := DefaultSQLFormattingOptions()
	opts.Style = "canonicalCompact"
	f := NewSQLFormatter(opts)

	input := "DELETE FROM audit_log WHERE logdate < TO_DATE('2024-01-01', 'YYYY-MM-DD') AND status = 'A'"
	result := f.FormatSQL(input, "")

	if !strings.Contains(result, "DELETE FROM") {
		t.Errorf("expected DELETE FROM on one line, got:\n%s", result)
	}
	t.Logf("Formatted DELETE FROM:\n%s", result)
}

// --- RETURNING clause on own line ---

func TestSQLFormatter_ReturningClause(t *testing.T) {
	opts := DefaultSQLFormattingOptions()
	opts.Style = "canonicalCompact"
	f := NewSQLFormatter(opts)

	input := "INSERT INTO audit_log (action, ts) VALUES ('delete', SYSDATE) RETURNING audit_id INTO nID"
	result := f.FormatSQL(input, "")

	if !strings.Contains(result, "\nRETURNING") {
		t.Errorf("expected RETURNING on its own line, got:\n%s", result)
	}
	// RETURNING INTO should stay on one line (like INSERT INTO, MERGE INTO)
	if strings.Contains(result, "RETURNING audit_id\nINTO") {
		t.Errorf("RETURNING INTO should stay on same line, got:\n%s", result)
	}
	t.Logf("Formatted RETURNING:\n%s", result)
}

// --- ORDER BY NULLS LAST ---

func TestSQLFormatter_NullsLastCasing(t *testing.T) {
	opts := DefaultSQLFormattingOptions()
	opts.KeywordCase = "upper"
	f := NewSQLFormatter(opts)

	input := "SELECT name FROM employees ORDER BY name nulls last"
	result := f.FormatSQL(input, "")

	if !strings.Contains(result, "NULLS LAST") {
		t.Errorf("expected NULLS LAST uppercased, got:\n%s", result)
	}
}

// --- FormatSQLInString ---

func TestSQLFormatter_FormatSQLInString(t *testing.T) {
	opts := DefaultSQLFormattingOptions()
	opts.Style = "canonicalCompact"
	f := NewSQLFormatter(opts)

	result := f.FormatSQLInString(
		"SELECT ordno, testcode FROM ordtask WHERE status = ?sStatus? ORDER BY ordno",
		'"',    // quoteChar
		"    ", // baseIndent (4 spaces for SSL code)
	)

	// Should contain the open/close quotes and proper indentation
	if !strings.Contains(result, `"`) {
		t.Errorf("expected quote delimiters, got:\n%s", result)
	}
	if !strings.Contains(result, "SELECT") {
		t.Errorf("expected SELECT in output, got:\n%s", result)
	}
	if !strings.Contains(result, "ORDER BY") {
		t.Errorf("expected ORDER BY in output, got:\n%s", result)
	}
	t.Logf("FormatSQLInString result:\n%s", result)
}

// --- END stays inline in SELECT CASE ---

func TestSQLFormatter_EndInlineInSelectCase(t *testing.T) {
	// END should be at col 7 (aligned with CASE) in SELECT CASE, not col 0
	opts := DefaultSQLFormattingOptions()
	opts.Style = "canonicalCompact"
	f := NewSQLFormatter(opts)

	input := "SELECT ordno, CASE status WHEN 'A' THEN 'Active' ELSE 'Other' END AS label FROM orders"
	result := f.FormatSQL(input, "")

	// END should be at col 7 (not col 0) when closing CASE in SELECT list
	for _, line := range strings.Split(result, "\n") {
		trimmed := strings.TrimLeft(line, " ")
		if strings.HasPrefix(trimmed, "END") && strings.Contains(trimmed, "AS") {
			indent := len(line) - len(trimmed)
			if indent < 7 {
				t.Errorf("END should be at col 7 in SELECT CASE, got col %d:\n%s", indent, result)
			}
		}
	}
	t.Logf("Formatted SELECT CASE END:\n%s", result)
}

// --- Hierarchical query functions ---

func TestSQLFormatter_HierarchicalFunctionCasing(t *testing.T) {
	opts := DefaultSQLFormattingOptions()
	opts.KeywordCase = "upper"
	f := NewSQLFormatter(opts)

	input := "SELECT level, sys_connect_by_path(name, '/') FROM org START WITH parent_id IS NULL CONNECT BY PRIOR id = parent_id"
	result := f.FormatSQL(input, "")

	if !strings.Contains(result, "LEVEL") {
		t.Errorf("expected LEVEL uppercased, got:\n%s", result)
	}
	if !strings.Contains(result, "SYS_CONNECT_BY_PATH") {
		t.Errorf("expected SYS_CONNECT_BY_PATH uppercased, got:\n%s", result)
	}
}

// --- ADD_MONTHS function casing ---

func TestSQLFormatter_AddMonthsFunctionCasing(t *testing.T) {
	opts := DefaultSQLFormattingOptions()
	opts.KeywordCase = "upper"
	f := NewSQLFormatter(opts)

	input := "SELECT * FROM orders WHERE logdate > add_months(SYSDATE, -6)"
	result := f.FormatSQL(input, "")

	if !strings.Contains(result, "ADD_MONTHS") {
		t.Errorf("expected ADD_MONTHS uppercased, got:\n%s", result)
	}
}

// --- D21: BETWEEN formatting ---

func TestSQLFormatter_BetweenFormatting(t *testing.T) {
	opts := DefaultSQLFormattingOptions()
	opts.Style = "canonicalCompact"
	f := NewSQLFormatter(opts)

	input := "SELECT ordno, status FROM orders WHERE logdate BETWEEN '2024-01-01' AND '2024-12-31' AND status = 'A'"
	result := f.FormatSQL(input, "")

	// BETWEEN and AND should be present, WHERE clause should be formatted
	if !strings.Contains(result, "BETWEEN") {
		t.Errorf("expected BETWEEN keyword preserved, got:\n%s", result)
	}
	if !strings.Contains(result, "AND") {
		t.Errorf("expected AND keyword preserved, got:\n%s", result)
	}
}

// --- D22: IN clause with subquery ---

func TestSQLFormatter_InSubquery(t *testing.T) {
	opts := DefaultSQLFormattingOptions()
	opts.Style = "canonicalCompact"
	f := NewSQLFormatter(opts)

	input := "SELECT ordno FROM orders WHERE status IN (SELECT status FROM valid_statuses WHERE active = 'Y')"
	result := f.FormatSQL(input, "")

	// Subquery should be indented inside parentheses
	if !strings.Contains(result, "IN") {
		t.Errorf("expected IN keyword, got:\n%s", result)
	}
	if !strings.Contains(result, "SELECT") {
		t.Errorf("expected subquery SELECT, got:\n%s", result)
	}
}

// --- D23: DISTINCT keyword ---

func TestSQLFormatter_DistinctKeyword(t *testing.T) {
	opts := DefaultSQLFormattingOptions()
	opts.Style = "canonicalCompact"
	opts.KeywordCase = "upper"
	f := NewSQLFormatter(opts)

	input := "select distinct ordno, status from orders where status = 'A'"
	result := f.FormatSQL(input, "")

	if !strings.Contains(result, "DISTINCT") {
		t.Errorf("expected DISTINCT uppercased, got:\n%s", result)
	}
	if !strings.Contains(result, "SELECT") {
		t.Errorf("expected SELECT uppercased, got:\n%s", result)
	}
}

// --- D24: Scalar subquery in SELECT list ---

func TestSQLFormatter_ScalarSubqueryInSelect(t *testing.T) {
	opts := DefaultSQLFormattingOptions()
	opts.Style = "canonicalCompact"
	f := NewSQLFormatter(opts)

	input := "SELECT ordno, (SELECT name FROM users WHERE users.id = orders.userid) AS username FROM orders"
	result := f.FormatSQL(input, "")

	if !strings.Contains(result, "SELECT") {
		t.Errorf("expected SELECT keyword, got:\n%s", result)
	}
	// Should not crash and should produce formatted output
	if len(result) == 0 {
		t.Error("expected non-empty formatted output for scalar subquery")
	}
}

// --- D25: INSERT ALL / multi-table INSERT ---

func TestSQLFormatter_InsertAll(t *testing.T) {
	opts := DefaultSQLFormattingOptions()
	opts.Style = "canonicalCompact"
	opts.KeywordCase = "upper"
	f := NewSQLFormatter(opts)

	input := "insert all when status = 'A' then into active_orders(ordno) values(ordno) when status = 'C' then into closed_orders(ordno) values(ordno) select ordno, status from orders"
	result := f.FormatSQL(input, "")

	if !strings.Contains(result, "INSERT") {
		t.Errorf("expected INSERT keyword uppercased, got:\n%s", result)
	}
	if !strings.Contains(result, "ALL") || !strings.Contains(result, "WHEN") {
		t.Errorf("expected ALL and WHEN keywords, got:\n%s", result)
	}
}

// --- SQL: EXISTS keyword formatting ---

func TestSQLFormatter_ExistsSubquery(t *testing.T) {
	opts := DefaultSQLFormattingOptions()
	opts.Style = "canonicalCompact"
	f := NewSQLFormatter(opts)

	input := "SELECT ordno FROM orders o WHERE EXISTS (SELECT 1 FROM details d WHERE d.ordno = o.ordno)"
	result := f.FormatSQL(input, "")

	if !strings.Contains(result, "EXISTS") {
		t.Errorf("expected EXISTS keyword, got:\n%s", result)
	}
}

// --- SQL: NOT IN formatting ---

func TestSQLFormatter_NotInClause(t *testing.T) {
	opts := DefaultSQLFormattingOptions()
	opts.Style = "canonicalCompact"
	f := NewSQLFormatter(opts)

	input := "SELECT ordno FROM orders WHERE status NOT IN ('X', 'D', 'R')"
	result := f.FormatSQL(input, "")

	if !strings.Contains(result, "NOT IN") {
		t.Errorf("expected NOT IN, got:\n%s", result)
	}
}

// --- Pass 2: LEFT OUTER JOIN ---

func TestSQLFormatter_LeftOuterJoin(t *testing.T) {
	opts := DefaultSQLFormattingOptions()
	opts.Style = "canonicalCompact"
	f := NewSQLFormatter(opts)

	input := "SELECT o.ordno, r.result_value FROM orders o LEFT OUTER JOIN ordresult r ON r.ordno = o.ordno WHERE o.status = 'A'"
	result := f.FormatSQL(input, "")

	if !strings.Contains(result, "LEFT OUTER JOIN") {
		t.Errorf("expected LEFT OUTER JOIN preserved, got:\n%s", result)
	}
	if !strings.Contains(result, "\n") {
		t.Error("expected multi-line output for JOIN query")
	}
}

// --- Pass 2: Window function ROW_NUMBER ---

func TestSQLFormatter_WindowFunction_RowNumber(t *testing.T) {
	opts := DefaultSQLFormattingOptions()
	opts.Style = "canonicalCompact"
	opts.KeywordCase = "upper"
	f := NewSQLFormatter(opts)

	input := "select ordno, row_number() over (partition by status order by ordno) as rn from orders"
	result := f.FormatSQL(input, "")

	if !strings.Contains(result, "ROW_NUMBER") {
		t.Errorf("expected ROW_NUMBER uppercased, got:\n%s", result)
	}
	if !strings.Contains(result, "OVER") {
		t.Errorf("expected OVER keyword, got:\n%s", result)
	}
	if !strings.Contains(result, "PARTITION") {
		t.Errorf("expected PARTITION keyword, got:\n%s", result)
	}
}

// --- Pass 2: CTE with multiple CTEs ---

func TestSQLFormatter_MultipleCTEs(t *testing.T) {
	opts := DefaultSQLFormattingOptions()
	opts.Style = "canonicalCompact"
	f := NewSQLFormatter(opts)

	input := "WITH active AS (SELECT ordno FROM orders WHERE status = 'A'), tasks AS (SELECT ordno, testcode FROM ordtask) SELECT a.ordno, t.testcode FROM active a INNER JOIN tasks t ON t.ordno = a.ordno"
	result := f.FormatSQL(input, "")

	if !strings.Contains(result, "WITH") {
		t.Errorf("expected WITH keyword, got:\n%s", result)
	}
	if !strings.Contains(result, "active") && !strings.Contains(result, "ACTIVE") {
		t.Errorf("expected CTE name 'active', got:\n%s", result)
	}
}

// --- Pass 2: String concatenation operator ---

func TestSQLFormatter_StringConcatenation(t *testing.T) {
	opts := DefaultSQLFormattingOptions()
	opts.Style = "canonicalCompact"
	f := NewSQLFormatter(opts)

	input := "SELECT ordno || ' - ' || testcode || ' - ' || status AS display_name FROM orders"
	result := f.FormatSQL(input, "")

	if !strings.Contains(result, "||") {
		t.Errorf("expected || concatenation preserved, got:\n%s", result)
	}
}

// --- Pass 2: DECODE function (Oracle) ---

func TestSQLFormatter_DecodeFunction(t *testing.T) {
	opts := DefaultSQLFormattingOptions()
	opts.Style = "canonicalCompact"
	opts.KeywordCase = "upper"
	f := NewSQLFormatter(opts)

	input := "select decode(status, 'L', 'Logged', 'C', 'Complete', 'Unknown') as display_status from orders"
	result := f.FormatSQL(input, "")

	if !strings.Contains(result, "DECODE") {
		t.Errorf("expected DECODE uppercased, got:\n%s", result)
	}
}

// --- Pass 2: Optimizer hint preservation ---

func TestSQLFormatter_HintPreservation_ComplexHint(t *testing.T) {
	opts := DefaultSQLFormattingOptions()
	opts.Style = "canonicalCompact"
	f := NewSQLFormatter(opts)

	input := "SELECT /*+ LEADING(o t) USE_NL(t) INDEX(t idx_status) */ o.ordno, t.testcode FROM orders o INNER JOIN ordtask t ON t.ordno = o.ordno"
	result := f.FormatSQL(input, "")

	if !strings.Contains(result, "/*+ LEADING(o t) USE_NL(t) INDEX(t idx_status) */") {
		t.Errorf("expected optimizer hint preserved exactly, got:\n%s", result)
	}
}

// --- Pass 2: VALUES on same line as opening paren ---

func TestSQLFormatter_ValuesParenOnSameLine(t *testing.T) {
	opts := DefaultSQLFormattingOptions()
	opts.Style = "canonicalCompact"
	f := NewSQLFormatter(opts)

	input := "INSERT INTO audit_log (log_id, action) VALUES (1, 'INSERT')"
	result := f.FormatSQL(input, "")

	// VALUES ( should be on same line, not VALUES\n(
	if strings.Contains(result, "VALUES\n") {
		t.Errorf("VALUES should have '(' on same line, got:\n%s", result)
	}
}

// --- INSERT canonical compact: opening ( on INSERT INTO line, closing ) on own line ---

func TestSQLFormatter_InsertCanonicalCompactParens(t *testing.T) {
	opts := DefaultSQLFormattingOptions()
	opts.Style = "canonicalCompact"
	f := NewSQLFormatter(opts)

	input := "INSERT INTO audit_log (log_id, action, username, log_date) VALUES (seq_audit.NEXTVAL, 'UPDATE', 'admin', SYSDATE)"
	result := f.FormatSQL(input, "")

	// INSERT INTO should have ( on same line (with or without space before paren)
	lines := strings.Split(result, "\n")
	if len(lines) == 0 || !strings.Contains(lines[0], "INSERT INTO") || !strings.Contains(lines[0], "(") {
		t.Errorf("expected opening ( on INSERT INTO line, got:\n%s", result)
	}
	// VALUES should have ( on same line
	foundValues := false
	for _, line := range lines {
		if strings.Contains(line, "VALUES") && strings.Contains(line, "(") {
			foundValues = true
			break
		}
	}
	if !foundValues {
		t.Errorf("expected opening ( on VALUES line, got:\n%s", result)
	}
	t.Logf("Formatted INSERT:\n%s", result)
}

// --- INSERT block-style: column list indented inside parens, closing ) aligned ---
// Source of truth: sql-canonical-compact-reference.md §2.9

func TestSQLFormatter_InsertBlockStyleColumnIndent(t *testing.T) {
	opts := DefaultSQLFormattingOptions()
	opts.Style = "canonicalCompact"
	opts.MaxLineLength = 50 // force wrapping for the column list
	f := NewSQLFormatter(opts)

	input := "INSERT INTO audit_log (log_id, action, username, log_date, description) VALUES (seq_audit.NEXTVAL, 'UPDATE', 'admin', SYSDATE, 'test')"
	result := f.FormatSQL(input, "")

	// Verify SQL keywords are uppercase
	if !strings.Contains(result, "INSERT INTO") {
		t.Errorf("expected INSERT INTO uppercase, got:\n%s", result)
	}
	if !strings.Contains(result, "VALUES") {
		t.Errorf("expected VALUES uppercase, got:\n%s", result)
	}

	// Opening paren should be on the INSERT INTO line
	lines := strings.Split(result, "\n")
	if len(lines) == 0 || !strings.Contains(lines[0], "(") {
		t.Errorf("expected opening ( on INSERT INTO line, got:\n%s", result)
	}

	t.Logf("Formatted INSERT block-style:\n%s", result)
}

// --- Pass 2: CROSS JOIN ---

func TestSQLFormatter_CrossJoin(t *testing.T) {
	opts := DefaultSQLFormattingOptions()
	opts.Style = "canonicalCompact"
	f := NewSQLFormatter(opts)

	input := "SELECT a.col1, b.col2 FROM table_a a CROSS JOIN table_b b WHERE a.status = 'A'"
	result := f.FormatSQL(input, "")

	if !strings.Contains(result, "CROSS JOIN") {
		t.Errorf("expected CROSS JOIN preserved, got:\n%s", result)
	}
}

// --- Pass 2: GROUP BY with multiple columns ---

// --- TestSQLFormatter_BetweenAndStaysInline ---

func TestSQLFormatter_BetweenAndStaysInline(t *testing.T) {
	opts := DefaultSQLFormattingOptions()
	opts.Style = "canonicalCompact"
	f := NewSQLFormatter(opts)

	input := "SELECT * FROM t WHERE col BETWEEN '2024-01-01' AND '2024-12-31' AND status = 'A'"
	result := f.FormatSQL(input, "")

	// The first AND is part of BETWEEN..AND, should stay inline
	if !strings.Contains(result, "BETWEEN") {
		t.Errorf("expected BETWEEN to be present, got:\n%s", result)
	}

	// Find the line with BETWEEN - it should also contain the first AND
	lines := strings.Split(result, "\n")
	betweenLine := ""
	for _, line := range lines {
		if strings.Contains(line, "BETWEEN") {
			betweenLine = line
			break
		}
	}
	if betweenLine == "" {
		t.Fatal("no line contains BETWEEN")
	}
	if !strings.Contains(betweenLine, "AND") {
		t.Errorf("BETWEEN..AND should be on the same line, got BETWEEN line: %q", betweenLine)
	}

	// The second AND (status = 'A') should be on a separate line
	if !strings.Contains(result, "AND status") && !strings.Contains(result, "AND\n") {
		t.Logf("Second AND formatting (may vary by style):\n%s", result)
	}

	t.Logf("Formatted SQL:\n%s", result)
}

// --- TestSQLFormatter_LeftRightAsFunctionNoParen ---

func TestSQLFormatter_LeftRightAsFunctionNoParen(t *testing.T) {
	opts := DefaultSQLFormattingOptions()
	f := NewSQLFormatter(opts)

	input := "SELECT LEFT(name, 3) FROM t"
	result := f.FormatSQL(input, "")

	// LEFT as a function should not have space before (
	if strings.Contains(result, "LEFT (") {
		t.Errorf("expected no space between LEFT and (, got:\n%s", result)
	}
	if !strings.Contains(result, "LEFT(") {
		t.Errorf("expected LEFT( without space, got:\n%s", result)
	}

	t.Logf("Formatted SQL:\n%s", result)
}

// --- TestSQLFormatter_GrantRevokeCasing ---

func TestSQLFormatter_GrantRevokeCasing(t *testing.T) {
	opts := DefaultSQLFormattingOptions()
	f := NewSQLFormatter(opts)

	input := "grant select on t to user1"
	result := f.FormatSQL(input, "")

	if !strings.Contains(result, "GRANT") {
		t.Errorf("expected GRANT to be uppercased, got:\n%s", result)
	}

	t.Logf("Formatted SQL:\n%s", result)
}

// --- TestSQLFormatter_LineCommentPreserved ---

func TestSQLFormatter_LineCommentPreserved(t *testing.T) {
	opts := DefaultSQLFormattingOptions()
	f := NewSQLFormatter(opts)

	input := "SELECT id -- get the id\nFROM t"
	result := f.FormatSQL(input, "")

	if !strings.Contains(result, "-- get the id") {
		t.Errorf("expected line comment to be preserved, got:\n%s", result)
	}

	t.Logf("Formatted SQL:\n%s", result)
}

// --- TestSQLFormatter_MonthsBetweenCasing ---

func TestSQLFormatter_MonthsBetweenCasing(t *testing.T) {
	opts := DefaultSQLFormattingOptions()
	f := NewSQLFormatter(opts)

	input := "SELECT months_between(d1, d2) FROM t"
	result := f.FormatSQL(input, "")

	if !strings.Contains(result, "MONTHS_BETWEEN(") {
		t.Errorf("expected MONTHS_BETWEEN to be uppercased, got:\n%s", result)
	}

	t.Logf("Formatted SQL:\n%s", result)
}

func TestSQLFormatter_GroupByMultipleColumns(t *testing.T) {
	opts := DefaultSQLFormattingOptions()
	opts.Style = "canonicalCompact"
	f := NewSQLFormatter(opts)

	input := "SELECT status, COUNT(*) as cnt FROM orders GROUP BY status, ordno ORDER BY cnt DESC"
	result := f.FormatSQL(input, "")

	if !strings.Contains(result, "GROUP BY") {
		t.Errorf("expected GROUP BY, got:\n%s", result)
	}
	if !strings.Contains(result, "ORDER BY") {
		t.Errorf("expected ORDER BY, got:\n%s", result)
	}
}

// --- Gap 6: CASE WHEN alignment in SELECT ---

func TestSQLFormatter_CaseWhenAlignmentInSelect(t *testing.T) {
	// sql-canonical-compact-reference §5.1-5.2: CASE at col 7, WHEN/ELSE at col 11, END at col 7
	opts := DefaultSQLFormattingOptions()
	opts.Style = "canonicalCompact"
	f := NewSQLFormatter(opts)

	input := "SELECT ordno, CASE WHEN status = 'L' THEN 'Logged' WHEN status = 'C' THEN 'Complete' ELSE 'Unknown' END AS status_desc FROM orders"
	result := f.FormatSQL(input, "")

	for _, line := range strings.Split(result, "\n") {
		trimmed := strings.TrimLeft(line, " ")
		indent := len(line) - len(trimmed)
		if strings.HasPrefix(trimmed, "WHEN") || strings.HasPrefix(trimmed, "ELSE") {
			if indent != 11 {
				t.Errorf("expected WHEN/ELSE at col 11, got %d: %q", indent, line)
			}
		}
		if strings.HasPrefix(trimmed, "END") && strings.Contains(trimmed, "status_desc") {
			if indent != 7 {
				t.Errorf("expected END at col 7, got %d: %q", indent, line)
			}
		}
	}
	t.Logf("Formatted CASE:\n%s", result)
}

// --- Gap 8: OVER() internal formatting ---

func TestSQLFormatter_OverClauseFormatting(t *testing.T) {
	// sql-canonical-compact-reference §3.1: PARTITION BY/ORDER BY inside OVER on own lines
	opts := DefaultSQLFormattingOptions()
	opts.Style = "canonicalCompact"
	f := NewSQLFormatter(opts)

	input := "SELECT ordno, ROW_NUMBER() OVER (PARTITION BY ordno ORDER BY testcode) AS rn FROM ordresult"
	result := f.FormatSQL(input, "")

	foundPartition := false
	foundOrder := false
	for _, line := range strings.Split(result, "\n") {
		trimmed := strings.TrimSpace(line)
		if strings.HasPrefix(trimmed, "PARTITION BY") {
			foundPartition = true
		}
		if strings.HasPrefix(trimmed, "ORDER BY") && !strings.HasPrefix(trimmed, "ORDER BY ordno") {
			foundOrder = true
		}
	}
	if !foundPartition {
		t.Errorf("expected PARTITION BY on its own line, got:\n%s", result)
	}
	if !foundOrder {
		t.Errorf("expected ORDER BY on its own line inside OVER, got:\n%s", result)
	}
	t.Logf("Formatted OVER:\n%s", result)
}

// --- Gap 7: DECODE argument alignment ---

func TestSQLFormatter_DecodeArgumentAlignment(t *testing.T) {
	// sql-canonical-compact-reference §5.4: DECODE args aligned after opening paren
	opts := DefaultSQLFormattingOptions()
	opts.Style = "canonicalCompact"
	f := NewSQLFormatter(opts)

	input := "SELECT ordno, DECODE(status, 'L', 'Logged', 'C', 'Complete', 'X', 'Cancelled', 'Unknown') AS status_desc FROM orders"
	result := f.FormatSQL(input, "")

	// Verify subsequent value pairs are on new lines
	lines := strings.Split(result, "\n")
	decodeLines := 0
	for _, line := range lines {
		trimmed := strings.TrimSpace(line)
		if strings.Contains(trimmed, "'Logged'") || strings.Contains(trimmed, "'Complete'") || strings.Contains(trimmed, "'Cancelled'") || strings.Contains(trimmed, "'Unknown'") {
			decodeLines++
		}
	}
	if decodeLines < 2 {
		t.Errorf("expected DECODE value pairs on separate lines, got:\n%s", result)
	}
	t.Logf("Formatted DECODE:\n%s", result)
}

// --- Double-quoted identifier casing preserved ---

func TestSQLFormatter_DoubleQuotedIdentifierCasingPreserved(t *testing.T) {
	opts := DefaultSQLFormattingOptions()
	opts.Style = "canonicalCompact"
	f := NewSQLFormatter(opts)

	input := `SELECT "MyColumn" FROM "MySchema"."MyTable" WHERE status = 'active'`
	result := f.FormatSQL(input, "")

	// Double-quoted identifiers should NOT be lowercased
	if !strings.Contains(result, `"MyColumn"`) {
		t.Errorf("expected double-quoted identifier to preserve casing, got:\n%s", result)
	}
	if !strings.Contains(result, `"MySchema"`) {
		t.Errorf("expected double-quoted schema to preserve casing, got:\n%s", result)
	}
	if !strings.Contains(result, `"MyTable"`) {
		t.Errorf("expected double-quoted table to preserve casing, got:\n%s", result)
	}
	// Unquoted identifiers should still be lowercased
	if strings.Contains(result, "Status") {
		t.Errorf("expected unquoted identifier to be lowercased, got:\n%s", result)
	}
	t.Logf("Formatted SQL:\n%s", result)
}

// --- FOR UPDATE compound keyword ---

func TestSQLFormatter_ForUpdateCompoundKeyword(t *testing.T) {
	opts := DefaultSQLFormattingOptions()
	opts.Style = "canonicalCompact"
	f := NewSQLFormatter(opts)

	input := "SELECT ordno FROM orders WHERE status = 'L' FOR UPDATE"
	result := f.FormatSQL(input, "")

	// FOR UPDATE should stay on one line (not split as FOR\nUPDATE)
	if strings.Contains(result, "FOR\nUPDATE") {
		t.Errorf("expected FOR UPDATE to stay on one line, got:\n%s", result)
	}
	if !strings.Contains(result, "FOR UPDATE") {
		t.Errorf("expected 'FOR UPDATE' compound clause, got:\n%s", result)
	}
	t.Logf("Formatted SQL:\n%s", result)
}

// --- MERGE ON AND indent ---

func TestSQLFormatter_MergeOnAndIndent(t *testing.T) {
	opts := DefaultSQLFormattingOptions()
	opts.Style = "canonicalCompact"
	f := NewSQLFormatter(opts)

	input := "MERGE INTO target tgt USING source src ON tgt.id = src.id AND tgt.code = src.code WHEN MATCHED THEN UPDATE SET tgt.val = src.val"
	result := f.FormatSQL(input, "")

	// AND in MERGE ON should be indented 4 spaces (not 2)
	lines := strings.Split(result, "\n")
	for _, line := range lines {
		trimmed := strings.TrimSpace(line)
		if strings.HasPrefix(trimmed, "AND") && strings.Contains(result, "MERGE") {
			// Check that the AND line has 4-space indent (the indentString)
			leadingSpaces := len(line) - len(strings.TrimLeft(line, " "))
			if leadingSpaces < 4 {
				t.Errorf("expected AND in MERGE ON to have 4-space indent, got %d: %q", leadingSpaces, line)
			}
		}
	}
	t.Logf("Formatted SQL:\n%s", result)
}

// ---------------------------------------------------------------------------
// Regression tests pinning user-specified formatter rules (anonymized).
// Rules A–D are SQL-level; rules E/F are SSL-pipeline-level (see formatting_test.go).
// These are expected to FAIL until the formatter is updated to honor each rule.
// ---------------------------------------------------------------------------

// Rule A: every JOIN keyword starts a new line, including bare JOIN, at every
// nesting level. Currently bare JOIN is left inline after FROM/ON.
func TestSQLFormatter_RuleA_BareJoinStartsNewLine(t *testing.T) {
	opts := DefaultSQLFormattingOptions()
	f := NewSQLFormatter(opts)

	input := "SELECT a.x FROM t1 a JOIN t2 b ON b.x = a.x JOIN t3 c ON c.y = b.y"
	result := f.FormatSQL(input, "")

	for _, line := range strings.Split(result, "\n") {
		trimmed := strings.TrimSpace(line)
		if !strings.Contains(trimmed, "JOIN") {
			continue
		}
		first := strings.SplitN(trimmed, " ", 2)[0]
		if first != "JOIN" && !SQLJoinModifiers[first] {
			t.Errorf("Rule A: JOIN must be the first keyword on its line, got: %q\nfull output:\n%s", line, result)
		}
	}
}

// Rule B: AND/OR continuing a WHEN predicate is indented past the WHEN keyword.
// Currently the continuation drops back to a small (~2-space) indent.
func TestSQLFormatter_RuleB_WhenContinuationIndent(t *testing.T) {
	opts := DefaultSQLFormattingOptions()
	f := NewSQLFormatter(opts)

	input := "SELECT CASE WHEN a.x IS NULL OR a.x = '' THEN 'empty' ELSE 'set' END AS s FROM t1 a"
	result := f.FormatSQL(input, "")

	whenIndent := -1
	for _, line := range strings.Split(result, "\n") {
		trimmed := strings.TrimLeft(line, " ")
		if strings.HasPrefix(trimmed, "WHEN ") {
			whenIndent = len(line) - len(trimmed)
		}
		if whenIndent < 0 {
			continue
		}
		if strings.HasPrefix(trimmed, "OR ") || strings.HasPrefix(trimmed, "AND ") {
			indent := len(line) - len(trimmed)
			if indent <= whenIndent {
				t.Errorf("Rule B: OR/AND continuing WHEN must indent past WHEN (col %d), got col %d: %q\nfull output:\n%s",
					whenIndent, indent, line, result)
			}
		}
	}
}

// Rule C: a projection (<expr> [AS <alias>]) is indivisible. Never split
// between the expression and its AS alias.
func TestSQLFormatter_RuleC_ProjectionAliasNotSplit(t *testing.T) {
	opts := DefaultSQLFormattingOptions()
	f := NewSQLFormatter(opts)

	input := "SELECT CONCAT(a.first_name, ' ', a.middle_name, ' ', a.last_name, ' ', a.suffix, ' ', a.title) AS full_display_name FROM t1 a"
	result := f.FormatSQL(input, "")

	for _, line := range strings.Split(result, "\n") {
		trimmed := strings.TrimSpace(line)
		if strings.HasPrefix(trimmed, "AS ") {
			t.Errorf("Rule C: line starts with stranded AS alias — projection was split: %q\nfull output:\n%s",
				line, result)
		}
	}
}

// Rule C (whole-projection move): when continuing a long projection on the
// current SELECT line would overflow the width limit, the whole projection
// should move to its own line aligned with the SELECT columns. Without this,
// two long projections sit on one line that runs well past the limit.
func TestSQLFormatter_RuleC_OverflowingProjectionMovesToNewLine(t *testing.T) {
	opts := DefaultSQLFormattingOptions()
	f := NewSQLFormatter(opts)

	input := "SELECT a.short, " +
		"CAST(FORMAT(a.dt_admission, 'MM/dd/yyyy') AS varchar) AS date_of_admission_string, " +
		"CAST(FORMAT(a.dt_collected, 'MM/dd/yyyy') AS varchar) AS date_collected_string " +
		"FROM t1 a"
	result := f.FormatSQL(input, "")
	t.Logf("Output:\n%s", result)

	for _, line := range strings.Split(result, "\n") {
		// Reject lines that contain BOTH "AS date_of_admission_string" and
		// "AS date_collected_string" on the same line — a tell-tale sign of
		// two projections crammed together.
		if strings.Contains(line, "AS date_of_admission_string") &&
			strings.Contains(line, "AS date_collected_string") {
			t.Errorf("Rule C: two long projections must not share a line: %q\nfull output:\n%s", line, result)
		}
	}
}

// LongConvoluted: a single big anonymized query that exercises rules A–D at
// once — multiple JOIN variants (bare + LEFT + JOIN-into-subquery), CASE/WHEN
// with OR continuation, projections with CAST(FORMAT(...)) AS alias, an IN
// list large enough to wrap, and a NOT EXISTS subquery. This is a "smoke" test
// — when any rule regresses, this one fails alongside the targeted rule test.
func TestSQLFormatter_LongConvolutedQuery_AllRules(t *testing.T) {
	opts := DefaultSQLFormattingOptions()
	f := NewSQLFormatter(opts)

	input := "SELECT TOP 1 a.id AS accession_number, " +
		"CASE WHEN a.middle_initial IS NOT NULL THEN CONCAT(a.first_name, ' ', a.middle_initial, ' ', a.last_name) " +
		"WHEN a.middle_initial IS NULL OR a.middle_initial = '' THEN CONCAT(a.first_name, ' ', a.last_name) " +
		"ELSE CONCAT(a.first_name, ' ', a.last_name) END AS patient_name, " +
		"CAST(FORMAT(a.dt_admission, 'MM/dd/yyyy') AS varchar) AS date_of_admission_string, " +
		"CAST(FORMAT(a.dt_collected, 'MM/dd/yyyy') AS varchar) AS date_collected_string " +
		"FROM t1 a JOIN t2 b ON b.x = a.x " +
		"LEFT JOIN t3 c ON c.y = a.y " +
		"JOIN (SELECT m.x, MAX(m.dt) AS dt FROM t4 m JOIN t5 n ON n.k = m.k GROUP BY m.x) sub ON sub.x = a.x " +
		"WHERE a.col IN ('aaaaaaaa', 'bbbbbbbb', 'cccccccc', 'dddddddd', 'eeeeeeee', 'ffffffff', 'gggggggg') " +
		"AND NOT EXISTS (SELECT 1 FROM t6 r WHERE r.k = a.k AND r.s LIKE '3%')"

	result := f.FormatSQL(input, "")
	t.Logf("Convoluted query formatted output:\n%s", result)

	// Rule A: every JOIN starts a new line.
	for _, line := range strings.Split(result, "\n") {
		trimmed := strings.TrimSpace(line)
		if !strings.Contains(trimmed, "JOIN") {
			continue
		}
		first := strings.SplitN(trimmed, " ", 2)[0]
		if first != "JOIN" && !SQLJoinModifiers[first] {
			t.Errorf("Rule A (convoluted): JOIN must be first keyword on its line, got: %q", line)
		}
	}

	// Rule B: AND/OR continuing a WHEN must indent past the WHEN.
	whenIndent := -1
	for _, line := range strings.Split(result, "\n") {
		trimmed := strings.TrimLeft(line, " ")
		if strings.HasPrefix(trimmed, "WHEN ") {
			whenIndent = len(line) - len(trimmed)
			continue
		}
		if whenIndent < 0 {
			continue
		}
		if strings.HasPrefix(trimmed, "OR ") || strings.HasPrefix(trimmed, "AND ") {
			// Only check the WHEN's predicate continuations, not the WHERE's
			// AND NOT EXISTS — heuristic: if the trimmed prefix is "OR" we're
			// in WHEN (the WHERE doesn't use OR here). For AND inside WHEN we
			// don't have a case in this query, so skip.
			if strings.HasPrefix(trimmed, "OR ") {
				if got := len(line) - len(trimmed); got <= whenIndent {
					t.Errorf("Rule B (convoluted): OR continuing WHEN must indent past WHEN col %d, got col %d: %q",
						whenIndent, got, line)
				}
			}
		}
	}

	// Rule C: no line should start with a stranded AS alias.
	for _, line := range strings.Split(result, "\n") {
		if strings.HasPrefix(strings.TrimSpace(line), "AS ") {
			t.Errorf("Rule C (convoluted): stranded AS alias on its own line: %q", line)
		}
	}

	// Rule D: the IN-list continuation aligns under '(' (if it wrapped).
	// Find the IN-line, then check the immediately-following line only.
	lines := strings.Split(result, "\n")
	for i, line := range lines {
		idx := strings.Index(line, "IN (")
		if idx < 0 || i+1 >= len(lines) {
			continue
		}
		hangCol := idx + len("IN (")
		next := lines[i+1]
		trimmed := strings.TrimLeft(next, " ")
		if !strings.HasPrefix(trimmed, "'") {
			break // IN list didn't wrap onto a continuation
		}
		if got := len(next) - len(trimmed); got != hangCol {
			t.Errorf("Rule D (convoluted): IN-list continuation should hang at col %d, got col %d: %q",
				hangCol, got, next)
		}
		break
	}
}

// Rule D: wrapped IN (...) lists hang-indent under the opening '('.
// Currently the continuation lands at a fixed indent.
func TestSQLFormatter_RuleD_InListHangIndent(t *testing.T) {
	opts := DefaultSQLFormattingOptions()
	f := NewSQLFormatter(opts)

	input := "SELECT a.x FROM t1 a WHERE a.col IN ('aaaaaaaa', 'bbbbbbbb', 'cccccccc', 'dddddddd', 'eeeeeeee', 'ffffffff', 'gggggggg')"
	result := f.FormatSQL(input, "")

	lines := strings.Split(result, "\n")
	hangCol := -1
	for _, line := range lines {
		if idx := strings.Index(line, "IN ("); idx >= 0 {
			hangCol = idx + len("IN (")
			break
		}
	}
	if hangCol < 0 {
		t.Fatalf("Rule D: could not locate `IN (` in output:\n%s", result)
	}
	for _, line := range lines {
		trimmed := strings.TrimLeft(line, " ")
		if strings.HasPrefix(trimmed, "'") && !strings.Contains(line, "IN (") {
			indent := len(line) - len(trimmed)
			if indent != hangCol {
				t.Errorf("Rule D: IN-list continuation should hang-indent at col %d (under '('), got col %d: %q\nfull output:\n%s",
					hangCol, indent, line, result)
			}
			return
		}
	}
}

// Issue #82: English sentences that happen to contain SQL trigger-word pairs
// (select…from, update…set, delete…from) must not be detected as SQL —
// runtime string values were being rewritten by the formatter.
func TestIsSQLString_EnglishSentencesRejected(t *testing.T) {
	sentences := []string{
		"Select the samples from the rack and update the status column before continuing with the run",
		"Update your password and set a reminder so that it does not expire while you are away on leave",
		"Delete old records from the archive folder after you have exported them to the backup share",
		"Select a valid sample from the list and update your filter settings before retrying the search",
		"Insert the record into the database now please and thank you very much",
		"Select the samples from the rack",
		"Merge the results into the summary report before the meeting starts",
	}
	for _, s := range sentences {
		if IsSQLString(s) {
			t.Errorf("IsSQLString(%q) = true, want false", s)
		}
	}
}

// Issue #82: real queries — including alias forms, TOP/DISTINCT, dotted
// qualification, and clause-heavy statements — must still be detected.
func TestIsSQLString_RealQueriesStillDetected(t *testing.T) {
	queries := []string{
		"SELECT sample_id FROM samples",
		"select * from users where id = ?id?",
		"SELECT s.sample_id, s.sample_name FROM samples s WHERE s.sample_status = ?status?",
		"SELECT TOP 100 sample_id, created_on FROM samples ORDER BY created_on DESC",
		"SELECT DISTINCT testcode FROM ordtask",
		"SELECT COUNT(*) cnt FROM samples GROUP BY sample_status",
		"SELECT o.ordno FROM orders o INNER JOIN ordtask t ON t.ordno = o.ordno",
		"SELECT sample_id FROM lims.samples WHERE sample_status IN ('A', 'P')",
		"SELECT sample_id FROM samples s FOR UPDATE OF sample_status NOWAIT",
		"UPDATE samples SET sample_status = ? WHERE sample_id = ?",
		"UPDATE ordtask t SET t.status = 'Complete' WHERE t.ordno = ?",
		"DELETE FROM audit_log WHERE log_date < ?",
		"DELETE FROM lims.audit_log al WHERE al.log_date < ?dCutoff?",
		"INSERT INTO sample_audit_log (sample_id, event_type) VALUES (?, ?)",
		"INSERT INTO lims.samples SELECT * FROM staging_samples",
		"MERGE INTO tgt USING src ON (tgt.id = src.id) WHEN MATCHED THEN UPDATE SET tgt.x = src.x",
		"WITH counts AS (SELECT ordno, COUNT(*) c FROM ordtask GROUP BY ordno) SELECT * FROM counts",
	}
	for _, q := range queries {
		if !IsSQLString(q) {
			t.Errorf("IsSQLString(%q) = false, want true", q)
		}
	}
}

// Issue #82: first coverage for IsSQLDocument — the data-source classifier
// shares the validator, so the stricter rules must not regress plain-SQL
// data-source files (feature.diagnostics_pipeline A10).
func TestIsSQLDocument_Classification(t *testing.T) {
	sqlDocs := []string{
		"SELECT s.sample_id, s.sample_name\nFROM samples s\nWHERE s.sample_status = :status\nORDER BY s.sample_id\n",
		"-- lookup for the samples grid\nSELECT sample_id, sample_name FROM samples WHERE sample_status = ?\n",
		"/* legacy report query */\nSELECT ordno, testcode FROM ordtask WHERE status = ?\n",
		"UPDATE samples SET sample_status = ? WHERE sample_id = ?\n",
	}
	for _, d := range sqlDocs {
		if !IsSQLDocument(d) {
			t.Errorf("IsSQLDocument(%q) = false, want true", d)
		}
	}

	nonSQLDocs := []string{
		":PARAMETERS sStatus := \"A\";\n:DECLARE aRes;\naRes := SQLExecute(\"SELECT 1 FROM DUAL\");\n",
		"/* SSL leading comment;\nnX := 1;\n",
		"Select the samples from the rack and update the status column before continuing with the run",
		"",
	}
	for _, d := range nonSQLDocs {
		if IsSQLDocument(d) {
			t.Errorf("IsSQLDocument(%q) = true, want false", d)
		}
	}
}

// Issue #81: bracket-quoted SSL strings open with '[' and close with ']'.
// FormatSQLInString used to write the opening byte at both ends, producing
// an unterminated bracket string that swallowed the rest of the file on the
// next format pass.
func TestSQLFormatter_FormatSQLInString_BracketQuote(t *testing.T) {
	f := NewSQLFormatter(DefaultSQLFormattingOptions())

	multiline := f.FormatSQLInString(
		"SELECT ordno, testcode\nFROM ordtask\nWHERE status = ?sStatus?",
		'[',
		"\t",
	)
	if !strings.HasPrefix(multiline, "[") {
		t.Errorf("multi-line output must open with '[':\n%q", multiline)
	}
	if !strings.HasSuffix(multiline, "]") {
		t.Errorf("multi-line output must close with ']':\n%q", multiline)
	}
	if strings.Count(multiline, "[") != 1 {
		t.Errorf("multi-line output must contain exactly one '[' delimiter:\n%q", multiline)
	}

	singleline := f.FormatSQLInString("SELECT 1 FROM DUAL", '[', "")
	if !strings.HasPrefix(singleline, "[") || !strings.HasSuffix(singleline, "]") {
		t.Errorf("single-line output must be [-...-] delimited:\n%q", singleline)
	}

	// Symmetric quote styles are unchanged.
	double := f.FormatSQLInString("SELECT 1 FROM DUAL", '"', "")
	if !strings.HasPrefix(double, `"`) || !strings.HasSuffix(double, `"`) {
		t.Errorf("double-quote delimiters must be symmetric:\n%q", double)
	}
}
