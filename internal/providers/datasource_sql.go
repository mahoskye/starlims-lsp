// Package providers implements LSP feature providers for SSL.
package providers

import (
	"fmt"
	"strings"

	"starlims-lsp/internal/lexer"
)

// checkDataSourceSQLSemicolons flags semicolons that sit outside comments
// and string literals in the SQL body of a SQL-mode data source
// (diag.datasource_sql_semicolon, issue #154). Semicolons inside SQL
// comments and quoted literals are content with no syntactic significance
// (style-guide schema
// module_structure.data_source_modules.sql_data_source.comments) and never
// flag — the SQL lexer already consumes them as part of the comment or
// string token. A bare `;` outside both is worth a warning: the body is
// sent to the database as a single command, and the schema does not define
// `;` statement separators for data-source bodies, so their behavior is
// platform-dependent at best.
//
// lineOffset shifts diagnostic positions down by the number of document
// lines that precede body — the directive header in the hybrid
// header-then-SQL shape; pass 0 when body is the whole document.
func checkDataSourceSQLSemicolons(body string, lineOffset int) []Diagnostic {
	var diagnostics []Diagnostic
	for _, tok := range NewSQLLexer(body).Tokenize() {
		if tok.Type != SQLTokenPunctuation || tok.Text != ";" {
			continue
		}
		diagnostics = append(diagnostics, Diagnostic{
			Severity: SeverityWarning,
			Range: Range{
				Start: Position{Line: lineOffset + tok.Line - 1, Character: tok.Column - 1},
				End:   Position{Line: lineOffset + tok.Line - 1, Character: tok.Column},
			},
			Message: "Semicolon outside comments and string literals in a SQL data-source body. The body runs as a single SQL command; ';' statement separators are not part of the data-source format and may fail on some database platforms.",
			Source:  "ssl-lsp",
			Code:    CodeDatasourceSQLSemicolon,
		})
	}
	return diagnostics
}

// dataSourceParameterNames extracts the declared parameter names from the
// first :PARAMETERS statement in a data-source header. Only depth-zero
// identifiers in name position count: inline := defaults may themselves
// contain identifiers, commas, and nested delimiters (`p1 := Foo(1, 2)`,
// `p2 := {1,2}`), which the delimiter tracking skips past.
func dataSourceParameterNames(header string) []string {
	tokens := lexer.NewLexer(header).Tokenize()
	i := 0
	for ; i < len(tokens); i++ {
		if tokens[i].Type == lexer.TokenKeyword && strings.EqualFold(tokens[i].Text, ":PARAMETERS") {
			break
		}
	}
	if i == len(tokens) {
		return nil
	}

	var names []string
	depth := 0
	expectName := true
	for i++; i < len(tokens); i++ {
		t := tokens[i]
		switch t.Type {
		case lexer.TokenPunctuation:
			switch t.Text {
			case "(", "{", "[":
				depth++
			case ")", "}", "]":
				if depth > 0 {
					depth--
				}
			case ",":
				if depth == 0 {
					expectName = true
				}
			case ";":
				if depth == 0 {
					return names
				}
			}
		case lexer.TokenIdentifier:
			if depth == 0 && expectName {
				names = append(names, t.Text)
				expectName = false
			}
		}
	}
	return names
}

// checkDataSourceUndeclaredPlaceholders flags @name placeholders in a
// SQL-mode data-source body that have no matching :PARAMETERS declaration
// (diag.datasource_undeclared_placeholder; style-guide schema
// module_structure.data_source_modules.lint_rules): an unmatched
// placeholder is not substituted and fails when the query executes.
// Matching is case-insensitive, per the schema's body_parameters note that
// names match the declarations. Never flagged:
//
//   - @name inside string literals or SQL comments — content, not
//     placeholders (the SQL lexer consumes them as part of those tokens);
//   - @@name — a database system function, not a placeholder;
//   - any placeholder when the body contains a DECLARE keyword: a body
//     that declares its own SQL variables is scripted SQL, where @name
//     may be a local variable, so the check bows out entirely rather
//     than risk false positives.
//
// declared holds the :PARAMETERS names (nil when the document has no
// header); lineOffset shifts positions as in
// checkDataSourceSQLSemicolons.
func checkDataSourceUndeclaredPlaceholders(body string, declared []string, lineOffset int) []Diagnostic {
	var tokens []SQLToken
	for _, t := range NewSQLLexer(body).Tokenize() {
		if t.Type == SQLTokenWhitespace {
			continue
		}
		if t.Type == SQLTokenIdentifier && strings.EqualFold(t.Text, "DECLARE") {
			return nil
		}
		tokens = append(tokens, t)
	}

	adjacent := func(a, b SQLToken) bool {
		return a.Line == b.Line && a.Column+len(a.Text) == b.Column
	}
	isAt := func(t SQLToken) bool {
		return t.Type == SQLTokenUnknown && t.Text == "@"
	}

	var diagnostics []Diagnostic
	for i, t := range tokens {
		if !isAt(t) || i+1 >= len(tokens) {
			continue
		}
		name := tokens[i+1]
		if name.Type != SQLTokenIdentifier && name.Type != SQLTokenFunction && name.Type != SQLTokenKeyword {
			continue
		}
		if !adjacent(t, name) {
			continue
		}
		// @@name is a system function, not a placeholder.
		if i > 0 && isAt(tokens[i-1]) && adjacent(tokens[i-1], t) {
			continue
		}
		known := false
		for _, d := range declared {
			if strings.EqualFold(d, name.Text) {
				known = true
				break
			}
		}
		if known {
			continue
		}
		diagnostics = append(diagnostics, Diagnostic{
			Severity: SeverityWarning,
			Range: Range{
				Start: Position{Line: lineOffset + t.Line - 1, Character: t.Column - 1},
				End:   Position{Line: lineOffset + name.Line - 1, Character: name.Column - 1 + len(name.Text)},
			},
			Message: fmt.Sprintf("SQL data-source placeholder '@%s' has no matching :PARAMETERS declaration — it is not substituted and fails when the query executes.", name.Text),
			Source:  "ssl-lsp",
			Code:    CodeDatasourceUndeclaredPlaceholder,
		})
	}
	return diagnostics
}
