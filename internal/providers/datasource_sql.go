// Package providers implements LSP feature providers for SSL.
package providers

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
