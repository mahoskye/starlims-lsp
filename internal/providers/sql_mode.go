// Package providers implements LSP feature providers for SSL.
package providers

import "strings"

// dataSourceHeaderKeywords are the statements that may legally precede the
// raw SQL body of a sql_data_source module: the builder directives
// (:DSN / :TABLENAME / :NULLASBLANK / :INVARIANTDATECOLUMNS := value;) and
// the inline-defaults :PARAMETERS header (issue #104; style-guide schema
// data_source_modules).
var dataSourceHeaderKeywords = map[string]bool{
	"DSN":                  true,
	"TABLENAME":            true,
	"NULLASBLANK":          true,
	"INVARIANTDATECOLUMNS": true,
	"PARAMETERS":           true,
}

// SplitDataSourceHeader splits data-source content into a leading header of
// builder-directive / :PARAMETERS lines and the remaining body. A header
// statement starts with one of the known header keywords and runs through
// the line containing its terminating semicolon; blank lines between header
// statements belong to the header. Content that does not start with a
// header statement returns header == "" and body == content.
func SplitDataSourceHeader(content string) (header, body string) {
	lines := strings.SplitAfter(content, "\n")
	end := 0 // number of leading lines consumed by the header
	i := 0
	for i < len(lines) {
		trimmed := strings.TrimSpace(lines[i])
		if trimmed == "" {
			i++
			continue
		}
		if !strings.HasPrefix(trimmed, ":") {
			break
		}
		word := trimmed[1:]
		for j, r := range word {
			if !(r >= 'a' && r <= 'z' || r >= 'A' && r <= 'Z') {
				word = word[:j]
				break
			}
		}
		if !dataSourceHeaderKeywords[strings.ToUpper(word)] {
			break
		}
		// Consume through the line holding the statement's semicolon.
		for i < len(lines) {
			done := strings.Contains(lines[i], ";")
			i++
			if done {
				break
			}
		}
		end = i
	}

	return strings.Join(lines[:end], ""), strings.Join(lines[end:], "")
}

// IsSQLModeDataSource reports whether data-source content is in SQL mode:
// either a plain SQL document (feature.diagnostics_pipeline A10) or builder
// directives / an inline-defaults :PARAMETERS header followed by a SQL
// statement — the canonical sql_data_source shape (issue #104). Callers
// must already know the document is a data-source file; this function only
// classifies its content.
func IsSQLModeDataSource(content string) bool {
	if IsSQLDocument(content) {
		return true
	}
	header, body := SplitDataSourceHeader(content)
	return strings.TrimSpace(header) != "" && IsSQLDocument(body)
}
