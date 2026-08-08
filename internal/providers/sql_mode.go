// Package providers implements LSP feature providers for SSL.
package providers

import (
	"strings"

	"starlims-lsp/internal/constants"
	"starlims-lsp/internal/lexer"
)

// dataSourceHeaderKeywords are the statements that may legally precede the
// raw SQL body of a sql_data_source module: the builder directives
// (:DSN / :TABLENAME / :NULLASBLANK / :INVARIANTDATECOLUMNS := value;) and
// the :PARAMETERS header (issue #104; style-guide schema
// data_source_modules).
var dataSourceHeaderKeywords = map[string]bool{
	"DSN":                  true,
	"TABLENAME":            true,
	"NULLASBLANK":          true,
	"INVARIANTDATECOLUMNS": true,
	"PARAMETERS":           true,
}

// maskLeadingSQLComments blanks out the leading run of terminated SQL
// comments (`/* ... */` blocks and `--` line comments) and returns the
// result. Newlines are preserved and every masked byte becomes a space, so
// positions in the returned string line up with the original. Masking stops
// at the first non-comment content; an unterminated `/*` comment — the SSL
// form, which has no `*/` — is never masked (feature.diagnostics_pipeline
// A16/A17, issue #148).
func maskLeadingSQLComments(content string) string {
	i := 0
	for i < len(content) {
		switch {
		case content[i] == ' ' || content[i] == '\t' || content[i] == '\r' || content[i] == '\n':
			i++
		case strings.HasPrefix(content[i:], "--"):
			end := strings.IndexByte(content[i:], '\n')
			if end < 0 {
				end = len(content) - i
			}
			i += end
		case strings.HasPrefix(content[i:], "/*"):
			end := strings.Index(content[i+2:], "*/")
			if end < 0 {
				// Unterminated: SSL comment territory — mask nothing here.
				return maskRange(content, i)
			}
			i += 2 + end + 2
		default:
			return maskRange(content, i)
		}
	}
	return maskRange(content, len(content))
}

// maskRange replaces every non-newline byte of content[:end] with a space.
func maskRange(content string, end int) string {
	if end == 0 {
		return content
	}
	masked := []byte(content[:end])
	for j := range masked {
		if masked[j] != '\n' {
			masked[j] = ' '
		}
	}
	return string(masked) + content[end:]
}

// IsSQLCommentOnly reports whether data-source content consists solely of
// terminated SQL comments and whitespace, with at least one comment — the
// canonical header_comment-only stub (a banner and nothing else). The
// unterminated SSL comment form never qualifies
// (feature.diagnostics_pipeline A16, issue #148).
func IsSQLCommentOnly(content string) bool {
	if strings.TrimSpace(content) == "" {
		return false
	}
	return strings.TrimSpace(maskLeadingSQLComments(content)) == ""
}

// SplitDataSourceHeader splits data-source content into a leading header of
// builder-directive / :PARAMETERS lines and the remaining body. Leading
// terminated SQL comments (the schema's optional header_comment) are
// transparent: they are masked to blanks — positions preserved — so a
// banner before the first directive neither defeats detection nor reaches
// SSL diagnostics (issue #148). A header statement starts with one of the
// known header keywords and runs through the line containing its
// terminating semicolon; blank lines between header statements belong to
// the header. Content that does not start with a header statement returns
// header == "" and body == content unchanged.
func SplitDataSourceHeader(content string) (header, body string) {
	masked := maskLeadingSQLComments(content)
	lines := strings.SplitAfter(masked, "\n")
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

	if end == 0 {
		return "", content
	}
	return strings.Join(lines[:end], ""), strings.Join(lines[end:], "")
}

// hasStrongSSLMarker reports whether body contains a SQL-exclusive SSL
// construct — decisive evidence that a data-source body is SSL, not SQL
// (issue #153):
//
//   - a non-directive colon keyword (:DECLARE, :IF, :RETURN, :PROCEDURE, …).
//     SQL has no `:KEYWORD` syntax at all, and the lexer only emits a
//     keyword token off a leading colon, so any such token is SSL. The one
//     SSL keyword that is also a data-source header directive, :PARAMETERS,
//     is excluded; the builder directives (:DSN/:TABLENAME/…) are not SSL
//     keywords, so they never match regardless.
//   - a `:=` assignment. A plain SQL statement never contains one; the
//     inline `:=` defaults of a :PARAMETERS header do, which is why callers
//     scan the body with the directive header already stripped by
//     SplitDataSourceHeader.
//
// Strings and comments are consumed as single tokens, so a `:DECLARE` or
// `:=` inside a SQL string or comment does not trip the check. Oracle-style
// binds (`= :status`) lex as a colon keyword whose name is not an SSL
// keyword, so they do not either. The unterminated SSL comment form is
// detected separately (hasUnterminatedLeadingBlockComment) because the SSL
// lexer stops a `/*` at the first `;`, mis-reading SQL comments.
func hasStrongSSLMarker(body string) bool {
	for _, t := range lexer.NewLexer(body).Tokenize() {
		switch t.Type {
		case lexer.TokenKeyword:
			name := strings.ToUpper(strings.TrimPrefix(t.Text, ":"))
			if name != "PARAMETERS" && constants.IsKeyword(name) {
				return true
			}
		case lexer.TokenOperator:
			if t.Text == ":=" {
				return true
			}
		}
	}
	return false
}

// hasUnterminatedLeadingBlockComment reports whether content begins with a
// `/*` block comment that has no `*/` terminator — the SSL comment form,
// which the comment-termination check must still flag
// (feature.diagnostics_pipeline A16). maskLeadingSQLComments blanks every
// leading terminated SQL comment (closing on `*/`, so embedded `;` and
// string content are irrelevant); if a bare `/*` still leads what remains,
// it is unterminated and the document is SSL, not SQL.
func hasUnterminatedLeadingBlockComment(content string) bool {
	rest := strings.TrimLeft(maskLeadingSQLComments(content), " \t\r\n")
	return strings.HasPrefix(rest, "/*")
}

// IsSQLModeDataSource reports whether data-source content is in SQL mode.
// A data-source file is SQL by default — the overwhelmingly common case —
// and only classifies as SSL when its body carries a strong SSL marker
// (hasStrongSSLMarker) or leads with an unterminated SSL comment (issue
// #153). The directive / :PARAMETERS header is stripped first so its
// keywords and inline `:=` defaults never read as SSL (A13/A17/A18); plain
// SQL (A10), a comment-only stub (A16), and the hybrid header-then-SQL
// shapes all fall through to SQL because their bodies hold no SSL marker.
// Callers must already know the document is a data-source file; this
// function only classifies its content.
func IsSQLModeDataSource(content string) bool {
	_, body := SplitDataSourceHeader(content)
	return !hasStrongSSLMarker(body) && !hasUnterminatedLeadingBlockComment(content)
}
