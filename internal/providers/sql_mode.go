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
			// Production banners close `*/;`. Left unmasked the `;`
			// aborted the header scan in SplitDataSourceHeader (issue
			// #208, criterion A24), so it is consumed here with any
			// same-line spacing before it.
			//
			// Masking it is a parsing convenience, NOT a judgement that
			// it is harmless. In an SSL document it genuinely is the
			// comment terminator. In a SQL-mode data source it is a
			// misplaced semicolon that STARLIMS rejects outright with
			// "Invalid SQL statement: remove any misplaced
			// semicolons(;)" — confirmed against a live server. That is
			// reported by checkDataSourceBannerSemicolon, which reads
			// the unmasked content (issue #249).
			j := i
			for j < len(content) && (content[j] == ' ' || content[j] == '\t') {
				j++
			}
			if j < len(content) && content[j] == ';' {
				i = j + 1
			}
		default:
			return maskRange(content, i)
		}
	}
	return maskRange(content, len(content))
}

// checkDataSourceHeaderSemicolons reports a semicolon in a SQL-mode
// data source's header region that terminates an *empty* statement.
//
// The body is covered by checkDataSourceSQLSemicolons; the header is not,
// because SplitDataSourceHeader masks it (comments to blanks, and the `;`
// closing a `*/;` banner consumed outright so the directive scan can
// proceed — criterion A24). Masking is a parsing convenience, not a
// verdict that what was masked is harmless.
//
// The rule is about the semicolon, not about any particular spelling
// around it: a data-source body runs as a single SQL command, so a `;`
// with no statement in front of it is a stray separator and STARLIMS
// refuses the document with "Invalid SQL statement: remove any misplaced
// semicolons(;)" — confirmed against a live server. A directive's own
// terminator has content before it and never flags. Deciding on
// emptiness rather than on a `*/;` pattern also covers the shapes a
// pattern misses: `*/` then `;` on its own line, a `;` after a `--`
// banner, and a doubled `;;` between directives (issue #249).
//
// Callers must already know the document is a SQL-mode data source.
func checkDataSourceHeaderSemicolons(region string) []Diagnostic {
	var diagnostics []Diagnostic
	line, col := 1, 1
	sawContent := false
	step := func(n int) {
		for k := 0; k < n; k++ {
			col++
		}
	}
	for i := 0; i < len(region); {
		c := region[i]
		switch {
		case c == '\n':
			line++
			col = 1
			i++
		case c == ' ' || c == '\t' || c == '\r':
			step(1)
			i++
		case strings.HasPrefix(region[i:], "/*"):
			end := strings.Index(region[i+2:], "*/")
			if end < 0 {
				return diagnostics
			}
			for _, r := range region[i : i+2+end+2] {
				if r == '\n' {
					line++
					col = 1
				} else {
					col++
				}
			}
			i += 2 + end + 2
		case strings.HasPrefix(region[i:], "--"):
			end := strings.IndexByte(region[i:], '\n')
			if end < 0 {
				return diagnostics
			}
			step(end)
			i += end
		case c == '\'':
			j := i + 1
			for j < len(region) {
				if region[j] == '\'' {
					if j+1 < len(region) && region[j+1] == '\'' {
						j += 2
						continue
					}
					j++
					break
				}
				j++
			}
			for _, r := range region[i:j] {
				if r == '\n' {
					line++
					col = 1
				} else {
					col++
				}
			}
			sawContent = true
			i = j
		case c == ';':
			if !sawContent {
				diagnostics = append(diagnostics, Diagnostic{
					Severity: SeverityError,
					Range: Range{
						Start: Position{Line: line - 1, Character: col - 1},
						End:   Position{Line: line - 1, Character: col},
					},
					Message: "Misplaced semicolon in a SQL data source. Nothing precedes it, so it is a stray statement separator - the body runs as a single SQL command and STARLIMS rejects the document with \"Invalid SQL statement: remove any misplaced semicolons(;)\". Delete it. (A ';' is required only after an SSL comment, and only in SSL documents.)",
					Source:  "ssl-lsp",
					Code:    CodeDatasourceSQLSemicolon,
				})
			}
			sawContent = false
			step(1)
			i++
		default:
			sawContent = true
			step(1)
			i++
		}
	}
	return diagnostics
}

// maskLeadingSSLBannerComments blanks a leading run of SSL-style block
// comments — `/*` runs closed by `;` rather than `*/` — when a
// data-source header statement follows them.
//
// maskLeadingSQLComments deliberately stops at an unterminated `/*`,
// because for a whole-document verdict that shape means SSL. But a
// banner written in SSL comment style sitting *above* a builder-directive
// header is header furniture, exactly like the `--` runs already made
// transparent here: it says nothing about whether the body below the
// directives is SQL. Left unmasked it aborted the header scan outright,
// so the header came back empty, the directives fell into the body, and
// their `:=` read as a strong SSL marker — classifying a plain SQL data
// source as SSL and lexing its SQL body as SSL. 32 of 1,610 production
// data sources carry this shape (issue #249).
//
// Only the unterminated form is handled; anything with a `*/` ahead is
// left to maskLeadingSQLComments, whose position this must not take.
func maskLeadingSSLBannerComments(content string) string {
	i := 0
	for i < len(content) {
		switch c := content[i]; {
		case c == ' ' || c == '\t' || c == '\r' || c == '\n':
			i++
			continue
		}
		if !strings.HasPrefix(content[i:], "/*") {
			break
		}
		// A `*/` anywhere ahead means this is the SQL comment form.
		if strings.Contains(content[i:], "*/") {
			return content
		}
		semi := strings.IndexByte(content[i:], ';')
		if semi < 0 {
			// No terminator at all: a genuinely unterminated comment, the
			// signal that this document is SSL. Leave it visible.
			return content
		}
		i += semi + 1
	}
	if i == 0 {
		return content
	}
	// Mask only when the banner actually introduces a header; otherwise
	// this is an SSL document whose comment happens to be terminated.
	rest := content[i:]
	for _, line := range strings.SplitAfter(rest, "\n") {
		if strings.TrimSpace(line) == "" {
			continue
		}
		if !isDataSourceHeaderLine(line) {
			return content
		}
		break
	}
	return maskRange(content, i)
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
// SSL diagnostics (issue #148). `--` line comments interleaved between
// header statements are equally transparent when a further header
// statement follows them (issue #208, criterion A25); trailing comments
// before the body stay with the body. A header statement starts with one
// of the known header keywords and runs through the line containing its
// terminating semicolon; blank lines between header statements belong to
// the header. Content that does not start with a header statement returns
// header == "" and body == content unchanged.
func SplitDataSourceHeader(content string) (header, body string) {
	masked := maskLeadingSQLComments(maskLeadingSSLBannerComments(content))
	lines := strings.SplitAfter(masked, "\n")
	end := 0 // number of leading lines consumed by the header
	i := 0
	for i < len(lines) {
		trimmed := strings.TrimSpace(lines[i])
		if trimmed == "" {
			i++
			continue
		}
		// A line holding nothing but stray separators is scanned past
		// rather than treated as the start of the body. Breaking here
		// dropped the directives below it into the body, where their
		// `:=` read as a strong SSL marker and misclassified a SQL
		// document. The separator itself is reported by
		// checkDataSourceHeaderSemicolons (issue #249).
		if strings.Trim(trimmed, "; \t") == "" {
			i++
			continue
		}
		if strings.HasPrefix(trimmed, "--") {
			// A run of `--` comment lines belongs to the header only when
			// another header statement follows it; otherwise it is body
			// commentary. Masked so the header text never carries SQL
			// comment syntax into SSL diagnostics.
			j := i
			for j < len(lines) {
				t := strings.TrimSpace(lines[j])
				if t == "" || strings.HasPrefix(t, "--") {
					j++
					continue
				}
				break
			}
			if j >= len(lines) || !isDataSourceHeaderLine(lines[j]) {
				break
			}
			for k := i; k < j; k++ {
				lines[k] = blankPreservingNewline(lines[k])
			}
			i = j
			continue
		}
		if !isDataSourceHeaderLine(lines[i]) {
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

// isDataSourceHeaderLine reports whether a line starts a builder-directive
// or :PARAMETERS header statement.
func isDataSourceHeaderLine(line string) bool {
	trimmed := strings.TrimSpace(line)
	if !strings.HasPrefix(trimmed, ":") {
		return false
	}
	word := trimmed[1:]
	for j, r := range word {
		if !(r >= 'a' && r <= 'z' || r >= 'A' && r <= 'Z') {
			word = word[:j]
			break
		}
	}
	return dataSourceHeaderKeywords[strings.ToUpper(word)]
}

// blankPreservingNewline replaces a line's content with spaces, keeping
// its length and trailing newline so positions stay aligned.
func blankPreservingNewline(line string) string {
	b := []byte(line)
	for i, c := range b {
		if c != '\n' && c != '\r' {
			b[i] = ' '
		}
	}
	return string(b)
}

// hasStrongSSLMarker reports whether body contains a SQL-exclusive SSL
// construct — decisive evidence that a data-source body is SSL, not SQL
// (issue #153):
//
//   - a non-directive colon keyword (:DECLARE, :IF, :RETURN, :PROCEDURE, …)
//     in **statement-leading** position: the start of the body, or just
//     after a `;`. SSL always writes them there. The position requirement
//     is what separates them from an Oracle-style bind, which sits after
//     an operator — `WHERE s = :default` used to read as the `:DEFAULT`
//     keyword and misclassify a plain SQL document as SSL, which is the
//     loud direction to get wrong (its SQL keywords then report as
//     undeclared variables). Binds whose name is not an SSL keyword were
//     always safe; this makes the colliding names safe too (issue #249).
//     The one SSL keyword that is also a data-source header directive,
//     :PARAMETERS, is excluded; the builder directives (:DSN/:TABLENAME/…)
//     are not SSL keywords, so they never match regardless.
//   - a `:=` assignment. A plain SQL statement never contains one; the
//     inline `:=` defaults of a :PARAMETERS header do, which is why callers
//     scan the body with the directive header already stripped by
//     SplitDataSourceHeader.
//
// Strings and comments are consumed as single tokens, so a `:DECLARE` or
// `:=` inside a SQL string or comment does not trip the check. The
// unterminated SSL comment form is detected separately
// (hasUnterminatedLeadingBlockComment) because the SSL lexer stops a `/*`
// at the first `;`, mis-reading SQL comments.
//
// Note the asymmetry this heuristic is built around: it tests for what
// SQL *cannot* contain rather than for what SSL looks like, and defaults
// to SQL. A document wrongly called SSL floods with bogus diagnostics; one
// wrongly called SQL merely runs fewer checks. Bias toward the quiet
// failure.
func hasStrongSSLMarker(body string) bool {
	// Line-leading `--` SQL comments are commentary, not code — a
	// commented-out directive (`--:PARAMETERS p := '';`, corpus-observed)
	// must not read as a `:=` marker (issue #208, criterion A26). Only the
	// line-leading form is blanked: `--` mid-line is SSL's decrement
	// operator territory and stays visible to the scan.
	lines := strings.SplitAfter(body, "\n")
	for i, line := range lines {
		if strings.HasPrefix(strings.TrimSpace(line), "--") {
			lines[i] = blankPreservingNewline(line)
		}
	}
	body = strings.Join(lines, "")
	statementLeading := true
	for _, t := range lexer.NewLexer(body).Tokenize() {
		switch t.Type {
		case lexer.TokenWhitespace, lexer.TokenComment:
			// Neither content nor a statement boundary: leave the
			// leading flag as it was.
			continue
		case lexer.TokenKeyword:
			name := strings.ToUpper(strings.TrimPrefix(t.Text, ":"))
			if statementLeading && name != "PARAMETERS" && constants.IsKeyword(name) {
				return true
			}
		case lexer.TokenOperator:
			if t.Text == ":=" {
				return true
			}
		}
		// Only a `;` opens a new statement; anything else means the
		// next token sits mid-expression.
		statementLeading = t.Type == lexer.TokenPunctuation && t.Text == ";"
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
	header, body := SplitDataSourceHeader(content)
	return IsSQLModeDataSourceSplit(content, header, body)
}

// IsSQLModeDataSourceSplit is IsSQLModeDataSource for a caller that has
// already split the header, so the two cannot drift apart.
//
// When a builder-directive header was found, the body decides and the
// leading-comment signal is not consulted: a banner above the header is
// furniture whatever comment style it uses, and the structure beneath it
// is the stronger evidence. The leading-comment rule still decides the
// headerless shapes it was written for — a comment-only stub, and an SSL
// data source that opens with an unterminated comment (issue #153,
// criteria A16/A22).
func IsSQLModeDataSourceSplit(content, header, body string) bool {
	if hasStrongSSLMarker(body) {
		return false
	}
	if strings.TrimSpace(header) != "" {
		return true
	}
	return !hasUnterminatedLeadingBlockComment(content)
}
