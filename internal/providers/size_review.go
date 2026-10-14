package providers

// Info-tier review triggers for size (issue #344): a long procedure or a
// large server script is a prompt to check its responsibilities, not a
// limit. Thresholds come from settings; ssl-style-guide records the
// defaults (procedure_length_review, script_size_review), measured on
// STARLIMS 12's base layer.

import (
	"fmt"
	"strings"
	"unicode/utf8"

	"starlims-lsp/internal/lexer"
)

// Default review-trigger thresholds (ssl-style-guide schema
// lints.style_rules.procedure_length_review / script_size_review).
const (
	defaultMaxProcedureCodeLines = 80
	defaultMaxScriptCodeLines    = 400
	defaultMaxScriptProcedures   = 12
)

// codeLineSet returns the 1-based lines that carry code: any token other
// than whitespace or a comment, counting every line a multi-line token
// spans. A comment runs from `/*` to its first `;`, so a line holding only
// comment text is not a code line.
func codeLineSet(tokens []lexer.Token) map[int]bool {
	lines := map[int]bool{}
	for _, t := range tokens {
		switch t.Type {
		case lexer.TokenWhitespace, lexer.TokenComment, lexer.TokenEOF:
			continue
		}
		span := strings.Count(t.Text, "\n")
		for l := t.Line; l <= t.Line+span; l++ {
			lines[l] = true
		}
	}
	return lines
}

// countCodeLines counts the code lines in [from, to], 1-based inclusive.
func countCodeLines(lines map[int]bool, from, to int) int {
	n := 0
	for l := from; l <= to; l++ {
		if lines[l] {
			n++
		}
	}
	return n
}

// checkSizeReview reports procedure_length_review on each procedure with
// more than maxProcLines code lines, and script_size_review once per server
// script with more than maxScriptLines code lines or more than maxProcs
// local procedures. A zero threshold disables its half of the check.
// Class files are measured per method only, and data sources not at all
// (the caller gates them), as the style guide scopes the rules.
func checkSizeReview(tokens []lexer.Token, maxProcLines, maxScriptLines, maxProcs int) []Diagnostic {
	if maxProcLines <= 0 && maxScriptLines <= 0 && maxProcs <= 0 {
		return nil
	}
	lines := codeLineSet(tokens)
	var out []Diagnostic

	isClass := false
	procCount := 0
	openIdx := -1 // index of the open :PROCEDURE keyword
	closeProc := func(endLine int) {
		if openIdx < 0 {
			return
		}
		idx := openIdx
		openIdx = -1
		kw := tokens[idx]
		n := countCodeLines(lines, kw.Line, endLine)
		if maxProcLines <= 0 || n <= maxProcLines {
			return
		}
		r := tokenToRange(kw)
		name := ""
		if j := nextSignificantTokenIndex(tokens, idx+1); j >= 0 && tokens[j].Type == lexer.TokenIdentifier && tokens[j].Line == kw.Line {
			name = tokens[j].Text
			r.End = tokenToRange(tokens[j]).End
		}
		what := "This procedure"
		if name != "" {
			what = fmt.Sprintf("Procedure '%s'", name)
		}
		out = append(out, Diagnostic{
			Severity: SeverityInfo,
			Range:    r,
			Message:  fmt.Sprintf("%s has %d code lines (review trigger: %d). Check whether it does more than one thing", what, n, maxProcLines),
			Source:   "ssl-lsp",
			Code:     CodeProcedureLengthReview,
		})
	}

	lastLine := 1
	for i, t := range tokens {
		if t.Type == lexer.TokenEOF {
			continue
		}
		if end := t.Line + strings.Count(t.Text, "\n"); end > lastLine {
			lastLine = end
		}
		if t.Type != lexer.TokenKeyword {
			continue
		}
		switch strings.ToUpper(strings.TrimPrefix(t.Text, ":")) {
		case "CLASS":
			isClass = true
		case "PROCEDURE":
			// A :PROCEDURE with no :ENDPROC ends where the next begins.
			closeProc(t.Line - 1)
			openIdx = i
			procCount++
		case "ENDPROC":
			closeProc(t.Line)
		}
	}
	closeProc(lastLine)

	if isClass {
		return out
	}
	total := countCodeLines(lines, 1, lastLine)
	overLines := maxScriptLines > 0 && total > maxScriptLines
	overProcs := maxProcs > 0 && procCount > maxProcs
	if overLines || overProcs {
		out = append(out, Diagnostic{
			Severity: SeverityInfo,
			Range:    Range{Start: Position{Line: 0, Character: 0}, End: Position{Line: 0, Character: firstLineWidth(tokens)}},
			Message: fmt.Sprintf("This script has %d code lines and %d procedures (review triggers: %d lines, %d procedures). Check its responsibilities: use regions first, and a separate script only for a reusable responsibility",
				total, procCount, maxScriptLines, maxProcs),
			Source: "ssl-lsp",
			Code:   CodeScriptSizeReview,
		})
	}
	return out
}

// firstLineWidth returns the length in characters of line 1.
func firstLineWidth(tokens []lexer.Token) int {
	width := 0
	for _, t := range tokens {
		if t.Line != 1 || t.Type == lexer.TokenEOF {
			break
		}
		if k := strings.IndexByte(t.Text, '\n'); k >= 0 {
			return width + utf8.RuneCountInString(strings.TrimRight(t.Text[:k], "\r"))
		}
		width += utf8.RuneCountInString(t.Text)
	}
	return width
}
