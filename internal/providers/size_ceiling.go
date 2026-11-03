package providers

// The size ceiling (issue #358): unlike the review triggers in
// size_review.go, a file over ssl.diagnostics.maxScriptBytes is past a
// limit. STARLIMS compiles a script as one unit with its :INCLUDEd code on
// every load, and the agents that write and review SSL read at most about
// 50 KB at a time. ssl-style-guide records the rule (script_size_ceiling).

import (
	"fmt"
	"strings"
	"unicode/utf8"

	"starlims-lsp/internal/lexer"
)

// defaultMaxScriptBytes is the style guide's ceiling, 50 KB.
const defaultMaxScriptBytes = 50 * 1024

// tokensByteLen returns the byte length of the text tokens were read from:
// the lexer keeps every byte, line endings included, in some token.
func tokensByteLen(tokens []lexer.Token) int {
	n := 0
	for _, t := range tokens {
		n += len(t.Text)
	}
	return n
}

// checkScriptSizeCeiling reports script_size_ceiling, ranged over line 1
// (width characters wide), when size bytes is over maxBytes. A zero
// maxBytes disables it.
func checkScriptSizeCeiling(size, width, maxBytes int) []Diagnostic {
	if maxBytes <= 0 || size <= maxBytes {
		return nil
	}
	return []Diagnostic{{
		Severity: SeverityWarning,
		Range:    Range{Start: Position{Line: 0, Character: 0}, End: Position{Line: 0, Character: width}},
		Message:  fmt.Sprintf("This file is %s; the limit is %s. Split it by responsibility until each part fits", formatFileSize(size, maxBytes), formatFileSize(maxBytes, maxBytes)),
		Source:   "ssl-lsp",
		Code:     CodeScriptSizeCeiling,
	}}
}

// formatFileSize writes n bytes in whole KB, rounded up so a file just over
// the limit never reads as the limit itself. A limit that is no whole
// number of KB puts both sizes in bytes instead.
func formatFileSize(n, limit int) string {
	if limit%1024 != 0 {
		return fmt.Sprintf("%d bytes", n)
	}
	return fmt.Sprintf("%d KB", (n+1023)/1024)
}

// firstTextLineWidth returns the length in characters of the first line of
// text, without its line ending.
func firstTextLineWidth(text string) int {
	if k := strings.IndexByte(text, '\n'); k >= 0 {
		text = text[:k]
	}
	return utf8.RuneCountInString(strings.TrimSuffix(text, "\r"))
}
