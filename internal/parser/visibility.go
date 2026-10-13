package parser

import (
	"regexp"
	"strings"

	"starlims-lsp/internal/lexer"
)

// ParseVisibilityAnnotation reports whether a comment token's text is a
// procedure visibility annotation in its one recognized form —
// `/*@private;` or `/*@protected;`, lowercase, with no space anywhere —
// and returns the content ("private" or "protected"). Any other spelling
// is an ordinary comment to SSL and leaves the procedure public
// (ssl-style-guide visibility_annotation_exact_form). The matching rules
// are shared with the visibility_annotation diagnostic
// (catalog/diagnostics/visibility_annotation.md, issue #255).
func ParseVisibilityAnnotation(commentText string) (string, bool) {
	switch strings.TrimSpace(commentText) {
	case "/*@private;":
		return "private", true
	case "/*@protected;":
		return "protected", true
	}
	return "", false
}

var nearMissAnnotation = regexp.MustCompile(`(?i)^/\*\s*@\s*(private|protected)\s*;$`)

// ParseNearMissVisibilityAnnotation reports whether a comment is shaped
// like a visibility annotation — `/*`, `@`, private/protected, `;` with any
// casing or interior spacing — without being the exact form. Such a
// comment is inert, which the visibility_annotation diagnostic reports
// when it stands before a `:PROCEDURE`. Returns the lowercased content.
func ParseNearMissVisibilityAnnotation(commentText string) (string, bool) {
	text := strings.TrimSpace(commentText)
	if _, exact := ParseVisibilityAnnotation(text); exact {
		return "", false
	}
	m := nearMissAnnotation.FindStringSubmatch(text)
	if m == nil {
		return "", false
	}
	return strings.ToLower(m[1]), true
}

// AnnotationAttachment says how a visibility annotation relates to the
// `:PROCEDURE` that follows it.
type AnnotationAttachment int

const (
	// AnnotationNoProcedure: the next significant token is not
	// `:PROCEDURE` (another keyword, an expression, or end of file).
	AnnotationNoProcedure AnnotationAttachment = iota
	// AnnotationAttached: the annotation sits on the line immediately
	// before `:PROCEDURE`, with only a line break between them — the one
	// placement that takes effect.
	AnnotationAttached
	// AnnotationDetached: a `:PROCEDURE` follows, but a blank line,
	// another comment, or a shared line separates them, so the annotation
	// does not attach (ssl-style-guide visibility_annotation_form).
	AnnotationDetached
)

// VisibilityAnnotationAttachment classifies the comment at tokens[idx]
// against the next `:PROCEDURE`. It returns the index of the next
// significant token (skipping whitespace and comments), or -1.
func VisibilityAnnotationAttachment(tokens []lexer.Token, idx int) (AnnotationAttachment, int) {
	newlines := 0
	sawComment := false
	for j := idx + 1; j < len(tokens); j++ {
		t := tokens[j]
		switch t.Type {
		case lexer.TokenWhitespace:
			newlines += strings.Count(t.Text, "\n")
			continue
		case lexer.TokenComment:
			sawComment = true
			continue
		case lexer.TokenEOF:
			return AnnotationNoProcedure, -1
		}
		if t.Type != lexer.TokenKeyword || !strings.EqualFold(strings.TrimPrefix(t.Text, ":"), "PROCEDURE") {
			return AnnotationNoProcedure, j
		}
		if sawComment || newlines != 1 {
			return AnnotationDetached, j
		}
		return AnnotationAttached, j
	}
	return AnnotationNoProcedure, -1
}
