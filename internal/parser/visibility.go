package parser

import "strings"

// ParseVisibilityAnnotation reports whether a comment token's text is a
// procedure visibility annotation (`/*@private;` or `/*@protected;`) and
// returns the annotation content in its original casing. The matching rules
// are shared with the visibility_annotation diagnostic
// (catalog/diagnostics/visibility_annotation.md): the comment must start
// literally with "/*@" (no space), and the content before the terminating
// ';' must be "private" or "protected", case-insensitive.
func ParseVisibilityAnnotation(commentText string) (string, bool) {
	text := strings.TrimSpace(commentText)
	if !strings.HasPrefix(text, "/*@") {
		return "", false
	}
	content := strings.TrimSpace(strings.TrimSuffix(text[3:], ";"))
	lower := strings.ToLower(content)
	if lower != "private" && lower != "protected" {
		return "", false
	}
	return content, true
}
