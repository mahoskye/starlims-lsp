package providers

import (
	"regexp"
	"strings"

	"starlims-lsp/internal/lexer"
	"starlims-lsp/internal/parser"
)

// Location represents a location in a document.
type Location struct {
	URI   string
	Range Range
}

// FindDefinition finds the definition for a symbol.
func FindDefinition(text string, line, column int, uri string, procedures []parser.ProcedureInfo, variables []parser.VariableInfo) *Location {
	word := lexer.GetWordAtPosition(text, line, column)

	if word == "" {
		return nil
	}

	wordLower := strings.ToLower(word)

	// Check if it's a procedure
	for _, proc := range procedures {
		if strings.ToLower(proc.Name) == wordLower {
			return &Location{
				URI: uri,
				Range: Range{
					Start: Position{Line: proc.StartLine - 1, Character: 0},
					End:   Position{Line: proc.StartLine - 1, Character: len(proc.Name) + 11}, // :PROCEDURE + name
				},
			}
		}
	}

	// Check if it's a variable
	for _, v := range variables {
		if strings.ToLower(v.Name) == wordLower {
			return &Location{
				URI: uri,
				Range: Range{
					Start: Position{Line: v.Line - 1, Character: v.Column - 1},
					End:   Position{Line: v.Line - 1, Character: v.Column - 1 + len(v.Name)},
				},
			}
		}
	}

	return nil
}

// FindReferences finds all references to a symbol.
func FindReferences(text string, line, column int, uri string, includeDeclaration bool) []Location {
	word := lexer.GetWordAtPosition(text, line, column)

	if word == "" {
		return nil
	}

	var locations []Location
	lines := strings.Split(text, "\n")

	// Simple text-based search for the word
	wordRegex := regexp.MustCompile(`(?i)\b` + escapeRegex(word) + `\b`)

	// Find the declaration position (first occurrence, typically where cursor is)
	var declarationLine, declarationChar int
	declarationFound := false

	// Check if the cursor position is on a declaration keyword line
	if line > 0 && line <= len(lines) {
		cursorLineText := lines[line-1]
		cursorLineLower := strings.ToLower(cursorLineText)
		// Declaration keywords that indicate this is the definition site
		if strings.Contains(cursorLineLower, ":declare") ||
			strings.Contains(cursorLineLower, ":parameters") ||
			strings.Contains(cursorLineLower, ":public") ||
			strings.Contains(cursorLineLower, ":procedure") {
			declarationLine = line - 1 // Convert to 0-based
			declarationChar = column - 1
			declarationFound = true
		}
	}

	for i, lineText := range lines {
		matches := wordRegex.FindAllStringIndex(lineText, -1)
		for _, match := range matches {
			// Skip declaration if not including it
			if !includeDeclaration && declarationFound {
				if i == declarationLine && match[0] <= declarationChar && declarationChar < match[1] {
					continue
				}
			}

			locations = append(locations, Location{
				URI: uri,
				Range: Range{
					Start: Position{Line: i, Character: match[0]},
					End:   Position{Line: i, Character: match[0] + len(word)},
				},
			})
		}
	}

	return locations
}

// escapeRegex escapes special regex characters.
func escapeRegex(s string) string {
	special := regexp.MustCompile(`[.*+?^${}()|[\]\\]`)
	return special.ReplaceAllStringFunc(s, func(m string) string {
		return `\` + m
	})
}
