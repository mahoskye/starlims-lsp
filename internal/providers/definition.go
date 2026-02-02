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
// Respects scope precedence: local/parameter variables take precedence over public variables.
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

	// Find which procedure contains the cursor position (for scope awareness)
	var cursorProc *parser.ProcedureInfo
	if procedures != nil {
		cursorProc = parser.FindProcedureAtLine(procedures, line)
	}

	// Check for variables with scope precedence:
	// 1. First look for local/parameter variable in current procedure
	// 2. Then look for public variable
	// 3. Then fallback to any declared variable (for cases where procedures info isn't provided)

	// Step 1: Look for local/parameter variable in current procedure
	if cursorProc != nil {
		for _, v := range variables {
			if strings.ToLower(v.Name) == wordLower {
				if v.Scope == parser.ScopeLocal || v.Scope == parser.ScopeParameter {
					// Check if this variable is declared within the current procedure
					if v.Line >= cursorProc.StartLine && v.Line <= cursorProc.EndLine {
						return &Location{
							URI: uri,
							Range: Range{
								Start: Position{Line: v.Line - 1, Character: v.Column - 1},
								End:   Position{Line: v.Line - 1, Character: v.Column - 1 + len(v.Name)},
							},
						}
					}
				}
			}
		}
	}

	// Step 2: Look for public variable
	for _, v := range variables {
		if strings.ToLower(v.Name) == wordLower {
			if v.Scope == parser.ScopePublic {
				return &Location{
					URI: uri,
					Range: Range{
						Start: Position{Line: v.Line - 1, Character: v.Column - 1},
						End:   Position{Line: v.Line - 1, Character: v.Column - 1 + len(v.Name)},
					},
				}
			}
		}
	}

	// Step 3: Fallback - return any matching variable (for backward compatibility
	// when procedures info isn't provided)
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
// This is a simple text-based search without scope awareness.
func FindReferences(text string, line, column int, uri string, includeDeclaration bool) []Location {
	return FindReferencesWithScope(text, line, column, uri, includeDeclaration, nil, nil)
}

// FindReferencesWithScope finds all references to a symbol with scope awareness.
// For local variables and parameters, only returns references within the same procedure.
// For public variables and procedures, returns all references in the document.
func FindReferencesWithScope(text string, line, column int, uri string, includeDeclaration bool, procedures []parser.ProcedureInfo, variables []parser.VariableInfo) []Location {
	word := lexer.GetWordAtPosition(text, line, column)

	if word == "" {
		return nil
	}

	wordLower := strings.ToLower(word)
	lines := strings.Split(text, "\n")

	// Determine scope of the symbol we're searching for
	scopeStart := 0
	scopeEnd := len(lines)
	isLocalScope := false

	// Check if this is a local/parameter variable (scoped to a procedure)
	if procedures != nil && variables != nil {
		// Find which procedure contains the cursor position
		cursorProc := parser.FindProcedureAtLine(procedures, line)

		if cursorProc != nil {
			// Check if this word is a local/parameter variable in this procedure
			for _, v := range variables {
				if strings.ToLower(v.Name) == wordLower {
					// If it's a local or parameter variable, scope it to the procedure
					if v.Scope == parser.ScopeLocal || v.Scope == parser.ScopeParameter {
						// Check if this variable is declared within the cursor's procedure
						if v.Line >= cursorProc.StartLine && v.Line <= cursorProc.EndLine {
							scopeStart = cursorProc.StartLine - 1 // Convert to 0-based
							scopeEnd = cursorProc.EndLine         // Keep as 1-based for comparison
							isLocalScope = true
							break
						}
					}
				}
			}
		}
	}

	var locations []Location

	// Simple text-based search for the word
	wordRegex := regexp.MustCompile(`(?i)\b` + escapeRegex(word) + `\b`)

	// Find the declaration position
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
		// For local scope, skip lines outside the procedure
		if isLocalScope {
			lineNum := i + 1 // Convert to 1-based
			if lineNum < scopeStart+1 || lineNum > scopeEnd {
				continue
			}
		}

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
