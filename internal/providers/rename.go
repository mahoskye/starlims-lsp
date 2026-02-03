package providers

import (
	"strings"

	"starlims-lsp/internal/constants"
	"starlims-lsp/internal/lexer"
	"starlims-lsp/internal/parser"
)

// RenameResult contains the result of a rename operation.
type RenameResult struct {
	// Changes maps document URIs to the text edits for that document.
	Changes map[string][]TextEdit
}

// PrepareRenameResult contains the result of a prepare rename operation.
type PrepareRenameResult struct {
	Range       Range
	Placeholder string
}

// PrepareRename checks if a rename is valid at the given position.
// Returns nil if renaming is not allowed at this position.
func PrepareRename(text string, line, column int, uri string, procedures []parser.ProcedureInfo, variables []parser.VariableInfo) *PrepareRenameResult {
	tokens := lexer.NewLexer(text).Tokenize()

	// Check if we're inside a string or comment
	ctx := lexer.GetContextAtPosition(tokens, line, column)
	if ctx == lexer.ContextString || ctx == lexer.ContextComment {
		return nil
	}

	word := lexer.GetWordAtPosition(text, line, column)
	if word == "" {
		return nil
	}

	// Check if this is a renameable symbol
	if !isRenameableSymbol(word, line, column, text, procedures, variables) {
		return nil
	}

	// Find the exact range of the word
	wordRange := getWordRange(text, line, column, word)
	if wordRange == nil {
		return nil
	}

	return &PrepareRenameResult{
		Range:       *wordRange,
		Placeholder: word,
	}
}

// Rename performs a rename operation, returning all text edits needed.
func Rename(text string, line, column int, newName string, uri string, procedures []parser.ProcedureInfo, variables []parser.VariableInfo) *RenameResult {
	// Validate the new name
	if !isValidIdentifier(newName) {
		return nil
	}

	word := lexer.GetWordAtPosition(text, line, column)
	if word == "" {
		return nil
	}

	// Check if this is a renameable symbol
	if !isRenameableSymbol(word, line, column, text, procedures, variables) {
		return nil
	}

	// Find all references using scope-aware search (always include declaration)
	locations := FindReferencesWithScope(text, line, column, uri, true, procedures, variables)
	if len(locations) == 0 {
		return nil
	}

	// Convert locations to text edits
	edits := make([]TextEdit, 0, len(locations))
	for _, loc := range locations {
		edits = append(edits, TextEdit{
			Range:   loc.Range,
			NewText: newName,
		})
	}

	return &RenameResult{
		Changes: map[string][]TextEdit{
			uri: edits,
		},
	}
}

// isRenameableSymbol checks if the symbol at the given position can be renamed.
// Keywords, built-in functions, and built-in classes cannot be renamed.
func isRenameableSymbol(word string, line, column int, text string, procedures []parser.ProcedureInfo, variables []parser.VariableInfo) bool {
	wordLower := strings.ToLower(word)

	// Cannot rename keywords
	for _, kw := range constants.SSLKeywords {
		if strings.ToLower(kw) == wordLower {
			return false
		}
	}

	// Cannot rename built-in functions
	if _, ok := constants.GetFunctionSignature(word); ok {
		return false
	}

	// Cannot rename built-in classes
	for _, cls := range constants.SSLClassNames {
		if strings.ToLower(cls) == wordLower {
			return false
		}
	}

	// Cannot rename literals
	if wordLower == ".t." || wordLower == ".f." || wordLower == "nil" {
		return false
	}

	// Cannot rename operators
	if wordLower == ".and." || wordLower == ".or." || wordLower == ".not." {
		return false
	}

	// Cannot rename 'Me' keyword
	if wordLower == "me" {
		return false
	}

	// Check if it's a procedure name (can rename)
	for _, proc := range procedures {
		if strings.ToLower(proc.Name) == wordLower {
			return true
		}
	}

	// Check if it's a variable (can rename)
	for _, v := range variables {
		if strings.ToLower(v.Name) == wordLower {
			return true
		}
	}

	// If we got here, it might be an undeclared variable or identifier
	// Allow renaming if it looks like a user-defined identifier
	return isUserDefinedIdentifier(word, text, line, column)
}

// isUserDefinedIdentifier checks if the word appears to be a user-defined identifier
// (not a keyword, built-in function, etc.)
func isUserDefinedIdentifier(word string, text string, line, column int) bool {
	// Must start with a letter or underscore
	if len(word) == 0 {
		return false
	}

	first := word[0]
	if !((first >= 'a' && first <= 'z') || (first >= 'A' && first <= 'Z') || first == '_') {
		return false
	}

	// Rest must be alphanumeric or underscore
	for _, c := range word[1:] {
		if !((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c == '_') {
			return false
		}
	}

	return true
}

// isValidIdentifier checks if the new name is a valid SSL identifier.
func isValidIdentifier(name string) bool {
	if len(name) == 0 {
		return false
	}

	// Cannot be a keyword
	nameLower := strings.ToLower(name)
	for _, kw := range constants.SSLKeywords {
		if strings.ToLower(kw) == nameLower {
			return false
		}
	}

	// Cannot be a built-in function
	if _, ok := constants.GetFunctionSignature(name); ok {
		return false
	}

	// Must start with letter or underscore
	first := name[0]
	if !((first >= 'a' && first <= 'z') || (first >= 'A' && first <= 'Z') || first == '_') {
		return false
	}

	// Rest must be alphanumeric or underscore
	for _, c := range name[1:] {
		if !((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c == '_') {
			return false
		}
	}

	return true
}

// getWordRange finds the range of the word at the given position.
func getWordRange(text string, line, column int, word string) *Range {
	lines := strings.Split(text, "\n")
	if line < 1 || line > len(lines) {
		return nil
	}

	lineText := lines[line-1]
	wordLower := strings.ToLower(word)

	// Find the word in the line, accounting for the cursor position
	pos := 0
	for {
		idx := strings.Index(strings.ToLower(lineText[pos:]), wordLower)
		if idx == -1 {
			break
		}

		startCol := pos + idx + 1 // 1-based column
		endCol := startCol + len(word)

		// Check if cursor is within this occurrence
		if column >= startCol && column <= endCol {
			// Verify it's a whole word (not part of a larger identifier)
			isWordStart := startCol == 1 || !isIdentifierChar(lineText[startCol-2])
			isWordEnd := endCol > len(lineText) || !isIdentifierChar(lineText[endCol-1])

			if isWordStart && isWordEnd {
				return &Range{
					Start: Position{Line: line - 1, Character: startCol - 1},
					End:   Position{Line: line - 1, Character: endCol - 1},
				}
			}
		}

		pos += idx + 1
		if pos >= len(lineText) {
			break
		}
	}

	return nil
}

// isIdentifierChar checks if a character can be part of an identifier.
func isIdentifierChar(c byte) bool {
	return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c == '_'
}
