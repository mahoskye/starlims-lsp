package providers

import (
	"strings"
	"unicode/utf8"

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

	// Dispatch-target last segment (issue #125): the procedure segment of a
	// dotted DoProc/ExecFunction string is renameable — the server carries
	// the edit workspace-wide. Category/script segments are not (script
	// rename is a file rename, out of scope), and 1-part targets keep the
	// string rejection below — same-file rename starts from the identifier.
	if dt := DispatchTargetAt(tokens, line, column); dt != nil && len(dt.Parts) >= 2 {
		lastSeg := dt.Parts[len(dt.Parts)-1]
		segStart := dt.Range.End.Character - utf8.RuneCountInString(lastSeg)
		if column-1 >= segStart && column-1 <= dt.Range.End.Character {
			return &PrepareRenameResult{
				Range: Range{
					Start: Position{Line: dt.Range.Start.Line, Character: segStart},
					End:   Position{Line: dt.Range.Start.Line, Character: dt.Range.End.Character},
				},
				Placeholder: lastSeg,
			}
		}
		return nil
	}

	// Check if we're inside a string or comment
	ctx := lexer.GetContextAtPosition(tokens, line, column)
	if ctx == lexer.ContextString || ctx == lexer.ContextComment {
		return nil
	}

	word := lexer.GetWordAtPosition(text, line, column)
	if word == "" {
		return nil
	}

	// A member name belongs to its receiver's type, which is not resolved
	// here: renaming it would edit every like-spelled member in the file,
	// built-in .NET properties included (D10, issue #255).
	if cursorOnMember(text, line, column) {
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

	// Rename repeats PrepareRename's guards — a client may skip prepare
	// (issue #258): no edits from inside a comment or a string, and member
	// names are never renamed.
	if ctx := lexer.GetContextAtPosition(lexer.NewLexer(text).Tokenize(), line, column); ctx == lexer.ContextString || ctx == lexer.ContextComment {
		return nil
	}
	if cursorOnMember(text, line, column) {
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

	// Cannot rename class-context forms (Me, Base, Constructor)
	if constants.IsSSLClassContextForm(word) {
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
	return isUserDefinedIdentifier(word)
}

// isUserDefinedIdentifier checks if the word appears to be a user-defined identifier
// (not a keyword, built-in function, etc.)
func isUserDefinedIdentifier(word string) bool {
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

// IsValidNewName reports whether name is acceptable as a rename target —
// the same validation Rename applies, exported for the server's cross-file
// orchestration (issue #125).
func IsValidNewName(name string) bool {
	return isValidIdentifier(name)
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

	// Cannot be a class-context form (Me, Base, Constructor)
	if constants.IsSSLClassContextForm(name) {
		return false
	}

	// Cannot be a literal (NIL) or a built-in class name (issue #258): the
	// result would no longer be a user identifier.
	if nameLower == "nil" {
		return false
	}
	for _, cls := range constants.SSLClassNames {
		if strings.ToLower(cls) == nameLower {
			return false
		}
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

// getWordRange finds the range of the word at the given position. Columns
// are runes, matching the lexer and every other range the server emits —
// a byte column skews the range after any non-ASCII character on the line
// (issue #258).
func getWordRange(text string, line, column int, word string) *Range {
	lines := strings.Split(text, "\n")
	if line < 1 || line > len(lines) {
		return nil
	}

	runes := []rune(lines[line-1])
	wordRunes := []rune(strings.ToLower(word))
	n := len(wordRunes)
	if n == 0 {
		return nil
	}

	for start := 0; start+n <= len(runes); start++ {
		if strings.ToLower(string(runes[start:start+n])) != string(wordRunes) {
			continue
		}
		startCol := start + 1 // 1-based
		endCol := startCol + n
		if column < startCol || column > endCol {
			continue
		}
		isWordStart := start == 0 || !isIdentifierRune(runes[start-1])
		isWordEnd := start+n >= len(runes) || !isIdentifierRune(runes[start+n])
		if isWordStart && isWordEnd {
			return &Range{
				Start: Position{Line: line - 1, Character: start},
				End:   Position{Line: line - 1, Character: start + n},
			}
		}
	}

	return nil
}

// isIdentifierRune checks if a rune can be part of an identifier.
func isIdentifierRune(r rune) bool {
	return r < 128 && isIdentifierChar(byte(r))
}

// isIdentifierChar checks if a character can be part of an identifier.
func isIdentifierChar(c byte) bool {
	return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c == '_'
}
