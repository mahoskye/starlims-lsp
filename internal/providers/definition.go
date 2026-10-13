package providers

import (
	"regexp"
	"strings"
	"unicode/utf8"

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
// Also supports go-to-definition for DoProc/ExecFunction string targets.
func FindDefinition(text string, line, column int, uri string, procedures []parser.ProcedureInfo, variables []parser.VariableInfo) *Location {
	// First, check if cursor is inside a DoProc/ExecFunction string target
	if loc := findDoProcDefinition(text, line, column, uri, procedures); loc != nil {
		return loc
	}

	// A word inside a comment or a non-dispatch string is prose, not a
	// symbol use; dispatch strings were handled above (issue #255).
	tokens := lexer.NewLexer(text).Tokenize()
	if ctx := lexer.GetContextAtPosition(tokens, line, column); ctx == lexer.ContextComment || ctx == lexer.ContextString {
		return nil
	}

	word := lexer.GetWordAtPosition(text, line, column)

	if word == "" {
		return nil
	}

	wordLower := strings.ToLower(word)

	// What the identifier under the cursor *is* decides which symbol table
	// to consult (issue #184). Without it, `oRec:sName` jumped to a local
	// variable named sName, and a variable jumped to a like-named
	// procedure because procedures were searched first.
	cursorRole := roleAtTextPosition(text, line-1, column-1)
	if cursorRole == parser.RoleMember {
		// A property or method belongs to the receiver's type, which is
		// not resolved here; jumping to a like-named local would be a
		// wrong answer, and no answer is better than a wrong one.
		return nil
	}
	if cursorRole == parser.RoleInstantiation {
		return nil
	}

	// Check if it's a procedure
	if cursorRole != parser.RoleVariable && cursorRole != parser.RoleDeclaredName {
		if loc := procedureLocation(tokens, word, uri, procedures); loc != nil {
			return loc
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

	// Step 3: Fallback. Without procedure info — a script with no
	// :PROCEDURE blocks, where ExtractProcedures returns nil — there is no
	// scope to respect, so any match wins. With procedure info, only
	// file-level declarations qualify — another procedure's local is out of
	// scope at the cursor and must not be surfaced (issue #41).
	for _, v := range variables {
		if strings.ToLower(v.Name) != wordLower {
			continue
		}
		if procedures != nil && parser.FindProcedureAtLine(procedures, v.Line) != nil {
			continue
		}
		return &Location{
			URI: uri,
			Range: Range{
				Start: Position{Line: v.Line - 1, Character: v.Column - 1},
				End:   Position{Line: v.Line - 1, Character: v.Column - 1 + len(v.Name)},
			},
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

	// Find which procedure contains the cursor position
	var cursorProc *parser.ProcedureInfo
	if procedures != nil {
		cursorProc = parser.FindProcedureAtLine(procedures, line)
	}

	// Check if this is a local/parameter variable (scoped to a procedure)
	if procedures != nil && variables != nil {
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

	// A file-wide subject (a top-level or :PUBLIC variable) is shadowed
	// inside any procedure that declares the same name as a local or
	// parameter — mirroring definition's local-over-global precedence
	// (A4). Those procedures' variable occurrences belong to their own
	// local (issue #258).
	var shadowingProcs []parser.ProcedureInfo
	if !isLocalScope && procedures != nil {
		for _, proc := range procedures {
			for _, v := range variables {
				if strings.ToLower(v.Name) == wordLower &&
					(v.Scope == parser.ScopeLocal || v.Scope == parser.ScopeParameter) &&
					v.Line >= proc.StartLine && v.Line <= proc.EndLine {
					shadowingProcs = append(shadowingProcs, proc)
					break
				}
			}
		}
	}
	inShadowingProc := func(lineNum int) bool {
		for _, proc := range shadowingProcs {
			if lineNum >= proc.StartLine && lineNum <= proc.EndLine {
				return true
			}
		}
		return false
	}

	var locations []Location

	// Simple text-based search for the word
	wordRegex := regexp.MustCompile(`(?i)\b` + escapeRegex(word) + `\b`)

	// Resolve the declaration position from the parsed symbol info so it can
	// be excluded under includeDeclaration=false regardless of where the
	// request originated (issue #42).
	declarationLine, declarationChar, declarationFound := resolveDeclarationPosition(
		wordLower, wordRegex, lines, procedures, variables, cursorProc)

	// Fallback: without parsed symbol info (or when the symbol has no known
	// declaration), keep the original cursor-line heuristic — the declaration
	// is only detected when the request position sits on it.
	if !declarationFound && line > 0 && line <= len(lines) {
		cursorLineLower := strings.ToLower(lines[line-1])
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

	// Rune offset of the start of each line within text (tokens carry rune
	// offsets, while regex matches are byte offsets within a line).
	lineStartOffsets := make([]int, len(lines))
	runeOffset := 0
	for i, lineText := range lines {
		lineStartOffsets[i] = runeOffset
		runeOffset += utf8.RuneCountInString(lineText) + 1 // +1 for the split '\n'
	}

	// Tokenize once so each text match can be classified: matches inside
	// comments and non-dispatch strings are not references (issue #43).
	tokens := lexer.NewLexer(text).Tokenize()

	// Classify what each identifier occurrence *is* (issue #184). Word
	// matching cannot separate a variable `sName` from the property in
	// `oRec:sName` or from a procedure of the same name; the expression
	// tree can, so occurrences playing a different role than the symbol
	// under the cursor are not references to it.
	roles := parser.IdentifierRoles(tokens)
	cursorRole := roleAtPosition(tokens, roles, lineStartOffsets, line-1, column-1)

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
			// Regex matches are byte offsets; positions are rune columns
			// like every other range the server emits (issue #258).
			startChar := utf8.RuneCountInString(lineText[:match[0]])
			matchLen := utf8.RuneCountInString(lineText[match[0]:match[1]])

			// Skip declaration if not including it
			if !includeDeclaration && declarationFound {
				if i == declarationLine && startChar <= declarationChar && declarationChar < startChar+matchLen {
					continue
				}
			}

			// Skip matches inside comments and non-dispatch strings; keep
			// DoProc/ExecFunction first-argument dispatch targets (issue #43).
			matchOffset := lineStartOffsets[i] + startChar
			if !isReferenceMatch(tokens, matchOffset, matchLen) {
				continue
			}

			// Drop occurrences that name a different kind of symbol
			// (issue #184). Unclassified positions keep the word-match
			// behavior — the tree makes no claim there.
			// A surviving string match is a DoProc/ExecFunction dispatch
			// target, which names a procedure — never a member or a
			// variable (issue #255).
			matchRole := roleAtOffset(tokens, roles, matchOffset)
			inString := offsetInString(tokens, matchOffset)
			if matchRole == parser.RoleUnclassified && inString {
				matchRole = parser.RoleCall
			}
			if !rolesReferToSameSymbol(cursorRole, matchRole) {
				continue
			}

			// Variable occurrences inside a procedure that shadows a
			// file-wide subject belong to that procedure's local.
			if !inString && inShadowingProc(i+1) &&
				(symbolFamily(matchRole) == 1 || matchRole == parser.RoleUnclassified) {
				continue
			}

			locations = append(locations, Location{
				URI: uri,
				Range: Range{
					Start: Position{Line: i, Character: startChar},
					End:   Position{Line: i, Character: startChar + matchLen},
				},
			})
		}
	}

	return locations
}

// resolveDeclarationPosition locates the symbol's declaration from the parsed
// procedures/variables (0-based line, rune column within the line). Scope
// precedence mirrors FindDefinition: local/parameter in the cursor's
// procedure, then public, then any declared variable. Returns found=false
// when the symbol has no known declaration or no symbol info was provided.
func resolveDeclarationPosition(wordLower string, wordRegex *regexp.Regexp, lines []string, procedures []parser.ProcedureInfo, variables []parser.VariableInfo, cursorProc *parser.ProcedureInfo) (int, int, bool) {
	// Procedure declaration: the name on its :PROCEDURE line.
	for _, proc := range procedures {
		if strings.ToLower(proc.Name) != wordLower {
			continue
		}
		declLine := proc.StartLine - 1
		if declLine < 0 || declLine >= len(lines) {
			return 0, 0, false
		}
		if m := wordRegex.FindStringIndex(lines[declLine]); m != nil {
			return declLine, utf8.RuneCountInString(lines[declLine][:m[0]]), true
		}
		return declLine, 0, true
	}

	match := func(v parser.VariableInfo) (int, int, bool) {
		return v.Line - 1, v.Column - 1, true
	}

	// Local/parameter variable declared in the cursor's procedure.
	if cursorProc != nil {
		for _, v := range variables {
			if strings.ToLower(v.Name) == wordLower &&
				(v.Scope == parser.ScopeLocal || v.Scope == parser.ScopeParameter) &&
				v.Line >= cursorProc.StartLine && v.Line <= cursorProc.EndLine {
				return match(v)
			}
		}
	}

	// Public variable.
	for _, v := range variables {
		if strings.ToLower(v.Name) == wordLower && v.Scope == parser.ScopePublic {
			return match(v)
		}
	}

	// Fallback: any declared variable.
	for _, v := range variables {
		if strings.ToLower(v.Name) == wordLower {
			return match(v)
		}
	}

	return 0, 0, false
}

// isReferenceMatch reports whether a whole-word text match (rune offset and
// rune length within the document) is a real reference. Matches inside
// comment tokens never are; matches inside string tokens only count when the
// string is the first argument of DoProc/ExecFunction and the match spans the
// entire string content — the dispatch-target case (feature.references/A7).
// Matches in code (including code blocks) always count. (issue #43)
func isReferenceMatch(tokens []lexer.Token, matchOffset, matchLen int) bool {
	for i := range tokens {
		tok := &tokens[i]
		if tok.Type == lexer.TokenEOF || tok.Offset > matchOffset {
			break
		}
		if matchOffset >= tok.Offset+utf8.RuneCountInString(tok.Text) {
			continue
		}
		switch tok.Type {
		case lexer.TokenComment:
			return false
		case lexer.TokenString:
			return isDispatchTargetMatch(tokens, i, matchOffset, matchLen)
		default:
			return true
		}
	}
	return true
}

// isDispatchTargetMatch reports whether the match inside the string token at
// stringIdx is a DoProc/ExecFunction dispatch target: the string must be the
// first argument of a DoProc/ExecFunction call and the match must cover the
// whole string content (a string merely mentioning the name is not a call).
func isDispatchTargetMatch(tokens []lexer.Token, stringIdx, matchOffset, matchLen int) bool {
	tok := tokens[stringIdx]
	runes := []rune(tok.Text)

	// Properly quoted, non-empty string ("..." or '...'); bracket strings are
	// not legal dispatch syntax.
	if len(runes) < 3 || (runes[0] != '"' && runes[0] != '\'') || runes[len(runes)-1] != runes[0] {
		return false
	}

	// The match must be the entire string content.
	if matchOffset != tok.Offset+1 || matchLen != len(runes)-2 {
		return false
	}

	// Walk back: the string must directly follow "(" which follows a
	// DoProc/ExecFunction identifier.
	i := stringIdx - 1
	for i >= 0 && (tokens[i].Type == lexer.TokenWhitespace || tokens[i].Type == lexer.TokenComment) {
		i--
	}
	if i < 0 || tokens[i].Type != lexer.TokenPunctuation || tokens[i].Text != "(" {
		return false
	}
	i--
	for i >= 0 && (tokens[i].Type == lexer.TokenWhitespace || tokens[i].Type == lexer.TokenComment) {
		i--
	}
	if i < 0 || tokens[i].Type != lexer.TokenIdentifier {
		return false
	}
	name := strings.ToLower(tokens[i].Text)
	return name == "doproc" || name == "execfunction"
}

// escapeRegex escapes special regex characters.
func escapeRegex(s string) string {
	special := regexp.MustCompile(`[.*+?^${}()|[\]\\]`)
	return special.ReplaceAllStringFunc(s, func(m string) string {
		return `\` + m
	})
}

// findDoProcDefinition checks if the cursor is inside a 1-part
// DoProc/ExecFunction string target and returns the same-file procedure it
// names. The target is found by the token walk (ExtractCallSites), so a call
// split across lines resolves and a call-shaped comment never does
// (issue #255).
func findDoProcDefinition(text string, line, column int, uri string, procedures []parser.ProcedureInfo) *Location {
	tokens := lexer.NewLexer(text).Tokenize()
	dt := DispatchTargetAt(tokens, line, column)
	if dt == nil || len(dt.Parts) != 1 {
		return nil
	}
	return procedureLocation(tokens, dt.Parts[0], uri, procedures)
}

// procedureLocation returns the location of the same-file procedure named
// name (case-insensitive), ranged over exactly the name on its :PROCEDURE
// line, or nil.
func procedureLocation(tokens []lexer.Token, name, uri string, procedures []parser.ProcedureInfo) *Location {
	for _, proc := range procedures {
		if strings.EqualFold(proc.Name, name) {
			return &Location{URI: uri, Range: procedureNameSelectionRange(tokens, proc)}
		}
	}
	return nil
}

// roleAtOffset returns the identifier role of the token covering a rune
// offset, or RoleUnclassified when no identifier token covers it.
func roleAtOffset(tokens []lexer.Token, roles []parser.IdentifierRole, offset int) parser.IdentifierRole {
	for i := range tokens {
		tok := &tokens[i]
		if tok.Type == lexer.TokenEOF || tok.Offset > offset {
			break
		}
		if offset >= tok.Offset+utf8.RuneCountInString(tok.Text) {
			continue
		}
		if i < len(roles) {
			return roles[i]
		}
		return parser.RoleUnclassified
	}
	return parser.RoleUnclassified
}

// offsetInString reports whether a rune offset falls inside a string token.
func offsetInString(tokens []lexer.Token, offset int) bool {
	for i := range tokens {
		tok := &tokens[i]
		if tok.Type == lexer.TokenEOF || tok.Offset > offset {
			break
		}
		if offset < tok.Offset+utf8.RuneCountInString(tok.Text) {
			return tok.Type == lexer.TokenString
		}
	}
	return false
}

// roleAtPosition returns the identifier role at a 0-based line/character.
func roleAtPosition(tokens []lexer.Token, roles []parser.IdentifierRole, lineStartOffsets []int, line, character int) parser.IdentifierRole {
	if line < 0 || line >= len(lineStartOffsets) || character < 0 {
		return parser.RoleUnclassified
	}
	return roleAtOffset(tokens, roles, lineStartOffsets[line]+character)
}

// rolesReferToSameSymbol reports whether an occurrence playing role `other`
// can be the same symbol as the one under the cursor playing role `cursor`.
// Either role being unclassified means the tree makes no claim, so the
// word match stands.
func rolesReferToSameSymbol(cursor, other parser.IdentifierRole) bool {
	if cursor == parser.RoleUnclassified || other == parser.RoleUnclassified {
		return true
	}
	return symbolFamily(cursor) == symbolFamily(other)
}

// symbolFamily groups roles that can denote the same symbol. A variable
// and its declaration are one symbol; a procedure and its call sites are
// another; a member name belongs to whatever object supplies it, and a
// class name to the inventory — neither can be the file's variable or
// procedure of the same spelling.
func symbolFamily(role parser.IdentifierRole) int {
	switch role {
	case parser.RoleVariable, parser.RoleDeclaredName:
		return 1
	case parser.RoleCall, parser.RoleProcedureName:
		return 2
	case parser.RoleMember:
		return 3
	case parser.RoleInstantiation:
		return 4
	default:
		return 0
	}
}

// CursorMayNameProcedure reports whether the identifier at a 1-based
// line/column of text can name a procedure: a procedure header, a call
// callee, or a position the expression tree makes no claim about (a word
// inside a DoProc/ExecFunction string is unclassified). A member name, a
// variable, a declared name, or a class name never names the file's
// procedure of the same spelling (issue #255).
func CursorMayNameProcedure(text string, line, column int) bool {
	switch roleAtTextPosition(text, line-1, column-1) {
	case parser.RoleUnclassified, parser.RoleProcedureName, parser.RoleCall:
		return true
	default:
		return false
	}
}

// cursorOnMember reports whether the identifier at a 1-based line/column is
// the member name of a `:` access.
func cursorOnMember(text string, line, column int) bool {
	return roleAtTextPosition(text, line-1, column-1) == parser.RoleMember
}

// roleAtTextPosition classifies the identifier at a 0-based line/character
// of a source text.
func roleAtTextPosition(text string, line, character int) parser.IdentifierRole {
	tokens := lexer.NewLexer(text).Tokenize()
	lines := strings.Split(text, "\n")
	lineStartOffsets := make([]int, len(lines))
	runeOffset := 0
	for i, lineText := range lines {
		lineStartOffsets[i] = runeOffset
		runeOffset += utf8.RuneCountInString(lineText) + 1
	}
	return roleAtPosition(tokens, parser.IdentifierRoles(tokens), lineStartOffsets, line, character)
}
