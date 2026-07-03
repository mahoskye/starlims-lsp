// Cross-file resolution contract. Providers stay pure: they extract targets
// and render results, while the server implements WorkspaceResolver on top
// of its workspace index (providers must never import the server package).
// Normative behavior: catalog/features/cross_file_resolution.md.
package providers

import (
	"strings"

	"starlims-lsp/internal/lexer"
	"starlims-lsp/internal/parser"
)

// ResolvedTargetKind classifies what a cross-file resolution landed on.
type ResolvedTargetKind int

const (
	// ResolvedScriptEntry is a script's entry point — the target of a
	// 2-part ExecFunction("Category.Script") call or an :INCLUDE.
	ResolvedScriptEntry ResolvedTargetKind = iota
	// ResolvedProcedure is a named procedure inside a target script.
	ResolvedProcedure
)

// ResolvedTarget is one candidate location for a cross-file target.
type ResolvedTarget struct {
	URI  string
	Line int // 0-based target line
	Kind ResolvedTargetKind
}

// WorkspaceResolver resolves dispatch and include targets across the
// workspace. Implementations must be nil-input-safe and return candidates
// in normative order (anchored-layout matches first, then
// path-lexicographic), capped per the catalog entry.
type WorkspaceResolver interface {
	// ResolveDispatch resolves a dotted DoProc/ExecFunction target string
	// ("Cat.Script", "Cat.Script.Proc", "Script.Proc", ...). Bare 1-part
	// targets are the caller's same-file concern and return nil here.
	ResolveDispatch(target string) []ResolvedTarget
	// ResolveInclude resolves an :INCLUDE target ("Name" or "Cat.Script",
	// already unquoted) to candidate files.
	ResolveInclude(target string) []ResolvedTarget
}

// DispatchTarget is a DoProc/ExecFunction string target under the cursor.
type DispatchTarget struct {
	Raw      string
	Parts    []string
	IsDoProc bool
	Range    Range
}

// DispatchTargetAt returns the dispatch target whose string literal contains
// the cursor position (1-based line/column), or nil. Reuses the same
// line-based pattern as same-file DoProc navigation; multi-line and
// concatenated targets are out of scope (see the catalog entry).
func DispatchTargetAt(text string, line, column int) *DispatchTarget {
	lines := strings.Split(text, "\n")
	if line < 1 || line > len(lines) {
		return nil
	}
	lineText := lines[line-1]

	for _, match := range doProcPattern.FindAllStringSubmatchIndex(lineText, -1) {
		if len(match) < 6 {
			continue
		}
		nameStart := match[4] + 1 // 1-based
		nameEnd := match[5] + 1
		if column < nameStart || column > nameEnd {
			continue
		}
		raw := lineText[match[4]:match[5]]
		fn := strings.ToLower(lineText[match[2]:match[3]])
		return &DispatchTarget{
			Raw:      raw,
			Parts:    strings.Split(raw, "."),
			IsDoProc: fn == "doproc",
			Range: Range{
				Start: Position{Line: line - 1, Character: match[4]},
				End:   Position{Line: line - 1, Character: match[5]},
			},
		}
	}
	return nil
}

// IncludeTarget is an :INCLUDE path under the cursor.
type IncludeTarget struct {
	Raw   string // joined dotted name, quotes stripped
	Range Range
}

// IncludeTargetAt returns the :INCLUDE target when the cursor sits on the
// include statement (keyword or path), or nil. Include paths lex as
// identifier and unknown-token chunks (issue #56: pieces like ".B." or a
// glued ".D;"), or as a quoted string; the pieces are joined and the
// trailing ';' stripped.
func IncludeTargetAt(tokens []lexer.Token, line, column int) *IncludeTarget {
	for i, token := range tokens {
		if token.Type != lexer.TokenKeyword ||
			!strings.EqualFold(strings.TrimPrefix(token.Text, ":"), "INCLUDE") {
			continue
		}
		if token.Line != line {
			continue
		}

		var raw strings.Builder
		startCol, endCol := -1, -1
		done := false
		for j := i + 1; j < len(tokens) && !done; j++ {
			t := tokens[j]
			if t.Type == lexer.TokenWhitespace {
				if raw.Len() > 0 {
					break // whitespace after the path ends it
				}
				continue
			}
			if t.Line != line {
				break
			}
			switch t.Type {
			case lexer.TokenPunctuation:
				if t.Text == ";" {
					done = true
					continue
				}
				if t.Text != "." {
					done = true
					continue
				}
				raw.WriteString(t.Text)
			case lexer.TokenIdentifier, lexer.TokenUnknown, lexer.TokenNumber:
				raw.WriteString(t.Text)
			case lexer.TokenString:
				raw.WriteString(strings.Trim(t.Text, `"'`))
			default:
				done = true
				continue
			}
			if startCol < 0 {
				startCol = t.Column - 1
			}
			endCol = t.Column - 1 + len(t.Text)
		}

		target := strings.TrimSuffix(strings.TrimSpace(raw.String()), ";")
		if target == "" || startCol < 0 {
			return nil
		}

		// Cursor must sit on the include statement: keyword through path.
		kwStart := token.Column - 1
		if column-1 < kwStart || column-1 > endCol {
			return nil
		}

		return &IncludeTarget{
			Raw: target,
			Range: Range{
				Start: Position{Line: line - 1, Character: startCol},
				End:   Position{Line: line - 1, Character: endCol},
			},
		}
	}
	return nil
}

// FindDefinitionCrossFile is the definition entry point with cross-file
// resolution (catalog: feature.definition A8-A12). Dotted dispatch targets
// and :INCLUDE paths resolve through the workspace resolver; 1-part
// dispatch targets keep same-file semantics; everything else falls through
// to the word-based FindDefinition. A nil resolver disables the
// cross-file paths.
func FindDefinitionCrossFile(text string, tokens []lexer.Token, line, column int, uri string,
	procedures []parser.ProcedureInfo, variables []parser.VariableInfo,
	resolver WorkspaceResolver) []Location {

	if dt := DispatchTargetAt(text, line, column); dt != nil {
		if len(dt.Parts) == 1 {
			// Same-script semantics, unchanged (feature.definition A6/A7).
			if loc := findDoProcDefinition(text, line, column, uri, procedures); loc != nil {
				return []Location{*loc}
			}
			return nil
		}
		if resolver == nil {
			return nil
		}
		return resolvedToLocations(resolver.ResolveDispatch(dt.Raw))
	}

	if it := IncludeTargetAt(tokens, line, column); it != nil {
		if resolver == nil {
			return nil
		}
		return resolvedToLocations(resolver.ResolveInclude(it.Raw))
	}

	if loc := FindDefinition(text, line, column, uri, procedures, variables); loc != nil {
		return []Location{*loc}
	}
	return nil
}

func resolvedToLocations(targets []ResolvedTarget) []Location {
	if len(targets) == 0 {
		return nil
	}
	locations := make([]Location, 0, len(targets))
	for _, t := range targets {
		locations = append(locations, Location{
			URI: t.URI,
			Range: Range{
				Start: Position{Line: t.Line, Character: 0},
				End:   Position{Line: t.Line, Character: 0},
			},
		})
	}
	return locations
}
