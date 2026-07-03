// Cross-file resolution contract. Providers stay pure: they extract targets
// and render results, while the server implements WorkspaceResolver on top
// of its workspace index (providers must never import the server package).
// Normative behavior: catalog/features/cross_file_resolution.md.
package providers

import (
	"regexp"
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
	// ResolveDataSource resolves a RunDS target ("Cat.Name" or bare
	// "Name") to candidate data-source files — and only data-source
	// files (spec feature.cross_file_resolution/A15-A17).
	ResolveDataSource(target string) []ResolvedTarget
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

// runDSPattern matches RunDS calls to extract the data-source target,
// mirroring doProcPattern's shape (single capture group for the target).
var runDSPattern = regexp.MustCompile(`(?i)\bRunDS\s*\(\s*["']([^"']+)["']`)

// DataSourceTargetAt returns the RunDS target whose string literal contains
// the cursor position (1-based line/column), or nil. Same line-based
// limitations as DispatchTargetAt.
func DataSourceTargetAt(text string, line, column int) *DispatchTarget {
	lines := strings.Split(text, "\n")
	if line < 1 || line > len(lines) {
		return nil
	}
	lineText := lines[line-1]

	for _, match := range runDSPattern.FindAllStringSubmatchIndex(lineText, -1) {
		if len(match) < 4 {
			continue
		}
		nameStart := match[2] + 1 // 1-based
		nameEnd := match[3] + 1
		if column < nameStart || column > nameEnd {
			continue
		}
		raw := lineText[match[2]:match[3]]
		return &DispatchTarget{
			Raw:   raw,
			Parts: strings.Split(raw, "."),
			Range: Range{
				Start: Position{Line: line - 1, Character: match[2]},
				End:   Position{Line: line - 1, Character: match[3]},
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

		target, startCol, endCol := includePathAfter(tokens, i)
		if target == "" {
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

// includePathAfter joins the include path following the :INCLUDE keyword at
// token index i (same line only), returning the unquoted dotted target and
// its 0-based column span. Empty target means no path was found.
func includePathAfter(tokens []lexer.Token, i int) (string, int, int) {
	line := tokens[i].Line
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
	if startCol < 0 {
		return "", -1, -1
	}
	return target, startCol, endCol
}

// ExtractIncludeTargets returns every :INCLUDE target in the token stream,
// in order of appearance (unquoted, dotted form preserved). Used by the
// workspace index and the include-aware diagnostics closure
// (spec feature.cross_file_resolution/A18-A19).
func ExtractIncludeTargets(tokens []lexer.Token) []string {
	var targets []string
	for i, token := range tokens {
		if token.Type != lexer.TokenKeyword ||
			!strings.EqualFold(strings.TrimPrefix(token.Text, ":"), "INCLUDE") {
			continue
		}
		if target, _, _ := includePathAfter(tokens, i); target != "" {
			targets = append(targets, target)
		}
	}
	return targets
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

	if dst := DataSourceTargetAt(text, line, column); dst != nil {
		// RunDS targets resolve even as 1-part names — a data source is
		// always a separate file (feature.definition A13).
		if resolver == nil {
			return nil
		}
		return resolvedToLocations(resolver.ResolveDataSource(dst.Raw))
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
