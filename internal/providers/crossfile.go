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

// CallSiteKind classifies a whole-file call-site extraction.
type CallSiteKind int

const (
	// CallDispatch is a DoProc/ExecFunction first-argument string target.
	CallDispatch CallSiteKind = iota
	// CallDataSource is a RunDS first-argument string target.
	CallDataSource
	// CallInclude is an :INCLUDE path.
	CallInclude
)

// CallSite is one call site found by ExtractCallSites. Range covers the
// string CONTENT (or include path) — quotes excluded — 0-based.
type CallSite struct {
	Kind     CallSiteKind
	Raw      string
	IsDoProc bool // dispatch sites only: DoProc vs ExecFunction
	Range    Range
}

// ExtractCallSites walks the token stream and returns every dispatch
// (DoProc/ExecFunction), RunDS, and :INCLUDE call site in the file, in
// order of appearance (issue #125). The token walk is the forward form of
// isDispatchTargetMatch's walk-back: function identifier → optional
// whitespace/comments → '(' → optional whitespace/comments → quoted string.
// It handles multi-line calls the old line regex missed. Bracket strings,
// empty strings, and strings spanning lines are not legal dispatch syntax
// and are skipped; concatenated targets ("CAT." + sName) extract only when
// the first operand is itself a complete quoted target — the walk stops at
// the string token, so a leading partial never matches a real procedure.
func ExtractCallSites(tokens []lexer.Token) []CallSite {
	var sites []CallSite
	for i, token := range tokens {
		switch token.Type {
		case lexer.TokenKeyword:
			if !strings.EqualFold(strings.TrimPrefix(token.Text, ":"), "INCLUDE") {
				continue
			}
			if target, startCol, endCol := includePathAfter(tokens, i); target != "" {
				sites = append(sites, CallSite{
					Kind: CallInclude,
					Raw:  target,
					Range: Range{
						Start: Position{Line: token.Line - 1, Character: startCol},
						End:   Position{Line: token.Line - 1, Character: endCol},
					},
				})
			}
		case lexer.TokenIdentifier:
			name := strings.ToLower(token.Text)
			var kind CallSiteKind
			switch name {
			case "doproc", "execfunction":
				kind = CallDispatch
			case "runds":
				kind = CallDataSource
			default:
				continue
			}
			parenIdx := nextSignificantTokenIndex(tokens, i+1)
			if parenIdx < 0 || tokens[parenIdx].Type != lexer.TokenPunctuation || tokens[parenIdx].Text != "(" {
				continue
			}
			strIdx := nextSignificantTokenIndex(tokens, parenIdx+1)
			if strIdx < 0 || tokens[strIdx].Type != lexer.TokenString {
				continue
			}
			str := tokens[strIdx]
			if len(str.Text) < 3 || (str.Text[0] != '"' && str.Text[0] != '\'') ||
				str.Text[len(str.Text)-1] != str.Text[0] || strings.Contains(str.Text, "\n") {
				continue
			}
			content := str.Text[1 : len(str.Text)-1]
			sites = append(sites, CallSite{
				Kind:     kind,
				Raw:      content,
				IsDoProc: name == "doproc",
				Range: Range{
					Start: Position{Line: str.Line - 1, Character: str.Column},
					End:   Position{Line: str.Line - 1, Character: str.Column + len(content)},
				},
			})
		}
	}
	return sites
}

// dispatchTargetFromSite converts an extracted call site to the cursor-side
// DispatchTarget shape.
func dispatchTargetFromSite(site CallSite) *DispatchTarget {
	return &DispatchTarget{
		Raw:      site.Raw,
		Parts:    strings.Split(site.Raw, "."),
		IsDoProc: site.IsDoProc,
		Range:    site.Range,
	}
}

// callSiteAt returns the call site of the wanted kind whose target range
// contains the (1-based) cursor position, or nil.
func callSiteAt(tokens []lexer.Token, line, column int, kind CallSiteKind) *CallSite {
	for _, site := range ExtractCallSites(tokens) {
		if site.Kind != kind || site.Range.Start.Line != line-1 {
			continue
		}
		if column-1 >= site.Range.Start.Character && column-1 <= site.Range.End.Character {
			s := site
			return &s
		}
	}
	return nil
}

// DispatchTargetAt returns the dispatch target whose string literal contains
// the cursor position (1-based line/column), or nil. Token-walk based via
// ExtractCallSites (issue #125), so multi-line calls resolve; concatenated
// targets remain out of scope (see the catalog entry).
func DispatchTargetAt(tokens []lexer.Token, line, column int) *DispatchTarget {
	if site := callSiteAt(tokens, line, column, CallDispatch); site != nil {
		return dispatchTargetFromSite(*site)
	}
	return nil
}

// DataSourceTargetAt returns the RunDS target whose string literal contains
// the cursor position (1-based line/column), or nil.
func DataSourceTargetAt(tokens []lexer.Token, line, column int) *DispatchTarget {
	if site := callSiteAt(tokens, line, column, CallDataSource); site != nil {
		return dispatchTargetFromSite(*site)
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

	if dt := DispatchTargetAt(tokens, line, column); dt != nil {
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

	if dst := DataSourceTargetAt(tokens, line, column); dst != nil {
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

	// Member access on a shape-inferred UDObject receiver navigates to the
	// property's definition (feature.definition A14); a shaped receiver
	// whose shape lacks the member is null, never a fallback to an
	// unrelated same-named symbol (A15). Unshaped receivers fall through.
	if recv, member, ok := MemberAccessAt(tokens, line, column); ok {
		shapes := BuildUDObjectShapesWithProcedures(tokens, procedures)
		if shape, shaped := shapes[strings.ToLower(recv)]; shaped {
			if prop := FindShapeProperty(shape, member); prop != nil && prop.Line > 0 {
				return []Location{{
					URI: uri,
					Range: Range{
						Start: Position{Line: prop.Line - 1, Character: prop.Column - 1},
						End:   Position{Line: prop.Line - 1, Character: prop.Column - 1 + len(prop.Name)},
					},
				}}
			}
			return nil
		}
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
