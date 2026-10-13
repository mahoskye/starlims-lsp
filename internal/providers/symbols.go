package providers

import (
	"fmt"
	"regexp"
	"sort"
	"strings"
	"unicode/utf8"

	"starlims-lsp/internal/lexer"
	"starlims-lsp/internal/parser"
)

// SymbolKind represents the kind of a symbol.
type SymbolKind int

const (
	SymbolKindFile          SymbolKind = 1
	SymbolKindModule        SymbolKind = 2
	SymbolKindNamespace     SymbolKind = 3
	SymbolKindPackage       SymbolKind = 4
	SymbolKindClass         SymbolKind = 5
	SymbolKindMethod        SymbolKind = 6
	SymbolKindProperty      SymbolKind = 7
	SymbolKindField         SymbolKind = 8
	SymbolKindConstructor   SymbolKind = 9
	SymbolKindEnum          SymbolKind = 10
	SymbolKindInterface     SymbolKind = 11
	SymbolKindFunction      SymbolKind = 12
	SymbolKindVariable      SymbolKind = 13
	SymbolKindConstant      SymbolKind = 14
	SymbolKindString        SymbolKind = 15
	SymbolKindNumber        SymbolKind = 16
	SymbolKindBoolean       SymbolKind = 17
	SymbolKindArray         SymbolKind = 18
	SymbolKindObject        SymbolKind = 19
	SymbolKindKey           SymbolKind = 20
	SymbolKindNull          SymbolKind = 21
	SymbolKindEnumMember    SymbolKind = 22
	SymbolKindStruct        SymbolKind = 23
	SymbolKindEvent         SymbolKind = 24
	SymbolKindOperator      SymbolKind = 25
	SymbolKindTypeParameter SymbolKind = 26
)

// DocumentSymbol represents a symbol in a document.
type DocumentSymbol struct {
	Name           string
	Detail         string
	Kind           SymbolKind
	Range          Range
	SelectionRange Range
	Children       []DocumentSymbol
}

// FoldingRange represents a folding range in a document.
type FoldingRange struct {
	StartLine int
	EndLine   int
	Kind      string // "comment", "imports", "region"
}

// RegionInfo contains information about a region.
type RegionInfo struct {
	Name      string
	StartLine int
	EndLine   int
	// NameColumn and NameLength locate the region's name on its opening
	// marker line (0-based character, rune length) — the `region` keyword
	// itself for an unnamed region.
	NameColumn int
	NameLength int
}

type regionEvent struct {
	Line       int
	Name       string
	IsStart    bool
	NameColumn int
	NameLength int
}

// GetDocumentSymbols returns all document symbols.
func GetDocumentSymbols(text string) []DocumentSymbol {
	lex := lexer.NewLexer(text)
	tokens := lex.Tokenize()
	p := parser.NewParser(tokens)
	ast := p.Parse()
	return buildDocumentSymbols(tokens, ast, p)
}

// GetDocumentSymbolsFromTokens returns document symbols from cached tokens/AST.
func GetDocumentSymbolsFromTokens(tokens []lexer.Token, ast *parser.Node) []DocumentSymbol {
	if len(tokens) == 0 {
		return nil
	}

	p := parser.NewParser(tokens)
	if ast == nil {
		ast = p.Parse()
	}

	return buildDocumentSymbols(tokens, ast, p)
}

func buildDocumentSymbols(tokens []lexer.Token, ast *parser.Node, p *parser.Parser) []DocumentSymbol {
	// Extract all elements first
	procedures := p.ExtractProcedures(ast)
	variables := p.ExtractVariables()
	regions := extractRegions(tokens)

	// Build procedure symbols
	procSymbols := make([]DocumentSymbol, 0, len(procedures))
	for _, proc := range procedures {
		procSymbol := DocumentSymbol{
			Name: proc.Name,
			Kind: SymbolKindFunction,
			Range: Range{
				Start: Position{Line: proc.StartLine - 1, Character: 0},
				End:   Position{Line: proc.EndLine - 1, Character: 0},
			},
			SelectionRange: procedureNameSelectionRange(tokens, proc),
			Children:       nil,
		}

		// Parameter children come from statement-based declaration
		// resolution, so a bare `:PARAMETERS` with its names on the
		// following lines still yields them, each ranged over its own name
		// (issue #255). The procedure's extracted parameter list is the
		// fallback when no declaration info is available.
		var paramNames []string
		for _, v := range variables {
			if v.Scope != parser.ScopeParameter || v.Line < proc.StartLine || v.Line > proc.EndLine {
				continue
			}
			paramNames = append(paramNames, v.Name)
			nameRange := Range{
				Start: Position{Line: v.Line - 1, Character: v.Column - 1},
				End:   Position{Line: v.Line - 1, Character: v.Column - 1 + len(v.Name)},
			}
			procSymbol.Children = append(procSymbol.Children, DocumentSymbol{
				Name:           v.Name,
				Kind:           SymbolKindVariable,
				Range:          nameRange,
				SelectionRange: nameRange,
				Detail:         "parameter",
			})
		}
		if len(paramNames) == 0 {
			paramNames = proc.Parameters
			for _, param := range proc.Parameters {
				approx := Range{
					Start: Position{Line: proc.StartLine - 1, Character: 0},
					End:   Position{Line: proc.StartLine - 1, Character: len(param)},
				}
				procSymbol.Children = append(procSymbol.Children, DocumentSymbol{
					Name:           param,
					Kind:           SymbolKindVariable,
					Range:          approx,
					SelectionRange: approx,
					Detail:         "parameter",
				})
			}
		}
		if len(paramNames) > 0 {
			procSymbol.Detail = fmt.Sprintf("(%s)", strings.Join(paramNames, ", "))
		}

		procSymbols = append(procSymbols, procSymbol)
	}

	// Build public variable symbols
	varSymbols := make([]DocumentSymbol, 0)
	for _, v := range variables {
		if v.Scope == parser.ScopePublic {
			varSymbols = append(varSymbols, DocumentSymbol{
				Name: v.Name,
				Kind: SymbolKindVariable,
				Range: Range{
					Start: Position{Line: v.Line - 1, Character: v.Column - 1},
					End:   Position{Line: v.Line - 1, Character: v.Column - 1 + len(v.Name)},
				},
				SelectionRange: Range{
					Start: Position{Line: v.Line - 1, Character: v.Column - 1},
					End:   Position{Line: v.Line - 1, Character: v.Column - 1 + len(v.Name)},
				},
				Detail: "public variable",
			})
		}
	}

	// Regions nest: each region's parent is the innermost other region that
	// contains it, and each procedure or public belongs to the innermost
	// region containing it — so a symbol appears exactly once (issue #255).
	sort.SliceStable(regions, func(a, b int) bool {
		if regions[a].StartLine != regions[b].StartLine {
			return regions[a].StartLine < regions[b].StartLine
		}
		return regions[a].EndLine > regions[b].EndLine
	})
	// With regions sorted outer-before-inner, the innermost container of a
	// span is the LAST region in order that contains it; a region's parent
	// can only come before it.
	innermost := func(startLine, endLine, limit int) int {
		best := -1
		for r := 0; r < limit; r++ {
			if regions[r].StartLine <= startLine && endLine <= regions[r].EndLine {
				best = r
			}
		}
		return best
	}

	regionParent := make([]int, len(regions))
	for r := range regions {
		regionParent[r] = innermost(regions[r].StartLine, regions[r].EndLine, r)
	}
	procRegion := make([]int, len(procSymbols))
	for i, procSym := range procSymbols {
		procRegion[i] = innermost(procSym.Range.Start.Line+1, procSym.Range.End.Line+1, len(regions))
	}
	varRegion := make([]int, len(varSymbols))
	for i, varSym := range varSymbols {
		line := varSym.Range.Start.Line + 1
		varRegion[i] = innermost(line, line, len(regions))
	}

	var buildRegion func(r int) DocumentSymbol
	buildRegion = func(r int) DocumentSymbol {
		region := regions[r]
		sym := DocumentSymbol{
			Name: region.Name,
			Kind: SymbolKindNamespace,
			Range: Range{
				Start: Position{Line: region.StartLine - 1, Character: 0},
				End:   Position{Line: region.EndLine - 1, Character: 0},
			},
			SelectionRange: Range{
				Start: Position{Line: region.StartLine - 1, Character: region.NameColumn},
				End:   Position{Line: region.StartLine - 1, Character: region.NameColumn + region.NameLength},
			},
			Detail: "region",
		}
		for i, procSym := range procSymbols {
			if procRegion[i] == r {
				sym.Children = append(sym.Children, procSym)
			}
		}
		for i, varSym := range varSymbols {
			if varRegion[i] == r {
				sym.Children = append(sym.Children, varSym)
			}
		}
		for child := range regions {
			if regionParent[child] == r {
				sym.Children = append(sym.Children, buildRegion(child))
			}
		}
		return sym
	}

	// Build final result: ungrouped procedures, ungrouped publics, then the
	// top-level region containers (which now hold their children).
	var symbols []DocumentSymbol
	for i, procSym := range procSymbols {
		if procRegion[i] < 0 {
			symbols = append(symbols, procSym)
		}
	}
	for i, varSym := range varSymbols {
		if varRegion[i] < 0 {
			symbols = append(symbols, varSym)
		}
	}
	for r := range regions {
		if regionParent[r] < 0 {
			symbols = append(symbols, buildRegion(r))
		}
	}

	return symbols
}

// procedureNameSelectionRange returns the range covering exactly the
// procedure's name identifier on its declaration line — not the :PROCEDURE
// keyword, whitespace, or the trailing semicolon (spec
// feature.document_symbols/A4). Falls back to a start-of-line range when the
// name token cannot be located.
func procedureNameSelectionRange(tokens []lexer.Token, proc parser.ProcedureInfo) Range {
	for i, token := range tokens {
		if token.Line != proc.StartLine || token.Type != lexer.TokenKeyword ||
			!strings.EqualFold(strings.TrimPrefix(token.Text, ":"), "PROCEDURE") {
			continue
		}
		for j := i + 1; j < len(tokens); j++ {
			next := tokens[j]
			if next.Type == lexer.TokenWhitespace || next.Type == lexer.TokenComment {
				continue
			}
			if next.Type == lexer.TokenIdentifier && strings.EqualFold(next.Text, proc.Name) {
				return Range{
					Start: Position{Line: next.Line - 1, Character: next.Column - 1},
					End:   Position{Line: next.Line - 1, Character: next.Column - 1 + len(next.Text)},
				}
			}
			break
		}
	}
	return Range{
		Start: Position{Line: proc.StartLine - 1, Character: 0},
		End:   Position{Line: proc.StartLine - 1, Character: len(proc.Name)},
	}
}

// extractRegions extracts region markers from tokens.
func extractRegions(tokens []lexer.Token) []RegionInfo {
	var events []regionEvent
	lastLine := 0

	// Match region comments, handling optional trailing semicolon and whitespace.
	// SSL comments are: /* comment text ;
	regionStartPattern := regexp.MustCompile(`(?i)^/\*\s*region\b\s*(.*?)(?:\s*;?\s*)?$`)
	regionEndPattern := regexp.MustCompile(`(?i)^/\*\s*endregion\b`)

	for _, token := range tokens {
		if token.Type != lexer.TokenEOF && token.Line > lastLine {
			lastLine = token.Line
		}

		if token.Type == lexer.TokenComment {
			text := strings.TrimSuffix(token.Text, ";")
			text = strings.TrimSpace(text)

			if matches := regionStartPattern.FindStringSubmatch(text); matches != nil {
				name := strings.TrimSpace(matches[1])
				name = strings.TrimSuffix(name, ";")
				name = strings.TrimSpace(name)
				// Locate the name (or, unnamed, the `region` keyword) within
				// the marker for an exact selection range.
				located := name
				if name == "" {
					name = "Region"
					located = ""
				}
				col, length := regionNameSpan(token, located)
				events = append(events, regionEvent{Line: token.Line, Name: name, IsStart: true, NameColumn: col, NameLength: length})
				continue
			}

			if regionEndPattern.MatchString(text) {
				events = append(events, regionEvent{Line: token.Line, IsStart: false})
			}
		}
	}

	sort.SliceStable(events, func(i, j int) bool {
		if events[i].Line == events[j].Line {
			return events[i].IsStart && !events[j].IsStart
		}
		return events[i].Line < events[j].Line
	})

	var regions []RegionInfo
	type stackItem struct {
		name      string
		startLine int
		nameCol   int
		nameLen   int
	}
	var regionStack []stackItem

	for _, event := range events {
		if event.IsStart {
			regionStack = append(regionStack, stackItem{name: event.Name, startLine: event.Line, nameCol: event.NameColumn, nameLen: event.NameLength})
			continue
		}
		if len(regionStack) == 0 {
			continue
		}

		// The canonical closer is a bare '/* endregion;' — it takes no name
		// and closes the innermost open region (LIFO). Any trailing text
		// before the ';' is prose and ignored for pairing. An endregion
		// with no open region closes nothing (diag.region_end_mismatch).
		start := regionStack[len(regionStack)-1]
		regionStack = regionStack[:len(regionStack)-1]
		regions = append(regions, RegionInfo{
			Name:       start.name,
			StartLine:  start.startLine,
			EndLine:    event.Line,
			NameColumn: start.nameCol,
			NameLength: start.nameLen,
		})
	}

	for _, region := range regionStack {
		regions = append(regions, RegionInfo{
			Name:       region.name,
			StartLine:  region.startLine,
			EndLine:    lastLine,
			NameColumn: region.nameCol,
			NameLength: region.nameLen,
		})
	}

	return regions
}

// regionNameSpan returns the 0-based character and rune length of name
// within a region-marker comment token; an empty name locates the `region`
// keyword instead. Falls back to the token start when not found.
func regionNameSpan(token lexer.Token, name string) (int, int) {
	lower := strings.ToLower(token.Text)
	kw := strings.Index(lower, "region")
	if kw < 0 {
		return token.Column - 1, 0
	}
	if name == "" {
		return token.Column - 1 + utf8.RuneCountInString(token.Text[:kw]), len("region")
	}
	after := kw + len("region")
	idx := strings.Index(token.Text[after:], name)
	if idx < 0 {
		return token.Column - 1 + utf8.RuneCountInString(token.Text[:kw]), len("region")
	}
	start := after + idx
	return token.Column - 1 + utf8.RuneCountInString(token.Text[:start]), utf8.RuneCountInString(name)
}

// GetFoldingRanges returns folding ranges for the document.
func GetFoldingRanges(text string) []FoldingRange {
	lex := lexer.NewLexer(text)
	tokens := lex.Tokenize()
	p := parser.NewParser(tokens)
	ast := p.Parse()
	return buildFoldingRanges(tokens, ast, p)
}

// GetFoldingRangesFromTokens returns folding ranges from cached tokens/AST.
func GetFoldingRangesFromTokens(tokens []lexer.Token, ast *parser.Node) []FoldingRange {
	if len(tokens) == 0 {
		return nil
	}

	p := parser.NewParser(tokens)
	if ast == nil {
		ast = p.Parse()
	}

	return buildFoldingRanges(tokens, ast, p)
}

func buildFoldingRanges(tokens []lexer.Token, ast *parser.Node, p *parser.Parser) []FoldingRange {
	var ranges []FoldingRange

	// Get procedure ranges (single-line procedures produce no range)
	procedures := p.ExtractProcedures(ast)
	for _, proc := range procedures {
		if proc.StartLine == proc.EndLine {
			continue
		}
		ranges = append(ranges, FoldingRange{
			StartLine: proc.StartLine - 1,
			EndLine:   proc.EndLine - 1,
			Kind:      "region",
		})
	}

	// Get region ranges (single-line region pairs produce no range)
	regions := extractRegions(tokens)
	for _, region := range regions {
		if region.StartLine == region.EndLine {
			continue
		}
		ranges = append(ranges, FoldingRange{
			StartLine: region.StartLine - 1,
			EndLine:   region.EndLine - 1,
			Kind:      "region",
		})
	}

	// Get control flow block ranges (IF, WHILE, FOR, BEGINCASE, TRY)
	controlFlowBlocks := parser.ExtractControlFlowBlocks(tokens)
	for _, block := range controlFlowBlocks {
		// Only include multi-line blocks (startLine != endLine)
		if block.StartLine != block.EndLine {
			ranges = append(ranges, FoldingRange{
				StartLine: block.StartLine - 1,
				EndLine:   block.EndLine - 1,
				Kind:      "region",
			})
		}
	}

	// Get comment block ranges
	commentRanges := findCommentBlocks(tokens)
	for _, cr := range commentRanges {
		ranges = append(ranges, FoldingRange{
			StartLine: cr.StartLine - 1,
			EndLine:   cr.EndLine - 1,
			Kind:      "comment",
		})
	}

	return ranges
}

// CommentBlockRange represents a comment block range.
type CommentBlockRange struct {
	StartLine int
	EndLine   int
}

// findCommentBlocks finds multi-line comment blocks.
func findCommentBlocks(tokens []lexer.Token) []CommentBlockRange {
	var blocks []CommentBlockRange

	for _, token := range tokens {
		if token.Type == lexer.TokenComment {
			lines := strings.Split(token.Text, "\n")
			if len(lines) > 1 {
				blocks = append(blocks, CommentBlockRange{
					StartLine: token.Line,
					EndLine:   token.Line + len(lines) - 1,
				})
			}
		}
	}

	return blocks
}
