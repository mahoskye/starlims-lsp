package providers

import (
	"fmt"
	"regexp"
	"strings"

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
	variables := p.ExtractVariables(ast)
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
			SelectionRange: Range{
				Start: Position{Line: proc.StartLine - 1, Character: 0},
				End:   Position{Line: proc.StartLine - 1, Character: len(proc.Name) + 11},
			},
			Children: nil,
		}

		if len(proc.Parameters) > 0 {
			procSymbol.Detail = fmt.Sprintf("(%s)", strings.Join(proc.Parameters, ", "))
		}

		// Add parameters as children
		for _, param := range proc.Parameters {
			procSymbol.Children = append(procSymbol.Children, DocumentSymbol{
				Name: param,
				Kind: SymbolKindVariable,
				Range: Range{
					Start: Position{Line: proc.StartLine - 1, Character: 0},
					End:   Position{Line: proc.StartLine - 1, Character: len(param)},
				},
				SelectionRange: Range{
					Start: Position{Line: proc.StartLine - 1, Character: 0},
					End:   Position{Line: proc.StartLine - 1, Character: len(param)},
				},
				Detail: "parameter",
			})
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

	// Build region symbols with procedures as children
	regionSymbols := make([]DocumentSymbol, 0, len(regions))
	usedProcs := make(map[int]bool) // Track which procedures are nested in regions

	for _, region := range regions {
		regionSymbol := DocumentSymbol{
			Name: region.Name,
			Kind: SymbolKindNamespace,
			Range: Range{
				Start: Position{Line: region.StartLine - 1, Character: 0},
				End:   Position{Line: region.EndLine - 1, Character: 0},
			},
			SelectionRange: Range{
				Start: Position{Line: region.StartLine - 1, Character: 0},
				End:   Position{Line: region.StartLine - 1, Character: len(region.Name)},
			},
			Detail:   "region",
			Children: nil,
		}

		// Find procedures that fall within this region
		for i, procSym := range procSymbols {
			procStartLine := procSym.Range.Start.Line + 1 // Convert back to 1-based
			procEndLine := procSym.Range.End.Line + 1

			// Check if procedure is fully contained within region
			if procStartLine >= region.StartLine && procEndLine <= region.EndLine {
				regionSymbol.Children = append(regionSymbol.Children, procSym)
				usedProcs[i] = true
			}
		}

		// Find public variables that fall within this region
		for _, varSym := range varSymbols {
			varLine := varSym.Range.Start.Line + 1 // Convert back to 1-based
			if varLine >= region.StartLine && varLine <= region.EndLine {
				regionSymbol.Children = append(regionSymbol.Children, varSym)
			}
		}

		regionSymbols = append(regionSymbols, regionSymbol)
	}

	// Build final result
	var symbols []DocumentSymbol

	// Add procedures that are not inside any region
	for i, procSym := range procSymbols {
		if !usedProcs[i] {
			symbols = append(symbols, procSym)
		}
	}

	// Add public variables that are not inside any region
	for _, varSym := range varSymbols {
		varLine := varSym.Range.Start.Line + 1
		insideRegion := false
		for _, region := range regions {
			if varLine >= region.StartLine && varLine <= region.EndLine {
				insideRegion = true
				break
			}
		}
		if !insideRegion {
			symbols = append(symbols, varSym)
		}
	}

	// Add regions (which now contain their children)
	symbols = append(symbols, regionSymbols...)

	return symbols
}

// extractRegions extracts region markers from tokens.
func extractRegions(tokens []lexer.Token) []RegionInfo {
	var regions []RegionInfo

	type stackItem struct {
		name      string
		startLine int
	}
	var regionStack []stackItem

	// Match region comments, handling optional trailing semicolon and whitespace
	// SSL comments are: /* comment text ;
	regionStartPattern := regexp.MustCompile(`(?i)^/\*\s*region\s*(.*?)(?:\s*;?\s*)?$`)
	regionEndPattern := regexp.MustCompile(`(?i)^/\*\s*endregion`)

	for _, token := range tokens {
		if token.Type == lexer.TokenComment {
			// Normalize the text by trimming trailing semicolon and whitespace
			text := strings.TrimSuffix(token.Text, ";")
			text = strings.TrimSpace(text)

			if matches := regionStartPattern.FindStringSubmatch(text); matches != nil {
				name := strings.TrimSpace(matches[1])
				// Also strip any trailing semicolon from the name itself
				name = strings.TrimSuffix(name, ";")
				name = strings.TrimSpace(name)
				if name == "" {
					name = "Region"
				}
				regionStack = append(regionStack, stackItem{name: name, startLine: token.Line})
				continue
			}

			if regionEndPattern.MatchString(text) {
				if len(regionStack) > 0 {
					start := regionStack[len(regionStack)-1]
					regionStack = regionStack[:len(regionStack)-1]
					regions = append(regions, RegionInfo{
						Name:      start.name,
						StartLine: start.startLine,
						EndLine:   token.Line,
					})
				}
			}
		}
	}

	return regions
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

	// Get procedure ranges
	procedures := p.ExtractProcedures(ast)
	for _, proc := range procedures {
		ranges = append(ranges, FoldingRange{
			StartLine: proc.StartLine - 1,
			EndLine:   proc.EndLine - 1,
			Kind:      "region",
		})
	}

	// Get region ranges
	regions := extractRegions(tokens)
	for _, region := range regions {
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
