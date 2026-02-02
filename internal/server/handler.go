package server

import (
	"strings"

	"starlims-lsp/internal/lexer"
	"starlims-lsp/internal/providers"

	"github.com/tliron/glsp"
	protocol "github.com/tliron/glsp/protocol_3_16"
)

// handleCompletion handles completion requests.
func (s *SSLServer) handleCompletion(context *glsp.Context, params *protocol.CompletionParams) (any, error) {
	uri := params.TextDocument.URI
	version := s.documentVersion[uri]

	cache := s.documents.ParseDocument(uri, version)

	// Check if we're inside a string or comment - if so, return no completions
	// LSP positions are 0-based, our functions expect 1-based
	line := int(params.Position.Line) + 1
	column := int(params.Position.Character) + 1
	if lexer.IsInsideStringOrComment(cache.Tokens, line, column) {
		return []protocol.CompletionItem{}, nil
	}

	// Get all completions
	completions := providers.GetAllCompletions(cache.Procedures, cache.Variables)
	snippets := providers.GetSnippetCompletions()

	items := make([]protocol.CompletionItem, 0, len(completions)+len(snippets))
	items = append(items, toProtocolCompletionItems(completions)...)
	items = append(items, toProtocolCompletionItems(snippets)...)

	return items, nil
}

// handleHover handles hover requests.
func (s *SSLServer) handleHover(context *glsp.Context, params *protocol.HoverParams) (*protocol.Hover, error) {
	uri := params.TextDocument.URI
	version := s.documentVersion[uri]

	content, ok := s.documents.GetDocument(uri)
	if !ok {
		return nil, nil
	}

	cache := s.documents.ParseDocument(uri, version)

	// LSP positions are 0-based, our functions expect 1-based
	line := int(params.Position.Line) + 1
	column := int(params.Position.Character) + 1

	// Check if we're inside a string or comment - if so, return no hover
	if lexer.IsInsideStringOrComment(cache.Tokens, line, column) {
		return nil, nil
	}

	hover := providers.GetHover(
		content,
		line,
		column,
		cache.Procedures,
		cache.Variables,
	)

	if hover == nil {
		return nil, nil
	}

	return &protocol.Hover{
		Contents: protocol.MarkupContent{
			Kind:  protocol.MarkupKindMarkdown,
			Value: hover.Contents,
		},
	}, nil
}

// handleDefinition handles go-to-definition requests.
func (s *SSLServer) handleDefinition(context *glsp.Context, params *protocol.DefinitionParams) (any, error) {
	uri := params.TextDocument.URI
	version := s.documentVersion[uri]

	content, ok := s.documents.GetDocument(uri)
	if !ok {
		return nil, nil
	}

	cache := s.documents.ParseDocument(uri, version)

	location := providers.FindDefinition(
		content,
		int(params.Position.Line)+1,
		int(params.Position.Character)+1,
		uri,
		cache.Procedures,
		cache.Variables,
	)

	if location == nil {
		return nil, nil
	}

	return toProtocolLocation(*location), nil
}

// handleReferences handles find references requests.
func (s *SSLServer) handleReferences(context *glsp.Context, params *protocol.ReferenceParams) ([]protocol.Location, error) {
	uri := params.TextDocument.URI

	content, ok := s.documents.GetDocument(uri)
	if !ok {
		return nil, nil
	}

	// Get cached procedures and variables for scope-aware search
	version := s.documentVersion[uri]
	cache := s.documents.ParseDocument(uri, version)

	locations := providers.FindReferencesWithScope(
		content,
		int(params.Position.Line)+1,
		int(params.Position.Character)+1,
		uri,
		params.Context.IncludeDeclaration,
		cache.Procedures,
		cache.Variables,
	)

	if locations == nil {
		return nil, nil
	}

	result := make([]protocol.Location, 0, len(locations))
	for _, loc := range locations {
		result = append(result, toProtocolLocation(loc))
	}

	return result, nil
}

// handleDocumentSymbol handles document symbol requests.
func (s *SSLServer) handleDocumentSymbol(context *glsp.Context, params *protocol.DocumentSymbolParams) (any, error) {
	uri := params.TextDocument.URI

	if _, ok := s.documents.GetDocument(uri); !ok {
		return nil, nil
	}

	version := s.documentVersion[uri]
	cache := s.documents.ParseDocument(uri, version)
	symbols := providers.GetDocumentSymbolsFromTokens(cache.Tokens, cache.AST)

	result := make([]protocol.DocumentSymbol, 0, len(symbols))
	for _, sym := range symbols {
		docSym := convertDocumentSymbol(sym)
		result = append(result, docSym)
	}

	return result, nil
}

func (s *SSLServer) handleWorkspaceSymbol(context *glsp.Context, params *protocol.WorkspaceSymbolParams) ([]protocol.SymbolInformation, error) {
	if params == nil {
		return nil, nil
	}

	query := params.Query
	results := make([]protocol.SymbolInformation, 0)

	for _, uri := range s.documents.AllDocuments() {
		version := s.documentVersion[uri]
		cache := s.documents.ParseDocument(uri, version)
		for _, proc := range cache.Procedures {
			if query != "" && !containsSubstring(proc.Name, query) {
				continue
			}
			rangeInfo := providers.Range{
				Start: providers.Position{Line: proc.StartLine - 1, Character: 0},
				End:   providers.Position{Line: proc.EndLine - 1, Character: 0},
			}
			results = append(results, protocol.SymbolInformation{
				Name:     proc.Name,
				Kind:     protocol.SymbolKindFunction,
				Location: protocol.Location{URI: uri, Range: toProtocolRange(rangeInfo)},
			})
		}
	}

	if len(results) == 0 {
		return nil, nil
	}

	return results, nil
}

func containsSubstring(value, query string) bool {
	if query == "" {
		return true
	}

	valueLower := strings.ToLower(value)
	queryLower := strings.ToLower(query)

	return strings.Contains(valueLower, queryLower)
}

func toProtocolCompletionItems(items []providers.CompletionItem) []protocol.CompletionItem {
	result := make([]protocol.CompletionItem, 0, len(items))
	for _, c := range items {
		item := protocol.CompletionItem{
			Label:  c.Label,
			Kind:   ptrTo(protocol.CompletionItemKind(c.Kind)),
			Detail: &c.Detail,
			Documentation: &protocol.MarkupContent{
				Kind:  protocol.MarkupKindMarkdown,
				Value: c.Documentation,
			},
			InsertText:       &c.InsertText,
			InsertTextFormat: ptrTo(protocol.InsertTextFormat(c.InsertTextFormat)),
		}
		result = append(result, item)
	}
	return result
}

func ensureRangeContainsSelection(fullRange providers.Range, selectionRange providers.Range) providers.Range {
	if isPositionBefore(selectionRange.Start, fullRange.Start) {
		fullRange.Start = selectionRange.Start
	}

	if isPositionAfter(selectionRange.End, fullRange.End) {
		fullRange.End = selectionRange.End
	}

	return fullRange
}

func isPositionBefore(left providers.Position, right providers.Position) bool {
	if left.Line != right.Line {
		return left.Line < right.Line
	}
	return left.Character < right.Character
}

func isPositionAfter(left providers.Position, right providers.Position) bool {
	if left.Line != right.Line {
		return left.Line > right.Line
	}
	return left.Character > right.Character
}

// convertDocumentSymbol converts our DocumentSymbol to protocol.DocumentSymbol.
func convertDocumentSymbol(sym providers.DocumentSymbol) protocol.DocumentSymbol {
	rangeToUse := ensureRangeContainsSelection(sym.Range, sym.SelectionRange)
	selectionRange := sym.SelectionRange

	docSym := protocol.DocumentSymbol{
		Name:           sym.Name,
		Kind:           protocol.SymbolKind(sym.Kind),
		Range:          toProtocolRange(rangeToUse),
		SelectionRange: toProtocolRange(selectionRange),
	}

	if sym.Detail != "" {
		docSym.Detail = &sym.Detail
	}

	if len(sym.Children) > 0 {
		children := make([]protocol.DocumentSymbol, 0, len(sym.Children))
		for _, child := range sym.Children {
			children = append(children, convertDocumentSymbol(child))
		}
		docSym.Children = children
	}

	return docSym
}

// handleFoldingRange handles folding range requests.
func (s *SSLServer) handleFoldingRange(context *glsp.Context, params *protocol.FoldingRangeParams) ([]protocol.FoldingRange, error) {
	uri := params.TextDocument.URI

	if _, ok := s.documents.GetDocument(uri); !ok {
		return nil, nil
	}

	version := s.documentVersion[uri]
	cache := s.documents.ParseDocument(uri, version)
	ranges := providers.GetFoldingRangesFromTokens(cache.Tokens, cache.AST)

	result := make([]protocol.FoldingRange, 0, len(ranges))
	for _, r := range ranges {
		fr := protocol.FoldingRange{
			StartLine: protocol.UInteger(r.StartLine),
			EndLine:   protocol.UInteger(r.EndLine),
		}

		switch r.Kind {
		case "comment":
			kind := string(protocol.FoldingRangeKindComment)
			fr.Kind = &kind
		case "region":
			kind := string(protocol.FoldingRangeKindRegion)
			fr.Kind = &kind
		}

		result = append(result, fr)
	}

	return result, nil
}

// handleSignatureHelp handles signature help requests.
func (s *SSLServer) handleSignatureHelp(context *glsp.Context, params *protocol.SignatureHelpParams) (*protocol.SignatureHelp, error) {
	uri := params.TextDocument.URI

	if _, ok := s.documents.GetDocument(uri); !ok {
		return nil, nil
	}

	version := s.documentVersion[uri]
	cache := s.documents.ParseDocument(uri, version)

	// Get signature help - LSP positions are 0-based, our functions expect 1-based
	help := providers.GetSignatureHelpWithProcedures(
		cache.Tokens,
		cache.Procedures,
		int(params.Position.Line)+1,
		int(params.Position.Character)+1,
	)

	if help == nil {
		return nil, nil
	}

	// Convert to protocol types
	signatures := make([]protocol.SignatureInformation, 0, len(help.Signatures))
	for _, sig := range help.Signatures {
		params := make([]protocol.ParameterInformation, 0, len(sig.Parameters))
		for _, param := range sig.Parameters {
			params = append(params, protocol.ParameterInformation{
				Label: param.Label,
				Documentation: &protocol.MarkupContent{
					Kind:  protocol.MarkupKindMarkdown,
					Value: param.Documentation,
				},
			})
		}

		signatures = append(signatures, protocol.SignatureInformation{
			Label: sig.Label,
			Documentation: &protocol.MarkupContent{
				Kind:  protocol.MarkupKindMarkdown,
				Value: sig.Documentation,
			},
			Parameters: params,
		})
	}

	return &protocol.SignatureHelp{
		Signatures:      signatures,
		ActiveSignature: ptrTo(protocol.UInteger(help.ActiveSignature)),
		ActiveParameter: ptrTo(protocol.UInteger(help.ActiveParameter)),
	}, nil
}

// handleFormatting handles document formatting requests.
func (s *SSLServer) handleFormatting(context *glsp.Context, params *protocol.DocumentFormattingParams) ([]protocol.TextEdit, error) {
	uri := params.TextDocument.URI

	content, ok := s.documents.GetDocument(uri)
	if !ok {
		return nil, nil
	}

	// Get formatting edits
	edits := providers.FormatDocument(content, s.settings.Formatting)

	// Convert to protocol text edits
	result := make([]protocol.TextEdit, 0, len(edits))
	for _, edit := range edits {
		result = append(result, toProtocolTextEdit(edit))
	}

	return result, nil
}

// handleRangeFormatting handles document range formatting requests.
func (s *SSLServer) handleRangeFormatting(context *glsp.Context, params *protocol.DocumentRangeFormattingParams) ([]protocol.TextEdit, error) {
	uri := params.TextDocument.URI

	content, ok := s.documents.GetDocument(uri)
	if !ok {
		return nil, nil
	}

	// Get formatting edits for the specified range
	edits := providers.FormatDocumentRange(
		content,
		int(params.Range.Start.Line),
		int(params.Range.Start.Character),
		int(params.Range.End.Line),
		int(params.Range.End.Character),
		s.settings.Formatting,
	)

	// Convert to protocol text edits
	result := make([]protocol.TextEdit, 0, len(edits))
	for _, edit := range edits {
		result = append(result, toProtocolTextEdit(edit))
	}

	return result, nil
}
