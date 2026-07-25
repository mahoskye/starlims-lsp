package server

import (
	"strings"

	"starlims-lsp/internal/constants"
	"starlims-lsp/internal/lexer"
	"starlims-lsp/internal/parser"
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
	// (with one exception below).
	// LSP positions are 0-based, our functions expect 1-based
	line := int(params.Position.Line) + 1
	column := int(params.Position.Character) + 1
	if lexer.IsInsideStringOrComment(cache.Tokens, line, column) {
		// Exception: inside a DoProc("…") / ExecFunction("…") string argument,
		// offer dispatch-target completions (vs-code-ssl-formatter#74; the
		// segment-aware cross-file levels are feature.completion A7-A10):
		// level 0 = same-file procedures + categories, "Cat." = its
		// scripts, "Cat.Script."/"Script." = that script's procedures.
		if isDoProcStringContext(cache.Tokens, line, column) {
			prefix := dispatchStringPrefix(cache.Tokens, line, column)
			ctx := (liveResolver{s}).dispatchCompletionContext(prefix, cache.Procedures)
			return toProtocolCompletionItems(providers.GetDispatchTargetSegmentCompletions(ctx)), nil
		}
		return []protocol.CompletionItem{}, nil
	}

	// Computed ahead of the context-aware shortcuts: the endpoint gate
	// controls the ambient Request:/Response: member surface (issue #123).
	endpointFile := false
	if content, ok := s.documents.GetDocument(uri); ok {
		endpointFile = isEndpointFile(uri, content, s.settings.EndpointPatterns)
	}

	// Context-aware shortcuts: if the cursor is right after `<ClassName>{`
	// or after a member-access `:`, return the focused completion list
	// instead of the full inventory. This produces better suggestions in
	// editors that display unfiltered LSP results.
	if focused := s.contextAwareCompletions(cache, line, column, endpointFile); focused != nil {
		return toProtocolCompletionItems(focused), nil
	}

	// Auto-triggered ':' with no context-aware match: show only keyword
	// completions (':' is the SSL keyword prefix). The full inventory is
	// reserved for explicit Ctrl+Space invocation — auto-popping it on
	// every member-access ':' produced a noisy popup that fought typing
	// flow. See issue #8.
	if params.Context != nil && params.Context.TriggerKind == protocol.CompletionTriggerKindTriggerCharacter {
		// Issues #11 / #12: only surface keywords when ':' begins a new
		// token (preceded by whitespace or SOL), and replace the typed ':'
		// via a TextEdit so selecting a keyword yields ':IF', not '::IF'.
		content, ok := s.documents.GetDocument(uri)
		if !ok {
			return []protocol.CompletionItem{}, nil
		}
		if !colonStartsToken(content, params.Position) {
			return []protocol.CompletionItem{}, nil
		}
		return keywordCompletionsForColonTrigger(params.Position), nil
	}

	classMethodContext := isClassMethodContext(cache.Tokens, cache.Procedures, line)
	dsFile := isDataSourceURI(uri)
	completions := providers.GetAllCompletions(cache.Procedures, cache.Variables, classMethodContext, dsFile, endpointFile)
	snippets := providers.GetSnippetCompletions(dsFile)

	items := make([]protocol.CompletionItem, 0, len(completions)+len(snippets))
	items = append(items, toProtocolCompletionItems(completions)...)
	items = append(items, toProtocolCompletionItems(snippets)...)

	return items, nil
}

// contextAwareCompletions returns a focused completion list when the cursor
// is in a context that maps cleanly to one of the new providers helpers:
//
//   - `<BuiltInClass>{`           — constructor signatures for that class
//   - `Me:` / `Base:` (in a class) — method/field suggestions for the
//     enclosing class declaration
//   - `<BuiltInClass>:`           — methods/properties of that class
//   - `Request:` / `Response:`    — the ambient's members (endpoint files)
//   - `<typedVar>:`               — members of the class or returns object
//     a producer chain assigned to the variable (issue #123)
//
// Returns nil when no context applies; the caller falls back to the full
// completion list.
func (s *SSLServer) contextAwareCompletions(cache *DocumentCache, line, column int, endpointFile bool) []providers.CompletionItem {
	// Find the most recently emitted token that ends at or before the
	// cursor. We then peek at the token immediately before it for context.
	idx := indexOfTokenBefore(cache.Tokens, line, column)
	if idx < 0 {
		return nil
	}

	prev := cache.Tokens[idx]

	switch prev.Text {
	case "{":
		// Constructor context: previous non-trivial token must be a built-in class name.
		if prior := indexOfPriorSignificantToken(cache.Tokens, idx); prior >= 0 {
			tok := cache.Tokens[prior]
			if tok.Type == lexer.TokenIdentifier && constants.IsSSLClass(tok.Text) {
				return providers.GetClassConstructorCompletions(tok.Text)
			}
		}
	case ":":
		// Member-access context.
		if prior := indexOfPriorSignificantToken(cache.Tokens, idx); prior >= 0 {
			tok := cache.Tokens[prior]
			if tok.Type != lexer.TokenIdentifier {
				return nil
			}
			switch {
			case strings.EqualFold(tok.Text, "Me"), strings.EqualFold(tok.Text, "Base"):
				if className := enclosingClassName(cache.Tokens); className != "" {
					if items := providers.GetClassMemberCompletions(className); items != nil {
						return items
					}
				}
			case constants.IsSSLClass(tok.Text):
				return providers.GetClassMemberCompletions(tok.Text)
			default:
				// Endpoint ambients complete from their backing returns
				// object (issue #123 D1).
				if typeName := providers.AmbientReceiverType(tok.Text, endpointFile); typeName != "" {
					return providers.GetReturnsMemberCompletions(typeName)
				}
				// Typed receivers — class instances and returns objects
				// tracked through producer chains (issue #123 D2); checked
				// before UDObject shapes, with any ad-hoc shape-augmented
				// properties of the same variable merged in.
				typed := providers.BuildTypedReceivers(cache.Tokens, endpointFile)
				shapes := providers.BuildUDObjectShapesWithProcedures(cache.Tokens, cache.Procedures)
				if typeName, isTyped := typed[strings.ToLower(tok.Text)]; isTyped {
					if items := providers.GetTypedMemberCompletions(typeName); items != nil {
						items = append(items, mergeShapeCompletions(items, tok.Text, shapes)...)
						return items
					}
				}
				// Issue #7: variable bound to an inferred UDObject shape.
				if items := providers.GetUDObjectShapeCompletions(tok.Text, shapes); items != nil {
					return items
				}
			}
		}
	}

	return nil
}

// mergeShapeCompletions returns the shape-augmented properties of varName
// that aren't already present (by label, case-insensitive) in the typed
// member list — a typed receiver can carry ad-hoc `oVar:prop := ...`
// properties on top of its class/returns-object members.
func mergeShapeCompletions(existing []providers.CompletionItem, varName string, shapes map[string]providers.UDObjectShape) []providers.CompletionItem {
	shapeItems := providers.GetUDObjectShapeCompletions(varName, shapes)
	if len(shapeItems) == 0 {
		return nil
	}
	seen := make(map[string]struct{}, len(existing))
	for _, it := range existing {
		seen[strings.ToLower(it.Label)] = struct{}{}
	}
	var out []providers.CompletionItem
	for _, it := range shapeItems {
		if _, dup := seen[strings.ToLower(it.Label)]; dup {
			continue
		}
		out = append(out, it)
	}
	return out
}

// isDoProcStringContext reports whether the cursor at (line, column) sits
// inside a string literal that is the first positional argument of a
// `DoProc(...)` or `ExecFunction(...)` call. It tolerates a cursor positioned
// anywhere within the string (including at the closing quote) and is
// case-insensitive on the function name.
func isDoProcStringContext(tokens []lexer.Token, line, column int) bool {
	stringIdx := tokenContainingPosition(tokens, line, column)
	if stringIdx < 0 || tokens[stringIdx].Type != lexer.TokenString {
		return false
	}

	parenIdx := indexOfPriorSignificantToken(tokens, stringIdx)
	if parenIdx < 0 || tokens[parenIdx].Text != "(" {
		return false
	}

	nameIdx := indexOfPriorSignificantToken(tokens, parenIdx)
	if nameIdx < 0 || tokens[nameIdx].Type != lexer.TokenIdentifier {
		return false
	}
	name := strings.ToLower(tokens[nameIdx].Text)
	return name == "doproc" || name == "execfunction"
}

// tokenContainingPosition returns the index of the token whose source range
// contains (line, column), or -1 if no token does. Multi-line tokens (strings,
// comments) are handled by counting newlines in the token text.
func tokenContainingPosition(tokens []lexer.Token, line, column int) int {
	for i := range tokens {
		tok := tokens[i]
		startLine := tok.Line
		startCol := tok.Column
		endLine := startLine
		endCol := startCol
		for _, r := range tok.Text {
			if r == '\n' {
				endLine++
				endCol = 1
			} else {
				endCol++
			}
		}
		within := false
		switch {
		case line == startLine && line == endLine:
			within = column >= startCol && column <= endCol
		case line == startLine:
			within = column >= startCol
		case line == endLine:
			within = column <= endCol
		case line > startLine && line < endLine:
			within = true
		}
		if within {
			return i
		}
	}
	return -1
}

// indexOfTokenBefore returns the index of the latest token whose end
// position is at or before (line, column). Whitespace, comment, and EOF
// tokens are skipped.
func indexOfTokenBefore(tokens []lexer.Token, line, column int) int {
	best := -1
	for i, tok := range tokens {
		if tok.Type == lexer.TokenWhitespace || tok.Type == lexer.TokenComment || tok.Type == lexer.TokenEOF {
			continue
		}
		endCol := tok.Column + len(tok.Text)
		if tok.Line < line || (tok.Line == line && endCol <= column) {
			best = i
			continue
		}
		break
	}
	return best
}

// indexOfPriorSignificantToken returns the most recent non-whitespace,
// non-comment, non-EOF token strictly before idx, or -1 if none exists.
func indexOfPriorSignificantToken(tokens []lexer.Token, idx int) int {
	for j := idx - 1; j >= 0; j-- {
		tok := tokens[j]
		if tok.Type == lexer.TokenWhitespace || tok.Type == lexer.TokenComment || tok.Type == lexer.TokenEOF {
			continue
		}
		return j
	}
	return -1
}

// enclosingClassName returns the name of the class declared at the file's
// top level, or "" when the file is not a class. SSL allows only one class
// per file, so we just walk forward until we find `:CLASS <name>`.
func enclosingClassName(tokens []lexer.Token) string {
	for i, tok := range tokens {
		if tok.Type == lexer.TokenKeyword && strings.EqualFold(tok.Text, ":CLASS") {
			if next := indexOfNextSignificantToken(tokens, i); next >= 0 {
				if tokens[next].Type == lexer.TokenIdentifier {
					return tokens[next].Text
				}
			}
			return ""
		}
	}
	return ""
}

func indexOfNextSignificantToken(tokens []lexer.Token, idx int) int {
	for j := idx + 1; j < len(tokens); j++ {
		tok := tokens[j]
		if tok.Type == lexer.TokenWhitespace || tok.Type == lexer.TokenComment || tok.Type == lexer.TokenEOF {
			continue
		}
		return j
	}
	return -1
}

func isClassMethodContext(tokens []lexer.Token, procedures []parser.ProcedureInfo, line int) bool {
	if !isClassFile(tokens) {
		return false
	}

	for _, proc := range procedures {
		if line >= proc.StartLine && line <= proc.EndLine {
			return true
		}
	}

	return false
}

func isClassFile(tokens []lexer.Token) bool {
	for _, token := range tokens {
		if token.Type == lexer.TokenWhitespace || token.Type == lexer.TokenComment {
			continue
		}

		return token.Type == lexer.TokenKeyword && strings.EqualFold(token.Text, ":CLASS")
	}

	return false
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

	// Check if we're inside a string or comment
	ctx := lexer.GetContextAtPosition(cache.Tokens, line, column)
	if ctx == lexer.ContextString {
		// Dispatch-target strings are the second string-hover exception
		// (after SQL placeholders): a dotted DoProc/ExecFunction target
		// that resolves cross-file shows the target's signature, and a
		// bare 1-part target keeps same-file semantics — mirroring
		// go-to-definition — showing the local procedure's hover
		// (feature.hover A17). A dispatch string is never an SQL string,
		// so checking first is safe; unresolvable targets fall through
		// to the suppression.
		if isDoProcStringContext(cache.Tokens, line, column) {
			if dt := providers.DispatchTargetAt(cache.Tokens, line, column); dt != nil {
				var md string
				if len(dt.Parts) >= 2 {
					md = (liveResolver{s}).dispatchHoverMarkdown(dt.Raw)
				} else {
					md = providers.ProcedureHoverMarkdown(dt.Raw, cache.Procedures)
				}
				if md != "" {
					return &protocol.Hover{
						Contents: protocol.MarkupContent{
							Kind:  protocol.MarkupKindMarkdown,
							Value: md,
						},
					}, nil
				}
			}
		}
		// RunDS target strings show the resolved data-source summary
		// (feature.hover A14); like dispatch targets, unresolvable ones
		// fall through to the string suppression.
		if dst := providers.DataSourceTargetAt(cache.Tokens, line, column); dst != nil {
			if md := (liveResolver{s}).dataSourceHoverMarkdown(dst.Raw); md != "" {
				return &protocol.Hover{
					Contents: protocol.MarkupContent{
						Kind:  protocol.MarkupKindMarkdown,
						Value: md,
					},
				}, nil
			}
		}
		// Inside a string - check for SQL placeholder hover
		hover := providers.GetSQLPlaceholderHoverFromToken(cache.Tokens, line, column)
		if hover != nil {
			return &protocol.Hover{
				Contents: protocol.MarkupContent{
					Kind:  protocol.MarkupKindMarkdown,
					Value: hover.Contents,
				},
			}, nil
		}
		// No SQL placeholder found, return no hover for strings
		return nil, nil
	} else if ctx == lexer.ContextComment {
		// Inside a comment - return no hover
		return nil, nil
	}

	// :INCLUDE targets hover with the resolved script's summary.
	if it := providers.IncludeTargetAt(cache.Tokens, line, column); it != nil {
		if md := (liveResolver{s}).includeHoverMarkdown(it.Raw); md != "" {
			return &protocol.Hover{
				Contents: protocol.MarkupContent{
					Kind:  protocol.MarkupKindMarkdown,
					Value: md,
				},
			}, nil
		}
	}

	endpointFile := isEndpointFile(uri, content, s.settings.EndpointPatterns)

	// Member hover for typed receivers and shape-inferred UDObject
	// receivers (feature.hover A15-A16, issue #123): a known receiver's
	// known member shows its detail; its unknown member is null, never an
	// unrelated symbol. Typed receivers (ambient Request/Response, class
	// instances, returns objects from producer chains) are checked before
	// UDObject shapes.
	if recv, member, ok := providers.MemberAccessAt(cache.Tokens, line, column); ok {
		typeName := providers.AmbientReceiverType(recv, endpointFile)
		if typeName == "" {
			typed := providers.BuildTypedReceivers(cache.Tokens, endpointFile)
			typeName = typed[strings.ToLower(recv)]
		}
		if typeName != "" {
			if md := providers.RenderTypedMemberHover(typeName, recv, member); md != "" {
				return &protocol.Hover{
					Contents: protocol.MarkupContent{
						Kind:  protocol.MarkupKindMarkdown,
						Value: md,
					},
				}, nil
			}
			// A typed receiver can still carry ad-hoc shape-augmented
			// properties (`oClient:MyTag := ...`) — consult the shape
			// before answering null.
			shapes := providers.BuildUDObjectShapesWithProcedures(cache.Tokens, cache.Procedures)
			if shape, shaped := shapes[strings.ToLower(recv)]; shaped {
				if md := providers.RenderUDObjectMemberHover(shape, recv, member); md != "" {
					return &protocol.Hover{
						Contents: protocol.MarkupContent{
							Kind:  protocol.MarkupKindMarkdown,
							Value: md,
						},
					}, nil
				}
			}
			return nil, nil
		}
		shapes := providers.BuildUDObjectShapesWithProcedures(cache.Tokens, cache.Procedures)
		if shape, shaped := shapes[strings.ToLower(recv)]; shaped {
			if md := providers.RenderUDObjectMemberHover(shape, recv, member); md != "" {
				return &protocol.Hover{
					Contents: protocol.MarkupContent{
						Kind:  protocol.MarkupKindMarkdown,
						Value: md,
					},
				}, nil
			}
			return nil, nil
		}
	}

	var hover *providers.Hover
	if endpointFile {
		if word := lexer.GetWordAtPosition(content, line, column); word != "" {
			hover = providers.GetEndpointAmbientHover(word)
		}
	}
	if hover == nil {
		hover = providers.GetHover(
			content,
			line,
			column,
			cache.Procedures,
			cache.Variables,
		)
	}

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

	locations := providers.FindDefinitionCrossFile(
		content,
		cache.Tokens,
		int(params.Position.Line)+1,
		int(params.Position.Character)+1,
		uri,
		cache.Procedures,
		cache.Variables,
		liveResolver{s},
	)

	switch len(locations) {
	case 0:
		return nil, nil
	case 1:
		// Preserve the historical single-Location wire shape.
		return toProtocolLocation(locations[0]), nil
	default:
		out := make([]protocol.Location, 0, len(locations))
		for _, loc := range locations {
			out = append(out, toProtocolLocation(loc))
		}
		return out, nil
	}
}

// handleReferences handles find references requests. Procedure subjects
// extend cross-file through dispatch call sites (issue #125,
// feature.references A10-A14); everything else — variables, parameters —
// keeps the single-file path, and a nil workspace index reproduces the
// single-file behavior exactly.
func (s *SSLServer) handleReferences(context *glsp.Context, params *protocol.ReferenceParams) ([]protocol.Location, error) {
	uri := params.TextDocument.URI

	content, ok := s.documents.GetDocument(uri)
	if !ok {
		return nil, nil
	}

	// Get cached procedures and variables for scope-aware search
	version := s.documentVersion[uri]
	cache := s.documents.ParseDocument(uri, version)
	line := int(params.Position.Line) + 1
	column := int(params.Position.Character) + 1

	// Cursor on a dotted dispatch string: the subject is the resolved
	// procedure, wherever it lives — same-file word matches would name an
	// unrelated local symbol. Entry-point subjects (2-part Cat.Script) and
	// unresolvable targets keep today's behavior (D5 deferred).
	if s.workspaceIndex != nil {
		if dt := providers.DispatchTargetAt(cache.Tokens, line, column); dt != nil && len(dt.Parts) >= 2 {
			res := (liveResolver{s}).overlayResolutions(s.workspaceIndex.ResolveDispatchTarget(dt.Raw))
			if len(res) > 0 && !res[0].IsEntry {
				return toProtocolLocations(s.crossFileProcedureReferences(
					res[0].URI, res[0].ProcName, params.Context.IncludeDeclaration)), nil
			}
		}
	}

	locations := providers.FindReferencesWithScope(
		content,
		line,
		column,
		uri,
		params.Context.IncludeDeclaration,
		cache.Procedures,
		cache.Variables,
	)

	// Cursor on a procedure defined in this file: extend with dispatch
	// sites across the workspace (plus dotted self-sites the same-file
	// whole-content match cannot see). A local/parameter that shadows the
	// procedure's name keeps the scope-aware single-file result (F3).
	if s.workspaceIndex != nil {
		if word := lexer.GetWordAtPosition(content, line, column); word != "" {
			if procName, ok := procedureSubjectAt(cache, word, line); ok {
				locations = append(locations, s.dispatchSiteReferences(uri, procName, locations)...)
			}
		}
	}

	return toProtocolLocations(locations), nil
}

// toProtocolLocations converts provider locations, mapping empty to nil.
func toProtocolLocations(locations []providers.Location) []protocol.Location {
	if locations == nil {
		return nil
	}
	result := make([]protocol.Location, 0, len(locations))
	for _, loc := range locations {
		result = append(result, toProtocolLocation(loc))
	}
	return result
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

	// Phase 1: Results from open documents (highest priority — most up-to-date)
	openURIs := s.documents.OpenURIs()
	for _, uri := range s.documents.AllDocuments() {
		version := s.documentVersion[uri]
		cache := s.documents.ParseDocument(uri, version)
		// Match the index's classification: procedures in a :CLASS file are
		// methods, so a symbol keeps its kind whether its file is open or not.
		kind := protocol.SymbolKindFunction
		if isClassFileFromTokens(cache.Tokens) {
			kind = protocol.SymbolKindMethod
		}
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
				Kind:     kind,
				Location: protocol.Location{URI: uri, Range: toProtocolRange(rangeInfo)},
			})
		}
	}

	// Phase 2: Results from workspace index (excludes open documents)
	if s.workspaceIndex != nil {
		for _, sym := range s.workspaceIndex.SearchSymbols(query, openURIs) {
			rangeInfo := providers.Range{
				Start: providers.Position{Line: sym.StartLine - 1, Character: 0},
				End:   providers.Position{Line: sym.EndLine - 1, Character: 0},
			}
			results = append(results, protocol.SymbolInformation{
				Name:     sym.Name,
				Kind:     protocol.SymbolKind(sym.Kind),
				Location: protocol.Location{URI: sym.URI, Range: toProtocolRange(rangeInfo)},
			})
		}
	}

	// Cap results
	if len(results) > maxSymbolResults {
		results = results[:maxSymbolResults]
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

// colonStartsToken reports whether the ':' just typed at `position` begins a
// new token — i.e. the character immediately before it is whitespace, or the
// ':' is at the start of a line.
//
// `position` points to the cursor location AFTER the ':' was inserted, so the
// ':' itself sits at character-1 and the preceding char (if any) is at
// character-2. character is in UTF-16 code units per LSP, but SSL source is
// ASCII in practice so byte indexing is safe.
func colonStartsToken(content string, position protocol.Position) bool {
	if position.Character < 2 {
		return true
	}
	line := int(position.Line)
	idx := 0
	current := 0
	for idx < len(content) && current < line {
		if content[idx] == '\n' {
			current++
		}
		idx++
	}
	// idx now points at the start of the target line (or end of content).
	target := int(position.Character) - 2
	for j := 0; j < target; j++ {
		if idx+j >= len(content) || content[idx+j] == '\n' {
			return true
		}
	}
	if idx+target >= len(content) {
		return true
	}
	c := content[idx+target]
	return c == ' ' || c == '\t' || c == '\r'
}

// keywordCompletionsForColonTrigger builds keyword completion items for a
// ':' trigger. Each item carries a TextEdit that replaces the typed ':' with
// ':KEYWORD' so the editor cannot end up with '::KEYWORD'. See issue #12.
func keywordCompletionsForColonTrigger(position protocol.Position) []protocol.CompletionItem {
	// The typed ':' lives at the column before the cursor.
	colonRange := protocol.Range{
		Start: protocol.Position{Line: position.Line, Character: position.Character - 1},
		End:   protocol.Position{Line: position.Line, Character: position.Character},
	}
	items := providers.GetKeywordCompletions()
	result := make([]protocol.CompletionItem, 0, len(items))
	plain := protocol.InsertTextFormatPlainText
	for _, c := range items {
		detail := c.Detail
		doc := protocol.MarkupContent{Kind: protocol.MarkupKindMarkdown, Value: c.Documentation}
		newText := c.InsertText
		edit := protocol.TextEdit{Range: colonRange, NewText: newText}
		var editAny any = edit
		result = append(result, protocol.CompletionItem{
			Label:            c.Label,
			Kind:             ptrTo(protocol.CompletionItemKind(c.Kind)),
			Detail:           &detail,
			Documentation:    &doc,
			InsertTextFormat: &plain,
			TextEdit:         editAny,
		})
	}
	return result
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

	// SQL-mode data source (plain SQL or directives-then-SQL): the SSL
	// formatter would inject semicolons and re-case bind variables, so
	// return no edits (feature.formatting A9, issues #84/#104).
	if isDataSourceURI(uri) && providers.IsSQLModeDataSource(content) {
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

	// Same SQL-mode gate as handleFormatting (feature.formatting A9).
	if isDataSourceURI(uri) && providers.IsSQLModeDataSource(content) {
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

// handlePrepareRename handles prepare rename requests.
func (s *SSLServer) handlePrepareRename(context *glsp.Context, params *protocol.PrepareRenameParams) (any, error) {
	uri := params.TextDocument.URI

	content, ok := s.documents.GetDocument(uri)
	if !ok {
		return nil, nil
	}

	version := s.documentVersion[uri]
	cache := s.documents.ParseDocument(uri, version)

	result := providers.PrepareRename(
		content,
		int(params.Position.Line)+1,
		int(params.Position.Character)+1,
		uri,
		cache.Procedures,
		cache.Variables,
	)

	if result == nil {
		return nil, nil
	}

	return protocol.RangeWithPlaceholder{
		Range:       toProtocolRange(result.Range),
		Placeholder: result.Placeholder,
	}, nil
}

// handleRename handles rename requests.
// handleRename handles rename requests. Procedure subjects rename
// workspace-wide through dispatch call sites (issue #125, feature.rename
// A9-A15); class-file procedures refuse the cross-file path (D8) and keep
// same-file behavior; a nil workspace index reproduces the single-file
// rename exactly.
func (s *SSLServer) handleRename(context *glsp.Context, params *protocol.RenameParams) (*protocol.WorkspaceEdit, error) {
	uri := params.TextDocument.URI

	content, ok := s.documents.GetDocument(uri)
	if !ok {
		return nil, nil
	}

	version := s.documentVersion[uri]
	cache := s.documents.ParseDocument(uri, version)
	line := int(params.Position.Line) + 1
	column := int(params.Position.Character) + 1

	if s.workspaceIndex != nil {
		// Subject: dotted dispatch string under the cursor. Conservative
		// end to end — the subject itself must resolve unambiguously to a
		// procedure (D1); entry points are out of scope (D4/D5).
		if dt := providers.DispatchTargetAt(cache.Tokens, line, column); dt != nil && len(dt.Parts) >= 2 {
			res := (liveResolver{s}).overlayResolutions(s.workspaceIndex.ResolveDispatchTarget(dt.Raw))
			if len(res) == 1 && !res[0].IsEntry {
				if changes := s.crossFileRename(res[0].URI, res[0].ProcName, params.NewName); changes != nil {
					return toWorkspaceEdit(changes), nil
				}
			}
			return nil, nil
		}
		// Subject: a procedure defined in this file — unless a local or
		// parameter shadows the name at the cursor (F3), which keeps the
		// scope-aware single-file path. A refusal (class file, D8) falls
		// back to the same-file path below.
		if word := lexer.GetWordAtPosition(content, line, column); word != "" {
			if procName, ok := procedureSubjectAt(cache, word, line); ok {
				if changes := s.crossFileRename(uri, procName, params.NewName); changes != nil {
					return toWorkspaceEdit(changes), nil
				}
			}
		}
	}

	result := providers.Rename(
		content,
		line,
		column,
		params.NewName,
		uri,
		cache.Procedures,
		cache.Variables,
	)

	if result == nil {
		return nil, nil
	}

	return toWorkspaceEdit(result.Changes), nil
}

// toWorkspaceEdit converts provider edit maps to a protocol WorkspaceEdit.
func toWorkspaceEdit(providerChanges map[string][]providers.TextEdit) *protocol.WorkspaceEdit {
	changes := make(map[protocol.DocumentUri][]protocol.TextEdit)
	for docUri, edits := range providerChanges {
		protocolEdits := make([]protocol.TextEdit, 0, len(edits))
		for _, edit := range edits {
			protocolEdits = append(protocolEdits, toProtocolTextEdit(edit))
		}
		changes[docUri] = protocolEdits
	}
	return &protocol.WorkspaceEdit{
		Changes: changes,
	}
}

// handleInlayHint handles textDocument/inlayHint requests.
func (s *SSLServer) handleInlayHint(context *glsp.Context, params *InlayHintParams) ([]InlayHint, error) {
	uri := params.TextDocument.URI

	if _, ok := s.documents.GetDocument(uri); !ok {
		return nil, nil
	}

	version := s.documentVersion[uri]
	cache := s.documents.ParseDocument(uri, version)

	// Convert 0-based LSP range to 1-based internal range
	startLine := int(params.Range.Start.Line) + 1
	endLine := int(params.Range.End.Line) + 1

	// Get inlay hints from provider
	hints := providers.GetInlayHints(
		cache.Tokens,
		cache.Procedures,
		startLine,
		endLine,
		s.settings.InlayHints,
	)

	if len(hints) == 0 {
		return nil, nil
	}

	// Convert to protocol format
	result := make([]InlayHint, 0, len(hints))
	kind := InlayHintKindParameter
	for _, h := range hints {
		result = append(result, InlayHint{
			Position: InlayHintPosition{
				Line:      uint32(h.Line - 1),      // Convert to 0-based
				Character: uint32(h.Character - 1), // Convert to 0-based
			},
			Label:        h.Label + ":",
			Kind:         &kind,
			PaddingRight: true,
		})
	}

	return result, nil
}

// dispatchStringPrefix returns the string content typed before the cursor
// inside a dispatch-target string literal — the segment prefix that decides
// the completion level ("" | "Cat." | "Cat.Script.").
func dispatchStringPrefix(tokens []lexer.Token, line, column int) string {
	idx := tokenContainingPosition(tokens, line, column)
	if idx < 0 || tokens[idx].Type != lexer.TokenString {
		return ""
	}
	tok := tokens[idx]
	// Content starts after the opening quote; the cursor column is 1-based.
	start := tok.Column + 1
	if column <= start {
		return ""
	}
	end := column - start
	content := tok.Text[1:] // strip opening quote
	if end > len(content) {
		end = len(content)
	}
	return content[:end]
}
