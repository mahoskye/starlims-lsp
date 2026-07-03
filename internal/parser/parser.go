// Package parser provides AST parsing for STARLIMS Scripting Language (SSL).
package parser

import (
	"regexp"
	"strings"

	"starlims-lsp/internal/constants"
	"starlims-lsp/internal/lexer"
)

// NodeType represents the type of an AST node.
type NodeType int

const (
	NodeProgram NodeType = iota
	NodeBlock
	NodeStatement
	NodeComment
	NodeRegionStart
	NodeRegionEnd
	NodeProcedure
	NodeClass
	NodeUnknown
)

// String returns the string representation of a NodeType.
func (n NodeType) String() string {
	switch n {
	case NodeProgram:
		return "Program"
	case NodeBlock:
		return "Block"
	case NodeStatement:
		return "Statement"
	case NodeComment:
		return "Comment"
	case NodeRegionStart:
		return "RegionStart"
	case NodeRegionEnd:
		return "RegionEnd"
	case NodeProcedure:
		return "Procedure"
	case NodeClass:
		return "Class"
	default:
		return "Unknown"
	}
}

// Node represents an AST node.
type Node struct {
	Type      NodeType
	Tokens    []lexer.Token
	Children  []*Node
	Parent    *Node
	StartLine int
	EndLine   int
	Name      string
}

// ProcedureInfo contains information about a procedure.
type ProcedureInfo struct {
	Name       string
	Parameters []string
	StartLine  int
	EndLine    int
	Node       *Node
	// Doc is the parsed leading docblock immediately preceding the
	// :PROCEDURE declaration (if any). Zero value means no docblock.
	Doc ProcedureDoc
	// IsPrivate is true when a /*@private; or /*@protected; visibility
	// annotation immediately precedes the :PROCEDURE declaration. Both
	// annotations make the procedure unreachable via DoProc/ExecFunction
	// dispatch, so cross-file surfaces treat them alike.
	IsPrivate bool
}

// ProcedureDoc holds the structured pieces of a procedure's leading docblock,
// parsed from the SSL convention documented in the SSL style guide:
//
//	/*
//	 * Procedure: ProcedureName
//	 * Description: Brief description
//	 * Parameters:
//	 *   sParam1 - Description
//	 * Returns: sResult - Description
//	;
//
// Empty fields mean the docblock either didn't include them or wasn't present
// at all. Raw is the original comment text (without `/*` / `;` framing) so
// callers can fall back to the raw form.
type ProcedureDoc struct {
	Description string
	// ParameterDocs is keyed by parameter name (preserves doc-block casing).
	ParameterDocs map[string]string
	Returns       string
	Raw           string
}

// VariableScope represents the scope of a variable.
type VariableScope string

const (
	ScopeLocal     VariableScope = "local"
	ScopePublic    VariableScope = "public"
	ScopeParameter VariableScope = "parameter"
)

// VariableInfo contains information about a variable.
type VariableInfo struct {
	Name   string
	Line   int
	Column int
	Scope  VariableScope
}

var (
	regionStartPattern = regexp.MustCompile(`(?i)^/\*\s*region`)
	regionEndPattern   = regexp.MustCompile(`(?i)^/\*\s*endregion`)
)

// Parser parses SSL tokens into an AST.
type Parser struct {
	tokens []lexer.Token
}

// NewParser creates a new Parser.
func NewParser(tokens []lexer.Token) *Parser {
	return &Parser{tokens: tokens}
}

// Parse parses tokens into an AST.
func (p *Parser) Parse() *Node {
	root := &Node{
		Type:      NodeProgram,
		Tokens:    nil,
		Children:  nil,
		StartLine: 0,
		EndLine:   0,
	}

	statements := p.groupStatements(p.tokens)
	currentNode := root
	stack := []*Node{root}

	for _, stmt := range statements {
		blockStart := p.isBlockStart(stmt)
		blockEnd := p.isBlockEnd(stmt)
		blockMiddle := p.isBlockMiddle(stmt)

		if blockMiddle {
			currentNode = p.handleBlockMiddle(stmt, currentNode, &stack)
			continue
		}

		if blockEnd {
			currentNode = p.handleBlockEnd(stmt, currentNode, &stack)
		}

		currentNode.Children = append(currentNode.Children, stmt)

		if blockStart {
			currentNode = p.handleBlockStart(stmt, currentNode, &stack)
		}
	}

	if len(p.tokens) > 0 {
		lastLine := p.tokens[len(p.tokens)-1].Line
		for len(stack) > 0 {
			node := stack[len(stack)-1]
			stack = stack[:len(stack)-1]
			if node.EndLine == 0 || node.EndLine <= node.StartLine {
				node.EndLine = lastLine
			}
		}
	}

	return root
}

// ExtractProcedures extracts all procedures from the AST.
func (p *Parser) ExtractProcedures(root *Node) []ProcedureInfo {
	var procedures []ProcedureInfo
	p.findProcedures(root, &procedures)
	return procedures
}

func (p *Parser) findProcedures(node *Node, procedures *[]ProcedureInfo) {
	for _, child := range node.Children {
		firstToken := p.getFirstSignificantToken(child)
		if firstToken != nil && p.getNormalizedText(firstToken) == "PROCEDURE" {
			name := p.extractProcedureName(child)
			parameters := p.extractParameters(node, child)
			if name != "" {
				*procedures = append(*procedures, ProcedureInfo{
					Name:       name,
					Parameters: parameters,
					StartLine:  child.StartLine,
					EndLine:    p.findProcedureEndLine(node, child),
					Node:       child,
					Doc:        p.extractProcedureDoc(firstToken),
					IsPrivate:  p.isPrivateProcedure(firstToken),
				})
			}
		}
		p.findProcedures(child, procedures)
	}
}

// extractProcedureDoc walks backward from the :PROCEDURE token in the global
// token stream to find the immediately preceding /* ... ; comment block. The
// search skips whitespace tokens; any non-whitespace, non-comment token aborts
// (the comment isn't "attached" to this procedure). Multiple adjacent comment
// blocks are concatenated, with the closest one to the procedure taking
// precedence for parsed fields.
func (p *Parser) extractProcedureDoc(procToken *lexer.Token) ProcedureDoc {
	idx := -1
	for i := range p.tokens {
		if p.tokens[i].Offset == procToken.Offset && p.tokens[i].Line == procToken.Line {
			idx = i
			break
		}
	}
	if idx <= 0 {
		return ProcedureDoc{}
	}

	var commentText string
	for j := idx - 1; j >= 0; j-- {
		t := p.tokens[j]
		if t.Type == lexer.TokenWhitespace {
			continue
		}
		if t.Type == lexer.TokenComment {
			// A visibility annotation sits between the docblock and the
			// :PROCEDURE line — skip it and keep walking to the docblock.
			if _, ok := ParseVisibilityAnnotation(t.Text); ok {
				continue
			}
			commentText = t.Text
		}
		break
	}
	if commentText == "" {
		return ProcedureDoc{}
	}
	return parseProcedureDoc(commentText)
}

// isPrivateProcedure reports whether the comment immediately preceding the
// :PROCEDURE token (skipping whitespace) is a visibility annotation. Only
// the nearest comment counts, matching the placement rule the
// visibility_annotation diagnostic enforces (annotation on its own line
// immediately before :PROCEDURE).
func (p *Parser) isPrivateProcedure(procToken *lexer.Token) bool {
	idx := -1
	for i := range p.tokens {
		if p.tokens[i].Offset == procToken.Offset && p.tokens[i].Line == procToken.Line {
			idx = i
			break
		}
	}
	if idx <= 0 {
		return false
	}

	for j := idx - 1; j >= 0; j-- {
		t := p.tokens[j]
		if t.Type == lexer.TokenWhitespace {
			continue
		}
		if t.Type == lexer.TokenComment {
			_, ok := ParseVisibilityAnnotation(t.Text)
			return ok
		}
		break
	}
	return false
}

// ExtractTopLevelParameters returns the names and 1-based line of the
// script's entry-point :PARAMETERS statement — the first top-level
// :PARAMETERS appearing before any :PROCEDURE. Cross-script 2-part
// ExecFunction("Category.Script") calls bind their arguments to this list.
// Returns (nil, -1) when the script has none.
func (p *Parser) ExtractTopLevelParameters(root *Node) ([]string, int) {
	for _, child := range root.Children {
		firstToken := p.getFirstSignificantToken(child)
		if firstToken == nil {
			continue
		}
		switch p.getNormalizedText(firstToken) {
		case "PROCEDURE":
			return nil, -1
		case "PARAMETERS":
			var params []string
			for _, token := range child.Tokens {
				if token.Type == lexer.TokenIdentifier && p.getNormalizedText(&token) != "PARAMETERS" {
					params = append(params, token.Text)
				}
			}
			return params, firstToken.Line
		}
	}
	return nil, -1
}

// parseProcedureDoc reads the SSL convention docblock format. It is tolerant
// of variations: lines may or may not start with `*`, fields may be missing,
// and the Parameters section may be absent.
func parseProcedureDoc(raw string) ProcedureDoc {
	body := raw
	body = strings.TrimPrefix(body, "/*")
	body = strings.TrimSuffix(body, ";")

	doc := ProcedureDoc{Raw: strings.TrimSpace(body)}

	type section int
	const (
		sectionNone section = iota
		sectionParams
	)

	current := sectionNone
	descParts := []string{}
	for _, line := range strings.Split(body, "\n") {
		stripped := strings.TrimSpace(line)
		stripped = strings.TrimPrefix(stripped, "*")
		stripped = strings.TrimSpace(stripped)
		if stripped == "" {
			continue
		}

		lower := strings.ToLower(stripped)
		switch {
		case strings.HasPrefix(lower, "procedure:"):
			current = sectionNone
		case strings.HasPrefix(lower, "description:"):
			current = sectionNone
			descParts = append(descParts, strings.TrimSpace(stripped[len("description:"):]))
		case strings.HasPrefix(lower, "parameters:"):
			current = sectionParams
			rest := strings.TrimSpace(stripped[len("parameters:"):])
			if rest != "" {
				addParamDoc(&doc, rest)
			}
		case strings.HasPrefix(lower, "returns:"):
			current = sectionNone
			doc.Returns = strings.TrimSpace(stripped[len("returns:"):])
		default:
			if current == sectionParams {
				addParamDoc(&doc, stripped)
			} else if len(descParts) > 0 {
				descParts = append(descParts, stripped)
			}
		}
	}
	doc.Description = strings.TrimSpace(strings.Join(descParts, " "))
	return doc
}

// addParamDoc records a "name - description" line into the params map. Lines
// that don't match this shape are recorded under the bare name (no description).
func addParamDoc(doc *ProcedureDoc, line string) {
	if doc.ParameterDocs == nil {
		doc.ParameterDocs = map[string]string{}
	}
	dash := strings.IndexAny(line, "-:")
	if dash <= 0 {
		name := strings.Fields(line)
		if len(name) == 0 {
			return
		}
		doc.ParameterDocs[name[0]] = ""
		return
	}
	name := strings.TrimSpace(line[:dash])
	desc := strings.TrimSpace(line[dash+1:])
	if name == "" {
		return
	}
	doc.ParameterDocs[name] = desc
}

func (p *Parser) extractProcedureName(stmt *Node) string {
	foundProcedure := false
	for _, token := range stmt.Tokens {
		if token.Type == lexer.TokenWhitespace || token.Type == lexer.TokenComment {
			continue
		}
		if p.getNormalizedText(&token) == "PROCEDURE" {
			foundProcedure = true
			continue
		}
		if foundProcedure && token.Type == lexer.TokenIdentifier {
			return token.Text
		}
	}
	return ""
}

func (p *Parser) extractParameters(parent *Node, procedureStmt *Node) []string {
	blockNode := p.findProcedureBlock(parent, procedureStmt)
	if blockNode != nil {
		params := p.extractParametersFromStatements(blockNode.Children)
		if len(params) > 0 {
			return params
		}
	}

	startIdx := -1
	for i, child := range parent.Children {
		if child == procedureStmt {
			startIdx = i
			break
		}
	}
	if startIdx == -1 {
		return nil
	}

	return p.extractParametersFromStatements(parent.Children[startIdx+1:])
}

func (p *Parser) findProcedureBlock(parent *Node, procedureStmt *Node) *Node {
	for i, child := range parent.Children {
		if child == procedureStmt {
			if i+1 < len(parent.Children) && parent.Children[i+1].Type == NodeBlock {
				return parent.Children[i+1]
			}
			break
		}
	}
	return nil
}

func (p *Parser) extractParametersFromStatements(statements []*Node) []string {
	var params []string
	for i, child := range statements {
		if i >= 5 {
			break
		}
		firstToken := p.getFirstSignificantToken(child)
		if firstToken != nil && p.getNormalizedText(firstToken) == "PARAMETERS" {
			for _, token := range child.Tokens {
				if token.Type == lexer.TokenIdentifier && p.getNormalizedText(&token) != "PARAMETERS" {
					params = append(params, token.Text)
				}
			}
			break
		}
		if firstToken != nil && constants.IsBlockStartKeyword(p.getNormalizedText(firstToken)) {
			break
		}
	}
	return params
}

func (p *Parser) findProcedureEndLine(parent *Node, procedureStmt *Node) int {
	startIdx := -1

	for i, child := range parent.Children {
		if child == procedureStmt {
			startIdx = i
			break
		}
	}

	if startIdx == -1 {
		return procedureStmt.EndLine
	}

	for i := startIdx + 1; i < len(parent.Children); i++ {
		child := parent.Children[i]
		firstToken := p.getFirstSignificantToken(child)
		if firstToken != nil && p.getNormalizedText(firstToken) == "ENDPROC" {
			return child.EndLine
		}
	}

	// No :ENDPROC — the unclosed procedure extends to the end of the file,
	// like other unclosed blocks (the mistake is reported by unclosed_block).
	if len(p.tokens) > 0 {
		if lastLine := p.tokens[len(p.tokens)-1].Line; lastLine > procedureStmt.EndLine {
			return lastLine
		}
	}
	return procedureStmt.EndLine
}

// ExtractVariables extracts all variables from the AST.
func (p *Parser) ExtractVariables(root *Node) []VariableInfo {
	var variables []VariableInfo
	p.findVariables(root, &variables)
	return variables
}

func (p *Parser) findVariables(node *Node, variables *[]VariableInfo) {
	for _, child := range node.Children {
		firstToken := p.getFirstSignificantToken(child)
		if firstToken != nil {
			normalized := p.getNormalizedText(firstToken)
			if normalized == "DECLARE" || normalized == "PUBLIC" || normalized == "PARAMETERS" {
				var scope VariableScope
				switch normalized {
				case "PUBLIC":
					scope = ScopePublic
				case "PARAMETERS":
					scope = ScopeParameter
				default:
					scope = ScopeLocal
				}

				for _, token := range child.Tokens {
					upper := strings.ToUpper(token.Text)
					if token.Type == lexer.TokenIdentifier &&
						upper != "DECLARE" && upper != "PUBLIC" && upper != "PARAMETERS" {
						*variables = append(*variables, VariableInfo{
							Name:   token.Text,
							Line:   token.Line,
							Column: token.Column,
							Scope:  scope,
						})
					}
				}
			}
		}
		p.findVariables(child, variables)
	}
}

// --- Block Handlers ---

func (p *Parser) handleBlockMiddle(stmt *Node, currentNode *Node, stack *[]*Node) *Node {
	shouldPop := true
	firstToken := p.getFirstSignificantToken(stmt)
	isCase := firstToken != nil && constants.IsCaseKeyword(p.getNormalizedText(firstToken))

	if isCase && len(*stack) > 1 {
		currentBlock := (*stack)[len(*stack)-1]
		starter := p.getBlockStarter(currentBlock)
		if starter != nil {
			starterToken := p.getFirstSignificantToken(starter)
			if starterToken != nil && p.getNormalizedText(starterToken) == "BEGINCASE" {
				shouldPop = false
			}
		}
	}

	activeNode := currentNode
	if shouldPop && len(*stack) > 1 {
		popped := (*stack)[len(*stack)-1]
		*stack = (*stack)[:len(*stack)-1]
		popped.EndLine = stmt.StartLine - 1
		activeNode = (*stack)[len(*stack)-1]
	}

	activeNode.Children = append(activeNode.Children, stmt)

	newBlock := &Node{
		Type:      NodeBlock,
		Tokens:    nil,
		Children:  nil,
		Parent:    activeNode,
		StartLine: stmt.EndLine + 1,
		EndLine:   0,
	}
	activeNode.Children = append(activeNode.Children, newBlock)
	*stack = append(*stack, newBlock)

	return newBlock
}

func (p *Parser) handleBlockEnd(stmt *Node, currentNode *Node, stack *[]*Node) *Node {
	activeNode := currentNode
	if len(*stack) > 1 {
		popped := (*stack)[len(*stack)-1]
		*stack = (*stack)[:len(*stack)-1]
		popped.EndLine = stmt.EndLine
		activeNode = (*stack)[len(*stack)-1]
	}

	first := p.getFirstSignificantToken(stmt)
	if first != nil && p.getNormalizedText(first) == "ENDCASE" {
		if len(*stack) > 1 {
			currentBlock := (*stack)[len(*stack)-1]
			starter := p.getBlockStarter(currentBlock)
			if starter != nil {
				starterToken := p.getFirstSignificantToken(starter)
				if starterToken != nil && p.getNormalizedText(starterToken) == "BEGINCASE" {
					popped := (*stack)[len(*stack)-1]
					*stack = (*stack)[:len(*stack)-1]
					popped.EndLine = stmt.EndLine
					activeNode = (*stack)[len(*stack)-1]
				}
			}
		}
	}
	return activeNode
}

func (p *Parser) handleBlockStart(stmt *Node, currentNode *Node, stack *[]*Node) *Node {
	newBlock := &Node{
		Type:      NodeBlock,
		Tokens:    nil,
		Children:  nil,
		Parent:    currentNode,
		StartLine: stmt.StartLine,
		EndLine:   stmt.EndLine,
	}

	currentNode.Children = append(currentNode.Children, newBlock)
	*stack = append(*stack, newBlock)
	return newBlock
}

// --- Statement Grouping ---

func (p *Parser) groupStatements(tokens []lexer.Token) []*Node {
	var statements []*Node
	var currentTokens []lexer.Token

	for i := 0; i < len(tokens); i++ {
		token := tokens[i]

		if token.Type == lexer.TokenEOF {
			break
		}

		currentTokens = append(currentTokens, token)

		if token.Type == lexer.TokenPunctuation && token.Text == ";" {
			statements = append(statements, p.createNode(currentTokens))
			currentTokens = nil
		} else if token.Type == lexer.TokenWhitespace && strings.Contains(token.Text, "\n") {
			if p.isStatementContinuation(tokens, i, currentTokens) {
				continue
			}
			statements = append(statements, p.createNode(currentTokens))
			currentTokens = nil
		} else if token.Type == lexer.TokenComment {
			statements = append(statements, p.createNode([]lexer.Token{token}))
			currentTokens = nil
		}
	}

	if len(currentTokens) > 0 {
		statements = append(statements, p.createNode(currentTokens))
	}

	return statements
}

func (p *Parser) isStatementContinuation(tokens []lexer.Token, currentIndex int, currentStatementTokens []lexer.Token) bool {
	j := currentIndex + 1
	for j < len(tokens) && tokens[j].Type == lexer.TokenWhitespace {
		j++
	}

	if j >= len(tokens) {
		return false
	}

	next := tokens[j]
	k := len(currentStatementTokens) - 2
	for k >= 0 && currentStatementTokens[k].Type == lexer.TokenWhitespace {
		k--
	}

	if k < 0 {
		return false
	}

	last := currentStatementTokens[k]

	lastIsContinuation := last.Type == lexer.TokenOperator ||
		last.Text == "," || last.Text == "(" || last.Text == "[" || last.Text == "{"
	nextIsContinuation := next.Type == lexer.TokenOperator ||
		next.Text == "," || next.Text == "." || next.Text == ")" ||
		next.Text == "]" || next.Text == "}" || next.Text == ";"
	isFunctionCall := next.Text == "(" &&
		(last.Type == lexer.TokenIdentifier || last.Type == lexer.TokenKeyword)

	return lastIsContinuation || nextIsContinuation || isFunctionCall
}

func (p *Parser) createNode(tokens []lexer.Token) *Node {
	if len(tokens) == 0 {
		return &Node{Type: NodeStatement, Tokens: nil, Children: nil, StartLine: 0, EndLine: 0}
	}

	nodeType := NodeStatement
	if len(tokens) == 1 && tokens[0].Type == lexer.TokenComment {
		text := tokens[0].Text
		if p.isRegionStartText(text) {
			nodeType = NodeRegionStart
		} else if p.isRegionEndText(text) {
			nodeType = NodeRegionEnd
		} else {
			nodeType = NodeComment
		}
	}

	return &Node{
		Type:      nodeType,
		Tokens:    tokens,
		Children:  nil,
		StartLine: tokens[0].Line,
		EndLine:   tokens[len(tokens)-1].Line,
	}
}

// --- Helpers ---

func (p *Parser) getNormalizedText(token *lexer.Token) string {
	return strings.ToUpper(strings.TrimPrefix(token.Text, ":"))
}

func (p *Parser) isRegionStartText(text string) bool {
	return regionStartPattern.MatchString(text)
}

func (p *Parser) isRegionEndText(text string) bool {
	return regionEndPattern.MatchString(text)
}

func (p *Parser) isBlockStart(node *Node) bool {
	first := p.getFirstSignificantToken(node)
	if first == nil || first.Type != lexer.TokenKeyword {
		return false
	}
	return constants.IsBlockStartKeyword(p.getNormalizedText(first))
}

func (p *Parser) isBlockEnd(node *Node) bool {
	first := p.getFirstSignificantToken(node)
	if first == nil || first.Type != lexer.TokenKeyword {
		return false
	}
	return constants.IsBlockEndKeyword(p.getNormalizedText(first))
}

func (p *Parser) isBlockMiddle(node *Node) bool {
	first := p.getFirstSignificantToken(node)
	if first == nil || first.Type != lexer.TokenKeyword {
		return false
	}
	text := p.getNormalizedText(first)
	return constants.IsBlockMiddleKeyword(text) || constants.IsCaseKeyword(text)
}

func (p *Parser) getFirstSignificantToken(node *Node) *lexer.Token {
	for i := range node.Tokens {
		t := &node.Tokens[i]
		if t.Type != lexer.TokenWhitespace && t.Type != lexer.TokenComment {
			return t
		}
	}
	return nil
}

func (p *Parser) getBlockStarter(blockNode *Node) *Node {
	if blockNode.Parent == nil {
		return nil
	}
	siblings := blockNode.Parent.Children
	for i, sibling := range siblings {
		if sibling == blockNode && i > 0 {
			return siblings[i-1]
		}
	}
	return nil
}

// FindProcedureAtLine finds the procedure containing a specific line.
func FindProcedureAtLine(procedures []ProcedureInfo, line int) *ProcedureInfo {
	for i := range procedures {
		proc := &procedures[i]
		if line >= proc.StartLine && line <= proc.EndLine {
			return proc
		}
	}
	return nil
}

// ControlFlowBlock represents a control flow block (IF, WHILE, FOR, etc.).
type ControlFlowBlock struct {
	Kind      string // "IF", "WHILE", "FOR", "BEGINCASE", "TRY"
	StartLine int
	EndLine   int
}

// controlFlowPairs maps block start keywords to their corresponding end keywords.
var controlFlowPairs = map[string]string{
	"IF":        "ENDIF",
	"WHILE":     "ENDWHILE",
	"FOR":       "NEXT",
	"BEGINCASE": "ENDCASE",
	"TRY":       "ENDTRY",
}

// ExtractControlFlowBlocks extracts all control flow blocks from tokens.
// Uses a stack-based approach to match start/end keyword pairs.
func ExtractControlFlowBlocks(tokens []lexer.Token) []ControlFlowBlock {
	var blocks []ControlFlowBlock

	// Stack to track open blocks: each entry is {kind, startLine}
	type stackItem struct {
		kind      string
		startLine int
	}
	var stack []stackItem

	for _, token := range tokens {
		if token.Type != lexer.TokenKeyword {
			continue
		}

		// Normalize: remove leading colon and uppercase
		normalized := strings.ToUpper(strings.TrimPrefix(token.Text, ":"))

		// Check if this is a block start keyword
		if _, isStart := controlFlowPairs[normalized]; isStart {
			stack = append(stack, stackItem{
				kind:      normalized,
				startLine: token.Line,
			})
			continue
		}

		// Check if this is a block end keyword
		for startKw, endKw := range controlFlowPairs {
			if normalized == endKw {
				// Find matching start on stack (search from top)
				for i := len(stack) - 1; i >= 0; i-- {
					if stack[i].kind == startKw {
						// Found match - create block and remove from stack
						blocks = append(blocks, ControlFlowBlock{
							Kind:      stack[i].kind,
							StartLine: stack[i].startLine,
							EndLine:   token.Line,
						})
						// Remove this item from stack
						stack = append(stack[:i], stack[i+1:]...)
						break
					}
				}
				break
			}
		}
	}

	// Any remaining items on stack are unclosed - extend to last token line
	if len(stack) > 0 && len(tokens) > 0 {
		lastLine := tokens[len(tokens)-1].Line
		for _, item := range stack {
			blocks = append(blocks, ControlFlowBlock{
				Kind:      item.kind,
				StartLine: item.startLine,
				EndLine:   lastLine,
			})
		}
	}

	return blocks
}
