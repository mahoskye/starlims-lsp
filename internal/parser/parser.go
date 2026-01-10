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
				})
			}
		}
		p.findProcedures(child, procedures)
	}
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
