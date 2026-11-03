// Syntax tree for the ssl/syntaxTree request: the statement-level shape of
// the parse (parser.Parse), for clients that draw a document's structure.
// Normative behavior: catalog/features/syntax_tree.md.
package providers

import (
	"regexp"
	"strings"
	"unicode/utf8"

	"starlims-lsp/internal/constants"
	"starlims-lsp/internal/lexer"
	"starlims-lsp/internal/parser"
)

// Syntax node kinds (feature.syntax_tree).
const (
	SyntaxBanner      = "banner"      // the comment that opens the file
	SyntaxDocBlock    = "docBlock"    // the comment a procedure's hover reads
	SyntaxComment     = "comment"     // any other standalone comment
	SyntaxBlankLines  = "blankLines"  // a run of whitespace-only lines
	SyntaxStatement   = "statement"   // a statement that is no block
	SyntaxParameters  = "parameters"  // a :PARAMETERS statement
	SyntaxDefault     = "default"     // a :DEFAULT statement
	SyntaxError       = "error"       // code the parser could not read
	SyntaxRegion      = "region"      // /* region X; ... /* endregion;
	SyntaxRegionStart = "regionStart" // a region opener the tree cannot pair
	SyntaxRegionEnd   = "regionEnd"   // a region closer the tree cannot pair
	SyntaxTextRegion  = "textRegion"  // :REGION X; ... :ENDREGION;
	SyntaxText        = "text"        // the raw body of a :REGION block
	SyntaxInlineCode  = "inlineCode"  // :BEGININLINECODE X; ... :ENDINLINECODE;
	SyntaxProcedure   = "procedure"
	SyntaxIf          = "if"
	SyntaxWhile       = "while"
	SyntaxFor         = "for"
	SyntaxCase        = "case"   // :BEGINCASE; ... :ENDCASE;
	SyntaxTry         = "try"    // :TRY; ... :ENDTRY;
	SyntaxBranch      = "branch" // one arm of an if, try or case
)

// blockKinds maps each block-opening keyword to its node kind.
var blockKinds = map[string]string{
	"PROCEDURE":       SyntaxProcedure,
	"IF":              SyntaxIf,
	"WHILE":           SyntaxWhile,
	"FOR":             SyntaxFor,
	"BEGINCASE":       SyntaxCase,
	"TRY":             SyntaxTry,
	"REGION":          SyntaxTextRegion,
	"BEGININLINECODE": SyntaxInlineCode,
}

// SyntaxNode is one node of a document's syntax tree. Positions are 0-based
// lines and rune columns, like every provider Range.
type SyntaxNode struct {
	Kind  string
	Range Range
	// Keyword is the statement's leading keyword, uppercase without its
	// colon ("IF", "DECLARE", "ELSE"), or "" when it starts with none.
	Keyword string
	// Name and NameRange are set for named nodes: a procedure, a region,
	// a :REGION or :BEGININLINECODE block.
	Name      string
	NameRange *Range
	// Header is the statement that opens a block (`:WHILE x;`, the
	// region's opening comment); Closer is the one that closes it, nil
	// when the block is never closed. An if or try has no Header of its
	// own: each branch carries its opening statement.
	Header   *Range
	Closer   *Range
	Children []*SyntaxNode

	// annotation marks a /*@private; or /*@protected; comment, which sits
	// between a docblock and its :PROCEDURE.
	annotation bool
}

var (
	treeRegionStart = regexp.MustCompile(`(?i)^/\*\s*region\b\s*(.*?)(?:\s*;?\s*)?$`)
	treeRegionEnd   = regexp.MustCompile(`(?i)^/\*\s*endregion\b`)
)

// BuildSyntaxTree returns the top-level nodes of the document's syntax
// tree. The nodes follow the statements and blocks of ast (parser.Parse of
// tokens), so a block holds what the parser put in it; text is the source
// the tokens were read from. In order, without overlap, the top-level
// nodes and the whitespace between them make up the whole document, and
// the same holds inside every block (feature.syntax_tree A1).
func BuildSyntaxTree(text string, tokens []lexer.Token, ast *parser.Node) []*SyntaxNode {
	if ast == nil {
		ast = parser.NewParser(tokens).Parse()
	}
	b := &treeBuilder{lines: strings.Split(text, "\n"), unread: unreadTokens(tokens)}
	nodes := b.list(ast.Children, false)
	nodes = wrapRegions(nodes, pairRegions(nodes))

	beforeLine := len(b.lines)
	if strings.HasSuffix(text, "\n") {
		beforeLine-- // the empty remainder after the final newline is no line
	}
	nodes = b.fill(nodes, -1, beforeLine)
	markDocBlocks(nodes)
	if len(nodes) > 0 {
		switch first := nodes[0]; {
		case first.Kind == SyntaxComment && !first.annotation:
			first.Kind = SyntaxBanner
		case first.Kind == SyntaxDocBlock && len(nodes) > 1 && nodes[1].Kind == SyntaxBlankLines:
			first.Kind = SyntaxBanner
		}
	}
	return nodes
}

type treeBuilder struct {
	lines []string
	// unread holds the Offset of each token the statement grammar could
	// not accept (diag.unexpected_token, the formatter's unparsed code).
	unread map[int]bool
}

// unreadTokens returns the offsets of the tokens the statement grammar
// stopped at. A statement that ran out of input is charged to its last
// code token.
func unreadTokens(tokens []lexer.Token) map[int]bool {
	unread := map[int]bool{}
	for _, se := range unparsedStatements(tokens) {
		i := se.Unexpected
		for i > se.Start && (tokens[i].Type == lexer.TokenEOF || tokens[i].Type == lexer.TokenWhitespace) {
			i--
		}
		unread[tokens[i].Offset] = true
	}
	return unread
}

// list converts one parser child list. A statement that opens a block is
// followed in the list by the parser's NodeBlock holding the body; inCase
// is true for the body of a :BEGINCASE, where :CASE and :OTHERWISE open
// its branches.
func (b *treeBuilder) list(children []*parser.Node, inCase bool) []*SyntaxNode {
	var out []*SyntaxNode
	for i := 0; i < len(children); i++ {
		c := children[i]
		if c.Type == parser.NodeBlock {
			out = append(out, b.list(c.Children, false)...)
			continue
		}
		span, ok := statementSpan(c)
		if !ok {
			continue // whitespace only
		}
		kw := leadingKeyword(c)
		var body *parser.Node
		if i+1 < len(children) && children[i+1].Type == parser.NodeBlock {
			body = children[i+1]
		}
		switch {
		case body != nil && isArmKeyword(kw):
			arm := b.branch(c, span, kw, body)
			if !inCase || !constants.IsCaseKeyword(kw) {
				arm.Kind = SyntaxError // an arm outside the block it belongs to
			}
			out = append(out, arm)
			i++
		case body != nil && blockKinds[kw] != "":
			node, used := b.block(children, i, span, kw)
			out = append(out, node)
			i += used - 1
		case constants.IsBlockEndKeyword(kw):
			out = append(out, &SyntaxNode{Kind: SyntaxError, Range: span, Keyword: kw}) // closes nothing
		default:
			out = append(out, b.leaf(c, span, kw))
		}
	}
	return out
}

// block converts the block opened by children[i], whose body is
// children[i+1], and returns it with the number of list entries it used:
// the opener, the body, any further branches, and the closer.
func (b *treeBuilder) block(children []*parser.Node, i int, header Range, kw string) (*SyntaxNode, int) {
	n := &SyntaxNode{Kind: blockKinds[kw], Keyword: kw}
	j := i + 2
	switch kw {
	case "IF", "TRY":
		n.Children = []*SyntaxNode{b.branch(children[i], header, kw, children[i+1])}
		for j+1 < len(children) && children[j+1].Type == parser.NodeBlock {
			span, ok := statementSpan(children[j])
			armKw := leadingKeyword(children[j])
			if !ok || !isArmKeyword(armKw) {
				break
			}
			n.Children = append(n.Children, b.branch(children[j], span, armKw, children[j+1]))
			j += 2
		}
	default:
		n.Header = &header
		n.Children = b.list(children[i+1].Children, kw == "BEGINCASE")
		if kw == "PROCEDURE" || kw == "REGION" || kw == "BEGININLINECODE" {
			n.Name, n.NameRange = headerName(children[i])
		}
	}
	if j < len(children) {
		if span, ok := statementSpan(children[j]); ok && constants.IsBlockEndKeyword(leadingKeyword(children[j])) {
			n.Closer = &span
			j++
		}
	}
	n.Range = Range{Start: header.Start, End: header.End}
	extendTo(n)
	return n, j - i
}

// branch converts one arm: its opening statement and the parser block
// holding its body.
func (b *treeBuilder) branch(stmt *parser.Node, header Range, kw string, body *parser.Node) *SyntaxNode {
	n := &SyntaxNode{Kind: SyntaxBranch, Keyword: kw, Header: &header, Range: header}
	n.Children = b.list(body.Children, false)
	extendTo(n)
	return n
}

// leaf converts a statement that opens no block.
func (b *treeBuilder) leaf(c *parser.Node, span Range, kw string) *SyntaxNode {
	n := &SyntaxNode{Kind: SyntaxStatement, Range: span, Keyword: kw}
	if len(c.Tokens) == 1 {
		switch t := c.Tokens[0]; t.Type {
		case lexer.TokenRegionBody:
			n.Kind = SyntaxText
			return n
		case lexer.TokenComment:
			n.Kind = SyntaxComment
			text := strings.TrimSpace(strings.TrimSuffix(t.Text, ";"))
			if m := treeRegionStart.FindStringSubmatch(text); m != nil {
				name := strings.TrimSpace(strings.TrimSuffix(strings.TrimSpace(m[1]), ";"))
				located := name
				if name == "" {
					name, located = "Region", ""
				}
				col, length := regionNameSpan(t, located)
				n.Kind, n.Name = SyntaxRegionStart, name
				n.NameRange = &Range{
					Start: Position{Line: t.Line - 1, Character: col},
					End:   Position{Line: t.Line - 1, Character: col + length},
				}
			} else if treeRegionEnd.MatchString(text) {
				n.Kind = SyntaxRegionEnd
			} else if _, ok := parser.ParseVisibilityAnnotation(t.Text); ok {
				n.annotation = true
			}
			return n
		}
	}
	switch kw {
	case "PARAMETERS":
		n.Kind = SyntaxParameters
	case "DEFAULT":
		n.Kind = SyntaxDefault
	}
	for _, t := range c.Tokens {
		if b.unread[t.Offset] {
			n.Kind = SyntaxError
			break
		}
	}
	return n
}

// isArmKeyword reports whether kw opens a branch of an enclosing block.
func isArmKeyword(kw string) bool {
	return constants.IsBlockMiddleKeyword(kw) || constants.IsCaseKeyword(kw)
}

// extendTo sets n's range to end at its closer, or, unclosed, at its last
// child.
func extendTo(n *SyntaxNode) {
	if len(n.Children) > 0 {
		if last := n.Children[len(n.Children)-1].Range.End; after(last, n.Range.End) {
			n.Range.End = last
		}
	}
	if n.Closer != nil {
		n.Range.End = n.Closer.End
	}
	if n.Header == nil && len(n.Children) > 0 {
		n.Range.Start = n.Children[0].Range.Start
	}
}

// after reports whether a is later in the document than b.
func after(a, b Position) bool {
	return a.Line > b.Line || (a.Line == b.Line && a.Character > b.Character)
}

// statementSpan returns the range from a statement's first to its last
// token that is not whitespace; ok is false when it holds only whitespace.
func statementSpan(c *parser.Node) (Range, bool) {
	first, last := -1, -1
	for i, t := range c.Tokens {
		if t.Type == lexer.TokenWhitespace || t.Type == lexer.TokenEOF {
			continue
		}
		if first < 0 {
			first = i
		}
		last = i
	}
	if first < 0 {
		return Range{}, false
	}
	start := c.Tokens[first]
	return Range{
		Start: Position{Line: start.Line - 1, Character: start.Column - 1},
		End:   tokenEnd(c.Tokens[last]),
	}, true
}

// tokenEnd returns the position just past t.
func tokenEnd(t lexer.Token) Position {
	if i := strings.LastIndexByte(t.Text, '\n'); i >= 0 {
		return Position{Line: t.Line - 1 + strings.Count(t.Text, "\n"), Character: utf8.RuneCountInString(t.Text[i+1:])}
	}
	return Position{Line: t.Line - 1, Character: t.Column - 1 + utf8.RuneCountInString(t.Text)}
}

// leadingKeyword returns a statement's first code token as a keyword,
// uppercase without its colon, or "" when that token is no keyword.
func leadingKeyword(c *parser.Node) string {
	for _, t := range c.Tokens {
		if t.Type == lexer.TokenWhitespace || t.Type == lexer.TokenComment {
			continue
		}
		if t.Type == lexer.TokenKeyword {
			return strings.ToUpper(strings.TrimPrefix(t.Text, ":"))
		}
		return ""
	}
	return ""
}

// headerName returns the name a block's opening statement gives it: the
// first identifier after its keyword.
func headerName(c *parser.Node) (string, *Range) {
	seenKeyword := false
	for _, t := range c.Tokens {
		switch {
		case t.Type == lexer.TokenKeyword && !seenKeyword:
			seenKeyword = true
		case t.Type == lexer.TokenIdentifier && seenKeyword:
			start := Position{Line: t.Line - 1, Character: t.Column - 1}
			return t.Text, &Range{Start: start, End: tokenEnd(t)}
		case t.Type != lexer.TokenWhitespace && t.Type != lexer.TokenComment && seenKeyword:
			return "", nil
		}
	}
	return "", nil
}

// pairRegions pairs the region markers of the whole tree the way folding
// does (feature.folding A5): in document order, each closer ends the
// innermost open region. It maps each paired marker to its partner.
func pairRegions(nodes []*SyntaxNode) map[*SyntaxNode]*SyntaxNode {
	partner := map[*SyntaxNode]*SyntaxNode{}
	var open []*SyntaxNode
	var walk func([]*SyntaxNode)
	walk = func(list []*SyntaxNode) {
		for _, n := range list {
			switch n.Kind {
			case SyntaxRegionStart:
				open = append(open, n)
			case SyntaxRegionEnd:
				if len(open) > 0 {
					opener := open[len(open)-1]
					open = open[:len(open)-1]
					partner[opener], partner[n] = n, opener
				}
			}
			walk(n.Children)
		}
	}
	walk(nodes)
	return partner
}

// wrapRegions turns each region whose two markers sit in the same list into
// a region node holding the nodes between them. A marker whose partner is
// at another depth, or that has none, stays a regionStart or regionEnd
// leaf: a tree cannot hold a region that crosses a block boundary.
func wrapRegions(list []*SyntaxNode, partner map[*SyntaxNode]*SyntaxNode) []*SyntaxNode {
	inList := map[*SyntaxNode]bool{}
	for _, n := range list {
		inList[n] = true
		n.Children = wrapRegions(n.Children, partner)
	}
	var out []*SyntaxNode
	var open []int // indexes in out of openers whose closer is in this list
	for _, n := range list {
		if n.Kind == SyntaxRegionEnd && len(open) > 0 && partner[n] == out[open[len(open)-1]] {
			at := open[len(open)-1]
			open = open[:len(open)-1]
			opener := out[at]
			region := &SyntaxNode{
				Kind:      SyntaxRegion,
				Range:     Range{Start: opener.Range.Start, End: n.Range.End},
				Name:      opener.Name,
				NameRange: opener.NameRange,
				Header:    &opener.Range,
				Closer:    &n.Range,
				Children:  append([]*SyntaxNode(nil), out[at+1:]...),
			}
			out = append(out[:at], region)
			continue
		}
		if n.Kind == SyntaxRegionStart && inList[partner[n]] {
			open = append(open, len(out))
		}
		out = append(out, n)
	}
	return out
}

// fill adds a blankLines node for each run of whitespace-only lines between
// the nodes of list, counting only lines after afterLine and before
// beforeLine, and does the same inside every node. A branch takes the blank
// lines up to the next node, so the lines between two branches stay in the
// first one and an if or try holds only branches.
func (b *treeBuilder) fill(list []*SyntaxNode, afterLine, beforeLine int) []*SyntaxNode {
	var out []*SyntaxNode
	for i, n := range list {
		out = append(out, b.blankRuns(afterLine, n.Range.Start.Line)...)
		limit := beforeLine
		if i+1 < len(list) {
			limit = list[i+1].Range.Start.Line
		}
		b.fillNode(n, limit)
		out = append(out, n)
		afterLine = n.Range.End.Line
	}
	return append(out, b.blankRuns(afterLine, beforeLine)...)
}

// fillNode fills the inside of n; limit is the line of whatever follows n.
func (b *treeBuilder) fillNode(n *SyntaxNode, limit int) {
	switch {
	case n.Kind == SyntaxBranch:
		n.Children = b.fill(n.Children, n.Header.End.Line, limit)
		extendTo(n)
	case n.Header == nil:
		// An if or try: its branches, then the closer.
		for k, br := range n.Children {
			before := br.Range.End.Line + 1
			switch {
			case k+1 < len(n.Children):
				before = n.Children[k+1].Range.Start.Line
			case n.Closer != nil:
				before = n.Closer.Start.Line
			}
			b.fillNode(br, before)
		}
	case n.Closer != nil:
		n.Children = b.fill(n.Children, n.Header.End.Line, n.Closer.Start.Line)
	default:
		// Unclosed: blank lines after the last child belong to the parent.
		n.Children = b.fill(n.Children, n.Header.End.Line, n.Range.End.Line+1)
	}
}

// blankRuns returns a blankLines node for each run of whitespace-only lines
// strictly between afterLine and beforeLine.
func (b *treeBuilder) blankRuns(afterLine, beforeLine int) []*SyntaxNode {
	var out []*SyntaxNode
	for l := afterLine + 1; l < beforeLine && l < len(b.lines); l++ {
		if strings.TrimSpace(b.lines[l]) != "" {
			continue
		}
		end := l
		for end+1 < beforeLine && end+1 < len(b.lines) && strings.TrimSpace(b.lines[end+1]) == "" {
			end++
		}
		width := utf8.RuneCountInString(strings.TrimSuffix(b.lines[end], "\r"))
		out = append(out, &SyntaxNode{
			Kind:  SyntaxBlankLines,
			Range: Range{Start: Position{Line: l}, End: Position{Line: end, Character: width}},
		})
		l = end
	}
	return out
}

// markDocBlocks marks the comment above each procedure as its doc block:
// the comment its hover reads (blank lines and a visibility annotation may
// sit between them).
func markDocBlocks(list []*SyntaxNode) {
	for i, n := range list {
		markDocBlocks(n.Children)
		if n.Kind != SyntaxProcedure {
			continue
		}
		j := i - 1
		for j >= 0 && (list[j].Kind == SyntaxBlankLines || list[j].annotation) {
			j--
		}
		if j >= 0 && list[j].Kind == SyntaxComment {
			list[j].Kind = SyntaxDocBlock
		}
	}
}
