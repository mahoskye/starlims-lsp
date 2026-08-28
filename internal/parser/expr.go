package parser

// Expression-level AST (issue #184). The statement/block parser in
// parser.go is deliberately untouched: expression trees are built lazily,
// on demand, by the entry points in this file, so the fast structural path
// stays fast. Anything that cannot be resolved statically degrades to
// ExprUnknown silently — consumers must treat ExprUnknown as "no claim",
// never as evidence.
//
// The implementation target is the SSL v11 EBNF grammar
// (ssl-style-guide/ssl-ebnf-grammar.md): precedence .OR. < .AND. <
// equality/containment < relational < shift < additive < multiplicative <
// power (right-associative), with unary operators binding tighter than
// power (`-3 ^ 2` is `(-3) ^ 2`) and member access / calls / subscripts /
// instantiation as postfix operations. The grammar excludes data-source
// preprocessing syntax; data-source headers are not expression territory.

import (
	"strings"

	"starlims-lsp/internal/lexer"
)

// ExprKind discriminates expression node types.
type ExprKind int

const (
	// ExprUnknown marks a region the parser could not resolve. Start/End
	// still bracket the tokens consumed. No other field is meaningful.
	ExprUnknown ExprKind = iota
	// ExprLiteral is a number, string, boolean (.T./.F.), or NIL leaf.
	ExprLiteral
	// ExprIdentifier is a bare variable access, including `Me`.
	ExprIdentifier
	// ExprArrayLiteral is `{a, b, ...}`; Children are the elements.
	ExprArrayLiteral
	// ExprCodeBlock is a `{|params| body}` literal. The lexer captures the
	// whole block as one opaque token; the body is not sub-parsed.
	ExprCodeBlock
	// ExprUnary is `-x`, `!x`, or `.NOT. x`; Op holds the operator text,
	// Children[0] the operand.
	ExprUnary
	// ExprBinary is any infix operation; Op holds the operator text,
	// Children[0]/Children[1] the operands.
	ExprBinary
	// ExprCall is `callee(args)`; Children[0] is the callee (identifier or
	// member access), Children[1:] the arguments.
	ExprCall
	// ExprIndex is `recv[a, b]` or chained `recv[a][b]` (each bracket pair
	// is one node); Children[0] is the receiver, Children[1:] the
	// subscripts.
	ExprIndex
	// ExprMember is `recv:Name` property access (also `Base:Name`); Name
	// holds the member name, Children[0] the receiver.
	ExprMember
	// ExprGroup is a parenthesized expression; Children[0] is the inner
	// expression.
	ExprGroup
	// ExprIncrement is `x++` / `--x` etc.; Op is "++" or "--", Prefix
	// distinguishes the forms, Children[0] is the target.
	ExprIncrement
	// ExprInstantiate is built-in class instantiation `ClassName{args}`;
	// Name holds the class name, Children the arguments.
	ExprInstantiate
	// ExprSkipped is an explicitly skipped argument slot (`f(a,,c)`). It
	// appears only inside ExprCall argument lists.
	ExprSkipped
)

// String returns a short tag for the kind, used by Expr.String.
func (k ExprKind) String() string {
	switch k {
	case ExprLiteral:
		return "lit"
	case ExprIdentifier:
		return "id"
	case ExprArrayLiteral:
		return "array"
	case ExprCodeBlock:
		return "codeblock"
	case ExprUnary:
		return "unary"
	case ExprBinary:
		return "binary"
	case ExprCall:
		return "call"
	case ExprIndex:
		return "index"
	case ExprMember:
		return "member"
	case ExprGroup:
		return "group"
	case ExprIncrement:
		return "incr"
	case ExprInstantiate:
		return "new"
	case ExprSkipped:
		return "skip"
	default:
		return "unknown"
	}
}

// Expr is one node of an expression tree. Start and End are inclusive
// indices into the token slice the tree was parsed from, bracketing every
// token the node covers (whitespace and comments included when interior),
// so consumers can derive source ranges from tokens[Start] / tokens[End].
type Expr struct {
	Kind     ExprKind
	Start    int
	End      int
	Op       string // operator text for ExprUnary/ExprBinary/ExprIncrement
	Name     string // identifier text, member name, class name, literal text
	Prefix   bool   // ExprIncrement: true for ++x / --x
	Children []*Expr
}

// String renders the tree as a compact s-expression — for tests and
// debugging, not user display.
func (e *Expr) String() string {
	if e == nil {
		return "<nil>"
	}
	var b strings.Builder
	e.write(&b)
	return b.String()
}

func (e *Expr) write(b *strings.Builder) {
	switch e.Kind {
	case ExprLiteral, ExprIdentifier:
		b.WriteString(e.Name)
		return
	case ExprSkipped:
		b.WriteString("<skip>")
		return
	case ExprCodeBlock:
		b.WriteString("(codeblock)")
		return
	}
	b.WriteByte('(')
	switch e.Kind {
	case ExprUnary, ExprBinary, ExprIncrement:
		b.WriteString(e.Op)
		if e.Kind == ExprIncrement && e.Prefix {
			b.WriteString(":pre")
		}
	case ExprMember:
		b.WriteString("member ")
		b.WriteString(e.Name)
	case ExprInstantiate:
		b.WriteString("new ")
		b.WriteString(e.Name)
	default:
		b.WriteString(e.Kind.String())
	}
	for _, c := range e.Children {
		b.WriteByte(' ')
		c.write(b)
	}
	b.WriteByte(')')
}

// binaryPrecedence maps infix operator text to its binding strength.
// The C-style `&&` / `||` forms are invalid SSL but lexed for diagnostics;
// they parse at their logical precedence so the tree survives around them.
var binaryPrecedence = map[string]int{
	".OR.": 1, "||": 1,
	".AND.": 2, "&&": 2,
	"=": 3, "==": 3, "!=": 3, "<>": 3, "#": 3, "$": 3,
	"<": 4, ">": 4, "<=": 4, ">=": 4,
	"<<": 5, ">>": 5,
	"+": 6, "-": 6,
	"*": 7, "/": 7, "%": 7,
	"^": 8, "**": 8,
}

const maxExprDepth = 100

// exprParser walks a token slice building expression trees.
type exprParser struct {
	tokens []lexer.Token
	pos    int
	depth  int
}

// ParseExpression parses one expression from tokens starting at index
// `start` (insignificant tokens are skipped). It returns the tree and the
// index of the first token after the expression. When no expression starts
// at `start`, it returns an ExprUnknown covering nothing and next == start.
// The parser never panics and always advances past what it consumed;
// unresolvable regions come back as ExprUnknown subtrees.
func ParseExpression(tokens []lexer.Token, start int) (*Expr, int) {
	p := &exprParser{tokens: tokens, pos: start}
	e := p.parseBinary(1)
	return e, p.pos
}

func (p *exprParser) skipInsignificant() {
	for p.pos < len(p.tokens) {
		switch p.tokens[p.pos].Type {
		case lexer.TokenWhitespace, lexer.TokenComment:
			p.pos++
		default:
			return
		}
	}
}

// peek returns the current significant token, or nil at end of input.
func (p *exprParser) peek() *lexer.Token {
	p.skipInsignificant()
	if p.pos >= len(p.tokens) {
		return nil
	}
	return &p.tokens[p.pos]
}

func (p *exprParser) unknownHere() *Expr {
	at := p.pos
	if at >= len(p.tokens) && at > 0 {
		at = len(p.tokens) - 1
	}
	return &Expr{Kind: ExprUnknown, Start: at, End: at}
}

func (p *exprParser) parseBinary(minPrec int) *Expr {
	if p.depth >= maxExprDepth {
		return p.unknownHere()
	}
	p.depth++
	defer func() { p.depth-- }()

	left := p.parseUnary()
	if left.Kind == ExprUnknown {
		return left
	}
	for {
		tok := p.peek()
		if tok == nil || tok.Type != lexer.TokenOperator {
			return left
		}
		prec, ok := binaryPrecedence[strings.ToUpper(tok.Text)]
		if !ok || prec < minPrec {
			return left
		}
		p.pos++
		// Power is right-associative (2^3^2 = 2^(3^2)); everything else
		// left-associative.
		nextMin := prec + 1
		if prec == binaryPrecedence["^"] {
			nextMin = prec
		}
		right := p.parseBinary(nextMin)
		left = &Expr{
			Kind:     ExprBinary,
			Start:    left.Start,
			End:      right.End,
			Op:       tok.Text,
			Children: []*Expr{left, right},
		}
		if right.Kind == ExprUnknown {
			return left
		}
	}
}

func (p *exprParser) parseUnary() *Expr {
	if p.depth >= maxExprDepth {
		return p.unknownHere()
	}
	p.depth++
	defer func() { p.depth-- }()

	tok := p.peek()
	if tok == nil {
		return p.unknownHere()
	}
	if tok.Type == lexer.TokenOperator {
		switch strings.ToUpper(tok.Text) {
		case "-", "!", ".NOT.":
			start := p.pos
			p.pos++
			operand := p.parseUnary()
			return &Expr{Kind: ExprUnary, Start: start, End: operand.End, Op: tok.Text, Children: []*Expr{operand}}
		case "++", "--":
			start := p.pos
			p.pos++
			operand := p.parsePostfix()
			return &Expr{Kind: ExprIncrement, Start: start, End: operand.End, Op: tok.Text, Prefix: true, Children: []*Expr{operand}}
		}
	}
	return p.parsePostfix()
}

func (p *exprParser) parsePostfix() *Expr {
	e := p.parsePrimary()
	if e.Kind == ExprUnknown {
		return e
	}
	for {
		tok := p.peek()
		if tok == nil {
			return e
		}
		switch {
		case tok.Type == lexer.TokenPunctuation && tok.Text == ":":
			// Member access: `:` must be followed by an identifier.
			save := p.pos
			p.pos++
			name := p.peek()
			if name == nil || name.Type != lexer.TokenIdentifier {
				p.pos = save
				return e
			}
			p.pos++
			e = &Expr{Kind: ExprMember, Start: e.Start, End: p.pos - 1, Name: name.Text, Children: []*Expr{e}}

		case tok.Type == lexer.TokenPunctuation && tok.Text == "(" &&
			(e.Kind == ExprIdentifier || e.Kind == ExprMember):
			args, endIdx, ok := p.parseArgumentList(")")
			if !ok {
				return &Expr{Kind: ExprUnknown, Start: e.Start, End: endIdx}
			}
			e = &Expr{Kind: ExprCall, Start: e.Start, End: endIdx, Children: append([]*Expr{e}, args...)}

		case tok.Type == lexer.TokenPunctuation && tok.Text == "[":
			subs, endIdx, ok := p.parseSubscripts()
			if !ok {
				return &Expr{Kind: ExprUnknown, Start: e.Start, End: endIdx}
			}
			e = &Expr{Kind: ExprIndex, Start: e.Start, End: endIdx, Children: append([]*Expr{e}, subs...)}

		case tok.Type == lexer.TokenPunctuation && tok.Text == "{" && e.Kind == ExprIdentifier:
			// Built-in class instantiation `Email{...}` — identifier
			// receivers only, per the grammar.
			args, endIdx, ok := p.parseArgumentList("}")
			if !ok {
				return &Expr{Kind: ExprUnknown, Start: e.Start, End: endIdx}
			}
			e = &Expr{Kind: ExprInstantiate, Start: e.Start, End: endIdx, Name: e.Name, Children: args}

		case tok.Type == lexer.TokenOperator && (tok.Text == "++" || tok.Text == "--"):
			p.pos++
			e = &Expr{Kind: ExprIncrement, Start: e.Start, End: p.pos - 1, Op: tok.Text, Children: []*Expr{e}}
			return e

		default:
			return e
		}
	}
}

func (p *exprParser) parsePrimary() *Expr {
	tok := p.peek()
	if tok == nil {
		return p.unknownHere()
	}
	at := p.pos
	switch tok.Type {
	case lexer.TokenNumber, lexer.TokenString:
		p.pos++
		return &Expr{Kind: ExprLiteral, Start: at, End: at, Name: tok.Text}
	case lexer.TokenKeyword:
		// The literals `.T.` / `.F.` / `NIL` lex as keywords; every other
		// keyword ends the expression.
		upper := strings.ToUpper(tok.Text)
		if upper == ".T." || upper == ".F." || upper == "NIL" {
			p.pos++
			return &Expr{Kind: ExprLiteral, Start: at, End: at, Name: tok.Text}
		}
		return p.unknownHere()
	case lexer.TokenIdentifier:
		p.pos++
		if strings.EqualFold(tok.Text, "NIL") {
			return &Expr{Kind: ExprLiteral, Start: at, End: at, Name: tok.Text}
		}
		return &Expr{Kind: ExprIdentifier, Start: at, End: at, Name: tok.Text}
	case lexer.TokenCodeBlock:
		p.pos++
		return &Expr{Kind: ExprCodeBlock, Start: at, End: at, Name: tok.Text}
	case lexer.TokenPunctuation:
		switch tok.Text {
		case "(":
			p.pos++
			inner := p.parseBinary(1)
			// Assignment-in-group is an idiomatic SSL expression form
			// (`:WHILE (i += 1) <= nCount;`) even though the canonical
			// grammar keeps assignment a statement; the tree carries it as
			// a binary node with the assignment operator.
			if op := p.peek(); op != nil && op.Type == lexer.TokenOperator && isAssignmentOperator(op.Text) &&
				inner.Kind != ExprUnknown {
				p.pos++
				rhs := p.parseBinary(1)
				inner = &Expr{Kind: ExprBinary, Start: inner.Start, End: rhs.End, Op: op.Text, Children: []*Expr{inner, rhs}}
			}
			if closer := p.peek(); closer != nil && closer.Type == lexer.TokenPunctuation && closer.Text == ")" {
				p.pos++
				return &Expr{Kind: ExprGroup, Start: at, End: p.pos - 1, Children: []*Expr{inner}}
			}
			return &Expr{Kind: ExprUnknown, Start: at, End: p.pos - 1}
		case "{":
			elems, endIdx, ok := p.parseArgumentList("}")
			if !ok {
				return &Expr{Kind: ExprUnknown, Start: at, End: endIdx}
			}
			return &Expr{Kind: ExprArrayLiteral, Start: at, End: endIdx, Children: elems}
		}
	}
	return p.unknownHere()
}

// parseArgumentList consumes an already-verified opener and everything up
// to `close`, returning the element expressions, the index of the closing
// token, and whether the close was found. Empty slots between commas come
// back as ExprSkipped (skipped arguments); an empty list returns no
// elements. On a missing closer it returns ok=false with everything
// consumed so far.
func (p *exprParser) parseArgumentList(close string) ([]*Expr, int, bool) {
	p.skipInsignificant()
	p.pos++ // consume the opener (caller verified it)
	var elems []*Expr
	expectingElem := true
	for {
		tok := p.peek()
		if tok == nil {
			return elems, p.pos - 1, false
		}
		if tok.Type == lexer.TokenPunctuation {
			switch tok.Text {
			case close:
				// `f(a,)`: a trailing comma leaves expectingElem with at
				// least one element already parsed — that final slot is a
				// skipped argument.
				if expectingElem && len(elems) > 0 {
					elems = append(elems, &Expr{Kind: ExprSkipped, Start: p.pos, End: p.pos})
				}
				end := p.pos
				p.pos++
				return elems, end, true
			case ",":
				if expectingElem {
					elems = append(elems, &Expr{Kind: ExprSkipped, Start: p.pos, End: p.pos})
				}
				p.pos++
				expectingElem = true
				continue
			}
		}
		if !expectingElem {
			// Two element expressions with no comma between them: broken
			// list, bail without the closer.
			return elems, p.pos, false
		}
		elem := p.parseBinary(1)
		elems = append(elems, elem)
		expectingElem = false
		if elem.Kind == ExprUnknown {
			// The element failed to parse; without progress this loop
			// cannot terminate.
			return elems, p.pos, false
		}
	}
}

// parseSubscripts consumes `[ expr {, expr} ]` and returns the subscript
// expressions, the closing bracket's index, and whether it closed.
func (p *exprParser) parseSubscripts() ([]*Expr, int, bool) {
	p.skipInsignificant()
	p.pos++ // consume '['
	var subs []*Expr
	for {
		sub := p.parseBinary(1)
		subs = append(subs, sub)
		if sub.Kind == ExprUnknown {
			return subs, p.pos, false
		}
		tok := p.peek()
		if tok == nil {
			return subs, p.pos - 1, false
		}
		if tok.Type == lexer.TokenPunctuation {
			switch tok.Text {
			case "]":
				end := p.pos
				p.pos++
				return subs, end, true
			case ",":
				p.pos++
				continue
			}
		}
		return subs, p.pos, false
	}
}
