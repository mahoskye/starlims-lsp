// Package providers implements LSP feature providers for SSL.
package providers

import (
	"sort"
	"strings"

	"starlims-lsp/internal/lexer"
)

// wrapLongLines is the line-wrap engine (issue #89): a post-format pass that
// re-flows physical lines exceeding opts.MaxLineLength. Operating on whole
// lines — instead of the old token-streaming wrap that could only react at
// the token that overflowed — lets it guarantee: a line stays over-long only
// when no legal break sequence can prevent it.
//
// Break candidates, computed per line:
//   - after a comma (trailing-comma style);
//   - after ':=';
//   - before a binary operator (.AND./.OR./.NOT., arithmetic, compound
//     assignment, '$') — the operator leads its continuation line.
//
// Never: inside '[...]' subscripts (the index binds to its array like a
// member-access chain binds to its receiver), before comparison operators,
// around member-access ':', or inside strings/comments — lines overlapped by
// a multi-line token are left untouched entirely.
//
// Packing is greedy latest-fitting with a no-gain guard: a break is only
// taken when the following segment fits within the limit on its
// continuation line, so over-long atomic tokens stay put
// (fmt.max_line_length). Continuation lines take exactly one extra indent
// level past the original line, except a declaration list's, which align
// with its first item.
func wrapLongLines(text string, opts FormattingOptions) string {
	if opts.MaxLineLength <= 0 {
		return text
	}

	tokens := lexer.NewLexer(text).Tokenize()
	lines := strings.Split(text, "\n")

	frozen := make([]bool, len(lines))
	perLine := make(map[int][]lexer.Token)
	var stack []byte
	stackAtLine := make(map[int]int)  // line index -> open '[' count at first token
	delimsAtLine := make(map[int]int) // line index -> total open delimiters at first token
	firstTokOfLine := make(map[int]lexer.Token)
	lastTokBeforeLine := make(map[int]lexer.Token) // last significant token of any earlier line
	openSubscripts := 0
	var lastTok lexer.Token
	haveLast := false

	for _, t := range tokens {
		if t.Type == lexer.TokenEOF || t.Type == lexer.TokenWhitespace {
			continue
		}
		li := t.Line - 1
		if _, seen := stackAtLine[li]; !seen {
			stackAtLine[li] = openSubscripts
			delimsAtLine[li] = len(stack)
			firstTokOfLine[li] = t
			if haveLast {
				lastTokBeforeLine[li] = lastTok
			}
		}
		// Mirror the stream formatter's lastNonWSToken: an end-of-line
		// comment (single-line, same line as preceding code) rides the
		// pending-comment path there and never becomes the "last token" —
		// the wrap engine's continuation detection must agree or
		// `.OR. /*note;` lines hide the operator from the next line's
		// classification (issue #218 residual). Everything else about the
		// token (freeze, delimiters, per-line list) processes normally.
		isEOLComment := t.Type == lexer.TokenComment && haveLast &&
			lastTok.Line == t.Line && !strings.Contains(t.Text, "\n")
		if !isEOLComment {
			lastTok = t
			haveLast = true
		}
		// A non-whitespace token spanning lines (multi-line string or
		// comment, including reflowed SQL) freezes every line it touches.
		if strings.Contains(t.Text, "\n") {
			n := strings.Count(t.Text, "\n")
			for l := li; l <= li+n && l < len(lines); l++ {
				frozen[l] = true
			}
			continue
		}
		switch t.Text {
		case "(", "{":
			stack = append(stack, t.Text[0])
		case "[":
			if t.Type == lexer.TokenPunctuation {
				stack = append(stack, '[')
				openSubscripts++
			}
		case ")", "}", "]":
			if len(stack) > 0 {
				if stack[len(stack)-1] == '[' {
					openSubscripts--
				}
				stack = stack[:len(stack)-1]
			}
		}
		perLine[li] = append(perLine[li], t)
	}

	var out []string
	for i, line := range lines {
		if frozen[i] || visualWidth(line, opts) <= opts.MaxLineLength {
			out = append(out, line)
			continue
		}
		// A line that is already an expression continuation — it starts
		// inside an open delimiter, leads with a binary operator, or
		// follows a line ending in ':=' or a binary operator — sits at the
		// statement's fixed continuation level (schema
		// continuation_indent: 1). Fresh wrap fragments of such a line
		// stay at ITS indent; indenting one further produced a level the
		// second pass flattened back, oscillating forever (issue #218,
		// 784 corpus files).
		// A line led by a member-access ':' continues a call chain; the
		// stream formatter already indents it as a continuation, so its
		// fragments must stay at its indent too (issue #255).
		isCont := delimsAtLine[i] > 0 ||
			isContinuationOperator(firstTokOfLine[i]) ||
			firstTokOfLine[i].Text == "," ||
			isMemberAccessColon(firstTokOfLine[i]) ||
			lastTokBeforeLine[i].Text == ":=" ||
			lastTokBeforeLine[i].Text == "," ||
			isContinuationOperator(lastTokBeforeLine[i]) ||
			(awaitsOperand(lastTokBeforeLine[i]) && firstTokOfLine[i].Type != lexer.TokenKeyword)
		wrapped := wrapOneLine(line, perLine[i], stackAtLine[i], isCont, opts)
		out = append(out, wrapped...)
	}
	return strings.Join(out, "\n")
}

// wrapOneLine splits a single over-long line at its break candidates.
// inheritedSubscripts is the number of '[' delimiters already open when the
// line starts (a source continuation inside an index expression).
func wrapOneLine(line string, toks []lexer.Token, inheritedSubscripts int, isContinuationLine bool, opts FormattingOptions) []string {
	runes := []rune(line)

	// Candidate rune positions (break BEFORE the token starting at pos),
	// each owned by the innermost bracket opened on this line and still
	// open there (-1: none). A continuation inside such a bracket aligns
	// with the bracket's first item, or — when its items cannot fit at
	// that column — the bracket breaks after itself and its items hang one
	// level in (fmt.indent_style bracket alignment).
	type bracket struct {
		align   int  // rune column of its first item on this line, -1 if none
		hanging bool // items do not fit at align: break after the bracket
		capped  bool // its first item sits past alignCap: items hang one level in
	}
	var brackets []bracket
	var open []int // indexes into brackets
	var cands, owner []int
	subDepth := inheritedSubscripts
	var prev lexer.Token
	havePrev := false
	pendingOpen := -1
	for _, t := range toks {
		if pendingOpen >= 0 && t.Type != lexer.TokenComment {
			ind := len([]rune(leadingIndentString(line)))
			if !isCloseParen(t) && visualWidth(string(runes[ind:t.Column-1]), opts) <= alignCap(opts) {
				brackets[pendingOpen].align = t.Column - 1
			} else if !isCloseParen(t) {
				brackets[pendingOpen].capped = true
			}
			pendingOpen = -1
		}
		if t.Type == lexer.TokenPunctuation {
			switch t.Text {
			case "[":
				subDepth++
			case "]":
				if subDepth > 0 {
					subDepth--
				}
			}
		}
		if havePrev && subDepth == 0 {
			pos := t.Column - 1
			switch {
			// A comment is never a break target: breaking before an
			// end-of-line comment detaches it onto a line of its own, and
			// the next pass re-classifies it (issue #258).
			case prev.Text == "," && t.Text != "," && t.Text != ";" && !isCloseParen(t) &&
				t.Type != lexer.TokenComment,
				prev.Text == ":=" && t.Type != lexer.TokenComment,
				isContinuationOperator(t) && !isUnaryContext(prev, false):
				cands = append(cands, pos)
				o := -1
				if n := len(open); n > 0 {
					o = open[n-1]
				}
				owner = append(owner, o)
			}
		}
		switch {
		case isOpenParen(t):
			brackets = append(brackets, bracket{align: -1})
			open = append(open, len(brackets)-1)
			pendingOpen = len(brackets) - 1
		case isCloseParen(t) && len(open) > 0:
			open = open[:len(open)-1]
		}
		prev = t
		havePrev = true
	}
	if len(cands) == 0 {
		return []string{line}
	}

	indent := leadingIndentString(line)
	contIndent := indent + oneIndentLevel(opts)
	if isContinuationLine {
		contIndent = indent
	} else if len(toks) > 1 && isDeclListKeyword(toks[0]) && toks[1].Type != lexer.TokenComment &&
		toks[1].Line == toks[0].Line {
		// A declaration list's hanging lines align with its first item
		// (wrapped_list_alignment, ssl-style-guide#120); the stream
		// formatter writes the same column on the next pass.
		contIndent = indent + strings.Repeat(" ", toks[1].Column-toks[0].Column)
	}
	alignPrefix := func(col int) string {
		return indent + strings.Repeat(" ", visualWidth(string(runes[len([]rune(indent)):col]), opts))
	}
	hangPrefix := hangFrom(indent, opts)

	// Hanging is decided by packing (below): a bracket hangs only when one
	// of its items still overflows at the aligned column once the line is
	// packed. A hanging bracket breaks after itself — its first item
	// becomes one more candidate, forced — and its items hang one level in.
	baseCands, baseOwner := cands, owner
	withHanging := func() ([]int, []int, map[int]bool) {
		cs := append([]int{}, baseCands...)
		os := append([]int{}, baseOwner...)
		forced := map[int]bool{}
		for bi, b := range brackets {
			if b.hanging {
				forced[b.align] = true
				cs = append(cs, b.align)
				os = append(os, bi)
			}
		}
		order := make([]int, len(cs))
		for i := range order {
			order[i] = i
		}
		sort.SliceStable(order, func(x, y int) bool { return cs[order[x]] < cs[order[y]] })
		var sc, so []int
		for _, i := range order {
			if len(sc) > 0 && sc[len(sc)-1] == cs[i] {
				if forced[cs[i]] {
					so[len(so)-1] = os[i]
				}
				continue
			}
			sc, so = append(sc, cs[i]), append(so, os[i])
		}
		return sc, so, forced
	}

	// pack lays the line out for the current hanging set and reports
	// whether a bracket's item overflowed at its aligned column though it
	// would fit hanging — that bracket then hangs and the line is packed
	// again.
	pack := func() ([]string, bool) {
		cands, owner, forcedAt := withHanging()
		contFor := func(k int) string {
			if o := owner[k]; o >= 0 && brackets[o].capped {
				return hangPrefix
			}
			if o := owner[k]; o >= 0 && brackets[o].align >= 0 {
				if brackets[o].hanging {
					return hangPrefix
				}
				return alignPrefix(brackets[o].align)
			}
			return contIndent
		}
		// Spans between candidates; span 0 includes the line's indent.
		// Each span remembers the whitespace that separated it from the
		// previous span in the source line: when two spans stay on one
		// line they rejoin with exactly those bytes, so a line the wrapper
		// does not break at a candidate keeps the spacing the formatter
		// (or, with commaSpacing / operatorSpacing off, the author) gave it
		// (issue #255).
		bounds := append(append([]int{0}, cands...), len(runes))
		type span struct {
			text  string
			width int
			sep   string // whitespace before this span in the source line
		}
		var spans []span
		prevSep := ""
		for k := 0; k+1 < len(bounds); k++ {
			raw := string(runes[bounds[k]:bounds[k+1]])
			txt := strings.TrimRight(raw, " \t")
			spans = append(spans, span{txt, visualWidth(txt, opts), prevSep})
			prevSep = raw[len(txt):]
		}

		// Greedy packing with no-gain guard: break only when the next span
		// fits on its continuation line; an over-wide span stays glued to
		// the current line (atomic tokens are never moved just to overflow
		// elsewhere).
		var outLines []string
		cur := spans[0].text
		curWidth := spans[0].width
		again := false
		for k, sp := range spans[1:] {
			sepWidth := visualWidth(sp.sep, opts)
			pre := contFor(k)
			preWidth := visualWidth(pre, opts)
			fitsHere := curWidth+sepWidth+sp.width <= opts.MaxLineLength
			fitsCont := preWidth+sp.width <= opts.MaxLineLength
			if !forcedAt[cands[k]] && !fitsHere && !fitsCont {
				if o := owner[k]; o >= 0 && brackets[o].align >= 0 && !brackets[o].hanging &&
					visualWidth(hangPrefix, opts)+sp.width <= opts.MaxLineLength {
					brackets[o].hanging = true
					again = true
				}
			}
			if !forcedAt[cands[k]] && (fitsHere || !fitsCont) {
				// Keep packing — either it fits, or moving it gains nothing
				// (the span overflows a continuation line too).
				cur = cur + sp.sep + sp.text
				curWidth += sepWidth + sp.width
				continue
			}
			outLines = append(outLines, cur)
			cur = pre + sp.text
			curWidth = preWidth + sp.width
		}
		outLines = append(outLines, cur)
		return outLines, again
	}
	for range len(brackets) + 1 {
		out, again := pack()
		if !again {
			return out
		}
	}
	out, _ := pack()
	return out
}

// oneIndentLevel returns one indentation level in the configured style.
func oneIndentLevel(opts FormattingOptions) string {
	if opts.IndentStyle == "space" {
		return strings.Repeat(" ", opts.IndentSize)
	}
	return "\t"
}

// visualWidth counts columns with tabs expanded to opts.IndentSize.
func visualWidth(s string, opts FormattingOptions) int {
	w := 0
	for _, r := range s {
		if r == '\t' {
			w += opts.IndentSize
		} else {
			w++
		}
	}
	return w
}
