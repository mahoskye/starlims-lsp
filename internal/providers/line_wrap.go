// Package providers implements LSP feature providers for SSL.
package providers

import (
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
// level past the original line.
func wrapLongLines(text string, opts FormattingOptions) string {
	if opts.MaxLineLength <= 0 {
		return text
	}

	tokens := lexer.NewLexer(text).Tokenize()
	lines := strings.Split(text, "\n")

	frozen := make([]bool, len(lines))
	perLine := make(map[int][]lexer.Token)
	var stack []byte
	stackAtLine := make(map[int]int) // line index -> open '[' count at first token
	openSubscripts := 0

	for _, t := range tokens {
		if t.Type == lexer.TokenEOF || t.Type == lexer.TokenWhitespace {
			continue
		}
		li := t.Line - 1
		if _, seen := stackAtLine[li]; !seen {
			stackAtLine[li] = openSubscripts
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
		wrapped := wrapOneLine(line, perLine[i], stackAtLine[i], opts)
		out = append(out, wrapped...)
	}
	return strings.Join(out, "\n")
}

// wrapOneLine splits a single over-long line at its break candidates.
// inheritedSubscripts is the number of '[' delimiters already open when the
// line starts (a source continuation inside an index expression).
func wrapOneLine(line string, toks []lexer.Token, inheritedSubscripts int, opts FormattingOptions) []string {
	runes := []rune(line)

	// Candidate rune positions (break BEFORE the token starting at pos).
	var cands []int
	subDepth := inheritedSubscripts
	var prev lexer.Token
	havePrev := false
	for _, t := range toks {
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
			case prev.Text == "," && t.Text != "," && t.Text != ";" && !isCloseParen(t):
				cands = append(cands, pos)
			case prev.Text == ":=" && t.Type != lexer.TokenComment:
				cands = append(cands, pos)
			case isContinuationOperator(t) && !isUnaryContext(prev, false):
				cands = append(cands, pos)
			}
		}
		prev = t
		havePrev = true
	}
	if len(cands) == 0 {
		return []string{line}
	}

	indent := leadingIndentString(line)
	contIndent := indent + oneIndentLevel(opts)
	contWidth := visualWidth(contIndent, opts)

	// Spans between candidates; span 0 includes the line's indent.
	bounds := append(append([]int{0}, cands...), len(runes))
	type span struct {
		text  string
		width int
	}
	var spans []span
	for k := 0; k+1 < len(bounds); k++ {
		txt := strings.TrimRight(string(runes[bounds[k]:bounds[k+1]]), " \t")
		spans = append(spans, span{txt, visualWidth(txt, opts)})
	}

	// Greedy packing with no-gain guard: break only when the next span fits
	// on its continuation line; an over-wide span stays glued to the current
	// line (atomic tokens are never moved just to overflow elsewhere).
	var outLines []string
	cur := spans[0].text
	curWidth := spans[0].width
	for _, sp := range spans[1:] {
		fitsHere := curWidth+1+sp.width <= opts.MaxLineLength
		fitsCont := contWidth+sp.width <= opts.MaxLineLength
		if fitsHere || !fitsCont {
			// Keep packing — either it fits, or moving it gains nothing
			// (the span overflows a continuation line too).
			cur = cur + " " + sp.text
			curWidth += 1 + sp.width
			continue
		}
		outLines = append(outLines, cur)
		cur = contIndent + sp.text
		curWidth = contWidth + sp.width
	}
	outLines = append(outLines, cur)
	return outLines
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
