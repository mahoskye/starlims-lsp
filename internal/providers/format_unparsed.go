package providers

import (
	"strings"

	"starlims-lsp/internal/lexer"
	"starlims-lsp/internal/parser"
)

// unparsedPlan records how the formatter treats code the statement grammar
// cannot parse (issue #351). Semicolon enforcement splits a statement at
// each line end needsSemicolonAtLineEnd accepts. That repairs a statement
// missing its ';', but raw SQL written outside a string, say, is several
// lines no split can make parse: each line became a statement of its own,
// so one parse error became several. The plan is computed from the tokens
// alone, so the formatter, the range widening and the post-format passes
// all reach the same decision.
type unparsedPlan struct {
	// noTerminator holds the index of each code token that ends a line the
	// formatter would terminate, where the ';' must not be added.
	noTerminator map[int]bool
	// verbatim maps the first code token of a line run that does not parse
	// to the run's last code token. The formatter writes such a run as the
	// source has it: no re-indent, no respacing, no wrapping, no casing.
	verbatim map[int]int
}

// planUnparsed walks each statement the parser could not accept and splits
// it where semicolon enforcement would: after every line whose last code
// token needsSemicolonAtLineEnd terminates. A line end gets its ';' only
// while the code since the last terminator parses as a statement, so a
// missing ';' is still repaired; once that code does not parse, the rest
// of the statement keeps its source terminators, and the statement stays
// one parse error. A piece between split points that does not parse on its
// own is left verbatim when the statement had a terminator withheld, and
// when doing so cannot confuse the block structure: the piece holds no
// colon keyword and its delimiters balance.
func planUnparsed(tokens []lexer.Token) unparsedPlan {
	plan := unparsedPlan{noTerminator: map[int]bool{}, verbatim: map[int]int{}}
	for _, se := range parser.ExtractStatementExpressions(tokens) {
		if se.Unexpected < 0 {
			continue
		}
		groupStart, pieceStart, lastCode := -1, -1, -1
		failed := false  // the code since the last terminator can never parse
		lineEnd := false // a newline has followed lastCode
		withheld := false
		var unparsedPieces [][2]int
		closePiece := func() {
			if pieceStart >= 0 && lastCode >= pieceStart {
				if ok, _ := statementParses(tokens, pieceStart, lastCode); !ok && verbatimSafe(tokens, pieceStart, lastCode) {
					unparsedPieces = append(unparsedPieces, [2]int{pieceStart, lastCode})
				}
			}
			pieceStart = -1
		}
		// terminates reports whether the formatter may end the group at
		// lastCode, latching failed once no later code can make it parse.
		terminates := func() bool {
			if failed {
				return false
			}
			ok, atEnd := statementParses(tokens, groupStart, lastCode)
			if !ok && !atEnd {
				failed = true
			}
			return ok
		}
		for j := se.Start; j <= se.End && j < len(tokens); j++ {
			t := tokens[j]
			switch t.Type {
			case lexer.TokenEOF, lexer.TokenComment:
				continue
			case lexer.TokenWhitespace:
				if lastCode < 0 || lineEnd || !strings.Contains(t.Text, "\n") {
					continue
				}
				lineEnd = true
				if !needsSemicolonAtLineEnd(tokens[lastCode], tokens, j) {
					continue
				}
				closePiece()
				if terminates() {
					groupStart = -1
				} else {
					plan.noTerminator[lastCode] = true
					withheld = true
				}
				continue
			}
			if groupStart < 0 {
				groupStart = j
			}
			if pieceStart < 0 {
				pieceStart = j
			}
			lastCode = j
			lineEnd = false
		}
		closePiece()
		// A statement running to end of file gets the ';' the formatter
		// adds there only when it parses (formatTokens' EOF check).
		if lastCode >= 0 && groupStart >= 0 && tokens[lastCode].Text != ";" && !terminates() {
			plan.noTerminator[lastCode] = true
			withheld = true
		}
		// Only a run the formatter could not delimit is left as written; a
		// statement that is malformed but whole keeps the normalization
		// that often repairs it (`1===1`, `9E + 1`).
		if withheld {
			for _, p := range unparsedPieces {
				plan.verbatim[p[0]] = p[1]
			}
		}
	}
	return plan
}

// statementParses reports whether tokens[start..end] parse as one complete
// statement, and, when they do not, whether the parser only ran out of
// input (more code could still complete it).
func statementParses(tokens []lexer.Token, start, end int) (ok, atEnd bool) {
	sub := make([]lexer.Token, 0, end-start+2)
	sub = append(sub, tokens[start:end+1]...)
	sub = append(sub, lexer.Token{Type: lexer.TokenEOF})
	for _, se := range parser.ExtractStatementExpressions(sub) {
		if se.Unexpected >= 0 {
			return false, sub[se.Unexpected].Type == lexer.TokenEOF
		}
	}
	return true, false
}

// verbatimSafe reports whether tokens[start..end] can be written as the
// source has them without the formatter losing track of the code around
// them: a colon keyword (block structure, indentation) or a region body
// must go through the formatter, and an unbalanced delimiter would leave
// its depth wrong for the lines after.
func verbatimSafe(tokens []lexer.Token, start, end int) bool {
	depth := 0
	for j := start; j <= end; j++ {
		t := tokens[j]
		switch {
		case t.Type == lexer.TokenKeyword && strings.HasPrefix(t.Text, ":"),
			t.Type == lexer.TokenRegionBody:
			return false
		case isOpenParen(t):
			depth++
		case isCloseParen(t):
			if depth == 0 {
				return false
			}
			depth--
		}
	}
	return depth == 0
}

// verbatimLines marks the 0-based lines a verbatim run of the plan covers,
// for the text-level passes. A run's first line counts only when the run
// leads it: code before it on that line is formatted as usual.
func (p unparsedPlan) verbatimLines(tokens []lexer.Token, nLines int) []bool {
	mask := make([]bool, nLines)
	for start, end := range p.verbatim {
		first := tokens[start].Line - 1
		if _, leads := sourceIndent(tokens, start); !leads {
			first++
		}
		last := tokens[end].Line - 1 + strings.Count(tokens[end].Text, "\n")
		for l := first; l <= last && l < nLines; l++ {
			if l >= 0 {
				mask[l] = true
			}
		}
	}
	return mask
}

// sourceIndent is the indentation the source gives the line tokens[i]
// starts, when tokens[i] is the first token on its line; ok is false when
// other code precedes it there.
func sourceIndent(tokens []lexer.Token, i int) (indent string, ok bool) {
	for j := i - 1; j >= 0; j-- {
		t := tokens[j]
		if k := strings.LastIndex(t.Text, "\n"); k >= 0 {
			if t.Type != lexer.TokenWhitespace && k != len(t.Text)-1 {
				return "", false // a multi-line token ends on this line
			}
			return t.Text[k+1:] + indent, true
		}
		if t.Type != lexer.TokenWhitespace {
			return "", false
		}
		if t.Text != "\uFEFF" { // the byte-order mark is written apart
			indent = t.Text + indent
		}
	}
	return indent, true
}

// writeVerbatim writes tokens[start..end] as the source has them, then
// leaves the state as finalizeToken would after tokens[end].
func (s *formatState) writeVerbatim(tokens []lexer.Token, start, end int) {
	first := tokens[start]
	s.flushPendingStatementBreak(first)
	if s.lineStart {
		if indent, ok := sourceIndent(tokens, start); ok {
			s.writeLinePrefix(indent)
			s.stmtIndentWidth = s.currentLineLen
		} else {
			s.writeIndentIfNeeded(first)
		}
	}
	var raw strings.Builder
	for j := start; j <= end; j++ {
		raw.WriteString(tokens[j].Text)
	}
	text := raw.String()
	s.builder.WriteString(text)
	if k := strings.LastIndex(text, "\n"); k >= 0 {
		s.currentLineLen = visualWidth(text[k+1:], s.opts)
	} else {
		s.currentLineLen += visualWidth(text, s.opts)
	}
	s.lineStart = false
	s.declPending = false
	s.upperNextIdent = false
	s.orphanColon = false
	s.commentBlockStart = -1
	for k := range s.delims {
		s.delims[k].pending = false
	}
	last := tokens[end]
	if last.Text == ";" {
		s.pendingStatementBreak = true
		s.prevKeyword = ""
		s.declListAlign = 0
	}
	s.prevToken = last
	s.beforeLastNonWS = s.lastNonWSToken
	for j := end - 1; j >= start; j-- {
		if t := tokens[j]; t.Type != lexer.TokenWhitespace && t.Type != lexer.TokenComment {
			s.beforeLastNonWS = t
			break
		}
	}
	s.lastNonWSToken = last
	s.lastNonWSIndex = end
}
