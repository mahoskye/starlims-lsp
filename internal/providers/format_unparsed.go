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
	// joined holds the colon keyword leading each line that stays in the
	// statement of the unparsed code before it, so range widening keeps the
	// line with that code instead of deciding it alone.
	joined map[int]bool
}

// linePiece is the code of a statement between two points where semicolon
// enforcement would split it, as token indices.
type linePiece struct {
	start, end int
	keyword    bool // led by a colon keyword
}

// planUnparsed walks each statement the parser could not accept and splits
// it where semicolon enforcement would: after every line whose last code
// token needsSemicolonAtLineEnd terminates. A line end gets its ';' only
// while the code since the last terminator parses as a statement, so a
// missing ';' is still repaired; once that code does not parse, later line
// ends stay unterminated and the statement stays one parse error. A line
// led by a colon keyword starts afresh after such code when nothing after
// it fails to parse, so the keyword line is decided by its own code, as
// range formatting decides it (issue #355). A piece that does not parse on
// its own is left verbatim when the statement had a terminator withheld,
// and when doing so cannot confuse the block structure: the piece holds no
// colon keyword and its delimiters balance.
func planUnparsed(tokens []lexer.Token) unparsedPlan {
	plan := unparsedPlan{noTerminator: map[int]bool{}, verbatim: map[int]int{}, joined: map[int]bool{}}
	for _, se := range unparsedStatements(tokens) {
		pieces := statementPieces(tokens, se)
		if len(pieces) == 0 {
			continue
		}
		// sourceEnded: the last piece carries the ';' the source wrote.
		sourceEnded := tokens[pieces[len(pieces)-1].end].Text == ";"
		// cleanFrom reports whether every group from piece k on parses, so
		// starting afresh there adds no error.
		cleanFrom := func(k int) bool {
			groupStart := pieces[k].start
			for m := k; m < len(pieces); m++ {
				ok, atEnd := statementParses(tokens, groupStart, pieces[m].end)
				switch {
				case ok && m+1 < len(pieces):
					groupStart = pieces[m+1].start
				case !ok && (!atEnd || m == len(pieces)-1):
					return false
				}
			}
			return true
		}
		groupStart := pieces[0].start
		failed := false // the code since the last terminator can never parse
		withheld := false
		for k, p := range pieces {
			if failed && p.keyword {
				if cleanFrom(k) {
					failed, groupStart = false, p.start
				} else {
					plan.joined[p.start] = true
				}
			}
			last := k == len(pieces)-1
			if last && sourceEnded {
				break
			}
			if !failed {
				ok, atEnd := statementParses(tokens, groupStart, p.end)
				if ok {
					if !last {
						groupStart = pieces[k+1].start
					}
					continue
				}
				failed = !atEnd
			}
			plan.noTerminator[p.end] = true
			withheld = true
		}
		// Only a run the formatter could not delimit is left as written; a
		// statement that is malformed but whole keeps the normalization
		// that often repairs it (`1===1`, `9E + 1`).
		if !withheld {
			continue
		}
		for _, p := range pieces {
			if ok, _ := statementParses(tokens, p.start, p.end); !ok && verbatimSafe(tokens, p.start, p.end) {
				plan.verbatim[p.start] = p.end
			}
		}
	}
	return plan
}

// statementPieces splits a statement where semicolon enforcement would: at
// the first line break after each line whose last code token
// needsSemicolonAtLineEnd terminates. Comments belong to no piece.
func statementPieces(tokens []lexer.Token, se parser.StatementExprs) []linePiece {
	var pieces []linePiece
	start, lastCode := -1, -1
	lineEnd := false // a newline has followed lastCode
	for j := se.Start; j <= se.End && j < len(tokens); j++ {
		t := tokens[j]
		switch t.Type {
		case lexer.TokenEOF, lexer.TokenComment:
			continue
		case lexer.TokenWhitespace:
			if start < 0 || lineEnd || !strings.Contains(t.Text, "\n") {
				continue
			}
			lineEnd = true
			if needsSemicolonAtLineEnd(tokens[lastCode], tokens, j) {
				pieces = append(pieces, newLinePiece(tokens, start, lastCode))
				start = -1
			}
			continue
		}
		if start < 0 {
			start = j
		}
		lastCode = j
		lineEnd = false
	}
	if start >= 0 {
		pieces = append(pieces, newLinePiece(tokens, start, lastCode))
	}
	return pieces
}

func newLinePiece(tokens []lexer.Token, start, end int) linePiece {
	t := tokens[start]
	return linePiece{start: start, end: end,
		keyword: t.Type == lexer.TokenKeyword && strings.HasPrefix(t.Text, ":")}
}

// unparsedStatements returns the statements the parser cannot accept: those
// it parsed with an unexpected token, and those it declines to parse that
// start with a closing delimiter. A stray `}` (C-style code saved as a
// script) starts no statement the parser forms, so it reports none, but
// the formatter would still terminate the line (issue #355).
func unparsedStatements(tokens []lexer.Token) []parser.StatementExprs {
	parsed := map[int]parser.StatementExprs{}
	for _, se := range parser.ExtractStatementExpressions(tokens) {
		parsed[se.Start] = se
	}
	var out []parser.StatementExprs
	for i := 0; i < len(tokens); i++ {
		t := tokens[i]
		if t.Type == lexer.TokenWhitespace || t.Type == lexer.TokenComment ||
			t.Type == lexer.TokenRegionBody || t.Type == lexer.TokenEOF ||
			(t.Type == lexer.TokenPunctuation && t.Text == ";") {
			continue
		}
		end := i
		for end < len(tokens)-1 && !(tokens[end].Type == lexer.TokenPunctuation && tokens[end].Text == ";") {
			end++
		}
		if se, ok := parsed[i]; ok {
			if se.Unexpected >= 0 {
				out = append(out, se)
			}
		} else if isCloseParen(t) {
			out = append(out, parser.StatementExprs{Start: i, End: end, Unexpected: i})
		}
		i = end
	}
	return out
}

// statementParses reports whether tokens[start..end] parse as one complete
// statement, and, when they do not, whether the parser only ran out of
// input (more code could still complete it). A statement cannot start with
// a closing delimiter (issue #355).
func statementParses(tokens []lexer.Token, start, end int) (ok, atEnd bool) {
	if isCloseParen(tokens[start]) {
		return false, false
	}
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
