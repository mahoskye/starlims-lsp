package providers

import (
	"fmt"
	"strings"

	"starlims-lsp/internal/constants"
	"starlims-lsp/internal/lexer"
	"starlims-lsp/internal/parser"
)

// checkUnexpectedTokens reports the first token in each statement that the
// statement grammar could not accept (catalog: diag.unexpected_token,
// issue #240). The parser already computes the position; this check owns
// the wording and the one-finding-per-cause deferrals to the lexer- and
// operator-level rules that already name the same token.
func checkUnexpectedTokens(tokens []lexer.Token, stmts []parser.StatementExprs) []Diagnostic {
	var diagnostics []Diagnostic
	reported := map[int]bool{}
	for i := range stmts {
		se := &stmts[i]
		idx := se.Unexpected
		if idx < 0 || idx >= len(tokens) {
			continue
		}
		if deferredToOtherRule(tokens, se, idx) {
			continue
		}
		tok := tokens[idx]

		expected := se.Expected
		if expected == "" {
			expected = expectedAt(tokens, se, idx)
		}
		msg := "Unexpected " + describeToken(tok)
		if se.Kind == parser.StmtFor {
			msg += " in ':FOR' header"
		}
		msg += " - expected " + expected
		if se.Kind == parser.StmtFor && expected == "':='" && len(se.Exprs) > 0 &&
			strings.EqualFold(se.Exprs[0].Name, "EACH") {
			msg += " (SSL has no ':FOR EACH' form)"
		}
		if terminatorAcceptable(expected) && lineLeading(tokens, idx) {
			msg += " (is the previous statement missing its ';'?)"
		}
		if se.Kind == parser.StmtFor && len(se.Exprs) == 3 && endsInStepMember(se.Exprs[2]) {
			// `:TO nMax:STEP 2` — with no space, `:STEP` reads as a member
			// of the limit expression (diag.step_spacing names the layout
			// rule; a numeric limit still lexes the keyword and is that
			// rule's to report).
			msg += " (missing space before ':STEP'?)"
		}

		reported[idx] = true
		r := tokenToRange(tok)
		if tok.Type == lexer.TokenEOF {
			r.End.Character = r.Start.Character + 1
		}
		diagnostics = append(diagnostics, Diagnostic{
			Severity: SeverityError,
			Range:    r,
			Message:  msg,
			Source:   "ssl-lsp",
			Code:     CodeUnexpectedToken,
		})
	}
	diagnostics = append(diagnostics, declarationListKeywords(tokens, reported)...)
	diagnostics = append(diagnostics, bareKeywordTails(tokens, reported)...)
	return append(diagnostics, orphanColons(tokens, reported)...)
}

// declarationKeywords take a bare name list: `:DECLARE a, b;`. The
// statement extractor carries no expressions for them, so the parser
// never judges their content.
var declarationKeywords = map[string]bool{"DECLARE": true, "PARAMETERS": true, "PUBLIC": true}

// declarationListKeywords reports the first keyword inside a declaration
// list — `:PARAMETERS sIn :DEFAULT sIn, "";` — which can only mean the
// list is missing its `;` (issue #323). A :DEFAULT inside a :DECLARE list
// is default_on_declare_line's finding, named there with its fix.
func declarationListKeywords(tokens []lexer.Token, reported map[int]bool) []Diagnostic {
	var out []Diagnostic
	for i := 0; i < len(tokens); i++ {
		t := tokens[i]
		if t.Type != lexer.TokenKeyword {
			continue
		}
		head := strings.ToUpper(strings.TrimPrefix(t.Text, ":"))
		if !declarationKeywords[head] {
			continue
		}
		for j := i + 1; j < len(tokens); j++ {
			tok := tokens[j]
			if tok.Type == lexer.TokenEOF || (tok.Type == lexer.TokenPunctuation && tok.Text == ";") {
				break
			}
			if tok.Type != lexer.TokenKeyword {
				continue
			}
			kw := strings.ToUpper(strings.TrimPrefix(tok.Text, ":"))
			if !reported[j] && !(head == "DECLARE" && kw == "DEFAULT") {
				reported[j] = true
				msg := fmt.Sprintf("Unexpected %s - expected ',' or ';' (is the ':%s' list missing its ';'?)", describeToken(tok), head)
				out = append(out, Diagnostic{
					Severity: SeverityError,
					Range:    tokenToRange(tok),
					Message:  msg,
					Source:   "ssl-lsp",
					Code:     CodeUnexpectedToken,
				})
			}
			// The keyword starts whatever follows; resume the outer scan
			// there so a :DECLARE after it is judged on its own.
			i = j - 1
			break
		}
	}
	return out
}

// bareKeywords are the keyword statements that are complete at the
// keyword: the grammar gives each as the keyword and its `;`
// (`EndIfStatement ::= ":" "ENDIF" ";"`). `:CATCH` is one too, and
// catch_clause_form reports what follows it.
var bareKeywords = map[string]bool{
	"ELSE": true, "ENDIF": true, "ENDWHILE": true, "EXITWHILE": true,
	"NEXT": true, "EXITFOR": true, "LOOP": true, "BEGINCASE": true,
	"OTHERWISE": true, "ENDCASE": true, "EXITCASE": true, "TRY": true,
	"FINALLY": true, "ENDTRY": true, "ENDPROC": true, "ERROR": true,
	"RESUME": true, "ENDREGION": true, "ENDINLINECODE": true,
}

// bareKeywordTails reports the first token that cannot stand where it does
// in a keyword statement the parser does not model. The statement extractor
// carries no expression for these statements, so the parser never judges
// what follows the keyword, and such code validated clean:
//
//   - a keyword that takes nothing, where only its `;` can follow
//     (`:ENDIF 1;`, `:ELSE nX := 1;`, an `:ENDIF` whose `;` is missing
//     before the next statement);
//   - a `:PROCEDURE` header, which is the keyword, one name and the `;`;
//   - a `:DECLARE`, `:PARAMETERS` or `:PUBLIC` list, which is names
//     separated by commas (`:DECLARE 1;`, `:DECLARE nA nB;`, `:PUBLIC;`);
//   - `:CLASS`, with an optional name; `:REGION`, with one name; and
//     `:INHERIT` and `:INCLUDE`, each with one name that may be dotted.
//     Here only a missing name and a second word are reported.
//
// A statement that ends at end of file is not reported, as for any other
// statement. What another rule already names stays with it:
// `:PROCEDURE Name(` (procedure_declaration_syntax), `:PARAMETERS;`
// (empty_parameters), a `:=` in a list (declare_initializer,
// inline_default_outside_datasource), a keyword in a list
// (declarationListKeywords above), and a quoted `:INCLUDE` target
// (include_quoted_target). A quoted `:INHERIT` parent is not reported at
// all.
func bareKeywordTails(tokens []lexer.Token, reported map[int]bool) []Diagnostic {
	var out []Diagnostic

	// One finding per statement, as for the statements the parser judges:
	// a statement is the tokens up to a ';', and one that already has an
	// unexpected token gets no second one here.
	statement := make([]int, len(tokens))
	for i, id := 0, 0; i < len(tokens); i++ {
		statement[i] = id
		if tokens[i].Type == lexer.TokenPunctuation && tokens[i].Text == ";" {
			id++
		}
	}
	judged := map[int]bool{}
	for idx := range reported {
		judged[statement[idx]] = true
	}

	report := func(idx int, expected, hint string) {
		tok := tokens[idx]
		// An unknown token is unknown_token's finding, and one finding
		// per cause is enough.
		if judged[statement[idx]] || tok.Type == lexer.TokenUnknown {
			return
		}
		judged[statement[idx]] = true
		reported[idx] = true
		msg := "Unexpected " + describeToken(tok) + " - expected " + expected
		if hint != "" && lineLeading(tokens, idx) {
			msg += " " + hint
		}
		out = append(out, Diagnostic{
			Severity: SeverityError,
			Range:    tokenToRange(tok),
			Message:  msg,
			Source:   "ssl-lsp",
			Code:     CodeUnexpectedToken,
		})
	}
	// following returns the index of the first token after i that is
	// neither whitespace nor a comment, or -1 at end of file.
	following := func(i int) int {
		next := nextSignificantTokenIndex(tokens, i+1)
		if next < 0 || tokens[next].Type == lexer.TokenEOF {
			return -1
		}
		return next
	}

	isColonKeyword := func(idx int) bool {
		return tokens[idx].Type == lexer.TokenKeyword && strings.HasPrefix(tokens[idx].Text, ":")
	}
	// oneName judges a statement that is the keyword, one name and the
	// `;`. The name is the identifier and whatever is written against it
	// with no space: the lexer gives the rest of a dotted name
	// (`Category.Level2Base`) as several tokens, and production code has
	// names this check cannot vouch for (`:CLASS Name[];`), so only a
	// second word, set apart by whitespace, is reported. what names the
	// name for the message.
	oneName := func(i int, head, what string, optional bool) {
		next := following(i)
		if next < 0 || (optional && tokens[next].Text == ";") {
			return
		}
		if tokens[next].Type != lexer.TokenIdentifier {
			expected := fmt.Sprintf("%s after ':%s'", what, head)
			if optional {
				expected = fmt.Sprintf("%s or ';' after ':%s'", what, head)
			}
			report(next, expected, "")
			return
		}
		last := next
		for last+1 < len(tokens) {
			t := tokens[last+1]
			if t.Type == lexer.TokenWhitespace || t.Type == lexer.TokenComment ||
				t.Type == lexer.TokenEOF || t.Text == ";" {
				break
			}
			last++
		}
		after := following(last)
		if after < 0 || tokens[after].Text == ";" {
			return
		}
		report(after, fmt.Sprintf("';' after the name in ':%s'", head),
			fmt.Sprintf("(is the ':%s' line missing its ';'?)", head))
	}
	// nameList judges a declaration list: one or more names separated by
	// commas.
	nameList := func(i int, head string) {
		item := following(i)
		if item < 0 {
			return
		}
		if tokens[item].Text == ";" {
			if head != "PARAMETERS" { // empty_parameters reports that one
				report(item, fmt.Sprintf("a name after ':%s'", head), "")
			}
			return
		}
		after := fmt.Sprintf("':%s'", head)
		for {
			if tokens[item].Type != lexer.TokenIdentifier {
				if !isColonKeyword(item) { // a keyword is the list missing its ';'
					report(item, "a name after "+after, "")
				}
				return
			}
			sep := following(item)
			if sep < 0 || tokens[sep].Text == ";" || tokens[sep].Text == ":=" || isColonKeyword(sep) {
				return
			}
			if tokens[sep].Text != "," {
				report(sep, "',' or ';'", fmt.Sprintf("(is the ':%s' list missing its ';'?)", head))
				return
			}
			if item = following(sep); item < 0 {
				return
			}
			after = "','"
		}
	}

	for i, t := range tokens {
		if !isColonKeyword(i) {
			continue
		}
		head := strings.ToUpper(t.Text[1:])
		next := following(i)
		if next < 0 {
			continue
		}
		switch {
		case bareKeywords[head]:
			if tokens[next].Text != ";" {
				report(next, fmt.Sprintf("';' after ':%s'", head),
					fmt.Sprintf("(is ':%s' missing its ';'?)", head))
			}
		case head == "PROCEDURE":
			if tokens[next].Type != lexer.TokenIdentifier {
				report(next, "a procedure name after ':PROCEDURE'", "")
				continue
			}
			after := following(next)
			if after < 0 || tokens[after].Text == ";" || tokens[after].Text == "(" {
				continue
			}
			report(after, "';' after the procedure name",
				"(is the ':PROCEDURE' line missing its ';'?)")
		case declarationKeywords[head]:
			nameList(i, head)
		case head == "CLASS":
			oneName(i, head, "a class name", true)
		case head == "REGION":
			oneName(i, head, "a region name", false)
		case head == "INHERIT":
			// A quoted parent is left alone: what the compiler makes of it
			// is not documented (scenario classes/inherit_quoted_name).
			if tokens[next].Type != lexer.TokenString {
				oneName(i, head, "a class name", false)
			}
		case head == "INCLUDE":
			if tokens[next].Type != lexer.TokenString { // include_quoted_target's
				oneName(i, head, "a script name", false)
			}
		}
	}
	return out
}

// orphanColons reports a ':' that starts a statement with nothing to be the
// member-access receiver before it — `:;`, `: IF x;`, a trailing `:`. The
// parser forms no statement around it, so no other finding names it
// (issue #328).
func orphanColons(tokens []lexer.Token, reported map[int]bool) []Diagnostic {
	var out []Diagnostic
	var prev *lexer.Token
	for i := range tokens {
		t := tokens[i]
		if t.Type == lexer.TokenWhitespace || t.Type == lexer.TokenEOF {
			continue
		}
		if t.Type == lexer.TokenPunctuation && t.Text == ":" && !reported[i] &&
			(prev == nil || prev.Text == ";" || prev.Type == lexer.TokenComment) {
			msg := "Unexpected ':' - a colon starts a keyword (written with no space, ':IF') or follows the object in a member access"
			if next := nextSignificant(tokens, i); next != nil && next.Type == lexer.TokenIdentifier && next.Line == t.Line &&
				isKeywordName(next.Text) {
				msg += fmt.Sprintf(" (remove the space: ':%s')", strings.ToUpper(next.Text))
			}
			out = append(out, Diagnostic{
				Severity: SeverityError,
				Range:    tokenToRange(t),
				Message:  msg,
				Source:   "ssl-lsp",
				Code:     CodeUnexpectedToken,
			})
		}
		if t.Type != lexer.TokenComment || prev == nil || prev.Line != t.Line {
			prev = &tokens[i]
		}
	}
	return out
}

// nextSignificant returns the first token after i that is neither
// whitespace nor a comment, or nil.
func nextSignificant(tokens []lexer.Token, i int) *lexer.Token {
	for j := i + 1; j < len(tokens); j++ {
		if tokens[j].Type == lexer.TokenEOF {
			return nil
		}
		if tokens[j].Type != lexer.TokenWhitespace && tokens[j].Type != lexer.TokenComment {
			return &tokens[j]
		}
	}
	return nil
}

// isKeywordName reports whether word, without its colon, is an SSL keyword.
func isKeywordName(word string) bool {
	return constants.IsKeyword(strings.ToUpper(word))
}

// describeToken renders a token for the message as `<class> '<text>'`,
// with the class dropped for punctuation (`';'`) and a plain phrase for
// end of input.
func describeToken(tok lexer.Token) string {
	text := tok.Text
	if r := []rune(text); len(r) > 40 {
		text = string(r[:40]) + "…"
	}
	switch tok.Type {
	case lexer.TokenIdentifier:
		return "identifier '" + text + "'"
	case lexer.TokenKeyword:
		return "keyword '" + text + "'"
	case lexer.TokenNumber:
		return "number '" + text + "'"
	case lexer.TokenString:
		return "string " + text
	case lexer.TokenOperator:
		return "operator '" + text + "'"
	case lexer.TokenCodeBlock:
		return "code block"
	case lexer.TokenEOF:
		return "end of file"
	default:
		return "'" + text + "'"
	}
}

// expectedAt words the expectation for an unexpected token the parser
// left unlabelled, from its surroundings inside the statement: inside an
// open delimiter the list wanted a separator or its closer; after an
// operator, a separator, or an opener an operand was due; after a finished
// operand only an operator or the terminator may follow.
func expectedAt(tokens []lexer.Token, se *parser.StatementExprs, idx int) string {
	prev := previousSignificantTokenIndex(tokens, idx-1)
	if prev < se.Start {
		return "a statement"
	}
	p := tokens[prev]
	switch {
	case p.Type == lexer.TokenOperator:
		return "an expression"
	case p.Type == lexer.TokenPunctuation && (p.Text == "," || closerFor[p.Text] != ""):
		return "an expression"
	}
	if opener := innermostOpener(tokens, se.Start, idx); opener != "" {
		return fmt.Sprintf("',' or '%s'", closerFor[opener])
	}
	if tokens[idx].Type == lexer.TokenOperator {
		// The token is an operator the grammar cannot place here — an
		// assignment inside a condition, say — so "an operator" would
		// read as a contradiction.
		return "a binary operator or ';'"
	}
	return "an operator or ';'"
}

var closerFor = map[string]string{"(": ")", "[": "]", "{": "}"}

// innermostOpener returns the delimiter still open at `idx` when scanning
// the statement from `start`, or "" when the position is at depth zero.
func innermostOpener(tokens []lexer.Token, start, idx int) string {
	var stack []string
	for j := start; j < idx && j < len(tokens); j++ {
		t := tokens[j]
		if t.Type != lexer.TokenPunctuation {
			continue
		}
		if closerFor[t.Text] != "" {
			stack = append(stack, t.Text)
		} else if len(stack) > 0 && closerFor[stack[len(stack)-1]] == t.Text {
			stack = stack[:len(stack)-1]
		}
	}
	if len(stack) == 0 {
		return ""
	}
	return stack[len(stack)-1]
}

// terminatorAcceptable reports whether a `;` would have satisfied the
// expectation — the case where a missing terminator on the previous line
// is the likely cause when the unexpected token leads its line.
func terminatorAcceptable(expected string) bool {
	return strings.HasSuffix(expected, "';'")
}

// lineLeading reports whether idx is the first significant token on its
// line: a line break sits between it and the previous significant token.
// Token line numbers alone would misread a multi-line string token ending
// just before idx as a line break.
func lineLeading(tokens []lexer.Token, idx int) bool {
	prev := previousSignificantTokenIndex(tokens, idx-1)
	if prev < 0 {
		return true
	}
	for j := prev + 1; j < idx; j++ {
		if tokens[j].Type == lexer.TokenWhitespace && strings.Contains(tokens[j].Text, "\n") {
			return true
		}
	}
	return false
}

// deferredToOtherRule reports whether another rule already names this
// token, so reporting it here would be a second finding for one cause.
// Each case mirrors the detection of the rule it defers to.
func deferredToOtherRule(tokens []lexer.Token, se *parser.StatementExprs, idx int) bool {
	tok := tokens[idx]

	// unknown_token (and its dot_property_access carve-out) report every
	// token the lexer could not classify.
	if tok.Type == lexer.TokenUnknown {
		return true
	}

	// bare_logical_operator, prefix form: in `:IF Not bA;` the parser reads
	// `Not` as an operand and stops on `bA`, but the cause is the bare
	// `Not` that rule already reports (issue #255).
	if prev := previousSignificantTokenIndex(tokens, idx-1); prev >= 0 &&
		tokens[prev].Type == lexer.TokenIdentifier && strings.EqualFold(tokens[prev].Text, "NOT") &&
		bareLogicalOperatorPosition(tokens, prev) {
		return true
	}

	// unterminated_string: a string with no closer runs to end of file, so
	// whatever stands around it is a symptom; that rule names the opener
	// (issue #255).
	if tok.Type == lexer.TokenString && tok.Unterminated {
		return true
	}

	// unclosed_delimiter: an opener never closed swallows the rest of the
	// file into this statement, so whatever the parser stopped on is a
	// symptom of that.
	if unclosedOpenerIn(tokens, se.Start, se.End) {
		return true
	}

	if tok.Type == lexer.TokenOperator {
		// invalid_operator_sequence: C-style operators, and the glued
		// `!==` / `===` pairs whose second half is what the parser trips on.
		switch tok.Text {
		case "&&", "||", "&", "|":
			return true
		}
		if idx > 0 && tokens[idx-1].Type == lexer.TokenOperator &&
			tokens[idx-1].Offset+len(tokens[idx-1].Text) == tok.Offset {
			switch tokens[idx-1].Text + tok.Text {
			case "!==", "===":
				return true
			}
		}
	}

	// An unknown colon form is not deferred to unknown_keyword: that rule
	// stays a warning, because a gap in the keyword list must not fail
	// validation, while a token the statement grammar cannot place is a
	// parse failure whatever its spelling (issue #291). A statement that
	// is only an unknown keyword (`:FOOBAR;`) is not a parse failure here
	// and stays unknown_keyword's alone. endfor_invalid is already an
	// error and owns `:ENDFOR`.
	if tok.Type == lexer.TokenKeyword && strings.EqualFold(tok.Text, ":ENDFOR") {
		return true
	}

	if tok.Type == lexer.TokenPunctuation && closerFor[tok.Text] == "" {
		if _, isCloser := openerFor[tok.Text]; isCloser {
			// unmatched_delimiter / mismatched_delimiter own a closer with
			// no matching opener; a closer that does match but arrives
			// where an operand was due (`(1 + )`) is this rule's.
			opener := innermostOpener(tokens, se.Start, idx)
			return opener == "" || closerFor[opener] != tok.Text
		}
	}

	if tok.Type == lexer.TokenIdentifier {
		// bare_logical_operator: AND / OR / NOT written without periods, in
		// operator position.
		if bareLogicalOperatorPosition(tokens, idx) {
			return true
		}
		// procedure_declaration_syntax: a C-style `PROCEDURE Name(` with no
		// leading colon stops the parser at the name; that rule names the
		// declaration typo. Only the shape it reports — the name followed
		// by `(` — is its; a bare `PROCEDURE Name;` stays here.
		if prev := previousSignificantTokenIndex(tokens, idx-1); prev >= 0 &&
			tokens[prev].Type == lexer.TokenIdentifier && strings.EqualFold(tokens[prev].Text, "PROCEDURE") {
			if next := nextSignificantTokenIndex(tokens, idx+1); next >= 0 &&
				tokens[next].Type == lexer.TokenPunctuation && tokens[next].Text == "(" {
				return true
			}
		}
		// scientific_notation: `7e2` lexes as number `7` glued to
		// identifier `e2`, and `.5e1` as `.5` glued to `e1`; the number is
		// what that rule reports. Only those shapes — a number with no
		// decimal point, or one starting with the point — are its. A
		// well-formed number glued to a stray `E` (`2.0E+nVar`) is nobody
		// else's and stays here.
		if idx > 0 && tokens[idx-1].Type == lexer.TokenNumber &&
			tokens[idx-1].Offset+len(tokens[idx-1].Text) == tok.Offset &&
			(tok.Text[0] == 'e' || tok.Text[0] == 'E') {
			num := tokens[idx-1].Text
			if !strings.Contains(num, ".") || strings.HasPrefix(num, ".") {
				return true
			}
		}
	}
	return false
}

var openerFor = map[string]string{")": "(", "]": "[", "}": "{"}

// unclosedOpenerIn reports whether an opener inside [start, end] is never
// closed within it.
func unclosedOpenerIn(tokens []lexer.Token, start, end int) bool {
	depth := 0
	for j := start; j <= end && j < len(tokens); j++ {
		t := tokens[j]
		if t.Type != lexer.TokenPunctuation {
			continue
		}
		if closerFor[t.Text] != "" {
			depth++
		} else if openerFor[t.Text] != "" && depth > 0 {
			depth--
		}
	}
	return depth > 0
}

// bareLogicalOperatorPosition mirrors checkBareLogicalOperators: the
// identifier is AND / OR / NOT, not a member name, and sits where an
// operator would.
func bareLogicalOperatorPosition(tokens []lexer.Token, i int) bool {
	upper := strings.ToUpper(tokens[i].Text)
	if upper != "AND" && upper != "OR" && upper != "NOT" {
		return false
	}
	prevIdx := previousSignificantTokenIndex(tokens, i-1)
	nextIdx := nextSignificantTokenIndex(tokens, i+1)
	var prev, next lexer.Token
	if prevIdx >= 0 {
		prev = tokens[prevIdx]
	}
	if nextIdx >= 0 {
		next = tokens[nextIdx]
	}
	if prev.Type == lexer.TokenPunctuation && prev.Text == ":" {
		return false
	}
	if upper == "NOT" {
		return nextIdx >= 0 && operandStart(next) && !(prevIdx >= 0 && operandEnd(prev))
	}
	return prevIdx >= 0 && operandEnd(prev) && nextIdx >= 0 && operandStart(next)
}

// endsInStepMember reports whether a `:FOR` limit expression ends in a
// member access named STEP — the parse of `:TO nMax:STEP 2`, where the
// missing space glues the keyword to the limit as `nMax:STEP`.
func endsInStepMember(e *parser.Expr) bool {
	for e != nil {
		switch e.Kind {
		case parser.ExprMember:
			return strings.EqualFold(e.Name, "STEP")
		case parser.ExprBinary:
			if len(e.Children) < 2 {
				return false
			}
			e = e.Children[1]
		default:
			return false
		}
	}
	return false
}
