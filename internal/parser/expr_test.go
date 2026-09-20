package parser

import (
	"strings"
	"testing"

	"starlims-lsp/internal/lexer"
)

func parseExprString(t *testing.T, code string) string {
	t.Helper()
	tokens := lexer.NewLexer(code).Tokenize()
	e, _ := ParseExpression(tokens, 0)
	return e.String()
}

func TestParseExpression_Shapes(t *testing.T) {
	// Tree shapes rendered as s-expressions; these pin the grammar's
	// precedence and associativity rules (ssl-ebnf-grammar.md).
	cases := []struct {
		code string
		want string
	}{
		// Literals and identifiers.
		{`42`, `42`},
		{`"hi"`, `"hi"`},
		{`.T.`, `.T.`},
		{`NIL`, `NIL`},
		{`sName`, `sName`},

		// Precedence ladder: .OR. < .AND. < equality < relational <
		// shift < additive < multiplicative < power.
		{`a .OR. b .AND. c`, `(.OR. a (.AND. b c))`},
		{`a = b .AND. c`, `(.AND. (= a b) c)`},
		{`a < b = c`, `(= (< a b) c)`},
		{`a << 1 < b`, `(< (<< a 1) b)`},
		{`a + b << 2`, `(<< (+ a b) 2)`},
		{`a + b * c`, `(+ a (* b c))`},
		{`a * b ^ c`, `(* a (^ b c))`},
		{`s $ t`, `($ s t)`},
		{`a # b`, `(# a b)`},
		{`a <> b`, `(<> a b)`},

		// Associativity: power right, everything else left.
		{`2 ^ 3 ^ 2`, `(^ 2 (^ 3 2))`},
		{`2 ** 3 ** 2`, `(** 2 (** 3 2))`},
		{`a - b - c`, `(- (- a b) c)`},
		{`a / b / c`, `(/ (/ a b) c)`},

		// Unary binds tighter than power: -3^2 = (-3)^2.
		{`-3 ^ 2`, `(^ (- 3) 2)`},
		{`.NOT. a .AND. b`, `(.AND. (.NOT. a) b)`},
		{`!bDone`, `(! bDone)`},
		{`- - n`, `(- (- n))`},

		// Grouping.
		{`(a + b) * c`, `(* (group (+ a b)) c)`},

		// Postfix: member, call, index, chaining.
		{`oReq:sName`, `(member sName oReq)`},
		{`obj:prop:subprop`, `(member subprop (member prop obj))`},
		{`Foo()`, `(call Foo)`},
		{`Foo(1, "x")`, `(call Foo 1 "x")`},
		{`obj:Method(1):Prop`, `(member Prop (call (member Method obj) 1))`},
		{`aData[1, 2]`, `(index aData 1 2)`},
		{`aData[1][2]`, `(index (index aData 1) 2)`},
		{`oReq:aItems[i]:ToString()`, `(call (member ToString (index (member aItems oReq) i)))`},
		{`Me:Count`, `(member Count Me)`},
		{`Base:Total(1)`, `(call (member Total Base) 1)`},
		{`(a):prop`, `(member prop (group a))`},

		// Skipped arguments.
		{`DoProc("X", {1,,3})`, `(call DoProc "X" (array 1 <skip> 3))`},
		{`Foo(a,)`, `(call Foo a <skip>)`},
		{`Foo(, a)`, `(call Foo <skip> a)`},

		// Array literals, nesting, mixed content.
		{`{1, {2, 3}, "x"}`, `(array 1 (array 2 3) "x")`},
		{`{}`, `(array)`},

		// Code blocks are opaque leaves.
		{`Eval({|x| x * x}, 2)`, `(call Eval (codeblock) 2)`},

		// Built-in class instantiation.
		{`Email{}`, `(new Email)`},
		{`SSLDataset{sQuery, "CONN"}`, `(new SSLDataset sQuery "CONN")`},

		// Increment forms.
		{`i++`, `(++ i)`},
		{`++i`, `(++:pre i)`},
		{`(i += 1) <= nCount`, `(<= (group (+= i 1)) nCount)`},

		// Invalid-but-lexed C-style operators still produce a tree.
		{`a && b || c`, `(|| (&& a b) c)`},
	}

	for _, tc := range cases {
		got := parseExprString(t, tc.code)
		if got != tc.want {
			t.Errorf("%q:\n  got  %s\n  want %s", tc.code, got, tc.want)
		}
	}
}

func TestParseExpression_UnknownDegradation(t *testing.T) {
	// Unresolvable input degrades to ExprUnknown without panicking and
	// without claiming coverage.
	for _, code := range []string{
		``, `;`, `:IF`, `)`, `,`, `(a + `, `{1, `, `a[1`, `+`,
	} {
		tokens := lexer.NewLexer(code).Tokenize()
		e, next := ParseExpression(tokens, 0)
		if e == nil {
			t.Fatalf("%q: nil expr", code)
		}
		if next < 0 || next > len(tokens) {
			t.Errorf("%q: next index %d out of range", code, next)
		}
		if !strings.Contains(e.String(), "unknown") && e.Kind != ExprUnknown {
			// Partial trees are fine; a completely well-formed claim on
			// broken input is not. `(a + ` should parse to a tree
			// containing an unknown leaf.
			if code == `(a + ` || code == `{1, ` || code == `a[1` {
				t.Errorf("%q: expected an unknown somewhere, got %s", code, e)
			}
		}
	}
}

func TestParseExpression_StopsAtKeywordsAndAssignment(t *testing.T) {
	// An expression parse must halt at statement keywords so statement
	// scanners can resume there.
	tokens := lexer.NewLexer(`i :TO 10`).Tokenize()
	e, next := ParseExpression(tokens, 0)
	if e.String() != "i" {
		t.Fatalf("expected bare identifier, got %s", e)
	}
	idx := nextSignificantIndex(tokens, next, len(tokens)-1)
	if idx < 0 || tokens[idx].Type != lexer.TokenKeyword {
		t.Fatalf("expected parse to stop at :TO keyword")
	}
}

func TestParseExpression_TokenRanges(t *testing.T) {
	// Start/End must bracket the full node span.
	code := `nTotal + Len(aItems[2])`
	tokens := lexer.NewLexer(code).Tokenize()
	e, _ := ParseExpression(tokens, 0)
	if e.Kind != ExprBinary {
		t.Fatalf("expected binary root, got %s", e)
	}
	if tokens[e.Start].Text != "nTotal" {
		t.Errorf("root Start at %q, want nTotal", tokens[e.Start].Text)
	}
	if tokens[e.End].Text != ")" {
		t.Errorf("root End at %q, want )", tokens[e.End].Text)
	}
	call := e.Children[1]
	if call.Kind != ExprCall || tokens[call.Start].Text != "Len" || tokens[call.End].Text != ")" {
		t.Errorf("call span wrong: %q..%q", tokens[call.Start].Text, tokens[call.End].Text)
	}
}

func extract(t *testing.T, code string) []StatementExprs {
	t.Helper()
	return ExtractStatementExpressions(lexer.NewLexer(code).Tokenize())
}

func TestExtractStatementExpressions_Shapes(t *testing.T) {
	code := `:PROCEDURE Demo;
:PARAMETERS nCount, sName;
:DEFAULT nCount, 10;
:DECLARE i, nSum, aItems;
aItems := {1, 2, 3};
nSum := 0;
:FOR i := 1 :TO Len(aItems) :STEP 1;
	nSum += aItems[i];
:NEXT;
:IF nSum > 5 .AND. !Empty(sName);
	DoProc("Log", {sName});
:ENDIF;
:RETURN nSum * 2;
:ENDPROC;`

	stmts := extract(t, code)
	var got []string
	for _, s := range stmts {
		var parts []string
		for _, e := range s.Exprs {
			parts = append(parts, e.String())
		}
		tag := ""
		if s.Assign != "" {
			tag = s.Assign + " "
		}
		if !s.Complete {
			tag += "INCOMPLETE "
		}
		got = append(got, tag+strings.Join(parts, " | "))
	}
	want := []string{
		"nCount | 10",
		":= aItems | (array 1 2 3)",
		":= nSum | 0",
		":= i | 1 | (call Len aItems) | 1",
		"+= nSum | (index aItems i)",
		"(.AND. (> nSum 5) (! (call Empty sName)))",
		`(call DoProc "Log" (array sName))`,
		"(* nSum 2)",
	}
	if len(got) != len(want) {
		t.Fatalf("statement count: got %d (%v), want %d", len(got), got, len(want))
	}
	for i := range want {
		if got[i] != want[i] {
			t.Errorf("stmt %d:\n  got  %s\n  want %s", i, got[i], want[i])
		}
	}
}

func TestExtractStatementExpressions_CompletenessFlag(t *testing.T) {
	// A statement whose tail the parser cannot consume must not claim
	// Complete, and region bodies must yield nothing at all.
	code := `:REGION Stored;
this is ! raw @ text
:ENDREGION;
x := 1 ?? 2;
y := 2 + 3;`
	stmts := extract(t, code)
	if len(stmts) != 2 {
		t.Fatalf("expected 2 expression statements, got %d", len(stmts))
	}
	if stmts[0].Complete {
		t.Errorf("x := 1 ?? 2 claimed Complete")
	}
	if !stmts[1].Complete {
		t.Errorf("y := 2 + 3 not Complete")
	}
}

func TestExtractStatementExpressions_NoPanicOnCorpusShapes(t *testing.T) {
	// Shapes that historically broke token scanners.
	for _, code := range []string{
		`:CLASS Widget;`,
		`/* just a comment;`,
		`:BEGININLINECODE "Block1";
x := 1;
:ENDINLINECODE;`,
		`sSql := "SELECT 1 FROM t WHERE a = ?x?";`,
		`:LABEL Retry;`,
		`Branch("LABEL Retry");`,
		`:ERROR;
:RESUME;`,
		`a := b:`,
		`( ( ( (`,
	} {
		_ = ExtractStatementExpressions(lexer.NewLexer(code).Tokenize())
	}
}

// [spec diag.unexpected_token] The extractor names the first token the
// statement grammar could not accept and, where it knows structurally,
// what belonged there. Wording for the "" cases is the consumer's.
func TestExtractStatementExpressions_UnexpectedToken(t *testing.T) {
	cases := []struct {
		code     string
		tok      string // text of the unexpected token; "" for end of file
		line     int    // 1-based line of the unexpected token
		expected string
	}{
		{"This is a bunch of text but it does not evaluate as wrong\nnCount := 2;", "is", 1, ""},
		{"foo bar baz;", "bar", 1, ""},
		{":FOR EACH nItem IN aItems;", "nItem", 1, "':='"},
		{":FOR i := 1 10;", "10", 1, "':TO'"},
		{":FOR :TO 10;", ":TO", 1, "a loop variable"},
		{":FOR i 1 :TO 10;", "1", 1, "':='"},
		{":FOR i := :TO 10;", ":TO", 1, "an expression after ':='"},
		{":FOR i := 1 :TO ;", ";", 1, "an expression after ':TO'"},
		{":FOR i := 1 :TO 10 :STEP;", ";", 1, "an expression after ':STEP'"},
		{":FOR i := 1 :TO 10\n:NEXT;", ":NEXT", 2, "':STEP' or ';'"},
		{":FOR i := 1 :TO 10 :STEP 2 3;", "3", 1, "';'"},
		{"nCount := 1\nnCount := 2;", "nCount", 2, ""},
		{":IF;", ";", 1, "a condition"},
		{":WHILE nA nB;", "nB", 1, ""},
		{"nCount := ;", ";", 1, "an expression"},
		{"x := 1 + ;", ";", 1, ""},
		{`DoProc("X" {a});`, "{", 1, ""},
		{"x := (1 + );", ")", 1, ""},
		{`:DEFAULT sName "x";`, `"x"`, 1, "','"},
		{":DEFAULT;", ";", 1, "a parameter name"},
		{":DEFAULT sName, ;", ";", 1, "a default value"},
		{":RETURN 1 2;", "2", 1, ""},
		{"*x;", "*", 1, "a statement"},
		{"x := 1 +", "", 1, ""},
	}
	for _, c := range cases {
		tokens := lexer.NewLexer(c.code).Tokenize()
		stmts := ExtractStatementExpressions(tokens)
		if len(stmts) != 1 {
			t.Errorf("%q: want 1 statement, got %d", c.code, len(stmts))
			continue
		}
		se := stmts[0]
		if se.Complete || se.Unexpected < 0 {
			t.Errorf("%q: claimed Complete (Unexpected=%d)", c.code, se.Unexpected)
			continue
		}
		got := tokens[se.Unexpected]
		if c.tok == "" {
			if got.Type != lexer.TokenEOF {
				t.Errorf("%q: want end of file, got %q", c.code, got.Text)
			}
		} else if got.Text != c.tok || got.Line != c.line {
			t.Errorf("%q: unexpected token got %q (line %d), want %q (line %d)", c.code, got.Text, got.Line, c.tok, c.line)
		}
		if se.Expected != c.expected {
			t.Errorf("%q: Expected got %q, want %q", c.code, se.Expected, c.expected)
		}
	}
}

// [spec diag.unexpected_token] Statements the grammar accepts in full are
// Complete with no unexpected token; a bare `:RETURN;` carries no
// expression and is omitted rather than reported as missing one; a final
// statement that ends at end of file without `;` is complete.
func TestExtractStatementExpressions_CompleteForms(t *testing.T) {
	complete := []string{
		"This;",
		"nCount := 1",
		"x := (1 + 2) * 3;",
		":FOR i := 10 :TO 1 :STEP -1;",
		":FOR i := 1\n\t:TO nMax;",
		"nTotal := nA\n\t+ nB;",
		"oEmail := Email{};",
		"x++;",
		"bOk := .T. .AND. .F.;",
		"fn := {|n| n + 1};",
		"x := oObj:Method(1)[2]:Prop;",
		":WHILE (i += 1) <= 10;",
		`DoProc("P", {a,,c});`,
		"-x;",
		":RETURN nSum * 2;",
	}
	for _, code := range complete {
		stmts := extract(t, code)
		if len(stmts) != 1 {
			t.Errorf("%q: want 1 statement, got %d", code, len(stmts))
			continue
		}
		if !stmts[0].Complete || stmts[0].Unexpected != -1 {
			t.Errorf("%q: not Complete (Unexpected=%d, Expected=%q)", code, stmts[0].Unexpected, stmts[0].Expected)
		}
	}
	if stmts := extract(t, ":RETURN;"); len(stmts) != 0 {
		t.Errorf(":RETURN; should yield no statement, got %d", len(stmts))
	}
}

// [spec diag.unexpected_token] Assignment is an expression wherever an
// operand can stand — chained, as a list element, as a `:RETURN` value, in
// a condition — and parses in full; only the statement's own left-hand
// side is not a value position.
func TestParseExpression_AssignmentExpressions(t *testing.T) {
	cases := []struct{ code, want string }{
		{`(a := b := c)`, `(group (:= a (:= b c)))`},
		{`{ .T., n += 1 }`, `(array .T. (+= n 1))`},
		{`Foo(a := 1, b)`, `(call Foo (:= a 1) b)`},
		{`(i += 1) <= nCount`, `(<= (group (+= i 1)) nCount)`},
	}
	for _, tc := range cases {
		if got := parseExprString(t, tc.code); got != tc.want {
			t.Errorf("%q:\n  got  %s\n  want %s", tc.code, got, tc.want)
		}
	}
	// ParseExpression itself is not a value position: a bare `a := b` is
	// the identifier `a` with the assignment left for the statement.
	if got := parseExprString(t, `a := b`); got != "a" {
		t.Errorf("bare assignment: got %s, want a", got)
	}

	stmts := extract(t, "a := b := c;\n:RETURN x := .T.;\n:IF x := 1;\n:DEFAULT sName, sOther := \"x\";\n")
	if len(stmts) != 4 {
		t.Fatalf("want 4 statements, got %d", len(stmts))
	}
	want := []string{"a | (:= b c)", "(:= x .T.)", "(:= x 1)", "sName | (:= sOther \"x\")"}
	for i, s := range stmts {
		if !s.Complete {
			t.Errorf("stmt %d not Complete (Unexpected=%d)", i, s.Unexpected)
		}
		var parts []string
		for _, e := range s.Exprs {
			parts = append(parts, e.String())
		}
		if got := strings.Join(parts, " | "); got != want[i] {
			t.Errorf("stmt %d: got %s, want %s", i, got, want[i])
		}
	}
	if stmts[0].Kind != StmtAssign || stmts[0].Assign != ":=" {
		t.Errorf("chained assignment lost its StmtAssign classification: %+v", stmts[0])
	}
}
