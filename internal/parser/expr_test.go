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
