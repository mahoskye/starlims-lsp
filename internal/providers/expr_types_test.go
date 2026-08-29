package providers

import (
	"testing"

	"starlims-lsp/internal/lexer"
	"starlims-lsp/internal/parser"
)

// exprOf parses the right-hand side of `x := <src>;` and returns its tree.
func exprOf(t *testing.T, src string) *parser.Expr {
	t.Helper()
	tokens := lexer.NewLexer("x := " + src + ";").Tokenize()
	stmts := parser.ExtractStatementExpressions(tokens)
	if len(stmts) != 1 || len(stmts[0].Exprs) != 2 {
		t.Fatalf("%q: expected one assignment with two expressions, got %+v", src, stmts)
	}
	return stmts[0].Exprs[1]
}

func TestInferExprTypeStructural(t *testing.T) {
	cases := []struct {
		src  string
		want sslType
	}{
		{`"text"`, typeString},
		{`'text'`, typeString},
		{`[text]`, typeString},
		{`42`, typeNumber},
		{`.T.`, typeBoolean},
		{`NIL`, typeNIL},
		{`{1, 2}`, typeArray},
		{`{|a| a + 1}`, typeCodeBlock},
		{`(1 + 2)`, typeNumber},
		{`-nCount`, typeNumber},
		{`.NOT. bFlag`, typeBoolean},
		{`1 = 2`, typeBoolean},
		{`"a" $ "abc"`, typeBoolean},
		{`2 * 3`, typeNumber},
		{`"a" + sTail`, typeString},
		{`AllTrim(sText)`, typeString},
		{`Len(sText)`, typeNumber},
		{`Empty(sText)`, typeBoolean},
		{`Today()`, typeDate},
		{`CreateUdObject()`, typeObject},
		{`Email{}`, typeObject},
		// No claim: a name is not evidence in the structural mode, an
		// array element's type is not pinned down, a user function has no
		// published return, and "any" returns claim nothing.
		{`sName`, typeUnknown},
		{`aItems[1]`, typeUnknown},
		{`MyHelper(1)`, typeUnknown},
		{`oCfg:Value`, typeUnknown},
	}
	for _, tc := range cases {
		if got := inferExprType(exprOf(t, tc.src)); got != tc.want {
			t.Errorf("inferExprType(%q) = %v, want %v", tc.src, got, tc.want)
		}
	}
}

func TestInferExprTypeNamedReadsHungarian(t *testing.T) {
	cases := []struct {
		src  string
		want sslType
	}{
		{`sName`, typeString},
		{`nCount`, typeNumber},
		{`bFlag`, typeBoolean},
		{`dStart`, typeDate},
		{`aItems`, typeArray},
		{`oDoc`, typeObject},
		{`fnAdd`, typeCodeBlock},
		{`_sName`, typeString},
		{`Me:sTemplate`, typeString},
		{`sFirst + sLast`, typeString},
		// Shape failures make no claim: no uppercase body letter, an
		// unlisted prefix, the variant prefix, a loop counter, and an
		// ALLCAPS constant.
		{`String`, typeUnknown},
		{`xThing`, typeUnknown},
		{`vThing`, typeUnknown},
		{`i`, typeUnknown},
		{`MAXROWS`, typeUnknown},
		{`fnord`, typeUnknown},
	}
	for _, tc := range cases {
		if got := inferExprTypeNamed(exprOf(t, tc.src)); got != tc.want {
			t.Errorf("inferExprTypeNamed(%q) = %v, want %v", tc.src, got, tc.want)
		}
	}
}
