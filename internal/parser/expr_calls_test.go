package parser

import (
	"testing"

	"starlims-lsp/internal/lexer"
)

func collect(t *testing.T, src string) []CallSite {
	t.Helper()
	return CollectCalls(lexer.NewLexer(src).Tokenize())
}

func TestCollectCallsShapes(t *testing.T) {
	calls := collect(t, `:PROCEDURE Main;
sOut := AllTrim(Upper(sText));
oDoc:Save("f.txt");
Me:Log(Len(aItems));
:ENDPROC;`)

	type want struct {
		name      string
		qualified bool
		args      int
	}
	got := make([]want, 0, len(calls))
	for _, c := range calls {
		got = append(got, want{c.Name, c.Qualified(), len(c.Args)})
	}
	expected := []want{
		{"AllTrim", false, 1},
		{"Upper", false, 1},
		{"Save", true, 1},
		{"Log", true, 1},
		{"Len", false, 1},
	}
	if len(got) != len(expected) {
		t.Fatalf("expected %d calls, got %d: %+v", len(expected), len(got), got)
	}
	for i := range expected {
		if got[i] != expected[i] {
			t.Errorf("call %d: expected %+v, got %+v", i, expected[i], got[i])
		}
	}
}

func TestCollectCallsReceiver(t *testing.T) {
	calls := collect(t, `:PROCEDURE Main;
sOut := AllTrim(sText):Format("{0}", aArgs);
:ENDPROC;`)

	var format *CallSite
	for i := range calls {
		if calls[i].Name == "Format" {
			format = &calls[i]
		}
	}
	if format == nil {
		t.Fatal("Format call not collected")
	}
	if format.Receiver == nil || format.Receiver.Kind != ExprCall {
		t.Fatalf("expected a call receiver, got %v", format.Receiver)
	}
	if len(format.Args) != 2 {
		t.Fatalf("expected 2 arguments, got %d", len(format.Args))
	}
}

func TestCollectCallsSkippedArguments(t *testing.T) {
	// A trailing comma names no argument; interior skips do.
	cases := []struct {
		src            string
		args, eff      int
		effLastSkipped bool
	}{
		{`f(a, b);`, 2, 2, false},
		{`f(a,, c);`, 3, 3, false},
		{`f(a, b,);`, 3, 2, false},
		{`f(a,,);`, 3, 2, true},
		{`f();`, 0, 0, false},
	}
	for _, tc := range cases {
		calls := collect(t, tc.src)
		if len(calls) != 1 {
			t.Fatalf("%s: expected one call, got %d", tc.src, len(calls))
		}
		if got := len(calls[0].Args); got != tc.args {
			t.Errorf("%s: expected %d args, got %d", tc.src, tc.args, got)
		}
		eff := calls[0].EffectiveArgs()
		if len(eff) != tc.eff {
			t.Errorf("%s: expected %d effective args, got %d", tc.src, tc.eff, len(eff))
		}
		if tc.effLastSkipped && (len(eff) == 0 || eff[len(eff)-1].Kind != ExprSkipped) {
			t.Errorf("%s: expected the last effective argument to stay skipped", tc.src)
		}
	}
}

func TestCollectCallsInEveryStatementPosition(t *testing.T) {
	// The rules built on this index must see calls wherever they appear,
	// not only on assignment right-hand sides.
	src := `:PROCEDURE Main;
:IF Empty(sText);
	:RETURN Len(sText);
:ENDIF;
:WHILE At("x", sText) > 0;
:ENDWHILE;
:FOR i := 1 :TO Len(aItems) :STEP Val("2");
:NEXT;
:DEFAULT sName, AllTrim(sRaw);
UsrMes("done");
:BEGINCASE;
:CASE Type(xValue) == "C";
:ENDCASE;
:ENDPROC;`

	seen := map[string]bool{}
	for _, c := range collect(t, src) {
		seen[c.Name] = true
	}
	for _, name := range []string{"Empty", "Len", "At", "Val", "AllTrim", "UsrMes", "Type"} {
		if !seen[name] {
			t.Errorf("call to %s was not collected", name)
		}
	}
}

func TestCollectCallsUnresolvableStaysSilent(t *testing.T) {
	// An unterminated argument list resolves to ExprUnknown, which yields
	// no call site — unknown is never reported as a call.
	for _, src := range []string{`sOut := AllTrim(sText;`, `sOut := ;`} {
		if calls := collect(t, src); len(calls) != 0 {
			t.Errorf("%q: expected no call sites, got %d", src, len(calls))
		}
	}
}
