package providers

import "testing"

// [spec diag.default_on_declare_line] A :DEFAULT inside an unterminated
// :DECLARE list is flagged on the keyword, once, and unexpected_token
// defers to it (issue #323).
func TestDefaultOnDeclareLine_UnterminatedDeclareList(t *testing.T) {
	script := ":DECLARE nCount :DEFAULT 0;\n:RETURN nCount;\n"
	diags := GetDiagnostics(script, DefaultDiagnosticOptions())
	got := diagsWithCode(diags, CodeDefaultOnDeclareLine)
	if len(got) != 1 {
		t.Fatalf("want 1 default_on_declare_line, got %d: %+v", len(got), diags)
	}
	if got[0].Range.Start.Line != 0 || got[0].Range.Start.Character != 16 || got[0].Range.End.Character != 24 {
		t.Errorf("range = %+v, want the ':DEFAULT' keyword", got[0].Range)
	}
	if n := len(diagsWithCode(diags, CodeUnexpectedToken)); n != 0 {
		t.Errorf("unexpected_token should defer, got %d", n)
	}

	// A terminated :DECLARE followed by :DEFAULT on a local is the target
	// check's finding, not this shape's — still exactly one.
	script = ":DECLARE nCount; :DEFAULT nCount, 0;\n"
	if n := len(diagsWithCode(GetDiagnostics(script, DefaultDiagnosticOptions()), CodeDefaultOnDeclareLine)); n != 1 {
		t.Errorf("terminated form: want 1, got %d", n)
	}
}

// [spec diag.unexpected_token] A keyword inside a declaration list is
// reported once, with the missing-';' hint; the next statement is judged
// on its own (issue #323).
func TestUnexpectedToken_DeclarationListKeyword(t *testing.T) {
	cases := []struct {
		script string
		col    int
		want   string
	}{
		{":PROCEDURE P;\n:PARAMETERS sIn :DEFAULT sIn, \"\";\n:ENDPROC;\n", 16,
			"Unexpected keyword ':DEFAULT' - expected ',' or ';' (is the ':PARAMETERS' list missing its ';'?)"},
		{":DECLARE a, b :IF a;\n:ENDIF;\n", 14,
			"Unexpected keyword ':IF' - expected ',' or ';' (is the ':DECLARE' list missing its ';'?)"},
		{":PUBLIC gA\n:DECLARE b;\n", 0,
			"Unexpected keyword ':DECLARE' - expected ',' or ';' (is the ':PUBLIC' list missing its ';'?)"},
	}
	for _, c := range cases {
		got := unexpectedTokens(t, c.script)
		if len(got) != 1 {
			t.Errorf("%q: want 1 unexpected_token, got %+v", c.script, got)
			continue
		}
		if got[0].Range.Start.Character != c.col || got[0].Message != c.want {
			t.Errorf("%q: got %q at col %d, want %q at col %d", c.script, got[0].Message, got[0].Range.Start.Character, c.want, c.col)
		}
	}

	for _, ok := range []string{
		":DECLARE a, b;\n:PARAMETERS c;\n:PUBLIC gD;\n",
		":DECLARE a,\n\tb;\n",
	} {
		if got := unexpectedTokens(t, ok); len(got) != 0 {
			t.Errorf("%q: want none, got %+v", ok, got)
		}
	}
}
