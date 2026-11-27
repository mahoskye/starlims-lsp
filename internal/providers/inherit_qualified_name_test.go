package providers

import (
	"strings"
	"testing"
)

// Issue #269: a plain :INHERIT parent is a compile error; a qualified one
// is not; base_requires_inherit stays quiet. A quoted one has its own test
// below.
func TestInheritQualifiedName(t *testing.T) {
	cases := map[string]int{
		":CLASS A;\n:INHERIT BaseCounter;\n":         1,
		":CLASS A;\n:INHERIT Samples.BaseCounter;\n": 0,
		":CLASS A;\n:INHERIT /* x; Samples.Base;\n":  0,
	}
	for script, want := range cases {
		var got []Diagnostic
		for _, d := range GetDiagnostics(script, DefaultDiagnosticOptions()) {
			if d.Code == CodeInheritQualifiedName {
				got = append(got, d)
			}
		}
		if len(got) != want {
			t.Errorf("%q: got %d findings, want %d", script, len(got), want)
			continue
		}
		if want == 1 {
			if r := got[0].Range; r.Start.Line != 1 || r.Start.Character != 9 || r.End.Character != 20 {
				t.Errorf("range = %+v, want the name", r)
			}
			if got[0].Severity != SeverityError || !strings.Contains(got[0].Message, "Category.ClassName") {
				t.Errorf("finding = %+v", got[0])
			}
		}
	}

	for _, d := range GetDiagnostics(":CLASS A;\n:INHERIT Lab:Parent;\n", DefaultDiagnosticOptions()) {
		if d.Code == CodeInheritQualifiedName && !strings.Contains(d.Message, "write ':INHERIT Lab.Parent;'") {
			t.Errorf("colon form message = %q", d.Message)
		}
	}

	withBase := ":CLASS A;\n:INHERIT BaseCounter;\n:PROCEDURE Go;\n\tBase:Go();\n:ENDPROC;\n"
	for _, d := range GetDiagnostics(withBase, DefaultDiagnosticOptions()) {
		if d.Code == CodeBaseRequiresInherit {
			t.Errorf("base_requires_inherit must not fire when a plain :INHERIT is present: %q", d.Message)
		}
	}
}

// Issue #372: a parent written as a string does not compile, in any quote
// style. The finding covers the string and gives the statement to write;
// nothing else reports the same statement.
func TestInheritQuotedParent(t *testing.T) {
	cases := []struct {
		script  string
		endChar int
		message string
	}{
		{":CLASS A;\n:INHERIT \"Samples.BaseCounter\";\n", 30, "write ':INHERIT Samples.BaseCounter;'"},
		{":CLASS A;\n:INHERIT 'Samples.BaseCounter';\n", 30, "write ':INHERIT Samples.BaseCounter;'"},
		{":CLASS A;\n:INHERIT [Samples.BaseCounter];\n", 30, "write ':INHERIT Samples.BaseCounter;'"},
		{":CLASS A;\n:INHERIT \"BaseCounter\";\n", 22, "full 'Category.ClassName' without quotes"},
		{":CLASS A;\n:INHERIT \"Samples.Base Counter\";\n", 31, "full 'Category.ClassName' without quotes"},
	}
	for _, c := range cases {
		all := GetDiagnostics(c.script, DefaultDiagnosticOptions())
		var got []Diagnostic
		for _, d := range all {
			if d.Code == CodeInheritQualifiedName {
				got = append(got, d)
			}
		}
		if len(got) != 1 {
			t.Errorf("%q: got %d findings, want 1", c.script, len(got))
			continue
		}
		d := got[0]
		if d.Severity != SeverityError {
			t.Errorf("%q: severity = %v, want error", c.script, d.Severity)
		}
		if r := d.Range; r.Start.Line != 1 || r.Start.Character != 9 || r.End.Character != c.endChar {
			t.Errorf("%q: range = %+v, want the string (1:9-1:%d)", c.script, r, c.endChar)
		}
		if !strings.Contains(d.Message, c.message) {
			t.Errorf("%q: message = %q, want it to contain %q", c.script, d.Message, c.message)
		}
		for _, other := range all {
			if other.Severity == SeverityError && other.Code != CodeInheritQualifiedName {
				t.Errorf("%q: a second error on the same statement: %s (%s)", c.script, other.Code, other.Message)
			}
		}
	}

	withBase := ":CLASS A;\n:INHERIT \"Lab.Parent\";\n:PROCEDURE Go;\n\tBase:Go();\n:ENDPROC;\n"
	for _, d := range GetDiagnostics(withBase, DefaultDiagnosticOptions()) {
		if d.Code == CodeBaseRequiresInherit {
			t.Errorf("base_requires_inherit must not fire when a quoted :INHERIT is present: %q", d.Message)
		}
	}
}
