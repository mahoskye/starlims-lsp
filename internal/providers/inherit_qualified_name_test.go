package providers

import (
	"strings"
	"testing"
)

// Issue #269: a plain :INHERIT parent is a compile error; a qualified one
// is not; a quoted one is left alone; base_requires_inherit stays quiet.
func TestInheritQualifiedName(t *testing.T) {
	cases := map[string]int{
		":CLASS A;\n:INHERIT BaseCounter;\n":             1,
		":CLASS A;\n:INHERIT Samples.BaseCounter;\n":     0,
		":CLASS A;\n:INHERIT /* x; Samples.Base;\n":      0,
		":CLASS A;\n:INHERIT \"Samples.BaseCounter\";\n": 0,
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
