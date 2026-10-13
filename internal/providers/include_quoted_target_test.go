package providers

import "testing"

// Issue #266: a quoted :INCLUDE target does not compile. The range covers
// the string token so a client quick fix can unquote it.
func TestIncludeQuotedTarget(t *testing.T) {
	script := ":INCLUDE \"Common.Helpers\";\n:INCLUDE 'Helpers';\n:INCLUDE Common.Fine;\n:INCLUDE /* note; [Common.Bracketed];\n"
	type hit struct{ line, start, end int }
	var got []hit
	for _, d := range GetDiagnostics(script, DefaultDiagnosticOptions()) {
		if d.Code != CodeIncludeQuotedTarget {
			continue
		}
		if d.Severity != SeverityError {
			t.Errorf("severity = %v, want error", d.Severity)
		}
		got = append(got, hit{d.Range.Start.Line, d.Range.Start.Character, d.Range.End.Character})
	}
	want := []hit{{0, 9, 25}, {1, 9, 18}, {3, 18, 36}}
	if len(got) != len(want) {
		t.Fatalf("got %v, want %v", got, want)
	}
	for i := range want {
		if got[i] != want[i] {
			t.Errorf("finding %d = %v, want %v", i, got[i], want[i])
		}
	}
}
