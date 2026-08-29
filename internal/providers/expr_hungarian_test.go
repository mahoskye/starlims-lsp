package providers

import (
	"strings"
	"testing"
)

func hungarianOptions() DiagnosticOptions {
	opts := DefaultDiagnosticOptions()
	opts.CheckHungarianNotation = true
	return opts
}

func typeMismatchLines(t *testing.T, script string) []int {
	t.Helper()
	var lines []int
	for _, d := range GetDiagnostics(script, hungarianOptions()) {
		if d.Code == CodeHungarianTypeMismatch {
			lines = append(lines, d.Range.Start.Line)
		}
	}
	return lines
}

func TestHungarianTypeMismatchOptIn(t *testing.T) {
	script := `:PROCEDURE Demo;
:DECLARE nCode, sText;
nCode := SubStr(sText, 1, 4);
:ENDPROC;`

	for _, d := range GetDiagnostics(script, DefaultDiagnosticOptions()) {
		if d.Code == CodeHungarianTypeMismatch {
			t.Fatalf("hungarian_type_mismatch fired with the setting off: %s", d.Message)
		}
	}
	if got := typeMismatchLines(t, script); len(got) != 1 || got[0] != 2 {
		t.Fatalf("expected one diagnostic on line 2 when enabled, got %v", got)
	}
}

func TestHungarianTypeMismatchFlags(t *testing.T) {
	script := `:PROCEDURE Demo;
:PARAMETERS nCount;
:DECLARE nCode, sText, sTotal, aNames, bReady, dWhen, oRec;
:DEFAULT nCount, "10";
nCode := SubStr(sText, 1, 4);
sTotal := 0;
aNames := "Ann,Bob";
bReady := "Y";
dWhen := "2026-01-01";
oRec := {1, 2};
:ENDPROC;`

	got := typeMismatchLines(t, script)
	want := []int{3, 4, 5, 6, 7, 8, 9}
	if len(got) != len(want) {
		t.Fatalf("expected %v, got %v", want, got)
	}
	for i := range want {
		if got[i] != want[i] {
			t.Fatalf("expected %v, got %v", want, got)
		}
	}
}

func TestHungarianTypeMismatchSilentWithoutEvidence(t *testing.T) {
	// Both ends demand a definite judgment: an unresolvable expression, an
	// undocumented operand combination, a target with no type-claiming
	// prefix, NIL, a compound assignment, a loop header, and a member
	// target all make no claim.
	script := `:PROCEDURE Demo;
:DECLARE nTotal, vThing, aList, sText, i, MAX_ROWS, Total;
nTotal := MyHelper(1);
nTotal := aList[1];
nTotal := Eval(fnBuild);
nTotal := aList + sText;
vThing := "anything";
Total := "anything";
MAX_ROWS := "anything";
sText := NIL;
nTotal += sText;
Me:nCount := "x";
:FOR i := 1 :TO 10;
:NEXT;
:ENDPROC;`

	if got := typeMismatchLines(t, script); len(got) != 0 {
		t.Fatalf("expected no diagnostics, got lines %v", got)
	}
}

func TestHungarianTypeMismatchMessage(t *testing.T) {
	script := `:PROCEDURE Demo;
:DECLARE nCode, sText;
nCode := AllTrim(sText);
:ENDPROC;`

	var msg string
	for _, d := range GetDiagnostics(script, hungarianOptions()) {
		if d.Code == CodeHungarianTypeMismatch {
			msg = d.Message
		}
	}
	for _, want := range []string{"'nCode'", "promises a number", "produces a string"} {
		if !strings.Contains(msg, want) {
			t.Errorf("expected %q in message, got %q", want, msg)
		}
	}
}
