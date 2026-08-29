package providers

import (
	"testing"
)

func undeclaredOptions() DiagnosticOptions {
	opts := DefaultDiagnosticOptions()
	opts.CheckUndeclaredVars = true
	return opts
}

func undeclaredNames(t *testing.T, script string) []string {
	t.Helper()
	var names []string
	for _, d := range GetDiagnostics(script, undeclaredOptions()) {
		if d.Code == CodeUndeclaredVariable {
			names = append(names, d.Message)
		}
	}
	return names
}

func TestUndeclaredVariableBareDeclareKeyword(t *testing.T) {
	// Regression (issue #184): a declaration whose keyword stands alone on
	// its line used to make every one of its names invisible — the names
	// were neither registered as declared nor exempted at their own
	// declaration site, so each flagged itself.
	script := `:PROCEDURE Demo;
:PARAMETERS chartNo, strRules, Mean
, STD, CV;
:DECLARE
	sDebugSQL,
	sSQL,
	aSel;
sSQL := "x";
UsrMes(sSQL + sDebugSQL + STD + CV + Mean + aSel[1] + chartNo + strRules);
:ENDPROC;`

	if got := undeclaredNames(t, script); len(got) != 0 {
		t.Fatalf("expected no undeclared diagnostics, got %v", got)
	}
}

func TestUndeclaredVariableStillFlagsRealUses(t *testing.T) {
	// The fix must not make the check silent: a name with no declaration
	// anywhere still flags, and a declaration's right-hand side is a use.
	script := `:PROCEDURE Demo;
:DECLARE
	sKnown;
sKnown := sMissing;
:ENDPROC;`

	got := undeclaredNames(t, script)
	if len(got) != 1 {
		t.Fatalf("expected exactly one undeclared diagnostic, got %v", got)
	}
	if got[0] != "Variable 'sMissing' is not declared" {
		t.Fatalf("expected sMissing to flag, got %q", got[0])
	}
}

func TestExtractedVariablesSeeBareDeclareKeyword(t *testing.T) {
	// Declared names feed document symbols, rename, the workspace index,
	// and every name-shaped diagnostic, so losing them was not confined to
	// the undeclared check.
	script := `:DECLARE
	sOne,
	sTwo;`

	opts := DefaultDiagnosticOptions()
	opts.CheckHungarianNotation = true
	opts.HungarianPrefixes = []string{"q"}

	var flagged int
	for _, d := range GetDiagnostics(script, opts) {
		if d.Code == CodeHungarianNotation {
			flagged++
		}
	}
	if flagged != 2 {
		t.Fatalf("expected both declared names to be visible to name-shaped rules, got %d", flagged)
	}
}
