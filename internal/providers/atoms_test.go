package providers

import "testing"

func atomOpts(hungarianTypes bool) DiagnosticOptions {
	opts := DefaultDiagnosticOptions()
	opts.IncludeInfoDiagnostics = true
	opts.CheckHungarianTypes = hungarianTypes
	return opts
}

// atomFindings returns code's diagnostics with the info tier on.
func atomFindings(script, code string, hungarianTypes bool) []Diagnostic {
	return diagsWithCode(GetDiagnostics(script, atomOpts(hungarianTypes)), code)
}

// [spec diag.increment_as_statement] Info for a single use inside an
// expression, warning when the statement reuses the variable, once per
// variable; whole statements and :FOR steps are clean (issue #347).
func TestIncrementAsStatement(t *testing.T) {
	cases := []struct {
		script string
		want   []DiagnosticSeverity
	}{
		{":DECLARE a, i, x;\nx := a[i++];\n", []DiagnosticSeverity{SeverityInfo}},
		{":DECLARE i, n;\nn := ++i + 1;\n", []DiagnosticSeverity{SeverityInfo}},
		{":DECLARE i, n;\n:WHILE ++i <= n;\n:ENDWHILE;\n", []DiagnosticSeverity{SeverityInfo}},
		{":DECLARE i, n;\nn := i++ + i++;\n", []DiagnosticSeverity{SeverityWarning}},
		{":DECLARE a, i;\na[i++] := i;\n", []DiagnosticSeverity{SeverityWarning}},
		{":DECLARE i, j, n;\nn := i++ + j--;\n", []DiagnosticSeverity{SeverityInfo, SeverityInfo}},
		{":DECLARE i;\ni++;\n++i;\n--i;\n", nil},
		{":DECLARE a;\na[1]++;\n", nil},
	}
	for _, c := range cases {
		got := atomFindings(c.script, CodeIncrementAsStatement, false)
		if len(got) != len(c.want) {
			t.Errorf("%q: want %d findings, got %+v", c.script, len(c.want), got)
			continue
		}
		for i := range got {
			if got[i].Severity != c.want[i] {
				t.Errorf("%q: finding %d severity = %v, want %v", c.script, i, got[i].Severity, c.want[i])
			}
		}
	}
	// The warning tier shows without the info tier.
	if got := diagsWithCode(GetDiagnostics(":DECLARE i, n;\nn := i++ + i++;\n", DefaultDiagnosticOptions()), CodeIncrementAsStatement); len(got) != 1 {
		t.Errorf("warning tier hidden by default: %+v", got)
	}
}

// [spec diag.parenthesize_mixed_logical] Each unparenthesized .AND. group
// under .OR. is reported on the group (issue #347).
func TestParenthesizeMixedLogical(t *testing.T) {
	got := atomFindings(":DECLARE bA, bB, bC, b;\nb := bA .OR. bB .AND. bC;\n", CodeParenthesizeMixedLogical, false)
	if len(got) != 1 || got[0].Range.Start.Character != 13 || got[0].Range.End.Character != 24 || got[0].Severity != SeverityWarning {
		t.Errorf("want one warning over 'bB .AND. bC', got %+v", got)
	}
	if got := atomFindings(":DECLARE bA, bB, bC, bD, b;\nb := bA .AND. bB .OR. bC .AND. bD;\n", CodeParenthesizeMixedLogical, false); len(got) != 2 {
		t.Errorf("two groups: want 2, got %+v", got)
	}
	for _, ok := range []string{
		":DECLARE bA, bB, bC, b;\nb := bA .OR. (bB .AND. bC);\n",
		":DECLARE bA, bB, bC, b;\nb := (bA .OR. bB) .AND. bC;\n",
		":DECLARE bA, bB, bC, b;\nb := bA .AND. bB .AND. bC;\n",
		":DECLARE bA, bB, bC, b;\nb := bA .OR. bB .OR. bC;\n",
	} {
		if got := atomFindings(ok, CodeParenthesizeMixedLogical, false); len(got) != 0 {
			t.Errorf("%q: want none, got %+v", ok, got)
		}
	}
}

// [spec diag.one_assignment_per_statement] An assignment inside a larger
// expression is info; statements, pure chains, and a whole-condition
// assignment are not (issue #347).
func TestOneAssignmentPerStatement(t *testing.T) {
	for _, bad := range []string{
		":DECLARE x, n;\nx := (n := 5) * 2;\n",
		":DECLARE x, n;\nx := {n := 1, n + 1};\n",
		":DECLARE x, n;\nx := LimsString(n := 3);\n",
		":DECLARE i, n;\n:WHILE (i += 1) <= n;\n:ENDWHILE;\n",
		":DECLARE n;\n:RETURN n := 1;\n",
	} {
		if got := atomFindings(bad, CodeOneAssignmentPerStatement, false); len(got) != 1 || got[0].Severity != SeverityInfo {
			t.Errorf("%q: want 1 info, got %+v", bad, got)
		}
	}
	for _, ok := range []string{
		":DECLARE a, b;\na := b := 7;\n",
		":DECLARE n;\nn := 5;\nn += 1;\n",
		":DECLARE n;\n:IF n := 1;\n:ENDIF;\n",
	} {
		if got := atomFindings(ok, CodeOneAssignmentPerStatement, false); len(got) != 0 {
			t.Errorf("%q: want none, got %+v", ok, got)
		}
	}
}

// [spec diag.prefer_explicit_trim_concat] String minus between known
// strings is info on the '-'; prefixes count only with hungarianTypes
// (issue #347).
func TestPreferExplicitTrimConcat(t *testing.T) {
	got := atomFindings(":DECLARE s;\ns := \"Sample   \" - \"Analysis\";\n", CodePreferExplicitTrimConcat, false)
	if len(got) != 1 || got[0].Range.Start.Character != 17 || got[0].Range.End.Character != 18 {
		t.Errorf("want one finding on the '-', got %+v", got)
	}
	if got := atomFindings(":DECLARE s, sA;\ns := AllTrim(sA) - \"x\";\n", CodePreferExplicitTrimConcat, false); len(got) != 1 {
		t.Errorf("builtin string operand: want 1, got %+v", got)
	}
	named := ":DECLARE s, sA, sB;\ns := sA - sB;\n"
	if got := atomFindings(named, CodePreferExplicitTrimConcat, false); len(got) != 0 {
		t.Errorf("prefixes without hungarianTypes: want none, got %+v", got)
	}
	if got := atomFindings(named, CodePreferExplicitTrimConcat, true); len(got) != 1 {
		t.Errorf("prefixes with hungarianTypes: want 1, got %+v", got)
	}
	if got := atomFindings(":DECLARE n, nA;\nn := 5 - 3;\nn := nA - 1;\n", CodePreferExplicitTrimConcat, true); len(got) != 0 {
		t.Errorf("numeric minus: want none, got %+v", got)
	}
}
