package providers

import "testing"

// findingsOf returns the diagnostics with code under default options.
func findingsOf(t *testing.T, script, code string) []Diagnostic {
	t.Helper()
	return diagsWithCode(GetDiagnostics(script, DefaultDiagnosticOptions()), code)
}

func wantCount(t *testing.T, script, code string, want int) []Diagnostic {
	t.Helper()
	got := findingsOf(t, script, code)
	if len(got) != want {
		t.Errorf("%s on %q: want %d, got %d: %+v", code, script, want, len(got), got)
	}
	return got
}

// [spec diag.branch_literal_target] Every non-literal first argument is
// flagged on the argument; literals, member calls, and an empty call are
// not (issue #321).
func TestBranchLiteralTarget(t *testing.T) {
	got := wantCount(t, ":PARAMETERS sTarget;\nBranch(sTarget);\n:LABEL SKIP;\n", CodeBranchLiteralTarget, 1)
	if len(got) == 1 && (got[0].Range.Start.Line != 1 || got[0].Range.Start.Character != 7 || got[0].Range.End.Character != 14) {
		t.Errorf("range = %+v, want the argument", got[0].Range)
	}
	wantCount(t, "Branch(\"LABEL \" + \"SKIP\");\n:LABEL SKIP;\n", CodeBranchLiteralTarget, 1)
	wantCount(t, "Branch(DoProc(\"Target\"));\n", CodeBranchLiteralTarget, 1)
	for _, ok := range []string{
		"Branch(\"LABEL SKIP\");\n:LABEL SKIP;\n",
		"Branch('LABELSKIP');\n:LABELSKIP;\n",
		":DECLARE oFlow, sTarget;\noFlow:Branch(sTarget);\n",
		"Branch();\n",
	} {
		wantCount(t, ok, CodeBranchLiteralTarget, 0)
	}
}

// [spec diag.non_logical_condition] Literal non-logical conditions and
// .AND./.OR. left operands are flagged; logical literals, variables,
// calls, and right operands are not (issue #322).
func TestNonLogicalCondition(t *testing.T) {
	for _, bad := range []string{
		":IF \"x\";\n:ENDIF;\n",
		":WHILE 1;\n:ENDWHILE;\n",
		":IF NIL;\n:ENDIF;\n",
		":IF ({1});\n:ENDIF;\n",
		":BEGINCASE;\n:CASE -1;\n\t:EXITCASE;\n:ENDCASE;\n",
		":DECLARE b;\nb := \"x\" .AND. .T.;\n",
		":DECLARE b;\nb := 0 .OR. .T.;\n",
	} {
		wantCount(t, bad, CodeNonLogicalCondition, 1)
	}
	// The condition is the .AND. expression, a logical value, so only its
	// literal left operand is reported.
	wantCount(t, ":IF \"x\" .AND. .T.;\n:ENDIF;\n", CodeNonLogicalCondition, 1)
	for _, ok := range []string{
		":IF .T.;\n:ENDIF;\n",
		":DECLARE s;\n:IF s;\n:ENDIF;\n",
		":IF Empty(\"\");\n:ENDIF;\n",
		":DECLARE b;\nb := .T. .AND. \"x\";\n",
		":DECLARE b, n;\nb := n > 0 .AND. .T.;\n",
	} {
		wantCount(t, ok, CodeNonLogicalCondition, 0)
	}
}

// [spec diag.parenthesize_negated_comparison] `.NOT. a = b` warns; a
// literal string or number operand is an error; parenthesized forms are
// clean (issue #339).
func TestParenthesizeNegatedComparison(t *testing.T) {
	got := wantCount(t, ":DECLARE b, sCode;\nb := .NOT. sCode = \"AB\";\n", CodeParenthesizeNegatedComparison, 1)
	if len(got) == 1 && got[0].Severity != SeverityWarning {
		t.Errorf("variable operand: severity = %v, want warning", got[0].Severity)
	}
	got = wantCount(t, ":DECLARE b, sList;\nb := .NOT. 'AB' $ sList;\n", CodeParenthesizeNegatedComparison, 1)
	if len(got) == 1 && got[0].Severity != SeverityError {
		t.Errorf("literal operand: severity = %v, want error", got[0].Severity)
	}
	wantCount(t, ":DECLARE b, bA;\nb := !bA == .F.;\n", CodeParenthesizeNegatedComparison, 1)
	wantCount(t, ":DECLARE n;\n:IF .NOT. n < 3;\n:ENDIF;\n", CodeParenthesizeNegatedComparison, 1)
	for _, ok := range []string{
		":DECLARE b, sCode;\nb := .NOT. (sCode = \"AB\");\n",
		":DECLARE b, bA;\nb := (.NOT. bA) = .F.;\n",
		":DECLARE b, bA, bB;\nb := .NOT. bA .AND. bB;\n",
		":DECLARE b, sCode;\nb := sCode != \"AB\";\n",
	} {
		wantCount(t, ok, CodeParenthesizeNegatedComparison, 0)
	}
}

// [spec diag.array_index_whole_number] A fractional literal subscript or a
// quotient subscript is flagged; whole literals, even literal quotients,
// and Integer()-wrapped quotients are not (issue #339).
func TestArrayIndexWholeNumber(t *testing.T) {
	for _, bad := range []string{
		":DECLARE a, x;\nx := a[2.5];\n",
		":DECLARE a, x, n;\nx := a[n / 2];\n",
		":DECLARE a, x, n;\nx := a[(n + 1) / 2];\n",
		":DECLARE a, x, n;\nx := a[n / 2 + 1];\n",
		":DECLARE a, x;\nx := a[3 / 2];\n",
		":DECLARE a, n;\na[1, n / 2] := 0;\n",
	} {
		wantCount(t, bad, CodeArrayIndexWholeNumber, 1)
	}
	for _, ok := range []string{
		":DECLARE a, x;\nx := a[1];\n",
		":DECLARE a, x;\nx := a[2.0];\n",
		":DECLARE a, x;\nx := a[4 / 2];\n",
		":DECLARE a, x, n;\nx := a[Integer(n / 2)];\n",
		":DECLARE a, x, n;\nx := a[n * 2];\n",
		":DECLARE x, n;\nx := n / 2;\n",
	} {
		wantCount(t, ok, CodeArrayIndexWholeNumber, 0)
	}
}
