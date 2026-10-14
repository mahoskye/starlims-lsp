package providers

import (
	"testing"
)

// codeLines returns the 0-based lines on which code fires.
func codeLines(t *testing.T, text, code string, opts DiagnosticOptions) []int {
	t.Helper()
	var lines []int
	for _, d := range GetDiagnostics(text, opts) {
		if d.Code == code {
			lines = append(lines, d.Range.Start.Line)
		}
	}
	return lines
}

func assertLines(t *testing.T, code string, got, want []int) {
	t.Helper()
	if len(got) != len(want) {
		t.Fatalf("%s: got lines %v, want %v", code, got, want)
	}
	for i := range got {
		if got[i] != want[i] {
			t.Fatalf("%s: got lines %v, want %v", code, got, want)
		}
	}
}

func TestArithmeticTypeMismatch_DocumentedPairs(t *testing.T) {
	// issue #255: operand pairs are judged against the reference's operator
	// tables on the expression tree.
	text := `:PROCEDURE Demo;
:PARAMETERS dDue, dStart, sA, sB, MAX_ROWS;
:DECLARE x, n, d;
d := dDue - 3;
n := dDue - dStart;
sA := sA - sB;
x := DoProc("Count");
n := MAX_ROWS - x;
n := 10 % "3";
n := .T. - .F.;
n := 2 ** "2";
n := (sA) - 1;
n := Len(sA) * "2";
:ENDPROC;`
	got := codeLines(t, text, CodeArithmeticTypeMismatch, DefaultDiagnosticOptions())
	assertLines(t, CodeArithmeticTypeMismatch, got, []int{8, 9, 10, 11, 12})
}

func TestArithmeticTypeMismatch_MemberOperandMakesNoClaim(t *testing.T) {
	text := `:PROCEDURE Demo;
:PARAMETERS oRec;
:DECLARE n;
n := oRec:sCode - 1;
n := (oRec:sCode + "") * 2;
:ENDPROC;`
	assertLines(t, CodeArithmeticTypeMismatch,
		codeLines(t, text, CodeArithmeticTypeMismatch, DefaultDiagnosticOptions()), nil)
}

func TestDollarStringOnly_TreeOperands(t *testing.T) {
	text := `:PROCEDURE Demo;
:PARAMETERS sList, ALLOWED, nCount;
:DECLARE x, b;
x := DoProc("Code");
b := "A" $ ALLOWED;
b := "B" $ x;
b := .NOT. "a" $ sList;
b := nCount $ sList;
b := .NOT. ("a" $ sList);
:ENDPROC;`
	// Line 6 (`.NOT. "a" $ sList`) is parenthesize_negated_comparison's
	// finding; this rule defers to it (issue #339).
	assertLines(t, CodeDollarStringOnly,
		codeLines(t, text, CodeDollarStringOnly, DefaultDiagnosticOptions()), []int{7})
	assertLines(t, CodeParenthesizeNegatedComparison,
		codeLines(t, text, CodeParenthesizeNegatedComparison, DefaultDiagnosticOptions()), []int{6})
}

func TestCodeBlockComparison_AllEqualityOperators(t *testing.T) {
	text := `:PROCEDURE Demo;
:PARAMETERS fnA, fnB;
:DECLARE b;
b := fnA = fnB;
b := fnA == fnB;
b := fnA != fnB;
b := fnA <> fnB;
b := fnA # fnB;
b := {|x| x} == NIL;
b := Empty(fnA);
:ENDPROC;`
	assertLines(t, CodeCodeBlockComparison,
		codeLines(t, text, CodeCodeBlockComparison, DefaultDiagnosticOptions()), []int{3, 4, 5, 6, 7, 8})
}

func TestForNumericValues_HeaderTrees(t *testing.T) {
	text := `:PROCEDURE Demo;
:PARAMETERS sStart, aRows, MAX_ROWS;
:DECLARE i, sIdx, nIdx;
:FOR i := 1 :TO Len(aRows);
:NEXT;
:FOR i := 1 :TO MAX_ROWS;
:NEXT;
:FOR sIdx := 1 :TO 10;
:NEXT;
:FOR nIdx := sStart :TO "ten" :STEP 1;
:NEXT;
:ENDPROC;`
	assertLines(t, CodeForNumericValues,
		codeLines(t, text, CodeForNumericValues, DefaultDiagnosticOptions()), []int{9, 9})
}

func TestEqualsEmptyString(t *testing.T) {
	text := `:PROCEDURE Demo;
:PARAMETERS sStatus, aRow, nCount;
:DECLARE b;
b := sStatus = "";
b := aRow[1] = '' .OR. .F.;
b := "" = sStatus;
b := sStatus == "";
b := nCount = "";
sStatus = "";
:IF sStatus = "";
:ENDIF;
:ENDPROC;`
	assertLines(t, CodeEqualsEmptyString,
		codeLines(t, text, CodeEqualsEmptyString, DefaultDiagnosticOptions()), []int{3, 4, 9})
}
