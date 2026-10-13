package providers

import "testing"

// Issue #255 audit fixes: counts and ranges the catalog fences cannot pin.

func infoOpts() DiagnosticOptions {
	opts := DefaultDiagnosticOptions()
	opts.IncludeInfoDiagnostics = true
	return opts
}

func diagsWithCode(diagnostics []Diagnostic, code string) []Diagnostic {
	var out []Diagnostic
	for _, d := range diagnostics {
		if d.Code == code {
			out = append(out, d)
		}
	}
	return out
}

func TestBlockDepth_BranchBodiesReportOnce(t *testing.T) {
	text := ":IF nA > 0;\n" +
		":IF nB > 0;\n" +
		":IF nC > 0;\n" +
		":IF nD > 0;\n" +
		":IF nE > 0;\n" +
		"x := 1;\n" +
		":ELSE;\n" +
		"x := 2;\n" +
		":ENDIF;\n" +
		":ENDIF;\n:ENDIF;\n:ENDIF;\n:ENDIF;\n"
	got := diagsWithCode(GetDiagnostics(text, infoOpts()), CodeMaxBlockDepth)
	if len(got) != 1 {
		t.Fatalf("want 1 max_block_depth (the :IF, not its :ELSE body), got %d: %+v", len(got), got)
	}
	if got[0].Range.Start.Line != 4 {
		t.Errorf("want the report on the opening :IF line (4), got %d", got[0].Range.Start.Line)
	}
}

func TestBlockDepth_CaseArmsAddNoLevel(t *testing.T) {
	text := ":IF nA > 0;\n" +
		":IF nB > 0;\n" +
		":IF nC > 0;\n" +
		":BEGINCASE;\n" +
		":CASE nD > 0;\n" +
		":IF nE > 0;\n" + // depth 5: the :IF inside the arm
		"x := 1;\n" +
		":ENDIF;\n" +
		":EXITCASE;\n" +
		":OTHERWISE;\n" +
		"x := 2;\n" +
		":EXITCASE;\n" +
		":ENDCASE;\n" +
		":ENDIF;\n:ENDIF;\n:ENDIF;\n"
	got := diagsWithCode(GetDiagnostics(text, infoOpts()), CodeMaxBlockDepth)
	if len(got) != 1 || got[0].Range.Start.Line != 5 {
		t.Fatalf("want exactly the depth-5 :IF on line 5, got %+v", got)
	}
	if got[0].Message != "Block nesting depth (5) exceeds maximum (4)" {
		t.Errorf("unexpected message %q", got[0].Message)
	}
}

func TestMaxParams_RangesOnProcedureName(t *testing.T) {
	text := ":CLASS Box;\n\t:PROCEDURE WideProc;\n\t:PARAMETERS a1, a2, a3, a4, a5, a6, a7, a8, a9;\n\t:ENDPROC;\n"
	got := diagsWithCode(GetDiagnostics(text, infoOpts()), CodeMaxParamsWarning)
	if len(got) != 1 {
		t.Fatalf("want 1 max_params_warning, got %d", len(got))
	}
	r := got[0].Range
	if r.Start.Line != 1 || r.Start.Character != 12 || r.End.Character != 20 {
		t.Errorf("want range 1:12-1:20 over 'WideProc', got %+v", r)
	}
}

func TestMixedErrorHandling_TopLevelSpanCrossesProcedures(t *testing.T) {
	text := ":DECLARE n;\n" +
		":TRY;\nn := 1;\n:CATCH;\nn := 2;\n:ENDTRY;\n" +
		":PROCEDURE Helper;\nn := 3;\n:ENDPROC;\n" +
		":ERROR;\nn := 4;\n:RESUME;\n"
	got := diagsWithCode(GetDiagnostics(text, DefaultDiagnosticOptions()), CodeMixedErrorHandlingFamilies)
	if len(got) != 1 || got[0].Range.Start.Line != 9 {
		t.Fatalf("want one report on the top-level :ERROR (line 9), got %+v", got)
	}
}

func TestMixedErrorHandling_ProcedureDoesNotJoinTopLevel(t *testing.T) {
	text := ":DECLARE n;\n" +
		":TRY;\nn := 1;\n:CATCH;\nn := 2;\n:ENDTRY;\n" +
		":PROCEDURE Legacy;\nn := 3;\n:ERROR;\nn := 4;\n:RESUME;\n:ENDPROC;\n"
	if got := diagsWithCode(GetDiagnostics(text, DefaultDiagnosticOptions()), CodeMixedErrorHandlingFamilies); len(got) != 0 {
		t.Fatalf("a procedure's legacy handler does not protect top-level code, got %+v", got)
	}
}

func TestNestedIIF_EachNestedCallOnce(t *testing.T) {
	text := `sR := IIF(b1, IIF(b2, IIF(b3, "a", "b"), "c"), "d");`
	got := diagsWithCode(GetDiagnostics(text, infoOpts()), CodeNestedIif)
	if len(got) != 2 {
		t.Fatalf("want 2 nested_iif (one per nested call), got %d: %+v", len(got), got)
	}
	if got[0].Range == got[1].Range {
		t.Errorf("duplicate range %+v", got[0].Range)
	}
}

func TestNilInOperations_TreeOperands(t *testing.T) {
	cases := []struct {
		text string
		want int
	}{
		{"x := NIL + 1;", 1},
		{`x := "Total: " + NIL;`, 1},
		{"x := Foo(1, -NIL);", 1},
		{"x := nA * NIL;", 1},
		{"x := 1 + NIL:ToString();", 0},
		{"x := 1 + NIL : ToString();", 0},
		{"x := nValue + 1;", 0},
		{"x := NIL;", 0},
		{":IF x = NIL;\n:ENDIF;", 0},
	}
	for _, tc := range cases {
		got := diagsWithCode(GetDiagnostics(tc.text, DefaultDiagnosticOptions()), CodeNilInOperations)
		if len(got) != tc.want {
			t.Errorf("%q: want %d nil_in_operations, got %d", tc.text, tc.want, len(got))
		}
	}
}

func TestMixedTypePlus_TreeOperands(t *testing.T) {
	cases := []struct {
		text string
		want int
	}{
		{":DECLARE sA, nB;\nsA := sA + nB;", 1},
		{":DECLARE sA, nB;\nsA := nB * 2 + sA;", 1},
		{":DECLARE sA;\nsA := Upper(sA) + 1;", 1},
		{":DECLARE sA, nB;\nsA := \"a\" + nB + \"b\";", 1},
		{":DECLARE sA, nB;\nsA := oRec:Name + \" - \" + nB;", 1},
		{":DECLARE dA, nB;\ndA := nB + dA;", 1},
		{":DECLARE dA, nB;\ndA := dA + nB;", 0},
		{":DECLARE dA;\ndA := dA + 1 + 2;", 0},
		{":DECLARE sA;\nsA := x + \"a\";", 0},
		{":DECLARE sA;\nsA := MAX_LEN + \"a\";", 0},
		{":DECLARE vX, sA;\nvX := 1;\nsA := vX + \"b\";", 0},
		{":DECLARE sA;\nsA := sA + NIL;", 0},
		{":DECLARE sA, nB;\nsA := sA + (nB);", 1},
	}
	for _, tc := range cases {
		got := diagsWithCode(GetDiagnostics(tc.text, DefaultDiagnosticOptions()), CodeMixedTypeOperator)
		if len(got) != tc.want {
			t.Errorf("%q: want %d mixed_type_operator, got %d: %+v", tc.text, tc.want, len(got), got)
		}
	}
}

func TestNegativeLogic_NegationMustGovernCondition(t *testing.T) {
	cases := []struct {
		cond string
		want int
	}{
		{".NOT. bA", 1},
		{"!bA", 1},
		{".NOT. (bA .AND. bB)", 1},
		{".NOT. bA .AND. bB", 0},
		{"!bA .OR. bB", 0},
		{".NOT. bA = bB", 0},
	}
	for _, tc := range cases {
		text := ":IF " + tc.cond + ";\nn := 1;\n:ELSE;\nn := 2;\n:ENDIF;"
		got := diagsWithCode(GetDiagnostics(text, infoOpts()), CodeNegativeLogic)
		if len(got) != tc.want {
			t.Errorf("%q: want %d negative_logic, got %d", tc.cond, tc.want, len(got))
		}
	}
}
