package providers

import (
	"strings"
	"testing"
)

// Issue #304: false-positive guards found by triaging findings on
// unmodified product code in the production corpus. Each test pins the
// guard and the true positive it must keep.

func TestDirectProcedureCall_SelfNamedCallWarns(t *testing.T) {
	selfCall := `:PROCEDURE ReadSetting;
:PARAMETERS sKey;
	:DECLARE sValue;
	sValue := ReadSetting(Upper(sKey));
	:RETURN sValue;
:ENDPROC;`
	got := diagsWithCode(GetDiagnostics(selfCall, DefaultDiagnosticOptions()), CodeDirectProcedureCall)
	if len(got) != 1 {
		t.Fatalf("expected one finding for the self-named call, got %v", got)
	}
	if got[0].Severity != SeverityWarning {
		t.Errorf("self-named call: expected warning, got severity %d", got[0].Severity)
	}
	if !strings.Contains(got[0].Message, "its own procedure") {
		t.Errorf("self-named call: message should name both readings: %s", got[0].Message)
	}

	// Calling another in-file procedure stays a provable error.
	sibling := `:PROCEDURE ReadSetting;
:ENDPROC;

:PROCEDURE Main;
	:DECLARE sValue;
	sValue := ReadSetting("key");
:ENDPROC;`
	got = diagsWithCode(GetDiagnostics(sibling, DefaultDiagnosticOptions()), CodeDirectProcedureCall)
	if len(got) != 1 || got[0].Severity != SeverityError {
		t.Errorf("sibling call: expected one error, got %v", got)
	}
}

func TestCommentTextAfterTerminator_KeywordNamedAssignmentIsCode(t *testing.T) {
	assigned := `:PROCEDURE Demo;
	:DECLARE label;
	/* collect the request values
	before they are parsed;
	label := GetFromSession("Values");
	label += "";
	:RETURN label;
:ENDPROC;`
	if got := diagsWithCode(GetDiagnostics(assigned, DefaultDiagnosticOptions()), CodeCommentTextAfterTerminator); len(got) != 0 {
		t.Errorf("keyword-named assignment after a comment must not flag: %v", got)
	}

	// The doc-header shape still flags at error.
	header := "/* Documentation for this script\nParameters are described here;\nDefault values apply to each one\n;\n"
	got := diagsWithCode(GetDiagnostics(header, DefaultDiagnosticOptions()), CodeCommentTextAfterTerminator)
	if len(got) == 0 || got[0].Severity != SeverityError {
		t.Errorf("doc header cut in half: expected the error, got %v", got)
	}
}

func TestZeroBasedArrayIndex_DataSetTablesChainSilent(t *testing.T) {
	opts := infoEnabledOptions()
	silent := []string{
		`oTable := dataSet:Tables[0];`,
		`oRow := dataSet:Tables[0]:Rows[0];`,
		`sName := dataSet:Tables[1]:Columns[0]:ColumnName;`,
		`vCell := dataSet:Tables[0]:Rows[0]:ItemArray[0];`,
	}
	for _, code := range silent {
		if got := diagsWithCode(GetDiagnostics(code, opts), CodeZeroBasedArrayIndex); len(got) != 0 {
			t.Errorf("%s: a :Tables chain must not report, got %v", code, got)
		}
	}

	// Other member receivers keep the info advisory, and a bare SSL array
	// keeps the error.
	if got := diagsWithCode(GetDiagnostics(`vFirst := oList:Items[0];`, opts), CodeZeroBasedArrayIndex); len(got) != 1 || got[0].Severity != SeverityInfo {
		t.Errorf("other member receiver: expected one info advisory, got %v", got)
	}
	if got := diagsWithCode(GetDiagnostics(`oRow := oTable:Rows[0];`, opts), CodeZeroBasedArrayIndex); len(got) != 1 {
		t.Errorf("Rows without a :Tables chain may be an SSL array: expected the advisory, got %v", got)
	}
	if got := diagsWithCode(GetDiagnostics(":DECLARE aItems;\nvFirst := aItems[0];", opts), CodeZeroBasedArrayIndex); len(got) != 1 || got[0].Severity != SeverityError {
		t.Errorf("native array: expected one error, got %v", got)
	}
}

func TestNilMethodCall_FlowGuards(t *testing.T) {
	cases := []struct {
		name  string
		code  string
		lines []int // 0-based lines expected to flag
	}{
		{
			name: "negated Empty guards the body",
			code: ":DECLARE oX;\noX := NIL;\n:IF !Empty(oX);\n\toX:Run();\n:ENDIF;\n",
		},
		{
			name: ".NOT. Empty and != NIL guard the body",
			code: ":DECLARE oX;\noX := NIL;\n:IF .NOT. Empty(oX);\n\toX:Run();\n:ENDIF;\n:IF NIL != oX;\n\toX:Run();\n:ENDIF;\n",
		},
		{
			name: "earlier .AND. operand guards the condition",
			code: ":DECLARE oX, bOk;\noX := NIL;\n:IF bOk .AND. !Empty(oX) .AND. oX:Size > 0;\n:ENDIF;\n",
		},
		{
			name:  "guard after the use does not count",
			code:  ":DECLARE oX;\noX := NIL;\n:IF oX:Size > 0 .AND. !Empty(oX);\n:ENDIF;\n",
			lines: []int{2},
		},
		{
			name:  "top-level .OR. breaks the body guard",
			code:  ":DECLARE oX, bOk;\noX := NIL;\n:IF bOk .OR. !Empty(oX);\n\toX:Run();\n:ENDIF;\n",
			lines: []int{3},
		},
		{
			name:  "else branch is not guarded",
			code:  ":DECLARE oX;\noX := NIL;\n:IF !Empty(oX);\n\toX:Run();\n:ELSE;\n\toX:Run();\n:ENDIF;\n",
			lines: []int{5},
		},
		{
			name:  "NIL reassigned inside the guarded branch",
			code:  ":DECLARE oX;\noX := NIL;\n:IF !Empty(oX);\n\toX := NIL;\n\toX:Run();\n:ENDIF;\n",
			lines: []int{4},
		},
		{
			name: "NIL in a branch that returns does not reach later code",
			code: ":PROCEDURE P;\n:PARAMETERS bStop;\n:DECLARE oX;\noX := Make();\n:IF bStop;\n\t:IF !Empty(oX);\n\t\toX := NIL;\n\t:ENDIF;\n\t:RETURN;\n:ENDIF;\noX:Run();\n:ENDPROC;\n",
		},
		{
			name:  "NIL in a branch without return reaches later code",
			code:  ":PROCEDURE P;\n:PARAMETERS bStop;\n:DECLARE oX;\noX := Make();\n:IF bStop;\n\toX := NIL;\n:ENDIF;\noX:Run();\n:ENDPROC;\n",
			lines: []int{7},
		},
		{
			name:  "a loop exit can bypass the return",
			code:  ":PROCEDURE P;\n:DECLARE oX, i;\noX := Make();\n:FOR i := 1 :TO 3;\n\t:IF i > 1;\n\t\toX := NIL;\n\t\t:IF i > 2;\n\t\t\t:EXITFOR;\n\t\t:ENDIF;\n\t\t:RETURN;\n\t:ENDIF;\n:NEXT;\noX:Run();\n:ENDPROC;\n",
			lines: []int{12},
		},
		{
			name: "NIL in the then-branch does not reach the else-branch",
			code: ":PROCEDURE P;\n:PARAMETERS bA;\n:DECLARE oX;\noX := Make();\n:IF bA;\n\toX := NIL;\n:ELSE;\n\toX:Run();\n:ENDIF;\n:ENDPROC;\n",
		},
		{
			name:  "then-branch NIL still reaches code after the :IF",
			code:  ":PROCEDURE P;\n:PARAMETERS bA;\n:DECLARE oX;\noX := Make();\n:IF bA;\n\toX := NIL;\n:ELSE;\n\toX:Run();\n:ENDIF;\noX:Run();\n:ENDPROC;\n",
			lines: []int{9},
		},
		{
			name:  "a :TRY body is not a separate path",
			code:  ":PROCEDURE P;\n:DECLARE oX;\noX := Make();\n:TRY;\n\toX := NIL;\n\t:RETURN;\n:CATCH;\n\toX:Run();\n:ENDTRY;\n:ENDPROC;\n",
			lines: []int{7},
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			got := diagsWithCode(GetDiagnostics(tc.code, DefaultDiagnosticOptions()), CodeNilMethodCall)
			var lines []int
			for _, d := range got {
				lines = append(lines, d.Range.Start.Line)
			}
			if len(lines) != len(tc.lines) {
				t.Fatalf("expected findings on lines %v, got %v", tc.lines, lines)
			}
			for i := range lines {
				if lines[i] != tc.lines[i] {
					t.Errorf("expected findings on lines %v, got %v", tc.lines, lines)
				}
			}
		})
	}
}

func TestUndeclaredVariable_InstantiationAndUnreadInclude(t *testing.T) {
	opts := DefaultDiagnosticOptions()
	opts.CheckUndeclaredVars = true

	inst := ":PROCEDURE Demo;\n\t:DECLARE oForm, oPanel;\n\toForm := FormDto{\"frm\", 0, 0};\n\toPanel := PanelDto {};\n\t:RETURN nMissing;\n:ENDPROC;\n"
	got := diagsWithCode(GetDiagnostics(inst, opts), CodeUndeclaredVariable)
	if len(got) != 1 || !strings.Contains(got[0].Message, "nMissing") {
		t.Errorf("instantiation: expected only nMissing to flag, got %v", got)
	}

	// A name on its own line before a `{` on the next is not instantiation.
	split := ":PROCEDURE Demo;\n\t:DECLARE aX;\n\taX := {nMissing\n\t};\n:ENDPROC;\n"
	if got := diagsWithCode(GetDiagnostics(split, opts), CodeUndeclaredVariable); len(got) != 1 {
		t.Errorf("array literal element: expected nMissing to flag, got %v", got)
	}

	// No resolver: the include cannot be read, so the file is not checked.
	withInclude := ":INCLUDE Shared.ErrorCodes;\n:PROCEDURE Demo;\n\tRaiseError(aErrNotFound[2]);\n:ENDPROC;\n"
	if got := diagsWithCode(GetDiagnostics(withInclude, opts), CodeUndeclaredVariable); len(got) != 0 {
		t.Errorf("unread include: expected no findings, got %v", got)
	}

	// The caller vouching for the closure restores the check.
	opts.IncludesResolved = true
	if got := diagsWithCode(GetDiagnostics(withInclude, opts), CodeUndeclaredVariable); len(got) != 1 {
		t.Errorf("resolved include: expected aErrNotFound to flag, got %v", got)
	}
}
