package providers

import (
	"strings"
	"testing"
)

// Behavior fixed in the issue #255 catalog audit that the spec fences
// cannot pin, because fences assert a code fires, not its severity or
// message.

func TestBaseOutsideClass_IncludeLibraryTier(t *testing.T) {
	library := `:PROCEDURE Helper;
Base:Init();
:ENDPROC;`
	script := `:DECLARE nCount;
nCount := 1;
Base:Init();`
	for _, tc := range []struct {
		name string
		text string
		want DiagnosticSeverity
	}{
		{"procedure library warns", library, SeverityWarning},
		{"script errors", script, SeverityError},
	} {
		found := false
		for _, d := range GetDiagnostics(tc.text, DefaultDiagnosticOptions()) {
			if d.Code == CodeBaseOutsideClass {
				found = true
				if d.Severity != tc.want {
					t.Errorf("%s: severity = %v, want %v", tc.name, d.Severity, tc.want)
				}
			}
		}
		if !found {
			t.Errorf("%s: expected base_outside_class", tc.name)
		}
	}
}

func TestCatchClauseForm_MissingTerminatorMessage(t *testing.T) {
	text := `:PROCEDURE Demo;
:DECLARE n;
:TRY;
n := 1;
:CATCH
n := 0;
:ENDTRY;
:ENDPROC;`
	found := false
	for _, d := range GetDiagnostics(text, DefaultDiagnosticOptions()) {
		if d.Code == CodeCatchClauseForm {
			found = true
			if !strings.Contains(d.Message, "missing its ';'") {
				t.Errorf("message = %q, want the missing-terminator wording", d.Message)
			}
			if d.Range.Start.Line != 4 {
				t.Errorf("range line = %d, want the :CATCH line 4", d.Range.Start.Line)
			}
		}
	}
	if !found {
		t.Error("expected catch_clause_form")
	}
}

func TestCodeBlockStructure_IsError(t *testing.T) {
	text := `:PROCEDURE Demo;
:DECLARE fnGet;
fnGet := {|| 42};
:ENDPROC;`
	for _, d := range GetDiagnostics(text, DefaultDiagnosticOptions()) {
		if d.Code == CodeCodeBlockStructure && d.Severity != SeverityError {
			t.Errorf("code_block_structure severity = %v, want error", d.Severity)
		}
	}
}

func TestEndForClosesLoopForStructure(t *testing.T) {
	text := `:PROCEDURE Demo;
:DECLARE i;
:FOR i := 1 :TO 3;
:ENDFOR;
:EXITFOR;
:ENDPROC;`
	var codes []string
	for _, d := range GetDiagnostics(text, DefaultDiagnosticOptions()) {
		codes = append(codes, d.Code)
	}
	joined := strings.Join(codes, ",")
	if strings.Contains(joined, CodeUnclosedBlock) {
		t.Errorf("unexpected unclosed_block alongside endfor_invalid: %v", codes)
	}
	if !strings.Contains(joined, CodeEndForInvalid) || !strings.Contains(joined, CodeExitForOutsideLoop) {
		t.Errorf("want endfor_invalid and exitfor_outside_loop (the loop ended), got %v", codes)
	}
}

func TestDotPropertyAccess_DispatchPathReportedOnce(t *testing.T) {
	text := `:PROCEDURE Demo;
:DECLARE n;
n := ExecFunction(Utils.Helper, {1});
:ENDPROC;`
	counts := map[string]int{}
	for _, d := range GetDiagnostics(text, DefaultDiagnosticOptions()) {
		counts[d.Code]++
	}
	if counts[CodeExecFunctionMissingQuotes] != 1 || counts[CodeDotPropertyAccess] != 0 || counts[CodeUnknownToken] != 0 {
		t.Errorf("want only execfunction_missing_quotes, got %v", counts)
	}
}
