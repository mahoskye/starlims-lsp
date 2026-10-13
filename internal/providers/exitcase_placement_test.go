package providers

import (
	"strings"
	"testing"
)

func exitCasePlacementLines(t *testing.T, script string) []int {
	t.Helper()
	var lines []int
	for _, d := range GetDiagnostics(script, DefaultDiagnosticOptions()) {
		if d.Code == CodeExitCaseLastInBranch {
			if d.Severity != SeverityError {
				t.Errorf("severity = %v, want error", d.Severity)
			}
			lines = append(lines, d.Range.Start.Line+1)
		}
	}
	return lines
}

// Issue #265 / #273: the repro from the issue, plus the outside form.
func TestExitCaseLastInBranch(t *testing.T) {
	script := `:PROCEDURE Demo;
:PARAMETERS nX, bY;
:DECLARE nZ;
:BEGINCASE;
:CASE nX == 1;
	:IF bY;
		:EXITCASE;
	:ENDIF;
:CASE nX == 2;
	:EXITCASE;
	/* a comment is not a statement;
	nZ := 1;
	nZ := 2;
:OTHERWISE;
	:EXITCASE;
	:EXITCASE;
:ENDCASE;
:EXITCASE;
:ENDPROC;`
	got := exitCasePlacementLines(t, script)
	want := []int{7, 12, 16, 18}
	if len(got) != len(want) {
		t.Fatalf("lines = %v, want %v", got, want)
	}
	for i := range want {
		if got[i] != want[i] {
			t.Fatalf("lines = %v, want %v", got, want)
		}
	}
}

// Nested blocks of every kind inside a branch make :EXITCASE nested.
func TestExitCaseNestedInEveryBlockKind(t *testing.T) {
	for _, body := range []string{
		":WHILE bY;\n\t:EXITCASE;\n:ENDWHILE;",
		":FOR i := 1 :TO 2;\n\t:EXITCASE;\n:NEXT;",
		":TRY;\n\tnZ := 1;\n:CATCH;\n\t:EXITCASE;\n:ENDTRY;",
		":IF bY;\n\tnZ := 1;\n:ELSE;\n\t:EXITCASE;\n:ENDIF;",
	} {
		script := ":PROCEDURE Demo;\n:PARAMETERS bY;\n:DECLARE nZ, i;\n:BEGINCASE;\n:CASE bY;\n" + body + "\n:ENDCASE;\n:ENDPROC;"
		if got := exitCasePlacementLines(t, script); len(got) != 1 {
			t.Errorf("%s: got %v, want one finding", strings.SplitN(body, "\n", 2)[0], got)
		}
	}
}

// A nested :BEGINCASE has its own branches; :RETURN before :EXITCASE
// keeps it last; prefer_exitcase does not double-report a nested one.
func TestExitCaseValidForms(t *testing.T) {
	script := `:PROCEDURE Demo;
:PARAMETERS nX, bY;
:DECLARE nZ;
:BEGINCASE;
:CASE nX == 1;
	:BEGINCASE;
	:CASE bY;
		nZ := 1;
		:EXITCASE;
	:OTHERWISE;
		:EXITCASE;
	:ENDCASE;
	:EXITCASE;
:CASE nX == 2;
	:RETURN nZ;
	:EXITCASE;
:OTHERWISE;
	nZ := 2;
	:EXITCASE;
	/* trailing;
:ENDCASE;
:RETURN nZ;
:ENDPROC;`
	if got := exitCasePlacementLines(t, script); len(got) != 0 {
		t.Fatalf("want no findings, got lines %v", got)
	}

	nested := `:PROCEDURE Demo;
:PARAMETERS bY;
:BEGINCASE;
:CASE bY;
	:IF bY;
		:EXITCASE;
	:ENDIF;
:OTHERWISE;
	:EXITCASE;
:ENDCASE;
:ENDPROC;`
	for _, d := range GetDiagnostics(nested, DefaultDiagnosticOptions()) {
		if d.Code == CodePreferExitCase {
			t.Errorf("prefer_exitcase must not also report the nested clause: %q", d.Message)
		}
	}
}
