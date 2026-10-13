package providers

import (
	"strings"
	"testing"
)

// Issue #272: a label, region or inline-code block name is not a variable
// reference, in either label form.
func TestUndeclaredSkipsLabelAndBlockNames(t *testing.T) {
	script := `:PROCEDURE Demo;
:DECLARE nX;
nX := 1;
:LABEL SKIP;
:LABELAGAIN;
:REGION Body;
SELECT 1
:ENDREGION;
:BEGININLINECODE Snippet;
nX := 2;
:ENDINLINECODE;
:RETURN nX;
:ENDPROC;`
	if got := undeclaredNames(t, script); len(got) != 0 {
		t.Fatalf("expected no undeclared diagnostics, got %v", got)
	}
}

// Issue #270: base-installation predefined globals are declared; a name
// the reference lists as not predefined still flags, with its
// replacement; a configured global overrides that.
func TestUndeclaredPredefinedGlobals(t *testing.T) {
	script := `:PROCEDURE Demo;
:DECLARE sOut;
sOut := MYUSERNAME + CRLF + UserLang + sCC + STARLIMSDEPT + PLATFORMA;
sOut += PublicConsts:Item + Session:SessionID + Released;
sOut += TAB + MYUSERID;
:RETURN sOut;
:ENDPROC;`
	got := undeclaredNames(t, script)
	if len(got) != 2 {
		t.Fatalf("want exactly TAB and MYUSERID flagged, got %v", got)
	}
	if !strings.Contains(got[0], "'TAB'") || !strings.Contains(got[0], "use Chr(9) instead") {
		t.Errorf("TAB message = %q", got[0])
	}
	if !strings.Contains(got[1], "'MYUSERID'") || !strings.Contains(got[1], "use MYUSERNAME instead") {
		t.Errorf("MYUSERID message = %q", got[1])
	}

	opts := undeclaredOptions()
	opts.GlobalVariables = []string{"tab", "MyUserId"}
	for _, d := range GetDiagnostics(script, opts) {
		if d.Code == CodeUndeclaredVariable {
			t.Errorf("configured globals must count as declared, got %q", d.Message)
		}
	}
}

// Issue #270: Request and Response keep DECISIONS.md D4 — declared only
// in endpoint scripts.
func TestUndeclaredEndpointAmbientsStayScoped(t *testing.T) {
	script := `:PROCEDURE Demo;
UsrMes(Request:Form("a"));
:ENDPROC;`
	got := undeclaredNames(t, script)
	if len(got) != 1 || !strings.Contains(got[0], "'Request'") {
		t.Fatalf("Request outside an endpoint must flag, got %v", got)
	}
}

// Issue #270: only MYUSERNAME of the predefined globals is write-checked;
// the application's own session code assigns the others.
func TestGlobalAssignmentKeepsCheckedSet(t *testing.T) {
	script := `:PROCEDURE Demo;
STARLIMSDEPT := "QC";
CRLF := Chr(10);
MYUSERNAME := "x";
:ENDPROC;`
	var got []string
	for _, d := range GetDiagnostics(script, DefaultDiagnosticOptions()) {
		if d.Code == CodeGlobalAssignment {
			got = append(got, d.Message)
		}
	}
	if len(got) != 1 || !strings.Contains(got[0], "MYUSERNAME") {
		t.Fatalf("want only the MYUSERNAME assignment flagged, got %v", got)
	}
}

// Issue #270: a variable read only through a ?name? marker is used.
func TestUnusedCountsSQLMarkers(t *testing.T) {
	script := `:PROCEDURE Demo;
:PARAMETERS sId, oUser, sUnused;
:DECLARE aRows;
aRows := SQLExecute("SELECT * FROM T WHERE A = ?sId? AND B = ?oUser:ID?");
:RETURN aRows;
:ENDPROC;`
	opts := DefaultDiagnosticOptions()
	opts.CheckUnusedVars = true
	var got []string
	for _, d := range GetDiagnostics(script, opts) {
		if d.Code == CodeUnusedVariable {
			got = append(got, d.Message)
		}
	}
	if len(got) != 1 || !strings.Contains(got[0], "'sUnused'") {
		t.Fatalf("want only sUnused reported, got %v", got)
	}
}
