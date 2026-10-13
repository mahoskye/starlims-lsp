package providers

import (
	"strings"
	"testing"
)

func nativeMemberFindings(t *testing.T, script string) []string {
	t.Helper()
	var out []string
	for _, d := range GetDiagnostics(script, DefaultDiagnosticOptions()) {
		if d.Code == CodeInvalidNativeMember {
			out = append(out, d.Message)
		}
	}
	return out
}

// Issue #267: documented failures on certain receivers are reported;
// everything uncertain or undocumented is not.
func TestInvalidNativeMember(t *testing.T) {
	script := `:PROCEDURE Demo;
:PARAMETERS aIn;
:DECLARE aX, sT, nN, fnC, dD, aZ, aW, sU;
aX := {1, 2};
nN := aX:Count;
nN := aX:Length;
aX:Append(3);
nN := aX:Clone():Length;
sT := "abc";
sT := sT:toupper();
sU := "x";
nN := sU:Length();
nN := aIn:Count;
fnC := {|x| x + 1};
nN := fnC:eval(1);
dD := Today();
nN := dD:year;
nN := dD:Year;
aZ := {};
DoProc("Fill", {aZ});
nN := aZ:Count;
aW := {};
aW := NIL;
nN := aW:Count;
:RETURN nN;
:ENDPROC;`
	got := nativeMemberFindings(t, script)
	want := []string{"':Count' on an array", "':Append()' on an array", "':Length()' on a string", "':eval()' on a code block", "':year' on a date"}
	if len(got) != len(want) {
		t.Fatalf("got %d findings %v, want %v", len(got), got, want)
	}
	for i := range want {
		if !strings.HasPrefix(got[i], want[i]) {
			t.Errorf("finding %d = %q, want prefix %q", i, got[i], want[i])
		}
	}
}

// Scopes are per procedure: the same name in another procedure, assigned
// a different type, does not poison or borrow the judgment.
func TestInvalidNativeMemberScopes(t *testing.T) {
	script := `:PROCEDURE A;
:DECLARE vX;
vX := {1};
:RETURN vX:Count;
:ENDPROC;
:PROCEDURE B;
:DECLARE vX;
vX := CreateUdObject();
:RETURN vX:Count;
:ENDPROC;`
	if got := nativeMemberFindings(t, script); len(got) != 1 {
		t.Fatalf("want only procedure A's finding, got %v", got)
	}
}
