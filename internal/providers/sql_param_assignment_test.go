package providers

import (
	"strings"
	"testing"
)

// An assigned-but-undeclared variable exists at run time, so its ?name?
// marker is valid; a member assignment does not create a variable.
func TestInvalidSQLParamAssignmentDeclares(t *testing.T) {
	script := `:PROCEDURE Demo;
:DECLARE aRows, oCfg;
sStatus := "A";
oCfg:sRegion := "N";
aRows := SQLExecute("SELECT * FROM T WHERE S = ?sStatus? AND R = ?sRegion?");
:RETURN aRows;
:ENDPROC;`
	opts := DefaultDiagnosticOptions()
	opts.CheckSQLParams = true
	var got []string
	for _, d := range GetDiagnostics(script, opts) {
		if d.Code == CodeInvalidSqlParam {
			got = append(got, d.Message)
		}
	}
	if len(got) != 1 || !strings.Contains(got[0], "'sRegion'") {
		t.Fatalf("want only sRegion reported, got %v", got)
	}
}
