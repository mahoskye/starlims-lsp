package providers

import "testing"

func TestGlobalAssignmentQualifiedMembers(t *testing.T) {
	cases := []struct {
		name string
		code string
	}{
		{"status property", `:DECLARE oSummary;
oSummary := CreateUdObject();
oSummary:pending := 0;
oSummary:active := 0;`},
		{"predefined global property", `:PARAMETERS oContext;
oContext:MYUSERNAME := "neutral";`},
		{"configured global property", `:PARAMETERS oContext;
oContext:gAppContext := "neutral";`},
		{"nested property", `:PARAMETERS oContext;
oContext:counts:Pending := 1;`},
		{"comments and whitespace", `:PARAMETERS oContext;
oContext : /* member ownership survives trivia;
pending := 1;`},
		{"class field", `:CLASS Neutral;
:DECLARE pending;
:PROCEDURE SetPending;
Me:pending := 1;
:ENDPROC;`},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			opts := DefaultDiagnosticOptions()
			opts.GlobalVariables = []string{"gAppContext"}
			for _, diagnostic := range GetDiagnostics(tc.code, opts) {
				if diagnostic.Code == CodeGlobalAssignment {
					t.Errorf("qualified member reported as global: %+v", diagnostic)
				}
			}
		})
	}
}

func TestGlobalAssignmentStillReportsBareNameAfterMember(t *testing.T) {
	code := `:PARAMETERS oSummary;
oSummary:pending := 1;
Pending := 2;`
	var findings []Diagnostic
	for _, diagnostic := range GetDiagnostics(code, DefaultDiagnosticOptions()) {
		if diagnostic.Code == CodeGlobalAssignment {
			findings = append(findings, diagnostic)
		}
	}
	if len(findings) != 1 {
		t.Fatalf("expected one bare-global finding, got %+v", findings)
	}
	if findings[0].Severity != SeverityError || findings[0].Range.Start.Line != 2 ||
		findings[0].Range.Start.Character != 0 {
		t.Errorf("wrong severity or range for bare global: %+v", findings[0])
	}
}
