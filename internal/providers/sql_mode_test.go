package providers

import "testing"

// A data-source document that is plain SQL produces zero diagnostics —
// every SSL check would false-flag SQL syntax such as dot-qualified
// column names and bare AND/OR. [spec feature.diagnostics_pipeline/A10]
func TestGetDiagnostics_SQLDataSourceProducesNone(t *testing.T) {
	sql := `-- active samples for the review screen
/* legacy status filter kept for reference */
SELECT s.SampleID, s.SampleName, o.OwnerName
FROM Samples s
JOIN Owners o ON o.OwnerID = s.OwnerID
WHERE s.Status = 'A' AND o.Active = 1
ORDER BY s.SampleName`

	opts := DefaultDiagnosticOptions()
	opts.IsDataSourceFile = true
	if diags := GetDiagnostics(sql, opts); len(diags) != 0 {
		t.Errorf("expected no diagnostics for SQL data source, got %d: %+v", len(diags), diags)
	}
}

// A data-source document containing SSL keeps its diagnostics — SQL mode
// only activates on SQL content. [spec feature.diagnostics_pipeline/A11]
func TestGetDiagnostics_SSLDataSourceKeepsDiagnostics(t *testing.T) {
	ssl := `:PARAMETERS sStatus;
oFilter.Value := sStatus;
`

	opts := DefaultDiagnosticOptions()
	opts.IsDataSourceFile = true
	diags := GetDiagnostics(ssl, opts)

	found := false
	for _, d := range diags {
		if d.Code == CodeDotPropertyAccess {
			found = true
			break
		}
	}
	if !found {
		t.Errorf("expected dot_property_access in SSL-mode data source, got %+v", diags)
	}
}

// SQL content outside a data-source file still gets SSL diagnostics —
// SQL-mode classification is scoped to data-source files.
// [spec feature.diagnostics_pipeline/A12]
func TestGetDiagnostics_SQLContentOutsideDataSourceStillFlags(t *testing.T) {
	sql := "SELECT s.SampleName FROM Samples s WHERE s.Status = 'A'"

	opts := DefaultDiagnosticOptions()
	if diags := GetDiagnostics(sql, opts); len(diags) == 0 {
		t.Error("expected SSL diagnostics on SQL content outside a data source")
	}
}

// IsSQLDocument classifies whole-document content, tolerating leading SQL
// comments while never classifying SSL content as SQL.
func TestIsSQLDocument(t *testing.T) {
	cases := []struct {
		name    string
		content string
		want    bool
	}{
		{"plain select", "SELECT SampleID FROM Samples", true},
		{"leading sql comments", "-- note\n/* block */\nSELECT SampleID FROM Samples", true},
		{"insert", "INSERT INTO AuditLog (Msg) VALUES ('x')", true},
		{"ssl leading comment", "/* datasource doc;\n:PARAMETERS sStatus;", false},
		{"ssl code", ":PARAMETERS sStatus;\nsSQL := \"SELECT X FROM T\";", false},
		{"empty", "", false},
		{"prose mentioning select", "SELECT is a keyword in SQL", false},
	}

	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			if got := IsSQLDocument(tc.content); got != tc.want {
				t.Errorf("IsSQLDocument(%q) = %v, want %v", tc.content, got, tc.want)
			}
		})
	}
}
