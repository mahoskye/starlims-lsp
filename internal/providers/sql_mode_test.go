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

// SplitDataSourceHeader separates builder directives and the inline-defaults
// :PARAMETERS header from the SQL body (issue #104).
func TestSplitDataSourceHeader(t *testing.T) {
	cases := []struct {
		name       string
		content    string
		wantHeader string
		wantBody   string
	}{
		{
			name:       "directives_then_sql",
			content:    ":DSN := myConnection;\n:TABLENAME := samples;\nSELECT sample_id FROM samples\n",
			wantHeader: ":DSN := myConnection;\n:TABLENAME := samples;\n",
			wantBody:   "SELECT sample_id FROM samples\n",
		},
		{
			name:       "parameters_then_sql",
			content:    ":PARAMETERS sStatus := \"A\";\nSELECT sample_id FROM samples WHERE sample_status = ?sStatus?\n",
			wantHeader: ":PARAMETERS sStatus := \"A\";\n",
			wantBody:   "SELECT sample_id FROM samples WHERE sample_status = ?sStatus?\n",
		},
		{
			name:       "blank_line_between_header_statements",
			content:    ":DSN := conn;\n\n:NULLASBLANK := true;\nSELECT 1 FROM DUAL\n",
			wantHeader: ":DSN := conn;\n\n:NULLASBLANK := true;\n",
			wantBody:   "SELECT 1 FROM DUAL\n",
		},
		{
			name:       "no_header",
			content:    "SELECT sample_id FROM samples\n",
			wantHeader: "",
			wantBody:   "SELECT sample_id FROM samples\n",
		},
		{
			name:       "ssl_keyword_is_not_header",
			content:    ":DECLARE nX;\nnX := 1;\n",
			wantHeader: "",
			wantBody:   ":DECLARE nX;\nnX := 1;\n",
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			header, body := SplitDataSourceHeader(tc.content)
			if header != tc.wantHeader {
				t.Errorf("header: want %q got %q", tc.wantHeader, header)
			}
			if body != tc.wantBody {
				t.Errorf("body: want %q got %q", tc.wantBody, body)
			}
		})
	}
}

// A hybrid data source — builder directives followed by raw SQL — keeps
// diagnostics on its header while the SQL body is fully suppressed: no
// dot-property, bare-AND, or unknown-token diagnostics on SQL syntax.
// [spec feature.diagnostics_pipeline/A13]
func TestGetDiagnostics_HybridDataSourceSuppressesSQLBody(t *testing.T) {
	hybrid := ":DSN := myConnection;\n" +
		":TABLENAME := samples;\n" +
		"SELECT s.sample_id FROM samples s WHERE s.sample_status = ? AND s.created_on < ?\n"

	opts := DefaultDiagnosticOptions()
	opts.IsDataSourceFile = true
	diags := GetDiagnostics(hybrid, opts)
	for _, d := range diags {
		if d.Range.Start.Line >= 2 {
			t.Errorf("diagnostic fired on the SQL body: [%d] %s: %s", d.Range.Start.Line, d.Code, d.Message)
		}
	}
}

// The header of a hybrid data source is still checked — a lowercase builder
// directive keeps its diagnostic. [spec feature.diagnostics_pipeline/A13]
func TestGetDiagnostics_HybridDataSourceHeaderStillChecked(t *testing.T) {
	hybrid := ":dsn := myConnection;\n" +
		"SELECT sample_id FROM samples WHERE sample_status = ?\n"

	opts := DefaultDiagnosticOptions()
	opts.IsDataSourceFile = true
	diags := GetDiagnostics(hybrid, opts)
	if len(diags) == 0 {
		t.Fatal("expected a diagnostic on the lowercase :dsn directive")
	}
	for _, d := range diags {
		if d.Range.Start.Line >= 1 {
			t.Errorf("diagnostic fired on the SQL body: [%d] %s: %s", d.Range.Start.Line, d.Code, d.Message)
		}
	}
}

// IsSQLModeDataSource classifies both pure-SQL and directive-headed
// data-source content as SQL mode; SSL content stays SSL. (issues #84/#104)
func TestIsSQLModeDataSource(t *testing.T) {
	sqlMode := []string{
		"SELECT sample_id FROM samples WHERE sample_status = :status\n",
		":DSN := conn;\nSELECT sample_id FROM samples\n",
		":PARAMETERS sStatus := \"A\";\nSELECT sample_id FROM samples WHERE sample_status = ?sStatus?\n",
	}
	for _, c := range sqlMode {
		if !IsSQLModeDataSource(c) {
			t.Errorf("IsSQLModeDataSource(%q) = false, want true", c)
		}
	}

	sslMode := []string{
		":PARAMETERS sStatus := \"A\";\n:DECLARE aRes;\naRes := SQLExecute(\"SELECT 1 FROM DUAL\");\n",
		":DSN := conn;\n:DECLARE nX;\nnX := 1;\n",
		":PROCEDURE P;\n:ENDPROC;\n",
	}
	for _, c := range sslMode {
		if IsSQLModeDataSource(c) {
			t.Errorf("IsSQLModeDataSource(%q) = true, want false", c)
		}
	}
}
