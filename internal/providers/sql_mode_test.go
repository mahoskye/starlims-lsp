package providers

import (
	"strings"
	"testing"
)

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
			// A terminated leading comment is masked to blanks inside the
			// header — positions preserved, content hidden from SSL checks
			// (issue #148).
			name:       "banner_then_directive",
			content:    "/* banner */\n:DSN := conn;\nSELECT 1 FROM DUAL\n",
			wantHeader: "            \n:DSN := conn;\n",
			wantBody:   "SELECT 1 FROM DUAL\n",
		},
		{
			// Unterminated SSL comment: nothing is masked, no header found,
			// content returned unchanged.
			name:       "ssl_comment_not_masked",
			content:    "/* doc;\n:DSN := conn;\nSELECT 1 FROM DUAL\n",
			wantHeader: "",
			wantBody:   "/* doc;\n:DSN := conn;\nSELECT 1 FROM DUAL\n",
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

// bannerComment is the canonical data-source header comment shape
// (style-guide schema data_source_modules structure: header_comment).
const bannerComment = `/*********************************************************
Description.. :

DS Type...... : SQL
Author....... : Name
Date......... : 2026-05-08
Parameters... : -

*********************************************************/
`

// A data-source document containing only terminated SQL comments and
// whitespace produces zero diagnostics — it is the schema's optional
// header_comment with nothing else yet (issue #148). The unterminated SSL
// comment form is NOT comment-classified and keeps SSL diagnostics.
// [spec feature.diagnostics_pipeline/A16]
func TestGetDiagnostics_CommentOnlyDataSourceProducesNone(t *testing.T) {
	opts := DefaultDiagnosticOptions()
	opts.IsDataSourceFile = true

	for name, content := range map[string]string{
		"banner":            bannerComment,
		"banner_whitespace": bannerComment + "\n\n",
		"line_comments":     "-- placeholder\n-- not written yet\n",
		"mixed":             "/* block */\n-- line\n",
	} {
		if diags := GetDiagnostics(content, opts); len(diags) != 0 {
			t.Errorf("%s: expected no diagnostics for comment-only data source, got %+v", name, diags)
		}
	}

	// SSL comment form (no */): stays SSL, keeps its diagnostics.
	if diags := GetDiagnostics("/* ssl comment without terminator\n", opts); len(diags) == 0 {
		t.Error("unterminated SSL-form comment should keep SSL diagnostics")
	}
}

// A terminated banner comment before the builder-directive header does not
// defeat hybrid detection: nothing fires on the comment or the SQL body,
// while header lines keep their checks at unshifted positions.
// [spec feature.diagnostics_pipeline/A17]
func TestGetDiagnostics_BannerBeforeHeaderKeepsHybridDetection(t *testing.T) {
	opts := DefaultDiagnosticOptions()
	opts.IsDataSourceFile = true

	clean := bannerComment + ":DSN := conn;\n:PARAMETERS sStatus;\n\nSELECT s.sample_id FROM samples s WHERE s.sample_status = @sStatus\n"
	if diags := GetDiagnostics(clean, opts); len(diags) != 0 {
		t.Errorf("expected no diagnostics for banner+header+SQL data source, got %+v", diags)
	}

	// A lowercase directive after the banner still flags, on its own line.
	flagged := bannerComment + ":dsn := conn;\nSELECT sample_id FROM samples\n"
	diags := GetDiagnostics(flagged, opts)
	if len(diags) == 0 {
		t.Fatal("expected a diagnostic on the lowercase :dsn directive after a banner")
	}
	bannerLines := strings.Count(bannerComment, "\n")
	for _, d := range diags {
		if d.Range.Start.Line != bannerLines {
			t.Errorf("diagnostic not on the :dsn line (want line %d): [%d] %s: %s", bannerLines, d.Range.Start.Line, d.Code, d.Message)
		}
	}
}

// A recognized header with nothing after it (directives-only stub,
// optionally behind a banner) keeps header checks and produces no
// comment diagnostics. [spec feature.diagnostics_pipeline/A18]
func TestGetDiagnostics_HeaderOnlyDataSource(t *testing.T) {
	opts := DefaultDiagnosticOptions()
	opts.IsDataSourceFile = true

	clean := bannerComment + ":DSN := conn;\n:PARAMETERS sStatus;\n"
	if diags := GetDiagnostics(clean, opts); len(diags) != 0 {
		t.Errorf("expected no diagnostics for banner+header-only data source, got %+v", diags)
	}

	flagged := bannerComment + ":dsn := conn;\n"
	if diags := GetDiagnostics(flagged, opts); len(diags) == 0 {
		t.Error("expected header checks to still run on a header-only data source")
	}
}

// IsSQLCommentOnly accepts terminated SQL comments and whitespace only —
// never the unterminated SSL comment form, never real content.
// [spec feature.diagnostics_pipeline/A16]
func TestIsSQLCommentOnly(t *testing.T) {
	cases := []struct {
		name    string
		content string
		want    bool
	}{
		{"banner", bannerComment, true},
		{"line_comment", "-- note\n", true},
		{"line_comment_no_newline", "-- note", true},
		{"mixed_comments", "/* a */\n-- b\n/* c */", true},
		{"empty", "", false},
		{"whitespace_only", "  \n\t\n", false},
		{"ssl_comment", "/* datasource doc;\n", false},
		{"comment_then_sql", "/* a */\nSELECT 1 FROM DUAL", false},
		{"comment_then_directive", "/* a */\n:DSN := conn;", false},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			if got := IsSQLCommentOnly(tc.content); got != tc.want {
				t.Errorf("IsSQLCommentOnly(%q) = %v, want %v", tc.content, got, tc.want)
			}
		})
	}
}

// IsSQLModeDataSource classifies both pure-SQL and directive-headed
// data-source content as SQL mode; SSL content stays SSL. (issues #84/#104)
func TestIsSQLModeDataSource(t *testing.T) {
	sqlMode := []string{
		"SELECT sample_id FROM samples WHERE sample_status = :status\n",
		":DSN := conn;\nSELECT sample_id FROM samples\n",
		":PARAMETERS sStatus := \"A\";\nSELECT sample_id FROM samples WHERE sample_status = ?sStatus?\n",
		bannerComment,
		bannerComment + "SELECT sample_id FROM samples WHERE sample_status = @sStatus\n",
		bannerComment + ":DSN := conn;\nSELECT sample_id FROM samples\n",
		bannerComment + ":DSN := conn;\n:PARAMETERS sStatus;\n",
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
