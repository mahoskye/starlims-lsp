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

// A SQL data source whose SELECT list uses implicit column aliases
// (`col alias`, no AS) — a shape that defeats strict SQL-statement
// validation — still classifies as SQL because a .ds file is SQL unless
// its body carries a strong SSL marker. Its lowercase `and`, dotted column
// names, and joins must not draw any SSL diagnostic, and in particular not
// bare_logical_operator. [spec feature.diagnostics_pipeline/A22] (issue #153)
func TestGetDiagnostics_SQLDataSource_ImplicitAliasesStaySQL(t *testing.T) {
	opts := DefaultDiagnosticOptions()
	opts.IsDataSourceFile = true

	// Whole-document SQL body: only the SQL-body checks may fire. Here the
	// @patOrigRec placeholder is undeclared (no :PARAMETERS header), so the
	// sole diagnostic is datasource_undeclared_placeholder — never an SSL
	// diagnostic on the `and` predicate.
	body := `SELECT p.ORIGREC PatientId, v.VISITDATE VisitDate
FROM PATIENTS p
INNER JOIN VISITS v ON v.PATID = p.ORIGREC
WHERE p.STATUS = 'A'
and PATIENTS.ORIGREC = @patOrigRec
ORDER BY v.VISITDATE`
	for _, d := range GetDiagnostics(body, opts) {
		if d.Source == "ssl-lsp" && d.Code != CodeDatasourceUndeclaredPlaceholder {
			t.Errorf("no SSL diagnostic should fire on an implicit-alias SQL data source, got %s: %s", d.Code, d.Message)
		}
		if d.Code == CodeBareLogicalOperator {
			t.Errorf("issue #153: bare_logical_operator must not fire on SQL `and`: %+v", d)
		}
	}

	// Hybrid shape: a :PARAMETERS header declares the placeholder, so the
	// implicit-alias body draws no diagnostic at all.
	hybrid := `:PARAMETERS patOrigRec;

SELECT p.ORIGREC PatientId
FROM PATIENTS p
WHERE p.STATUS = 'A' and PATIENTS.ORIGREC = @patOrigRec`
	if diags := GetDiagnostics(hybrid, opts); len(diags) != 0 {
		t.Errorf("hybrid implicit-alias data source with declared placeholder should be clean, got %+v", diags)
	}
}

// A `.ds` file whose lowercase `and` appears in an SSL script — not SQL —
// still draws bare_logical_operator: the body carries strong SSL markers
// (colon keywords and `:=`), so it classifies as SSL. This is the inverse
// of the issue #153 case and pins that the SQL default does not swallow
// genuine SSL. [spec feature.diagnostics_pipeline/A22] (issue #153)
func TestGetDiagnostics_SSLDataSource_BareAndStillFlags(t *testing.T) {
	opts := DefaultDiagnosticOptions()
	opts.IsDataSourceFile = true

	ssl := `:PARAMETERS pStatus;
:DECLARE aOut;
:IF pStatus == "A" and Len(pStatus) > 0;
	aOut := 1;
:ENDIF;`
	found := false
	for _, d := range GetDiagnostics(ssl, opts) {
		if d.Code == CodeBareLogicalOperator {
			found = true
		}
	}
	if !found {
		t.Error("bare `and` in an SSL-marker-bearing .ds must still flag bare_logical_operator")
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
			// An SSL-style banner above a directive header is header
			// furniture, the same as the terminated form above: masked to
			// blanks so the header scan reaches the directives beneath it.
			// It used to abort the scan, dropping the directives into the
			// body where their `:=` read as a strong SSL marker and the
			// SQL body was lexed as SSL (issue #249).
			name:       "ssl_banner_then_directive",
			content:    "/* doc;\n:DSN := conn;\nSELECT 1 FROM DUAL\n",
			wantHeader: "       \n:DSN := conn;\n",
			wantBody:   "SELECT 1 FROM DUAL\n",
		},
		{
			// With no header statement beneath it the banner is not
			// furniture, so nothing is masked and the document keeps the
			// shape the unterminated-comment rule reads as SSL.
			name:       "ssl_comment_without_header_not_masked",
			content:    "/* doc;\nSELECT 1 FROM DUAL\n",
			wantHeader: "",
			wantBody:   "/* doc;\nSELECT 1 FROM DUAL\n",
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
		// Implicit column aliases defeat strict SQL validation but carry no
		// SSL marker, so a .ds file with them is still SQL (issue #153).
		"SELECT p.ORIGREC PatientId FROM PATIENTS p WHERE p.STATUS = 'A' and p.ORIGREC = @p\n",
		":PARAMETERS p;\nSELECT o.ordno OrderNo FROM orders o WHERE o.x = @p and o.y = 1\n",
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
		// No colon keyword, but the body's `:=` assignment is a strong SSL
		// marker (issue #153; the A11 shape).
		":PARAMETERS sStatus;\noFilter.Value := sStatus;\n",
		// Unterminated SSL comment leads the document (A16).
		"/* not closed the SQL way\n",
	}
	for _, c := range sslMode {
		if IsSQLModeDataSource(c) {
			t.Errorf("IsSQLModeDataSource(%q) = true, want false", c)
		}
	}
}

// A SQL data-source body stays SQL-classified when a column or table name
// collides with a SQL builtin-function name (`set FORMAT = …`) and when
// string literals or SQL comments contain semicolons — the UpdateDocTypes
// shape from issue #154. None of these may fall back to SSL parsing, so
// none may produce any diagnostic. [spec feature.diagnostics_pipeline/A19]
func TestGetDiagnostics_SQLDataSource_FunctionNameColumnsAndInertSemicolons(t *testing.T) {
	opts := DefaultDiagnosticOptions()
	opts.IsDataSourceFile = true

	for name, content := range map[string]string{
		"function_name_column": "update DOCTYPESCONVERSION\nset FORMAT = 'all;msoffice->pdf'\nwhere ORIGREC = 1\n",
		"function_name_table":  "delete from FORMAT where ORIGREC = 1\n",
		"block_comment":        "/* examples: 'all;msoffice->pdf' and 'doc->pdf'; keep in sync */\nupdate DOCTYPESCONVERSION\nset FORMAT = 'all;msoffice->pdf'\nwhere ORIGREC = 1\n",
		"line_comment":         "update DOCTYPESCONVERSION\nset FORMAT = 'all;msoffice->pdf' -- default is 'all;msoffice->pdf'; keep\nwhere ORIGREC = 1\n",
		"hybrid":               ":DSN := \"LimsDB\";\n:PARAMETERS pDocType;\n\nupdate DOCTYPESCONVERSION\nset FORMAT = 'all;msoffice->pdf' /* e.g. 'a;b'; note */\nwhere DOCTYPE = @pDocType\n",
	} {
		if diags := GetDiagnostics(content, opts); len(diags) != 0 {
			t.Errorf("%s: expected no diagnostics, got %+v", name, diags)
		}
	}
}

// A bare `;` outside comments and string literals in a SQL-mode
// data-source body warns with datasource_sql_semicolon at the semicolon's
// position — including in the hybrid shape, where positions are offset
// past the directive header and the header's own `;` terminators never
// flag. The warning honors rule overrides and never fires outside
// data-source files. [spec feature.diagnostics_pipeline/A20]
func TestGetDiagnostics_DataSourceSQLSemicolon(t *testing.T) {
	opts := DefaultDiagnosticOptions()
	opts.IsDataSourceFile = true

	collect := func(content string, o DiagnosticOptions) []Diagnostic {
		var hits []Diagnostic
		for _, d := range GetDiagnostics(content, o) {
			if d.Code == CodeDatasourceSQLSemicolon {
				hits = append(hits, d)
			}
		}
		return hits
	}

	plain := "update A set X = 1;\nupdate B set Y = 2\n"
	hits := collect(plain, opts)
	if len(hits) != 1 {
		t.Fatalf("plain: expected exactly one separator warning, got %+v", hits)
	}
	// Error, not warning: STARLIMS refuses the document outright with
	// "Invalid SQL statement: remove any misplaced semicolons(;)"
	// (issue #249).
	if hits[0].Severity != SeverityError {
		t.Errorf("plain: expected error severity, got %d", hits[0].Severity)
	}
	if hits[0].Range.Start.Line != 0 || hits[0].Range.Start.Character != 18 {
		t.Errorf("plain: expected range at 0:18 (the ';'), got %+v", hits[0].Range)
	}

	hybrid := ":DSN := \"LimsDB\";\n:PARAMETERS p1;\n\nselect A from B where C = @p1;\n"
	hits = collect(hybrid, opts)
	if len(hits) != 1 {
		t.Fatalf("hybrid: expected exactly one separator warning (header ';' must not flag), got %+v", hits)
	}
	if hits[0].Range.Start.Line != 3 || hits[0].Range.Start.Character != 29 {
		t.Errorf("hybrid: expected range at 3:29 (the body ';'), got %+v", hits[0].Range)
	}

	override := opts
	override.RuleOverrides = map[string]string{CodeDatasourceSQLSemicolon: "off"}
	if hits := collect(plain, override); len(hits) != 0 {
		t.Errorf("override off: expected no separator warnings, got %+v", hits)
	}

	ssl := DefaultDiagnosticOptions()
	if hits := collect(":DECLARE nX;\nnX := 1;\n", ssl); len(hits) != 0 {
		t.Errorf("ssl file: separator warning must not fire outside data sources, got %+v", hits)
	}
}

// A @name placeholder in a SQL-mode data-source body with no matching
// :PARAMETERS declaration warns with datasource_undeclared_placeholder;
// declared placeholders (any casing), @@system functions, @name inside
// literals and comments, unused declared parameters, and DECLARE-scripted
// bodies stay silent. [spec feature.diagnostics_pipeline/A21]
func TestGetDiagnostics_DataSourceUndeclaredPlaceholder(t *testing.T) {
	opts := DefaultDiagnosticOptions()
	opts.IsDataSourceFile = true

	collect := func(content string) []Diagnostic {
		var hits []Diagnostic
		for _, d := range GetDiagnostics(content, opts) {
			if d.Code == CodeDatasourceUndeclaredPlaceholder {
				hits = append(hits, d)
			}
		}
		return hits
	}

	// Hybrid shape: typo'd placeholder flags at its position, offset past
	// the header; the declared one does not.
	hybrid := ":DSN := \"LimsDB\";\n:PARAMETERS pFolderNo;\n\nselect SAMPLEID from SAMPLES\nwhere FOLDERNO = @pFolder and STATUS = @pFolderNo\n"
	hits := collect(hybrid)
	if len(hits) != 1 {
		t.Fatalf("hybrid: expected exactly one undeclared-placeholder warning, got %+v", hits)
	}
	if hits[0].Severity != SeverityWarning {
		t.Errorf("hybrid: expected warning severity, got %d", hits[0].Severity)
	}
	if hits[0].Range.Start.Line != 4 || hits[0].Range.Start.Character != 17 || hits[0].Range.End.Character != 25 {
		t.Errorf("hybrid: expected range 4:17-4:25 (the '@pFolder'), got %+v", hits[0].Range)
	}

	// Whole-document SQL body with no header: every real placeholder is
	// undeclared.
	if hits := collect("select SAMPLEID from SAMPLES where FOLDERNO = @pFolderNo\n"); len(hits) != 1 {
		t.Errorf("headerless: expected one undeclared-placeholder warning, got %+v", hits)
	}

	for name, content := range map[string]string{
		"case_insensitive_match": ":PARAMETERS pFolderNo;\n\nselect SAMPLEID from SAMPLES where FOLDERNO = @PFOLDERNO\n",
		"default_with_commas":    ":PARAMETERS pIds := {1,2}, pStatus := Foo(1, 2);\n\nselect SAMPLEID from SAMPLES where STATUS = @pStatus and ORIGREC in (@pIds)\n",
		"system_function":        ":PARAMETERS pFolderNo;\n\nselect @@ROWCOUNT from SAMPLES where FOLDERNO = @pFolderNo\n",
		"literal_and_comment":    "select SAMPLEID from SAMPLES where NOTE = '@pX' -- mentions @pY\n",
		"unused_declared":        ":PARAMETERS pFolderNo;\n\nselect SAMPLEID from SAMPLES\n",
	} {
		if hits := collect(content); len(hits) != 0 {
			t.Errorf("%s: expected no undeclared-placeholder warnings, got %+v", name, hits)
		}
	}

	// A body that declares its own SQL variables bows the check out
	// entirely (tested against the helper directly: a DECLARE-first body
	// does not classify as SQL mode, so the wired path never sees one
	// with a leading DECLARE).
	if hits := checkDataSourceUndeclaredPlaceholders("declare @pLocal int\nselect SAMPLEID from SAMPLES where FOLDERNO = @pLocal and X = @pOther\n", nil, 0); len(hits) != 0 {
		t.Errorf("declare_scripted_body: expected bow-out, got %+v", hits)
	}

	// A DECLARE inside a comment or literal is content and does NOT bow
	// the check out.
	if hits := collect("select SAMPLEID from SAMPLES where NOTE = 'declare' and FOLDERNO = @pFolderNo -- declare\n"); len(hits) != 1 {
		t.Errorf("declare_in_content: expected one undeclared-placeholder warning, got %+v", hits)
	}
}

// A banner closing `*/;` masks including the `;`, so header detection
// still splits and the SQL body stays SQL. The corpus shape: boxed
// banner, `:PARAMETERS` with inline := defaults, multi-line SELECT with
// qualified columns, SQL and/or, and || concatenation.
//
// Masking is a parsing convenience only. In a SQL-mode document that
// `;` is a misplaced statement separator that STARLIMS rejects with
// "Invalid SQL statement: remove any misplaced semicolons(;)", so it is
// reported as an error even though the split succeeds (issue #249).
// [spec feature.diagnostics_pipeline/A24]
func TestGetDiagnostics_BannerClosingSemicolonKeepsHybridDetection(t *testing.T) {
	opts := DefaultDiagnosticOptions()
	opts.IsDataSourceFile = true

	banner := "/*******************************************************\n" +
		"Description.. : sample listing\n" +
		"Author....... : someone\n" +
		"*******************************************************/;\n"
	sql := "SELECT s.SAMPLE_ID, s.STATUS, t.NAME || ' (' || t.CODE || ')'\n" +
		"FROM SAMPLES s, TESTS t\n" +
		"WHERE s.TEST_ID = t.TEST_ID and s.STATUS = ?sStatus? or s.RUSH = 'Y'\n"

	onlySemicolon := func(label, content string) {
		t.Helper()
		diags := GetDiagnostics(content, opts)
		if len(diags) != 1 {
			t.Fatalf("%s: expected exactly the misplaced-semicolon error, got %+v", label, diags)
		}
		if diags[0].Code != CodeDatasourceSQLSemicolon {
			t.Errorf("%s: expected %s, got %s", label, CodeDatasourceSQLSemicolon, diags[0].Code)
		}
		if diags[0].Severity != SeverityError {
			t.Errorf("%s: expected error severity, got %d", label, diags[0].Severity)
		}
	}

	// The split still works — the SQL body draws no SSL diagnostics —
	// but the banner's `;` is reported.
	clean := banner + ":PARAMETERS sStatus := \"Done\", nLimit := 100;\n\n" + sql
	onlySemicolon("banner-*/;", clean)

	// Spaces before the `;` are the same idiom, and the same error.
	spaced := strings.Replace(banner, "*/;", "*/  ;", 1) + ":PARAMETERS sStatus;\n\n" + sql
	onlySemicolon("banner-*/ ;", spaced)

	// Dropping the `;` is the fix, and leaves the document clean.
	fixed := strings.Replace(banner, "*/;", "*/", 1) + ":PARAMETERS sStatus := \"Done\";\n\n" + sql
	if diags := GetDiagnostics(fixed, opts); len(diags) != 0 {
		t.Errorf("banner closed with plain */ should be clean, got %+v", diags)
	}

	// Header checks still run after the masked banner residue: a
	// lowercase directive flags on its own line.
	flagged := banner + ":parameters sStatus;\n\n" + sql
	if diags := GetDiagnostics(flagged, opts); len(diags) == 0 {
		t.Error("expected header checks to still run after a banner closing */;")
	}
}

// `--` line comments interleaved between header statements are header
// furniture when another header statement follows; a trailing comment run
// before the body belongs to the body.
// [spec feature.diagnostics_pipeline/A25]
func TestGetDiagnostics_CommentsBetweenHeaderStatements(t *testing.T) {
	opts := DefaultDiagnosticOptions()
	opts.IsDataSourceFile = true

	clean := ":DSN := DICTIONARY;\n:TABLENAME := PendingItems;\n\n" +
		"--default UserName value MUST BE NIL\n" +
		":PARAMETERS UserName := NIL;\n\n" +
		"SELECT p.ITEM_ID FROM PENDING p WHERE p.USR = @UserName and p.DONE = 'N'\n"
	if diags := GetDiagnostics(clean, opts); len(diags) != 0 {
		t.Errorf("expected no diagnostics for comment-interleaved header, got %+v", diags)
	}

	header, _ := SplitDataSourceHeader(clean)
	if !strings.Contains(header, "PARAMETERS") {
		t.Errorf("header split stopped at the interleaved comment: %q", header)
	}

	// A comment run followed by SQL (not a header statement) stays with
	// the body — the body must still start at the comment line.
	trailing := ":DSN := DICTIONARY;\n--just a note about the query\nSELECT 1 FROM DUAL\n"
	_, body := SplitDataSourceHeader(trailing)
	if !strings.HasPrefix(strings.TrimSpace(body), "--just a note") {
		t.Errorf("trailing comment should stay with the body, body = %q", body)
	}
}

// A commented-out directive in the body is never a strong SSL marker; a
// mid-line `--` does not blank marker detection.
// [spec feature.diagnostics_pipeline/A26]
func TestGetDiagnostics_CommentedOutDirectiveIsNotSSLMarker(t *testing.T) {
	opts := DefaultDiagnosticOptions()
	opts.IsDataSourceFile = true

	clean := ":PARAMETERS sFolderNo := '12345';\n" +
		"--:PARAMETERS sFolderNo := '';\n" +
		"SELECT f.FOLDER_NO, f.STATUS FROM FOLDERS f WHERE f.FOLDER_NO = ?sFolderNo?\n"
	if diags := GetDiagnostics(clean, opts); len(diags) != 0 {
		t.Errorf("expected SQL mode for body with commented-out directive, got %+v", diags)
	}

	// Mid-line `--` does not hide a genuine SSL marker on the same line.
	ssl := ":PARAMETERS nCount;\n:DECLARE nTotal;\nnTotal := nCount - -1;\n:RETURN nTotal;\n"
	if !hasStrongSSLMarker(ssl) {
		t.Error("SSL body with mid-line dashes should keep its strong marker")
	}
}

// [spec feature.diagnostics_pipeline/A31] A banner in SSL comment style
// above a directive header is furniture: the header still splits, so a
// SQL body stays in SQL mode. The headerless shapes the
// unterminated-comment rule was written for are unaffected (issue #249).
func TestSQLModeDataSource_SSLStyleBannerAboveHeader(t *testing.T) {
	cases := []struct {
		name    string
		content string
		wantSQL bool
	}{
		{
			name:    "ssl_banner_over_directives_and_sql",
			content: "/* DATA SOURCE: X;\n:DSN := conn;\n:PARAMETERS s := \"A\";\n\nSELECT a FROM b WHERE c = ?s?\n",
			wantSQL: true,
		},
		{
			name:    "multiline_ssl_banner_over_directives",
			content: "/* ====;\n/* DATA SOURCE: X;\n/* ====;\n:DSN := conn;\n\nSELECT a FROM b\n",
			wantSQL: true,
		},
		{
			name:    "c_style_banner_unchanged",
			content: "/* DATA SOURCE: X */\n:DSN := conn;\n\nSELECT a FROM b\n",
			wantSQL: true,
		},
		{
			// A16: a comment-only stub has no header, so the
			// unterminated-comment rule still decides, and still says SSL.
			name:    "comment_only_stub_stays_ssl",
			content: "/* just a note about this data source\n",
			wantSQL: false,
		},
		{
			// A22: a real SSL data source keeps its SSL classification —
			// its body carries strong markers whatever the banner does.
			name:    "ssl_data_source_stays_ssl",
			content: "/* banner;\n:PARAMETERS s := \"A\";\n\n:DECLARE x;\nx := 1;\n:RETURN x;\n",
			wantSQL: false,
		},
		{
			name:    "ssl_banner_without_header_stays_ssl",
			content: "/* doc;\nSELECT 1 FROM DUAL\n",
			wantSQL: false,
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			if got := IsSQLModeDataSource(tc.content); got != tc.wantSQL {
				t.Errorf("IsSQLModeDataSource = %v, want %v", got, tc.wantSQL)
			}
		})
	}
}

// [spec feature.diagnostics_pipeline/A31] The end-to-end consequence: the
// SQL body of such a document draws no SSL diagnostics.
func TestDiagnostics_SSLBannerDataSourceBodyIsNotLexedAsSSL(t *testing.T) {
	content := "/* DATA SOURCE: X;\n:DSN := conn;\n:PARAMETERS sStatus := \"A\";\n\nSELECT io.name FROM sample WHERE status = ?sStatus? AND a = 1\n"
	opts := DefaultDiagnosticOptions()
	opts.IsDataSourceFile = true
	opts.IncludeInfoDiagnostics = true
	for _, d := range GetDiagnostics(content, opts) {
		t.Errorf("SQL body drew an SSL diagnostic: %s — %s", d.Code, d.Message)
	}
}

// [spec diag.datasource_sql_semicolon] The rule is about the semicolon,
// not about the `*/;` spelling: any separator with no statement in front
// of it is stray, wherever it sits in a SQL-mode document. A stray one
// also must not abort the header scan, which used to drop the directives
// into the body and misclassify the document as SSL (issue #249).
func TestDataSourceStraySemicolonShapes(t *testing.T) {
	opts := DefaultDiagnosticOptions()
	opts.IsDataSourceFile = true
	opts.IncludeInfoDiagnostics = true

	body := "\nSELECT a FROM b\n"
	stray := []struct{ name, content string }{
		{"adjacent to the banner close", "/* X\n*/;\n:DSN := conn;\n" + body},
		{"on its own line after the banner", "/* X\n*/\n;\n:DSN := conn;\n" + body},
		{"after a -- banner", "-- X\n;\n:DSN := conn;\n" + body},
		{"doubled between directives", ":DSN := conn;;\n:PARAMETERS s := \"A\";\n" + body},
		{"headerless, after a leading comment", "/* X\n*/;\nSELECT a FROM b\n"},
	}
	for _, tc := range stray {
		t.Run(tc.name, func(t *testing.T) {
			var hits int
			for _, d := range GetDiagnostics(tc.content, opts) {
				if d.Code == CodeDatasourceSQLSemicolon {
					hits++
					if d.Severity != SeverityError {
						t.Errorf("expected error severity, got %d", d.Severity)
					}
				}
			}
			if hits != 1 {
				t.Errorf("expected exactly one misplaced-semicolon error, got %d", hits)
			}
		})
	}

	clean := []struct{ name, content string }{
		{"banner closed with a plain */", "/* X\n*/\n:DSN := conn;\n" + body},
		{"directives only", ":DSN := conn;\n:PARAMETERS s := \"A\";\n" + body},
		{"-- banner, no stray", "-- X\n:DSN := conn;\n" + body},
		{"plain SQL, no header", "SELECT a FROM b WHERE c = 1\n"},
	}
	for _, tc := range clean {
		t.Run(tc.name, func(t *testing.T) {
			for _, d := range GetDiagnostics(tc.content, opts) {
				t.Errorf("expected clean, got %s: %s", d.Code, d.Message)
			}
		})
	}

	// The same banner in an SSL-mode document is correct SSL: the `;` is
	// the comment terminator, so only the stylistic advisory applies.
	sslMode := "/* X\n*/;\n:PARAMETERS s := \"A\";\n\n:DECLARE x;\nx := 1;\n"
	for _, d := range GetDiagnostics(sslMode, opts) {
		if d.Code == CodeDatasourceSQLSemicolon {
			t.Error("fired on an SSL-mode data source, where the ';' terminates the comment")
		}
	}
}

// [spec feature.diagnostics_pipeline/A22] Mode detection tests for what
// SQL cannot contain, in the position SSL puts it. An Oracle-style bind
// sits after an operator, so a bind whose name collides with an SSL
// keyword no longer reads as that keyword (issue #249).
//
// These pin the direction that matters: a document wrongly called SSL
// has its SQL keywords reported as undeclared variables, while one
// wrongly called SQL merely runs fewer checks.
func TestSQLModeDetection_MarkerPositionAndEdges(t *testing.T) {
	cases := []struct {
		name    string
		content string
		wantSQL bool
	}{
		{"plain SELECT", ":DSN := c;\n\nSELECT a FROM b WHERE x = 1\n", true},
		{"bind named like a keyword (:default)", ":DSN := c;\n\nSELECT a FROM b WHERE s = :default\n", true},
		{"bind named like a keyword (:error)", ":DSN := c;\n\nSELECT a FROM b WHERE s = :error\n", true},
		{"ordinary bind (:status)", ":DSN := c;\n\nSELECT a FROM b WHERE s = :status\n", true},
		{"':=' inside a single-quoted literal", ":DSN := c;\n\nSELECT a FROM b WHERE n = 'x := 1'\n", true},
		{"':DECLARE' inside a literal", ":DSN := c;\n\nSELECT a FROM b WHERE n = 'use :DECLARE'\n", true},
		{"':=' inside a SQL comment", ":DSN := c;\n\nSELECT a FROM b /* x := 1 */\n", true},

		{"SSL body with :DECLARE", ":PARAMETERS s;\n\n:DECLARE x;\nx := 1;\n:RETURN x;\n", false},
		{"SSL keyword after a ';' on one line", ":PARAMETERS s;\n\nx := 1; :RETURN x;\n", false},
		{"SSL body that is only :RETURN", ":PARAMETERS s;\n\n:RETURN 1;\n", false},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			if got := IsSQLModeDataSource(tc.content); got != tc.wantSQL {
				mode := map[bool]string{true: "SQL", false: "SSL"}
				t.Errorf("classified as %s, want %s", mode[got], mode[tc.wantSQL])
			}
		})
	}
}
