package providers

import "testing"

func unterminatedStrings(t *testing.T, script string, opts DiagnosticOptions) []Diagnostic {
	t.Helper()
	var out []Diagnostic
	for _, d := range GetDiagnostics(script, opts) {
		if d.Code == CodeUnterminatedString {
			out = append(out, d)
		}
	}
	return out
}

// [spec diag.unterminated_string] The range covers the opening delimiter
// and the message names the closer that never came.
func TestUnterminatedString_FlagsOnOpener(t *testing.T) {
	cases := []struct {
		name   string
		script string
		line   int // 0-based
		col    int // 0-based
		want   string
	}{
		{"double quote", ":DECLARE sText;\nsText := \"never closed;\n", 1, 9,
			"Unterminated string literal - expected a closing '\"' before end of file"},
		{"issue prose apostrophe", ":DECLARE nCount;\nnCount := 1;\nThis is a bunch of text, but it doesn't evaluate as wrong.\nnCount := 2;\n", 2, 37,
			"Unterminated string literal - expected a closing ''' before end of file"},
		{"single quote", ":DECLARE sName;\nsName := 'never closed;\n", 1, 9,
			"Unterminated string literal - expected a closing ''' before end of file"},
		{"bracket", ":DECLARE sSql;\nsSql := [select 1 from dual;\n", 1, 8,
			"Unterminated string literal - expected a closing ']' before end of file"},
		{"nested bracket ending in ]", ":DECLARE sText;\nsText := [[a]", 1, 9,
			"Unterminated string literal - expected a closing ']' before end of file"},
	}
	for _, c := range cases {
		got := unterminatedStrings(t, c.script, DefaultDiagnosticOptions())
		if len(got) != 1 {
			t.Errorf("%s: want 1 diagnostic, got %d: %+v", c.name, len(got), got)
			continue
		}
		d := got[0]
		if d.Severity != SeverityError || d.Source != "ssl-lsp" {
			t.Errorf("%s: severity/source %v/%q", c.name, d.Severity, d.Source)
		}
		if d.Range.Start.Line != c.line || d.Range.Start.Character != c.col || d.Range.End.Character != c.col+1 {
			t.Errorf("%s: range %+v, want line %d col %d..%d", c.name, d.Range, c.line, c.col, c.col+1)
		}
		if d.Message != c.want {
			t.Errorf("%s:\n  got  %s\n  want %s", c.name, d.Message, c.want)
		}
	}
}

// [spec diag.unterminated_string] The string runs to end of file, so there
// is at most one per document; a quote inside the swallowed text is
// content.
func TestUnterminatedString_OnePerDocument(t *testing.T) {
	script := ":DECLARE sA, sB;\nsA := \"x;\n:IF sA;\nsB := 'y;\n:ENDIF;\n"
	got := unterminatedStrings(t, script, DefaultDiagnosticOptions())
	if len(got) != 1 || got[0].Range.Start.Line != 1 {
		t.Fatalf("want exactly one diagnostic on line 1, got %+v", got)
	}
}

// [spec diag.unterminated_string] Closed strings of any length, quotes in
// comments, the other quote kind inside a string, closed nested bracket
// strings, subscripts, and adjacent strings never flag.
func TestUnterminatedString_DoesNotFlag(t *testing.T) {
	clean := map[string]string{
		"multi-line SQL":     ":DECLARE sSql, aRows;\nsSql := \"select ITEMID, DISPLAYTEXT\n\tfrom ITEMS\n\twhere DONE = '0'\";\naRows := SQLExecute(sSql);\n",
		"quotes elsewhere":   ":DECLARE sA, sB, sC;\n/* don't flag a quote in a comment;\nsA := \"it's\";\nsB := 'say \"hi\"';\nsC := [it's \"quoted\" [and nested]];\n",
		"subscript+adjacent": ":DECLARE aItems, sFirst, sBoth;\nsFirst := aItems[1];\nsBoth := \"a\"\"b\";\n",
		"unclosed subscript": ":DECLARE aItems, sFirst;\nsFirst := aItems[1;\n",
		"empty string":       ":DECLARE sA;\nsA := \"\";\n",
	}
	for name, script := range clean {
		if got := unterminatedStrings(t, script, DefaultDiagnosticOptions()); len(got) != 0 {
			t.Errorf("%s: unexpected diagnostics: %+v", name, got)
		}
	}
	// The unclosed subscript is unclosed_delimiter's, and it does fire.
	found := false
	for _, d := range GetDiagnostics(clean["unclosed subscript"], DefaultDiagnosticOptions()) {
		if d.Code == CodeUnclosedDelimiter {
			found = true
		}
	}
	if !found {
		t.Errorf("unclosed subscript: unclosed_delimiter did not fire")
	}
}

// [spec diag.unterminated_string] The rule applies to whatever the pipeline
// lexes as SSL: an SSL-mode data source in full, a hybrid's header only,
// and never a SQL-mode body.
func TestUnterminatedString_DataSourceRouting(t *testing.T) {
	ds := DefaultDiagnosticOptions()
	ds.IsDataSourceFile = true

	sslMode := ":PARAMETERS sName;\nsName := 'never closed;\n"
	if got := unterminatedStrings(t, sslMode, ds); len(got) != 1 {
		t.Errorf("SSL-mode data source: want 1, got %+v", got)
	}
	hybrid := ":PARAMETERS sMode := \"Post\";\nselect x from t where y = 'a\n"
	if got := unterminatedStrings(t, hybrid, ds); len(got) != 0 {
		t.Errorf("hybrid data source (header lexed only): want 0, got %+v", got)
	}
	sqlMode := "select x from t where y = 'it''s\n"
	if got := unterminatedStrings(t, sqlMode, ds); len(got) != 0 {
		t.Errorf("SQL-mode data source (never lexed): want 0, got %+v", got)
	}
}

// [spec diag.unterminated_string] On the issue's own example both findings
// surface: the prose at `is`, and the apostrophe that hid everything after.
func TestUnterminatedString_IssueProseGetsBothFindings(t *testing.T) {
	script := ":DECLARE nCount;\nnCount := 1;\nThis is a bunch of text, but it doesn't evaluate as wrong.\nnCount := 2;\n"
	codes := map[string]bool{}
	for _, d := range GetDiagnostics(script, DefaultDiagnosticOptions()) {
		codes[d.Code] = true
	}
	if !codes[CodeUnexpectedToken] || !codes[CodeUnterminatedString] {
		t.Errorf("want both unexpected_token and unterminated_string, got %v", codes)
	}
}
