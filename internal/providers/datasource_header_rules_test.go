package providers

import "testing"

func codesIn(script string, opts DiagnosticOptions) map[string]int {
	got := map[string]int{}
	for _, d := range GetDiagnostics(script, opts) {
		got[d.Code]++
	}
	return got
}

// Issue #260: rules from the reference's data-sources guide and the
// :PARAMETERS page.
func TestDataSourceHeaderRules(t *testing.T) {
	ds := DefaultDiagnosticOptions()
	ds.IsDataSourceFile = true

	sql := ":DSN := conn;\n:NULLASBLANK := .F.;\n:INVARIANTDATECOLUMNS := \"logdate\", releasedate;\n:PARAMETERS sStatus := \"A\";\nSELECT id FROM item WHERE status = ?sStatus? AND x = @sStatus AND s = '?no?' -- ?nor?\n"
	got := codesIn(sql, ds)
	for code, want := range map[string]int{CodeNullAsBlankValue: 1, CodeInvariantDateColumnsQuoted: 1, CodeDatasourceSQLQuestionPlaceholder: 1} {
		if got[code] != want {
			t.Errorf("%s: got %d, want %d (all: %v)", code, got[code], want, got)
		}
	}

	clean := ":NULLASBLANK := TRUE;\n:INVARIANTDATECOLUMNS := logdate;\n:PARAMETERS sStatus := \"A\";\nSELECT id FROM item WHERE status = @sStatus\n"
	got = codesIn(clean, ds)
	for _, code := range []string{CodeNullAsBlankValue, CodeInvariantDateColumnsQuoted, CodeDatasourceSQLQuestionPlaceholder} {
		if got[code] != 0 {
			t.Errorf("clean data source: unexpected %s", code)
		}
	}

	sslMode := ":PARAMETERS sStatus := \"A\";\n:DECLARE aRows;\naRows := SQLExecute(\"SELECT id FROM item WHERE status = ?sStatus?\");\n:RETURN aRows;\n"
	if got := codesIn(sslMode, ds); got[CodeDatasourceSQLQuestionPlaceholder] != 0 {
		t.Errorf("SSL-mode data source must not flag SQLExecute markers")
	}

	for _, opts := range []DiagnosticOptions{DefaultDiagnosticOptions(), ds} {
		if got := codesIn(":PARAMETERS;\n:DECLARE nX;\nnX := 1;\n", opts); got[CodeEmptyParameters] != 1 {
			t.Errorf("empty :PARAMETERS (data source %v): got %v", opts.IsDataSourceFile, got)
		}
		if got := codesIn(":PARAMETERS sA;\n:RETURN sA;\n", opts); got[CodeEmptyParameters] != 0 {
			t.Errorf("named :PARAMETERS must not flag")
		}
	}
	// A directive outside a data source is another rule's concern.
	if got := codesIn(":NULLASBLANK := .F.;\n", DefaultDiagnosticOptions()); got[CodeNullAsBlankValue] != 0 {
		t.Errorf("directive values are checked only in data sources")
	}
}
