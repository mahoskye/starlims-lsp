package providers

import "testing"

func sqlContentCodes(script string, opts DiagnosticOptions) map[string]int {
	got := map[string]int{}
	for _, d := range GetDiagnostics(script, opts) {
		got[d.Code]++
	}
	return got
}

// Issue #294: the SQL-content checks that read only SQL text run on the
// body of a SQL-mode data source, through the info gate.
func TestDataSourceBodySQLContent(t *testing.T) {
	opts := DefaultDiagnosticOptions()
	opts.IsDataSourceFile = true
	opts.IncludeInfoDiagnostics = true

	body := ":PARAMETERS sId := \"\";\nSELECT * FROM t1 a, t2 b WHERE a.c1 = b.c1 AND a.c2 = N'x' AND a.id = @sId ORDER BY a.c3 COLLATE Latin1_General_CI_AS\n"
	got := sqlContentCodes(body, opts)
	for _, code := range []string{CodeSQLCommaJoin, CodeSQLSelectStar, CodeUnicodeLiteralPrefix, CodeUnjustifiedCollate} {
		if got[code] != 1 {
			t.Errorf("%s: got %d, want 1 (all %v)", code, got[code], got)
		}
	}
	for _, d := range GetDiagnostics(body, opts) {
		if d.Code == CodeSQLCommaJoin && d.Range.Start.Line != 1 {
			t.Errorf("finding should sit on the body's first line, got %+v", d.Range)
		}
	}

	justified := "-- case-insensitive keys in the legacy table\nSELECT a.c1 FROM t1 a ORDER BY a.c3 COLLATE Latin1_General_CI_AS\n"
	if got := sqlContentCodes(justified, opts); got[CodeUnjustifiedCollate] != 0 {
		t.Errorf("a SQL comment before the statement justifies COLLATE, got %v", got)
	}

	gated := DefaultDiagnosticOptions()
	gated.IsDataSourceFile = true
	if got := sqlContentCodes(body, gated); got[CodeSQLCommaJoin] != 0 || got[CodeSQLSelectStar] != 0 {
		t.Errorf("info-tier findings must be gated, got %v", got)
	}
}
