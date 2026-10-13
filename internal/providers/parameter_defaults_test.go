package providers

import (
	"strings"
	"testing"
)

func codesOf(t *testing.T, script string, opts DiagnosticOptions, code string) []Diagnostic {
	t.Helper()
	var out []Diagnostic
	for _, d := range GetDiagnostics(script, opts) {
		if d.Code == code {
			out = append(out, d)
		}
	}
	return out
}

// Issues #263/#269: defaults containing commas are one entry; identifiers
// inside them are not parameter names; SQL-mode headers are checked.
func TestDatasourceDefaultRequiredParsing(t *testing.T) {
	opts := DefaultDiagnosticOptions()
	opts.IsDataSourceFile = true

	ssl := `:PARAMETERS aIds := {1, nOther}, sMode := Upper("a,b"), nLast;
:RETURN aIds;`
	got := codesOf(t, ssl, opts, CodeDatasourceDefaultRequired)
	if len(got) != 1 || !strings.Contains(got[0].Message, "'nLast'") {
		t.Fatalf("SSL mode: want only nLast, got %v", got)
	}

	sql := "/* banner */\n:PARAMETERS sStatus, nLimit := 5;\nSELECT * FROM orders WHERE status = @sStatus\n"
	got = codesOf(t, sql, opts, CodeDatasourceDefaultRequired)
	if len(got) != 1 || got[0].Range.Start.Line != 1 || !strings.Contains(got[0].Message, "'sStatus'") {
		t.Fatalf("SQL mode: want sStatus on line 2, got %v", got)
	}
}

// Issue #264: inline defaults outside a data source are compile errors;
// the range covers the name through the :=.
func TestInlineDefaultOutsideDatasource(t *testing.T) {
	script := `:PARAMETERS sName := "a", nCount;
:PROCEDURE Lookup;
:PARAMETERS sKey, nLimit := Max(1, 2);
:DEFAULT sKey, "";
:RETURN sKey;
:ENDPROC;`
	got := codesOf(t, script, DefaultDiagnosticOptions(), CodeInlineDefaultOutsideDatasource)
	if len(got) != 2 {
		t.Fatalf("want sName and nLimit, got %v", got)
	}
	if r := got[0].Range; r.Start.Line != 0 || r.Start.Character != 12 || r.End.Character != 20 {
		t.Errorf("sName range = %+v, want name through :=", r)
	}
	if !strings.Contains(got[1].Message, "'nLimit'") || got[1].Severity != SeverityError {
		t.Errorf("second finding = %+v", got[1])
	}

	ds := DefaultDiagnosticOptions()
	ds.IsDataSourceFile = true
	if got := codesOf(t, `:PARAMETERS sName := "a";`, ds, CodeInlineDefaultOutsideDatasource); len(got) != 0 {
		t.Errorf("data sources must not be checked, got %v", got)
	}
}
