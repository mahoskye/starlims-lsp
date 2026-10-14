package providers

import (
	"strings"
	"testing"
)

func sizeOpts() DiagnosticOptions {
	opts := DefaultDiagnosticOptions()
	opts.IncludeInfoDiagnostics = true
	return opts
}

// procedureOf builds a procedure with n code lines in total, counting its
// :PROCEDURE and :ENDPROC lines, padded with blank and comment lines.
func procedureOf(name string, n int) string {
	var b strings.Builder
	b.WriteString(":PROCEDURE " + name + ";\n\n/* not\ncode;\n")
	for i := 0; i < n-2; i++ {
		b.WriteString("\tUsrMes(\"x\");\n")
	}
	b.WriteString(":ENDPROC;\n")
	return b.String()
}

// [spec diag.procedure_length_review] The default threshold is 80 code
// lines, blank and comment lines excluded; the range covers the keyword
// and the name; the setting moves or disables it (issue #344).
func TestProcedureLengthReview(t *testing.T) {
	opts := sizeOpts()
	if got := diagsWithCode(GetDiagnostics(procedureOf("Fits", 80), opts), CodeProcedureLengthReview); len(got) != 0 {
		t.Errorf("80 code lines: want none, got %+v", got)
	}
	got := diagsWithCode(GetDiagnostics(procedureOf("Long", 81), opts), CodeProcedureLengthReview)
	if len(got) != 1 {
		t.Fatalf("81 code lines: want 1, got %+v", got)
	}
	if got[0].Severity != SeverityInfo || got[0].Range.Start.Character != 0 || got[0].Range.End.Character != 15 {
		t.Errorf("got %+v, want info over ':PROCEDURE Long'", got[0])
	}
	if want := "Procedure 'Long' has 81 code lines (review trigger: 80)"; !strings.HasPrefix(got[0].Message, want) {
		t.Errorf("message = %q, want prefix %q", got[0].Message, want)
	}

	// Hidden without the info tier.
	if got := diagsWithCode(GetDiagnostics(procedureOf("Long", 81), DefaultDiagnosticOptions()), CodeProcedureLengthReview); len(got) != 0 {
		t.Errorf("info tier off: want none, got %+v", got)
	}

	opts.MaxProcedureCodeLines = 0
	if got := diagsWithCode(GetDiagnostics(procedureOf("Long", 500), opts), CodeProcedureLengthReview); len(got) != 0 {
		t.Errorf("disabled: want none, got %+v", got)
	}

	// Data sources are not measured.
	opts = sizeOpts()
	opts.IsDataSourceFile = true
	if got := diagsWithCode(GetDiagnostics(procedureOf("Long", 81), opts), CodeProcedureLengthReview); len(got) != 0 {
		t.Errorf("data source: want none, got %+v", got)
	}
}

// [spec diag.script_size_review] A script over 400 code lines or 12
// procedures reports once on line 1; class files are not measured
// (issue #344).
func TestScriptSizeReview(t *testing.T) {
	opts := sizeOpts()
	var twelve, thirteen strings.Builder
	for i := 0; i < 13; i++ {
		p := procedureOf("P", 2)
		if i < 12 {
			twelve.WriteString(p)
		}
		thirteen.WriteString(p)
	}
	if got := diagsWithCode(GetDiagnostics(twelve.String(), opts), CodeScriptSizeReview); len(got) != 0 {
		t.Errorf("12 procedures: want none, got %+v", got)
	}
	got := diagsWithCode(GetDiagnostics(thirteen.String(), opts), CodeScriptSizeReview)
	if len(got) != 1 || got[0].Range.Start.Line != 0 {
		t.Fatalf("13 procedures: want 1 on line 1, got %+v", got)
	}
	if want := "This script has 26 code lines and 13 procedures"; !strings.HasPrefix(got[0].Message, want) {
		t.Errorf("message = %q, want prefix %q", got[0].Message, want)
	}

	lines := strings.Repeat("UsrMes(\"x\");\n", 400)
	if got := diagsWithCode(GetDiagnostics(lines, opts), CodeScriptSizeReview); len(got) != 0 {
		t.Errorf("400 lines: want none, got %+v", got)
	}
	if got := diagsWithCode(GetDiagnostics(lines+"/* comment;\n\nUsrMes(\"y\");\n", opts), CodeScriptSizeReview); len(got) != 1 {
		t.Errorf("401 lines: want 1, got %+v", got)
	}

	if got := diagsWithCode(GetDiagnostics(":CLASS Big;\n"+thirteen.String(), opts), CodeScriptSizeReview); len(got) != 0 {
		t.Errorf("class file: want none, got %+v", got)
	}

	opts.MaxScriptCodeLines, opts.MaxScriptProcedures = 0, 0
	if got := diagsWithCode(GetDiagnostics(thirteen.String(), opts), CodeScriptSizeReview); len(got) != 0 {
		t.Errorf("disabled: want none, got %+v", got)
	}
}
