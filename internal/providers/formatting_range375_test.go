package providers

import (
	"strings"
	"testing"
)

// A bracket left open ends with its statement (issue #375).

// unclosedBracketDoc is a procedure whose fourth line leaves a bracket open
// before its ';'. Every line after it is a statement of its own.
func unclosedBracketDoc(line4 string) string {
	return ":PROCEDURE Run;\n" +
		":PARAMETERS nA, nB;\n" +
		":DECLARE nSum;\n" +
		line4 + "\n" +
		":IF nSum > 10;\n" +
		"\tnSum := 10;\n" +
		":ENDIF;\n" +
		"DoProc(\"Audit\", {nSum});\n" +
		":RETURN nSum;\n" +
		":ENDPROC;\n"
}

// [spec feature.formatting/A15] [spec feature.formatting/A2] — a range on
// the line whose bracket never closes is answered for that statement only:
// the ';' ends it, so the edit does not reach the statements after it.
func TestFormatDocumentRange_UnclosedBracketStaysInStatement(t *testing.T) {
	opts := DefaultFormattingOptions()
	cases := []struct {
		name               string
		line4              string
		startLine, endLine int
		want               string
	}{
		{"call paren left open, its line alone", "nSum := Max(nA, nB;", 3, 3,
			"nSum := Max(nA, nB;"},
		{"two parens left open, with the next line", "nSum   :=   ((nA  +  nB ;", 3, 4,
			"nSum := ((nA + nB;\n:IF nSum > 10;"},
		{"array brace left open", "aPair := {nA, nB;", 3, 3,
			"aPair := {nA, nB;"},
		{"subscript left open", "nSum := aTotals[nA;", 3, 3,
			"nSum := aTotals[nA;"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			doc := unclosedBracketDoc(tc.line4)
			edits := FormatDocumentRange(doc, tc.startLine, 0, tc.endLine, 0, opts)
			if len(edits) != 1 {
				t.Fatalf("want one edit, got %d", len(edits))
			}
			e := edits[0]
			if e.Range.Start.Line != tc.startLine || e.Range.End.Line != tc.endLine {
				t.Errorf("edit covers lines %d..%d, want %d..%d",
					e.Range.Start.Line+1, e.Range.End.Line+1, tc.startLine+1, tc.endLine+1)
			}
			if e.NewText != tc.want {
				t.Errorf("edit text:\n got: %q\nwant: %q", e.NewText, tc.want)
			}
		})
	}
}

// [spec feature.formatting/A15] — a line after the unclosed bracket's ';'
// is not a continuation of it: range-formatting that line alone does not
// reach back to the bracket's line.
func TestFormatDocumentRange_LineAfterUnclosedBracketIsItsOwnStatement(t *testing.T) {
	doc := unclosedBracketDoc("nSum := Max(nA, nB;")
	edits := FormatDocumentRange(doc, 7, 0, 7, 0, DefaultFormattingOptions())
	if len(edits) != 1 {
		t.Fatalf("want one edit, got %d", len(edits))
	}
	if e := edits[0]; e.Range.Start.Line != 7 || e.Range.End.Line != 7 ||
		e.NewText != "DoProc(\"Audit\", {nSum});" {
		t.Errorf("edit lines %d..%d text %q, want line 8 unchanged",
			e.Range.Start.Line+1, e.Range.End.Line+1, e.NewText)
	}
}

// [spec feature.formatting/A15] [spec feature.formatting/A6] — a full
// format keeps the indentation of the statements after a bracket that was
// never closed, and the result formats to itself.
func TestFormat_UnclosedBracketDoesNotIndentLaterStatements(t *testing.T) {
	opts := DefaultFormattingOptions()
	want := ":PROCEDURE Run;\n" +
		"\t:PARAMETERS nA, nB;\n" +
		"\t:DECLARE nSum;\n" +
		"\n" +
		"\tnSum := Max(nA, nB;\n" +
		"\n" +
		"\t:IF nSum > 10;\n" +
		"\t\tnSum := 10;\n" +
		"\t:ENDIF;\n" +
		"\n" +
		"\tDoProc(\"Audit\", {nSum});\n" +
		"\n" +
		"\t:RETURN nSum;\n" +
		":ENDPROC;\n"
	got := formatAll(unclosedBracketDoc("nSum := Max(nA, nB;"), opts)
	if got != want {
		t.Errorf("full format:\n got: %q\nwant: %q", got, want)
	}
	if again := formatAll(got, opts); again != got {
		t.Errorf("not idempotent:\nfirst:  %q\nsecond: %q", got, again)
	}
	// Every line of the result is range-stable, the bracket's included.
	for line := 0; line < strings.Count(got, "\n"); line++ {
		edits := FormatDocumentRange(got, line, 0, line, 0, opts)
		if out := applyRangeEdit(got, edits); out != got {
			t.Errorf("range-formatting line %d changed the formatted text:\n%s", line+1, out)
		}
	}
}

// A bracket that closes on a later line still continues across lines: only
// a ';' ends it. [spec feature.formatting/A2]
func TestFormatDocumentRange_OpenBracketWithoutSemicolonStillWidens(t *testing.T) {
	doc := ":PROCEDURE Run;\n" +
		"\taPair := {nA,\n" +
		"\t          nB};\n" +
		":ENDPROC;\n"
	edits := FormatDocumentRange(doc, 2, 0, 2, 0, DefaultFormattingOptions())
	if len(edits) != 1 || edits[0].Range.Start.Line != 1 || edits[0].Range.End.Line != 2 {
		t.Fatalf("want one edit over lines 2..3, got %+v", edits)
	}
	if out := applyRangeEdit(doc, edits); out != doc {
		t.Errorf("range-formatting a continuation line changed formatted code:\n%s", out)
	}
}
