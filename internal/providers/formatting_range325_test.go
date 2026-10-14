package providers

import (
	"strings"
	"testing"
)

// Range formatting of already-formatted code is a no-op (issue #325).

// applyRangeEdit applies FormatDocumentRange's edit to doc; the edit spans
// whole lines, ending at its last line's end.
func applyRangeEdit(doc string, edits []TextEdit) string {
	if len(edits) == 0 {
		return doc
	}
	lines := strings.Split(doc, "\n")
	e := edits[0]
	out := append([]string{}, lines[:e.Range.Start.Line]...)
	out = append(out, strings.Split(e.NewText, "\n")...)
	out = append(out, lines[e.Range.End.Line+1:]...)
	return strings.Join(out, "\n")
}

// [spec feature.formatting/A14] [spec feature.formatting/A2] — a range
// holding one line of a multi-line statement is widened to the whole
// statement, so the line before a continuation gets no ';', a
// continuation keeps its one level, and a leading `:Member` stays a
// member call.
func TestFormatDocumentRange_LineOfMultiLineStatement(t *testing.T) {
	doc := ":PROCEDURE Demo;\n" +
		"\t:IF nLow > 1\n" +
		"\t\t.AND. nHigh < 50;\n" +
		"\t\tsOut := oText:Trim()\n" +
		"\t\t\t:Replace(\"a\", \"b\");\n" +
		"\t\tnTotal := nFirstAmount\n" +
		"\t\t\t+ nSecondAmount;\n" +
		"\t:ENDIF;\n" +
		":PROCEDURE Other;\n" +
		"\t:PARAMETERS sAlpha, sBravo,\n" +
		"\t            sCharlie;\n" +
		"\t:FOR i := 1\n" +
		"\t\t:TO 3;\n" +
		"\t:NEXT;\n" +
		":ENDPROC;\n"
	opts := DefaultFormattingOptions()
	for line := 1; line < strings.Count(doc, "\n"); line++ {
		edits := FormatDocumentRange(doc, line, 0, line, 3, opts)
		if got := applyRangeEdit(doc, edits); got != doc {
			t.Errorf("range-formatting line %d changed formatted code:\n%s", line+1, got)
		}
	}
}

// [spec feature.formatting/A13] [spec feature.formatting/A14] — the
// interior lines of a multi-line string, comment, region body or code
// block inside a block keep their text when range formatting re-applies
// the base indent; a string's value never changes.
func TestFormatDocumentRange_MultiLineTokenInteriorsVerbatim(t *testing.T) {
	doc := ":PROCEDURE Demo;\n" +
		"\tsText := \"alpha\n" +
		"beta\";\n" +
		"\t/* first line\n" +
		"second line;\n" +
		"\t:REGION Payload;\n" +
		"SELECT a FROM t\n" +
		"  WHERE b = 1\n" +
		"\t:ENDREGION;\n" +
		"\tfnDouble := {|nX| nX +\n" +
		"2};\n" +
		":ENDPROC;\n"
	opts := DefaultFormattingOptions()
	for line := 1; line < strings.Count(doc, "\n"); line++ {
		edits := FormatDocumentRange(doc, line, 0, line, 3, opts)
		if got := applyRangeEdit(doc, edits); got != doc {
			t.Errorf("range-formatting line %d changed literal text:\n%s", line+1, got)
		}
	}
}

// [spec feature.formatting/A14] — a range whose formatting never converges
// (each pass appends a ';' after a code block left open at end of input)
// is declined, as the full formatter declines the document.
func TestFormatDocumentRange_DeclinesWithoutFixedPoint(t *testing.T) {
	doc := ":DECLARE fnDouble;\nfnDouble := {|n| n * 2;\n"
	if full := FormatDocument(doc, DefaultFormattingOptions()); len(full) != 0 {
		t.Fatalf("precondition: the full formatter should decline, got %q", full[0].NewText)
	}
	if edits := FormatDocumentRange(doc, 1, 0, 1, 5, DefaultFormattingOptions()); len(edits) != 0 {
		t.Errorf("range formatting should decline too, got %q", edits[0].NewText)
	}
}

// [spec feature.formatting/A2] — the widened range still formats an
// unformatted statement in context.
func TestFormatDocumentRange_FormatsWidenedStatement(t *testing.T) {
	doc := ":PROCEDURE Demo;\n\tnTotal:=nFirst\n+nSecond;\n\tnNext := 1;\n:ENDPROC;\n"
	edits := FormatDocumentRange(doc, 2, 0, 2, 3, DefaultFormattingOptions())
	want := ":PROCEDURE Demo;\n\tnTotal := nFirst\n\t\t+ nSecond;\n\tnNext := 1;\n:ENDPROC;\n"
	if got := applyRangeEdit(doc, edits); got != want {
		t.Errorf("got:\n%q\nwant:\n%q", got, want)
	}
}
