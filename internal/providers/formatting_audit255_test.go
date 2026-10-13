package providers

import (
	"strings"
	"testing"
)

// Tests for the formatter fixes from the catalog audit (issue #255) and the
// formatter code-review findings (issue #258).

// formatTwice formats src twice and fails unless the second pass is
// byte-identical to the first (feature.formatting A6).
func formatTwice(t *testing.T, src string, opts FormattingOptions) string {
	t.Helper()
	first := formatAll(src, opts)
	if second := formatAll(first, opts); second != first {
		t.Fatalf("not idempotent:\n--- pass 1 ---\n%s\n--- pass 2 ---\n%s", first, second)
	}
	return first
}

// [spec feature.formatting/A11] — a range that starts or ends inside a
// multi-line comment or string is widened to the whole token, and the
// token's text is never rewritten.
func TestFormatDocumentRange_WidensAcrossMultiLineTokens(t *testing.T) {
	opts := DefaultFormattingOptions()
	cases := []struct {
		name, doc  string
		line       int
		wantStart  int
		wantEnd    int
		wantInside string
	}{
		{"comment", "/* start\n   x  =  1\nend;\nnY := 2;\n", 1, 0, 2, "   x  =  1\n"},
		{"bracket string", "sT := [first\n   x  =  1\nend];\n", 1, 0, 2, "   x  =  1\n"},
		{"double-quoted string", "nA := 1;\nsT := \"one\n   x  =  1\";\n", 2, 1, 2, "   x  =  1\""},
	}
	for _, c := range cases {
		t.Run(c.name, func(t *testing.T) {
			edits := FormatDocumentRange(c.doc, c.line, 0, c.line, 5, opts)
			if len(edits) != 1 {
				t.Fatalf("expected 1 edit, got %d", len(edits))
			}
			e := edits[0]
			if e.Range.Start.Line != c.wantStart || e.Range.End.Line != c.wantEnd {
				t.Errorf("range = %d..%d, want %d..%d", e.Range.Start.Line, e.Range.End.Line, c.wantStart, c.wantEnd)
			}
			if !strings.Contains(e.NewText, c.wantInside) {
				t.Errorf("token text rewritten: %q", e.NewText)
			}
			if strings.Contains(e.NewText, "x = 1") {
				t.Errorf("literal content was formatted: %q", e.NewText)
			}
		})
	}
}

// A range that touches no multi-line token is not widened.
func TestFormatDocumentRange_NoWideningWithoutTokens(t *testing.T) {
	doc := "nA:=1;\nnB:=2;\nnC:=3;\n"
	edits := FormatDocumentRange(doc, 1, 0, 1, 6, DefaultFormattingOptions())
	if len(edits) != 1 || edits[0].Range.Start.Line != 1 || edits[0].Range.End.Line != 1 {
		t.Fatalf("unexpected edits: %+v", edits)
	}
	if edits[0].NewText != "nB := 2;" {
		t.Errorf("got %q", edits[0].NewText)
	}
}

// Issue #258: a nested bracket string left open (`[[a]`) ends in `]` but is
// unterminated; the formatter must leave the document alone instead of
// writing its ';' inside the string.
func TestFormat_UnterminatedNestedBracketString(t *testing.T) {
	if edits := FormatDocument("sText := [[a]", DefaultFormattingOptions()); edits != nil {
		t.Fatalf("expected no edits, got %q", edits[0].NewText)
	}
}

// Issue #258: a comment glued to a comma takes the canonical two-space
// separator on every pass.
func TestFormat_CommaThenCommentIdempotent(t *testing.T) {
	got := formatTwice(t, "aX := {{\"M\", sA},/*note;\n};\n", DefaultFormattingOptions())
	if !strings.Contains(got, "},  /*note;") {
		t.Errorf("got %q", got)
	}
}

// Issue #258: an end-of-line comment after a comma is never a wrap target —
// breaking there detached the comment onto its own line.
func TestFormat_WrapNeverBreaksBeforeComment(t *testing.T) {
	for _, width := range []int{40, 60} {
		opts := DefaultFormattingOptions()
		opts.MaxLineLength = width
		got := formatTwice(t, "aList := DoProc(\"Proc\", { sFile,  /*note about it;\n sOther});\n", opts)
		for _, line := range strings.Split(got, "\n") {
			if strings.HasPrefix(strings.TrimSpace(line), "/*") {
				t.Errorf("width %d: comment detached onto its own line:\n%s", width, got)
			}
		}
	}
}

// Issue #255: wrap fragments that stay on one line rejoin with the source
// whitespace, so spacing-off options are honored on over-long lines.
func TestFormat_WrapHonorsSpacingOffOptions(t *testing.T) {
	opts := DefaultFormattingOptions()
	opts.CommaSpacing = false
	got := formatTwice(t, "DoProc(\"CalculateTotals\",{nFirstQuarterRevenue,nSecondQuarterRevenue,nThirdQuarterRevenue,nFourth});\n", opts)
	want := "DoProc(\"CalculateTotals\",{nFirstQuarterRevenue,nSecondQuarterRevenue,nThirdQuarterRevenue,\n\tnFourth});\n"
	if got != want {
		t.Errorf("commaSpacing off:\ngot  %q\nwant %q", got, want)
	}

	opts = DefaultFormattingOptions()
	opts.OperatorSpacing = false
	got = formatTwice(t, "xResultValue:=nFirstValueInTheList+nSecondValueInTheList+nThirdValueInTheList+nFourthValue;\n", opts)
	if strings.Contains(got, " +") || strings.Contains(got, ":= ") {
		t.Errorf("operatorSpacing off gained spaces: %q", got)
	}
}

// Issue #255: a :RETURN whose operand starts the next line keeps it (no
// forced ';'), the operand is a continuation, and a long operand wraps
// stably.
func TestFormat_KeywordOperandOnNextLine(t *testing.T) {
	src := ":PROCEDURE P;\n:RETURN\nDoProc(\"CalculateTotals\", {nFirstQuarterRevenue, nSecondQuarterRevenue, nThirdQuarterRevenue});\n:ENDPROC;\n"
	got := formatTwice(t, src, DefaultFormattingOptions())
	if strings.Contains(got, ":RETURN;") {
		t.Fatalf("operand truncated by a forced ';':\n%s", got)
	}
	if !strings.Contains(got, "\t:RETURN\n\t\tDoProc(") {
		t.Errorf("operand not indented as a continuation:\n%s", got)
	}
}

// Issue #255: the line-oriented post-passes never edit comment or region
// interiors (D7).
func TestFormat_BlankLineBetweenBlocksSkipsLiteralInteriors(t *testing.T) {
	for _, src := range []string{
		"/* note\n:NEXT;\n:WHILE y;\n:ENDWHILE;\n",
		":REGION R;\n:ENDTRY;\n:TRY;\n:ENDREGION;\n",
	} {
		got := formatTwice(t, src, DefaultFormattingOptions())
		if strings.Contains(got, "\n\n") {
			t.Errorf("blank line inserted inside a literal:\n%s", got)
		}
	}
}

func TestFormat_MaxConsecutiveBlankLinesSkipsCommentInterior(t *testing.T) {
	opts := DefaultFormattingOptions()
	opts.MaxConsecutiveBlankLines = 1
	src := "/* first\n\n\n\nlast;\nnX := 1;\n"
	if got := formatTwice(t, src, opts); got != src {
		t.Errorf("comment interior changed:\n%q", got)
	}
}

// Issue #255: with commaSpacing off, a code-block body keeps its author's
// comma spacing.
func TestFormat_CodeBlockBodyHonorsCommaSpacingOff(t *testing.T) {
	opts := DefaultFormattingOptions()
	opts.CommaSpacing = false
	got := formatTwice(t, "fnB := {|a,b| f(a,b) + g(a, b)};\n", opts)
	if got != "fnB := {|a, b| f(a,b) + g(a, b)};\n" {
		t.Errorf("got %q", got)
	}
}

// Issue #255: member calls are never re-cased by the built-in casing pass.
func TestFormat_BuiltinCaseSkipsMemberCalls(t *testing.T) {
	got := formatTwice(t, "x := oS:replace(\"a\", \"b\") + oA:len() + len(sX);\n", DefaultFormattingOptions())
	if got != "x := oS:replace(\"a\", \"b\") + oA:len() + Len(sX);\n" {
		t.Errorf("got %q", got)
	}
}

// Issue #255: a wrapped fragment of a member-access chain continuation
// (line led by ':') stays at that line's indent, as the stream formatter
// places it — one level deeper oscillated between passes.
func TestFormat_WrapChainContinuationIdempotent(t *testing.T) {
	opts := DefaultFormattingOptions()
	opts.MaxLineLength = 40
	src := ":PROCEDURE P;\n:RETURN txt:ToString()\n:Replace(\"##TITLE##\", sTitle)\n:Replace(\"##DATE##\", Now():ToString());\n:ENDPROC;\n"
	got := formatTwice(t, src, opts)
	if !strings.Contains(got, "\t\t:Replace(\"##DATE##\",\n\t\tNow():ToString());") {
		t.Errorf("unexpected layout:\n%s", got)
	}
}
