package providers

import (
	"strings"
	"testing"
)

// Wrapped declaration lists align their hanging lines with the list's first
// item (style guide wrapped_list_alignment, ssl-style-guide#120): the
// statement's indent, then spaces up to the first item's column.

// [spec fmt.indent_style] — a source-wrapped :DECLARE, :PARAMETERS or
// :PUBLIC list aligns under its first item, whatever indent it came with.
func TestFormatDocument_WrappedDeclarationListAligns(t *testing.T) {
	cases := []struct {
		name, opt, in, want string
	}{
		{"declare top level", "tab",
			":DECLARE sFirstName, sLastName,\n\tsSuffix, sTitle;\n",
			":DECLARE sFirstName, sLastName,\n         sSuffix, sTitle;\n"},
		{"declare in procedure", "tab",
			":PROCEDURE Demo;\n:DECLARE sFirstName, sLastName,\nsSuffix,\n\t\t\tsTitle;\n:ENDPROC;\n",
			":PROCEDURE Demo;\n\t:DECLARE sFirstName, sLastName,\n\t         sSuffix,\n\t         sTitle;\n:ENDPROC;\n"},
		{"parameters", "tab",
			":PROCEDURE Demo;\n:PARAMETERS sAlpha,\nsBravo;\n:ENDPROC;\n",
			":PROCEDURE Demo;\n\t:PARAMETERS sAlpha,\n\t            sBravo;\n:ENDPROC;\n"},
		{"public", "tab",
			":PUBLIC gOne,\ngTwo;\n",
			":PUBLIC gOne,\n        gTwo;\n"},
		{"end-of-line comment", "tab",
			":DECLARE sA, sB, /* note;\nsC;\n",
			":DECLARE sA, sB,  /* note;\n         sC;\n"},
		{"space indent", "space",
			":PROCEDURE Demo;\n:DECLARE sFirstName,\nsSuffix;\n:ENDPROC;\n",
			":PROCEDURE Demo;\n    :DECLARE sFirstName,\n             sSuffix;\n:ENDPROC;\n"},
		// The alignment ends with the statement.
		{"next statement unaffected", "tab",
			":DECLARE sA,\nsB;\nnX := DoProc(\"Foo\", {nA,\nnB});\n",
			":DECLARE sA,\n         sB;\n\nnX := DoProc(\"Foo\", {nA,\n                     nB});\n"},
	}
	for _, c := range cases {
		t.Run(c.name, func(t *testing.T) {
			opts := DefaultFormattingOptions()
			opts.IndentStyle = c.opt
			if got := formatTwice(t, c.in, opts); got != c.want {
				t.Errorf("got:\n%s\nwant:\n%s", got, c.want)
			}
		})
	}
}

// [spec fmt.max_line_length] — a :DECLARE the wrap engine breaks sets its
// continuation lines at the aligned column and still fits the limit there.
func TestFormatDocument_WrapEngineAlignsDeclarationList(t *testing.T) {
	in := ":PROCEDURE Demo;\n:IF bReady;\n:DECLARE sRequestIdentifier, sRouteCode, sSettingKey, nRetryCount, nRetryLimit, bStarted, bCommit, sAnotherLongName, sAndOneMore;\n:ENDIF;\n:ENDPROC;\n"
	want := ":PROCEDURE Demo;\n\t:IF bReady;\n" +
		"\t\t:DECLARE sRequestIdentifier, sRouteCode, sSettingKey, nRetryCount, nRetryLimit,\n" +
		"\t\t         bStarted, bCommit, sAnotherLongName, sAndOneMore;\n" +
		"\t:ENDIF;\n:ENDPROC;\n"
	opts := DefaultFormattingOptions()
	got := formatTwice(t, in, opts)
	if got != want {
		t.Errorf("got:\n%s\nwant:\n%s", got, want)
	}
	for _, line := range strings.Split(got, "\n") {
		if w := visualWidth(line, opts); w > opts.MaxLineLength {
			t.Errorf("line over %d columns (%d): %q", opts.MaxLineLength, w, line)
		}
	}
}

// [spec feature.formatting/A2] — range-formatting the hanging line keeps
// its aligned column.
func TestFormatDocumentRange_KeepsDeclarationAlignment(t *testing.T) {
	doc := ":PROCEDURE Demo;\n\t:DECLARE sFirstName, sLastName,\n\t         sSuffix, sTitle;\n:ENDPROC;\n"
	for _, r := range [][2]int{{1, 2}, {2, 2}} {
		edits := FormatDocumentRange(doc, r[0], 0, r[1], 30, DefaultFormattingOptions())
		if len(edits) == 0 {
			continue // already formatted
		}
		// The edit spans whole lines, ending at the last line's end.
		lines := strings.Split(doc, "\n")
		e := edits[0]
		want := strings.Join(lines[e.Range.Start.Line:e.Range.End.Line+1], "\n")
		if got := strings.TrimSuffix(e.NewText, "\n"); got != want {
			t.Errorf("range %v changed a formatted declaration:\n%q\nwant:\n%q", r, got, want)
		}
	}
}
