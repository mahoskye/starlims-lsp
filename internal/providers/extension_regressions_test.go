package providers

import (
	"strings"
	"testing"
)

// Regressions inherited from the vs-code-ssl-formatter extension.
//
// The extension used to ship its own TypeScript formatter as a fallback for
// when this server could not start. That formatter was removed once a run over
// a 6,228-file production corpus showed it was non-idempotent on 18% of files,
// and formatting became LSP-only. Its test suite encoded real user bug reports,
// so the scenarios that were not already covered here were carried over rather
// than deleted with it.
//
// Cases the extension asserted that this formatter deliberately handles
// differently are recorded at the bottom of this file, so the difference reads
// as a decision rather than a gap.

// A user reported that brackets in an array subscript were being treated as
// string delimiters, which left the contents unlexed — keywords inside a
// subscript then escaped casing normalization.
func TestFormatDocument_KeywordInsideArraySubscript(t *testing.T) {
	input := `x := arr[:if];`

	opts := DefaultFormattingOptions()
	edits := FormatDocument(input, opts)
	formatted := strings.TrimSpace(edits[0].NewText)

	want := `x := arr[:IF];`
	if formatted != want {
		t.Errorf("subscript contents should be lexed as code, not string:\ngot:  %s\nwant: %s", formatted, want)
	}
}

// A user reported a SQL string spanning several source lines coming back with
// the token either side of the line break glued together — "tmv.VERSION" and
// "FROM" merging into "VERSIONFROM", silently changing the query.
func TestFormatDocument_MultiLineSQLStringNoTokenMerge(t *testing.T) {
	input := "x := SQLExecute(\"SELECT tmv.VERSION\n                 FROM TEST_METHODS_VERSIONS tmv\");"

	opts := DefaultFormattingOptions()
	edits := FormatDocument(input, opts)
	formatted := edits[0].NewText

	if strings.Contains(strings.ToUpper(formatted), "VERSIONFROM") {
		t.Errorf("tokens across the line break were merged:\n%s", formatted)
	}
	if !strings.Contains(strings.ToUpper(formatted), "TMV.VERSION") {
		t.Errorf("expected TMV.VERSION to survive formatting:\n%s", formatted)
	}
	if !strings.Contains(strings.ToUpper(formatted), "FROM TEST_METHODS_VERSIONS") {
		t.Errorf("expected FROM clause to survive formatting:\n%s", formatted)
	}
}

// A user reported a block comment at the end of a line swallowing the
// statement on the line below it.
func TestFormatDocument_EOLCommentDoesNotSwallowNextStatement(t *testing.T) {
	input := `x := 1; /* comment;
y := 2;`

	opts := DefaultFormattingOptions()
	edits := FormatDocument(input, opts)
	formatted := strings.TrimSuffix(edits[0].NewText, "\n")

	lines := strings.Split(formatted, "\n")
	if len(lines) != 2 {
		t.Fatalf("expected 2 lines, got %d:\n%s", len(lines), formatted)
	}
	if !strings.Contains(lines[0], "x := 1;") || !strings.Contains(lines[0], "/* comment;") {
		t.Errorf("first line should keep the statement and its trailing comment, got: %q", lines[0])
	}
	if !strings.Contains(lines[1], "y := 2;") {
		t.Errorf("second statement should stay on its own line, got: %q", lines[1])
	}
}

// :BEGINCASE takes no operand, so semicolon enforcement has to treat it as a
// complete statement. It used to be the only block opener that never got a
// semicolon, because the token after it is always :CASE or :OTHERWISE and both
// are continuation keywords that stop the lookahead.
func TestFormatDocument_BareBeginCaseGetsSemicolon(t *testing.T) {
	input := `:BEGINCASE
:CASE "1"
code := 1;
:OTHERWISE
code := 2;
:ENDCASE`

	opts := DefaultFormattingOptions()
	edits := FormatDocument(input, opts)
	formatted := edits[0].NewText

	if !strings.Contains(formatted, ":BEGINCASE;") {
		t.Errorf("bare :BEGINCASE should get a semicolon:\n%s", formatted)
	}
	if strings.Contains(formatted, ":BEGINCASE\n") {
		t.Errorf(":BEGINCASE should not be left bare:\n%s", formatted)
	}
}

// Every block opener should get the same treatment as :BEGINCASE above; this
// pins the whole family so the next one cannot regress unnoticed.
func TestFormatDocument_BareBlockOpenersGetSemicolons(t *testing.T) {
	cases := []struct {
		name  string
		input string
		want  string
	}{
		{"procedure", ":PROCEDURE Foo\nx := 1;\n:ENDPROC", ":PROCEDURE Foo;"},
		{"if", ":IF x = 1\ny := 2;\n:ENDIF;", ":IF x = 1;"},
		{"else", ":IF x;\ny := 1;\n:ELSE\nz := 2;\n:ENDIF;", ":ELSE;"},
		{"try", ":TRY\nx := 1;\n:CATCH\ny := 2;\n:ENDTRY", ":TRY;"},
		{"catch", ":TRY;\nx := 1;\n:CATCH\ny := 2;\n:ENDTRY;", ":CATCH;"},
		{"begincase", ":BEGINCASE\n:CASE \"1\";\na := 1;\n:ENDCASE;", ":BEGINCASE;"},
		{"otherwise", ":BEGINCASE;\n:CASE \"1\";\na := 1;\n:OTHERWISE\nb := 2;\n:ENDCASE;", ":OTHERWISE;"},
	}

	opts := DefaultFormattingOptions()
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			formatted := FormatDocument(tc.input, opts)[0].NewText
			if !strings.Contains(formatted, tc.want) {
				t.Errorf("expected %q in output:\n%s", tc.want, formatted)
			}
		})
	}
}

// A user reported wrapped argument lists coming back with the comma leading the
// continuation line instead of trailing the line before it.
func TestFormatDocument_WrappedLineNeverStartsWithComma(t *testing.T) {
	input := `:IF aaaaaaaaaa = 1 .AND. bbbbbbbbbb = 2 .AND. cccccccccc = 3;
x := 1;
:ENDIF;`

	opts := DefaultFormattingOptions()
	opts.MaxLineLength = 40
	formatted := FormatDocument(input, opts)[0].NewText

	for i, line := range strings.Split(formatted, "\n") {
		if strings.HasPrefix(strings.TrimSpace(line), ",") {
			t.Errorf("line %d starts with a comma: %q\nfull output:\n%s", i+1, line, formatted)
		}
	}
}

// A user reported a long SQL string being wrapped in the middle of a
// Table.Column reference, splitting the identifier across two lines.
func TestFormatDocument_SQLTableColumnNotSplitOnWrap(t *testing.T) {
	input := `SQLExecute("SELECT t1.col1, t1.col2, t1.col3, t1.col4, t1.col5, t1.col6, table_name.very_long_column_name_that_might_break FROM table_name");`

	opts := DefaultFormattingOptions()
	formatted := FormatDocument(input, opts)[0].NewText

	if strings.Contains(formatted, "table_name.\n") {
		t.Errorf("wrapped after the dot of a qualified column:\n%s", formatted)
	}
	for _, line := range strings.Split(formatted, "\n") {
		if strings.HasPrefix(strings.TrimSpace(line), ".") {
			t.Errorf("wrapped before the dot of a qualified column:\n%s", formatted)
		}
	}
	if !strings.Contains(formatted, "table_name.very_long_column_name_that_might_break") {
		t.Errorf("qualified column should stay intact:\n%s", formatted)
	}
}

// Deliberate differences from the extension's old formatter, recorded so they
// are not mistaken for missing behaviour:
//
//   - Statement consolidation. The old formatter rejoined a statement split
//     across lines ("x\n := 1\n ;" became "x := 1;"). This formatter preserves
//     the author's line breaks and only re-indents them; reflowing statement
//     boundaries is a rewrite, not formatting.
//
//   - Comma-list wrapping with visual alignment. The old formatter wrapped
//     long ":PARAMETERS a, b, c…" and "Func(a, b, c…)" lists at commas and
//     aligned continuations under the first operand. This formatter wraps at
//     operators (see TestFormatDocument_WrapBefore*) and leaves comma lists
//     intact, which keeps parameter lists diffable.
//
//   - Blank line between :CASE arms. The old formatter inserted one; this one
//     preserves what the author wrote (see TestFormat_MaxConsecutiveBlankLines_*).
