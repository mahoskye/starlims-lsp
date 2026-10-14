package providers

import "testing"

// fmt.blank_lines_between_sections: one blank line at each boundary between
// logical sections of a body.
func TestFormat_BlankLinesBetweenSections(t *testing.T) {
	cases := []struct{ name, in, want string }{
		{"after declarations",
			":PROCEDURE P;\n:PARAMETERS sA;\n:DEFAULT sA, \"\";\n:DECLARE nB;\nnB := 1;\n:ENDPROC;\n",
			":PROCEDURE P;\n\t:PARAMETERS sA;\n\t:DEFAULT sA, \"\";\n\t:DECLARE nB;\n\n\tnB := 1;\n:ENDPROC;\n"},
		{"comment between declarations stays with them",
			":PARAMETERS sA;\n/* locals;\n:DECLARE nB;\nnB := 1;\n",
			":PARAMETERS sA;\n/* locals;\n:DECLARE nB;\n\nnB := 1;\n"},
		{"around a block",
			":DECLARE n;\n\nn := 1;\n:IF n > 0;\nn := 2;\n:ENDIF;\nn := 3;\n",
			":DECLARE n;\n\nn := 1;\n\n:IF n > 0;\n\tn := 2;\n:ENDIF;\n\nn := 3;\n"},
		{"block body is not split",
			":IF bA;\nn := 1;\nn := 2;\n:ENDIF;\n",
			":IF bA;\n\tn := 1;\n\tn := 2;\n:ENDIF;\n"},
		{"section comment opens its section",
			"n := 1;\n/* next part;\nn := 2;\n",
			"n := 1;\n\n/* next part;\nn := 2;\n"},
		{"comment above a block takes the blank line",
			"n := 1;\n/* loop;\n:WHILE n < 3;\nn += 1;\n:ENDWHILE;\n",
			"n := 1;\n\n/* loop;\n:WHILE n < 3;\n\tn += 1;\n:ENDWHILE;\n"},
		{"before the final :RETURN",
			":PROCEDURE P;\n:DECLARE s;\n\ns := \"a\";\ns := s + \"b\";\n:RETURN s;\n:ENDPROC;\n",
			":PROCEDURE P;\n\t:DECLARE s;\n\n\ts := \"a\";\n\ts := s + \"b\";\n\n\t:RETURN s;\n:ENDPROC;\n"},
		{"a :RETURN inside a block is not final",
			":PROCEDURE P;\n:IF bA;\nn := 1;\n:RETURN n;\n:ENDIF;\n:ENDPROC;\n",
			":PROCEDURE P;\n\t:IF bA;\n\t\tn := 1;\n\t\t:RETURN n;\n\t:ENDIF;\n:ENDPROC;\n"},
		{"nothing before :EXITCASE or a middle keyword",
			":BEGINCASE;\n:CASE n == 1;\n:IF bA;\nn := 2;\n:ENDIF;\n:EXITCASE;\n:OTHERWISE;\nn := 3;\n:EXITCASE;\n:ENDCASE;\n",
			":BEGINCASE;\n:CASE n == 1;\n\t:IF bA;\n\t\tn := 2;\n\t:ENDIF;\n\t:EXITCASE;\n:OTHERWISE;\n\tn := 3;\n\t:EXITCASE;\n:ENDCASE;\n"},
		{"continuation lines belong to their statement",
			":DECLARE sOut;\n\nsOut := \"a\" +\n\"b\";\n:IF bA;\n:ENDIF;\n",
			":DECLARE sOut;\n\nsOut := \"a\" +\n\t\"b\";\n\n:IF bA;\n:ENDIF;\n"},
		{"text inside a string is literal",
			":DECLARE sT;\n\nsT := \"x\n:IF y;\n:ENDIF;\nz\";\n",
			":DECLARE sT;\n\nsT := \"x\n:IF y;\n:ENDIF;\nz\";\n"},
		{"author blank lines are kept, long runs capped at two",
			"n := 1;\n\n\n\n\nn := 2;\n",
			"n := 1;\n\n\nn := 2;\n"},
	}
	for _, c := range cases {
		t.Run(c.name, func(t *testing.T) {
			if got := formatTwice(t, c.in, DefaultFormattingOptions()); got != c.want {
				t.Errorf("got:\n%q\nwant:\n%q", got, c.want)
			}
		})
	}
}

// The setting turns the pass off.
func TestFormat_BlankLinesBetweenSections_Disabled(t *testing.T) {
	opts := DefaultFormattingOptions()
	opts.BlankLinesBetweenSections = false
	in := ":DECLARE n;\nn := 1;\n:IF n > 0;\nn := 2;\n:ENDIF;\n"
	want := ":DECLARE n;\nn := 1;\n:IF n > 0;\n\tn := 2;\n:ENDIF;\n"
	if got := formatTwice(t, in, opts); got != want {
		t.Errorf("got:\n%q\nwant:\n%q", got, want)
	}
}
