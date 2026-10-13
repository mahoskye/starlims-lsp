package providers

import (
	"strings"
	"testing"
)

// Catalog audit #255 (batch D) and issues #260/#291/#292: one mistake,
// one finding, at the severity its kind deserves. The spec fences pin
// which code fires; these tests pin counts, severities, and messages,
// which fences cannot express.

func codesOf(diags []Diagnostic) map[string]int {
	out := map[string]int{}
	for _, d := range diags {
		out[d.Code]++
	}
	return out
}

func TestAudit255_TryStructureReportedOncePerEmptyBody(t *testing.T) {
	for _, src := range []string{
		":PROCEDURE Demo;\n:TRY;\n:CATCH;\n:ENDTRY;\n:ENDPROC;",
		":PROCEDURE Demo;\n:DECLARE n;\n:TRY;\n:FINALLY;\nn := 1;\n:ENDTRY;\n:ENDPROC;",
		":PROCEDURE Demo;\n:DECLARE n;\n:TRY;\n:CATCH;\nn := 1;\n:ENDTRY;\n:ENDPROC;",
	} {
		if got := codesOf(GetDiagnostics(src, DefaultDiagnosticOptions()))[CodeTryStructure]; got != 1 {
			t.Errorf("want exactly 1 try_structure, got %d for:\n%s", got, src)
		}
	}
}

func TestAudit255_BlockPairing(t *testing.T) {
	tests := []struct {
		name string
		src  string
		want map[string]int
	}{
		{"ENDFOR is one finding", ":DECLARE i;\n:FOR i := 1 :TO 3;\n:ENDFOR;",
			map[string]int{CodeEndForInvalid: 1, CodeUnclosedBlock: 0}},
		{"stray closer in a procedure is unmatched", ":PROCEDURE Demo;\n:DECLARE n;\nn := 1;\n:ENDIF;\n:ENDPROC;",
			map[string]int{CodeUnmatchedBlockEnd: 1, CodeMismatchedBlockEnd: 0}},
		{"crossed closer in a procedure is still mismatched", ":PROCEDURE Demo;\n:DECLARE n;\n:WHILE n;\n:ENDIF;\n:ENDPROC;",
			map[string]int{CodeMismatchedBlockEnd: 1, CodeUnmatchedBlockEnd: 0}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			got := codesOf(GetDiagnostics(tc.src, DefaultDiagnosticOptions()))
			for code, n := range tc.want {
				if got[code] != n {
					t.Errorf("%s: want %d, got %d (all: %v)", code, n, got[code], got)
				}
			}
		})
	}
}

func TestAudit255_DelimiterRecoveryOneFindingPerMistake(t *testing.T) {
	tests := []struct {
		name string
		src  string
		code string
		text string
	}{
		{"missing inner closer", ":DECLARE aV, n;\nn := Len(aV[1);", CodeUnclosedDelimiter, "Unclosed '['"},
		{"extra closer inside a list", ":DECLARE a, b;\nDoProc(\"P\", {a, b}});", CodeUnmatchedDelimiter, "Unmatched '}'"},
		{"crossed pair", ":DECLARE n;\nn := (1 + 2];", CodeMismatchedDelimiter, "Expected ')' but found ']'"},
	}
	delimiterCodes := map[string]bool{CodeUnclosedDelimiter: true, CodeUnmatchedDelimiter: true, CodeMismatchedDelimiter: true}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			var found []Diagnostic
			for _, d := range GetDiagnostics(tc.src, DefaultDiagnosticOptions()) {
				if delimiterCodes[d.Code] {
					found = append(found, d)
				}
			}
			if len(found) != 1 || found[0].Code != tc.code || found[0].Message != tc.text {
				t.Errorf("want one %s %q, got %+v", tc.code, tc.text, found)
			}
		})
	}
}

func TestAudit255_StrayBlockKeyword(t *testing.T) {
	src := ":PROCEDURE Demo;\n:DECLARE n;\n:CATCH;\nn := 1;\n:FINALLY;\n:ELSE;\n:CASE n == 1;\n:OTHERWISE;\n:ENDPROC;"
	var msgs []string
	for _, d := range GetDiagnostics(src, DefaultDiagnosticOptions()) {
		if d.Code == CodeStrayBlockKeyword {
			if d.Severity != SeverityError {
				t.Errorf("want error severity, got %v", d.Severity)
			}
			msgs = append(msgs, d.Message)
		}
	}
	want := []string{
		"':CATCH' is outside any ':TRY' block",
		"':FINALLY' is outside any ':TRY' block",
		"':ELSE' is outside any ':IF' block",
		"':CASE' is outside any ':BEGINCASE' block",
		"':OTHERWISE' is outside any ':BEGINCASE' block",
	}
	if strings.Join(msgs, "|") != strings.Join(want, "|") {
		t.Errorf("got %q\nwant %q", msgs, want)
	}

	// An :IF opened outside the procedure does not contain an :ELSE in it;
	// one crossed by an unclosed inner block is the recovery's to report.
	if got := codesOf(GetDiagnostics(":IF .T.;\n:PROCEDURE A;\n:ELSE;\n:ENDPROC;\n:ENDIF;", DefaultDiagnosticOptions()))[CodeStrayBlockKeyword]; got != 1 {
		t.Errorf("procedure boundary: want 1 stray_block_keyword, got %d", got)
	}
	if got := codesOf(GetDiagnostics(":DECLARE n;\n:IF n;\n:WHILE n;\n:ELSE;\n:ENDIF;", DefaultDiagnosticOptions()))[CodeStrayBlockKeyword]; got != 0 {
		t.Errorf("crossed block: want 0 stray_block_keyword, got %d", got)
	}
}

func TestAudit255_ParseFailuresAreErrors(t *testing.T) {
	// Issue #291: every rule unexpected_token defers to reports at error
	// severity, so a file that cannot compile never validates clean.
	for _, tc := range []struct{ src, code string }{
		{":DECLARE n;\nn := `x`;", CodeUnknownToken},
		{":DECLARE n;\nn := 7e2;", CodeScientificNotation},
	} {
		found := false
		for _, d := range GetDiagnostics(tc.src, DefaultDiagnosticOptions()) {
			if d.Code == tc.code {
				found = true
				if d.Severity != SeverityError {
					t.Errorf("%s: want error severity, got %v", tc.code, d.Severity)
				}
			}
		}
		if !found {
			t.Errorf("%s did not fire for %q", tc.code, tc.src)
		}
	}

	// unknown_keyword stays a warning; mid-statement, the parse failure is
	// also reported. Statement-only, it is the keyword rule's alone.
	got := codesOf(GetDiagnostics(":DECLARE n;\nn := 1 :FOOBAR 2;", DefaultDiagnosticOptions()))
	if got[CodeUnknownKeyword] != 1 || got[CodeUnexpectedToken] != 1 {
		t.Errorf("mid-statement unknown keyword: got %v", got)
	}
	got = codesOf(GetDiagnostics(":DECLARE n;\n:FOOBAR;\nn := 1;", DefaultDiagnosticOptions()))
	if got[CodeUnknownKeyword] != 1 || got[CodeUnexpectedToken] != 0 {
		t.Errorf("statement-only unknown keyword: got %v", got)
	}
	for _, d := range GetDiagnostics(":FOOBAR;", DefaultDiagnosticOptions()) {
		if d.Code == CodeUnknownKeyword && d.Severity != SeverityWarning {
			t.Errorf("unknown_keyword must stay a warning, got %v", d.Severity)
		}
	}
}

func TestAudit255_UnterminatedStringSingleFinding(t *testing.T) {
	got := codesOf(GetDiagnostics(":DECLARE s;\ns := 1 \"abc;", DefaultDiagnosticOptions()))
	if got[CodeUnterminatedString] != 1 || got[CodeUnexpectedToken] != 0 {
		t.Errorf("want unterminated_string only, got %v", got)
	}
}

func TestAudit255_ByteOrderMarkIsNotAToken(t *testing.T) {
	src := "\xef\xbb\xbf:DECLARE n;\nn := 1;"
	for _, d := range GetDiagnostics(src, DefaultDiagnosticOptions()) {
		t.Errorf("unexpected diagnostic on BOM-prefixed file: %s %s", d.Code, d.Message)
	}
}

func TestAudit255_DotAccessAfterCallOrSubscript(t *testing.T) {
	src := ":DECLARE o, a, n;\nn := o:Get().Value;\nn := a[1].Value;"
	got := codesOf(GetDiagnostics(src, DefaultDiagnosticOptions()))
	if got[CodeDotPropertyAccess] != 2 || got[CodeUnknownToken] != 0 {
		t.Errorf("want 2 dot_property_access and no unknown_token, got %v", got)
	}
}

func TestAudit255_VisibilityAnnotationVerdicts(t *testing.T) {
	opts := DefaultDiagnosticOptions()
	opts.IncludeInfoDiagnostics = true
	tests := []struct {
		name, src, want string // want: "warn", "usage", or ""
	}{
		{"effective", "/*@private;\n:PROCEDURE H;\n:ENDPROC;", "usage"},
		{"uppercase near miss", "/*@Private;\n:PROCEDURE H;\n:ENDPROC;", "warn"},
		{"spaced near miss", "/* @private;\n:PROCEDURE H;\n:ENDPROC;", "warn"},
		{"interior space near miss", "/*@ private;\n:PROCEDURE H;\n:ENDPROC;", "warn"},
		{"blank line", "/*@private;\n\n:PROCEDURE H;\n:ENDPROC;", "warn"},
		{"comment between", "/*@private;\n/* doc;\n:PROCEDURE H;\n:ENDPROC;", "warn"},
		{"before keyword", "/*@private;\n:DECLARE n;", "warn"},
		{"before expression", ":DECLARE n;\n/*@private;\nn := 1;", ""},
		{"near miss as prose", "/* @private;\n:DECLARE n;", ""},
		{"class file", ":CLASS W;\n/*@private;\n:PROCEDURE R;\n:ENDPROC;", "warn"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			got := codesOf(GetDiagnostics(tc.src, opts))
			warn, usage := got[CodeVisibilityAnnotation], got[CodeVisibilityAnnotationUsage]
			switch tc.want {
			case "warn":
				if warn != 1 || usage != 0 {
					t.Errorf("want warning only, got %v", got)
				}
			case "usage":
				if warn != 0 || usage != 1 {
					t.Errorf("want usage note only, got %v", got)
				}
			default:
				if warn != 0 || usage != 0 {
					t.Errorf("want neither, got %v", got)
				}
			}
		})
	}
}

func TestAudit255_ZeroIndexEveryDimension(t *testing.T) {
	src := ":DECLARE aRows, s;\naRows := {{1}};\ns := aRows[1][0];\ns := aRows[1, 0];\ns := aRows[0, 1];\ns := aRows[1, 1];"
	n := 0
	for _, d := range GetDiagnostics(src, DefaultDiagnosticOptions()) {
		if d.Code == CodeZeroBasedArrayIndex {
			n++
			if d.Severity != SeverityError {
				t.Errorf("bare array: want error, got %v", d.Severity)
			}
		}
	}
	if n != 3 {
		t.Errorf("want 3 zero-index errors, got %d", n)
	}
}

func TestAudit255_UnqualifiedFieldAccess(t *testing.T) {
	type want struct{ write, read int }
	tests := []struct {
		name string
		src  string
		want want
	}{
		{"bare read", ":CLASS T;\n:DECLARE nTotal;\n:PROCEDURE Peek;\n:RETURN nTotal;\n:ENDPROC;", want{0, 1}},
		{"qualified read", ":CLASS T;\n:DECLARE nTotal;\n:PROCEDURE Peek;\n:RETURN Me:nTotal;\n:ENDPROC;", want{0, 0}},
		{"local shadow", ":CLASS T;\n:DECLARE nTotal;\n:PROCEDURE Peek;\n:DECLARE nTotal;\nnTotal := 1;\n:RETURN nTotal;\n:ENDPROC;", want{0, 0}},
		{"read and write in one statement", ":CLASS T;\n:DECLARE nTotal;\n:PROCEDURE Inc;\nnTotal := nTotal + 1;\n:ENDPROC;", want{1, 0}},
		{"read after a bare write", ":CLASS T;\n:DECLARE nTotal;\n:PROCEDURE Inc;\nnTotal := 1;\n:RETURN nTotal;\n:ENDPROC;", want{1, 0}},
		{"read before a bare write", ":CLASS T;\n:DECLARE nTotal;\n:PROCEDURE Inc;\nUsrMes(nTotal);\nnTotal := 1;\n:ENDPROC;", want{1, 1}},
		{"increment and element write", ":CLASS T;\n:DECLARE nTotal, aItems;\n:PROCEDURE Bump;\nnTotal++;\n++nTotal;\naItems[1] := 5;\n:ENDPROC;", want{2, 0}},
		{"call name is not a read", ":CLASS T;\n:DECLARE Total;\n:PROCEDURE Go;\nTotal();\n:ENDPROC;", want{0, 0}},
		{"public shadow", ":CLASS T;\n:DECLARE nTotal;\n:PROCEDURE Put;\n:PUBLIC nTotal;\nnTotal := 5;\n:ENDPROC;", want{0, 0}},
		{"chained assignment inner target", ":CLASS T;\n:DECLARE nTotal;\n:PROCEDURE Put;\n:DECLARE n;\nn := nTotal := 5;\n:ENDPROC;", want{1, 0}},
		{"condition read", ":CLASS T;\n:DECLARE bReady;\n:PROCEDURE Go;\n:IF bReady;\n:ENDIF;\n:ENDPROC;", want{0, 1}},
		{"not a class", ":PROCEDURE Peek;\n:DECLARE nTotal;\nnTotal := 1;\n:RETURN nTotal;\n:ENDPROC;", want{0, 0}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			got := codesOf(GetDiagnostics(tc.src, DefaultDiagnosticOptions()))
			if got[CodeUnqualifiedFieldAssignment] != tc.want.write || got[CodeUnqualifiedFieldRead] != tc.want.read {
				t.Errorf("want write=%d read=%d, got %v", tc.want.write, tc.want.read, got)
			}
		})
	}

	// The message names the write form.
	for _, d := range GetDiagnostics(":CLASS T;\n:DECLARE aItems;\n:PROCEDURE Put;\naItems[1] := 5;\n:ENDPROC;", DefaultDiagnosticOptions()) {
		if d.Code == CodeUnqualifiedFieldAssignment && !strings.Contains(d.Message, "element write") {
			t.Errorf("element write message: %s", d.Message)
		}
	}
}

func TestAudit255_FormatterKeepsByteOrderMark(t *testing.T) {
	src := "\xef\xbb\xbf:declare n;\nn:=1;\n"
	edits := FormatDocument(src, DefaultFormattingOptions())
	if len(edits) != 1 {
		t.Fatalf("want one edit, got %d", len(edits))
	}
	if !strings.HasPrefix(edits[0].NewText, "\xef\xbb\xbf:DECLARE n;") {
		t.Errorf("BOM not preserved: %q", edits[0].NewText)
	}
}
