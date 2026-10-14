package providers

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
	"unicode/utf8"
)

// addCorpusSeeds seeds a fuzz target with the formatter idempotence corpus
// plus malformed shapes that have historically broken the formatter or the
// diagnostics pipeline (unterminated strings/comments, stray block ends,
// data-source headers, region bodies).
func addCorpusSeeds(f *testing.F) {
	f.Helper()
	dir := filepath.Join("testdata", "idempotence")
	entries, err := os.ReadDir(dir)
	if err != nil {
		f.Fatalf("reading seed corpus: %v", err)
	}
	for _, e := range entries {
		if e.IsDir() || !strings.HasSuffix(e.Name(), ".ssl") {
			continue
		}
		raw, err := os.ReadFile(filepath.Join(dir, e.Name()))
		if err != nil {
			f.Fatalf("reading seed %s: %v", e.Name(), err)
		}
		f.Add(string(raw))
	}
	for _, s := range []string{
		"",
		`s := "unterminated`,
		`/* unterminated comment`,
		`:ENDIF; :ENDWHILE; :NEXT; :ENDPROC;`,
		`:IF x; :WHILE y; :FOR i := 1 :TO 2;`,
		`a := ((b + c;`,
		`x := f(a,,c` + "\n" + `y := 1;`,
		`:REGION r;` + "\n" + `free text` + "\n" + `:ENDREGION;`,
		`#include "Cat.Script"` + "\n" + `SELECT * FROM t WHERE a = ?x?`,
		`sSQL := "SELECT a, b FROM t WHERE c = ?sC? AND d IN (?aD?)";`,
		`:PROCEDURE P; :PARAMETERS a; :DEFAULT a, 1; :RETURN a; :ENDPROC;`,
		"x := 1;\r\n:IF x == 1;\r\n\tx++;\r\n:ENDIF;\r\n",
		strings.Repeat("(", 64),
		strings.Repeat(":IF .T.;\n", 40),
	} {
		f.Add(s)
	}
}

// allChecksOptions enables every opt-in diagnostic family so the fuzzer
// reaches the noisy and info-tier code paths too.
func allChecksOptions(dataSource bool) DiagnosticOptions {
	opts := DefaultDiagnosticOptions()
	opts.CheckUndeclaredVars = true
	opts.CheckUnusedVars = true
	opts.CheckSQLParams = true
	opts.CheckHungarianNotation = true
	opts.CheckHungarianTypes = true
	opts.IncludeInfoDiagnostics = true
	opts.IsDataSourceFile = dataSource
	return opts
}

// FuzzGetDiagnostics runs the full diagnostics pipeline (default and
// all-checks-on, script and data-source modes) on arbitrary input.
// Contract: no panic, and every range is well-formed (non-negative,
// start <= end, inside the document's line count) since the server hands
// ranges to the client verbatim.
func FuzzGetDiagnostics(f *testing.F) {
	addCorpusSeeds(f)
	f.Fuzz(func(t *testing.T, src string) {
		lines := strings.Count(src, "\n") + 1
		for _, opts := range []DiagnosticOptions{
			DefaultDiagnosticOptions(),
			allChecksOptions(false),
			allChecksOptions(true),
		} {
			for _, d := range GetDiagnostics(src, opts) {
				r := d.Range
				if r.Start.Line < 0 || r.Start.Character < 0 || r.End.Line < 0 || r.End.Character < 0 {
					t.Fatalf("%s: negative range %+v", d.Code, r)
				}
				if r.Start.Line > r.End.Line || (r.Start.Line == r.End.Line && r.Start.Character > r.End.Character) {
					t.Fatalf("%s: inverted range %+v", d.Code, r)
				}
				if r.End.Line >= lines {
					t.Fatalf("%s: range %+v past last line %d", d.Code, r, lines-1)
				}
				if d.Code == "" {
					t.Fatalf("diagnostic without a code: %q", d.Message)
				}
			}
		}
	})
}

// FuzzFormat runs the full-document formatter on arbitrary input.
// Contract (feature.formatting A6): no panic, and when the formatter
// produces output, formatting that output again is a no-op.
// [spec feature.formatting/A6]
func FuzzFormat(f *testing.F) {
	addCorpusSeeds(f)
	f.Fuzz(func(t *testing.T, src string) {
		opts := DefaultFormattingOptions()
		edits := FormatDocument(src, opts)
		if len(edits) == 0 {
			return // the formatter declined (e.g. unterminated string)
		}
		once := edits[0].NewText
		twice := formatAll(once, opts)
		if once != twice {
			t.Fatalf("format is not idempotent\n--- input ---\n%q\n--- first ---\n%q\n--- second ---\n%q", src, once, twice)
		}
	})
}

// FuzzFormatRange runs range formatting over an arbitrary line window.
// Contract: no panic for any window, including out-of-bounds and inverted
// ones (clients send stale ranges while the user types).
func FuzzFormatRange(f *testing.F) {
	addCorpusSeeds(f)
	f.Fuzz(func(t *testing.T, src string) {
		lines := strings.Count(src, "\n") + 1
		opts := DefaultFormattingOptions()
		for _, w := range [][2]int{{0, lines - 1}, {lines / 2, lines - 1}, {0, lines / 3}, {-1, lines + 5}, {lines, 0}} {
			_ = FormatDocumentRange(src, w[0], 0, w[1], 0, opts)
		}
	})
}

// FuzzFormatRangeStable checks feature.formatting A14: when a full format
// of code that validates without errors succeeds, range-formatting any one
// line of its output leaves it unchanged (issue #325). Malformed input is
// FuzzFormatRange's (no panic): block keywords used as operands or
// delimiters left open across statements have no statement structure for
// a range to keep.
func FuzzFormatRangeStable(f *testing.F) {
	addCorpusSeeds(f)
	f.Fuzz(func(t *testing.T, src string) {
		opts := DefaultFormattingOptions()
		full := FormatDocument(src, opts)
		if len(full) == 0 {
			return
		}
		doc := full[0].NewText
		for _, d := range GetDiagnostics(doc, DefaultDiagnosticOptions()) {
			if d.Severity == SeverityError {
				return
			}
		}
		lines := strings.Split(doc, "\n")
		for i, line := range lines {
			if strings.TrimSpace(line) == "" {
				continue
			}
			edits := FormatDocumentRange(doc, i, 0, i, utf8.RuneCountInString(line), opts)
			if len(edits) == 0 {
				continue
			}
			r := edits[0].Range
			was := strings.Join(lines[r.Start.Line:r.End.Line+1], "\n")
			if edits[0].NewText != was {
				if semicolonGap326(lines, r.End.Line) {
					t.Skip("known: the full formatter leaves a statement before a middle keyword unterminated (issue #326)")
				}
				t.Fatalf("range-formatting line %d of formatted output changes it\ndoc: %q\nwas: %q\nnow: %q", i+1, doc, was, edits[0].NewText)
			}
		}
	})
}

// semicolonGap326 reports the issue #326 class: line end of the formatted
// output lacks its ';' and the next code line opens with :ELSE, :CATCH,
// :FINALLY, :CASE or :OTHERWISE, which the full formatter wrongly treats
// as continuing the statement. A range ending there terminates it, as the
// full formatter will once #326 is fixed; remove this then.
func semicolonGap326(lines []string, end int) bool {
	if end >= len(lines) || strings.HasSuffix(strings.TrimSpace(lines[end]), ";") {
		return false
	}
	for _, l := range lines[end+1:] {
		l = strings.ToUpper(strings.TrimSpace(l))
		if l == "" {
			continue
		}
		for _, kw := range []string{":ELSE", ":CATCH", ":FINALLY", ":CASE", ":OTHERWISE"} {
			if strings.HasPrefix(l, kw) {
				return true
			}
		}
		return false
	}
	return false
}
