package providers

import (
	"os"
	"path/filepath"
	"strings"
	"testing"

	"starlims-lsp/internal/lexer"
)

// [spec feature.formatting/A1] — full-document formatting returns exactly
// one edit spanning the whole document.
func TestFormatDocument_BasicIndentation(t *testing.T) {
	input := `:PROCEDURE Test;:DECLARE x;x:=1;:IF x=1;x:=2;:ENDIF;:ENDPROC;`

	opts := DefaultFormattingOptions()
	edits := FormatDocument(input, opts)

	if len(edits) != 1 {
		t.Fatalf("expected 1 edit, got %d", len(edits))
	}

	formatted := edits[0].NewText

	// Check that the output has proper structure
	if !strings.Contains(formatted, "\n") {
		t.Error("formatted output should contain newlines")
	}

	// Check indentation is present
	if !strings.Contains(formatted, "\t") {
		t.Error("formatted output should contain tabs for indentation")
	}

	if strings.Contains(formatted, "x := 1;:IF") {
		t.Error("formatted output should place statements on separate lines after semicolons")
	}

	t.Logf("Formatted output:\n%s", formatted)
}

func TestFormatDocument_OperatorSpacing(t *testing.T) {
	input := `x:=1;y:=x+2;`

	opts := DefaultFormattingOptions()
	edits := FormatDocument(input, opts)
	formatted := edits[0].NewText

	// Check operator spacing
	if !strings.Contains(formatted, " := ") {
		t.Error("formatted output should have spaces around :=")
	}

	if !strings.Contains(formatted, " + ") {
		t.Error("formatted output should have spaces around +")
	}

	t.Logf("Formatted output:\n%s", formatted)
}

func TestFormatDocument_CommaSpacing(t *testing.T) {
	input := `:DECLARE a,b,c;`

	opts := DefaultFormattingOptions()
	edits := FormatDocument(input, opts)
	formatted := edits[0].NewText

	// Check comma spacing
	if !strings.Contains(formatted, ", ") {
		t.Error("formatted output should have space after comma")
	}

	t.Logf("Formatted output:\n%s", formatted)
}

func TestFormatDocument_NestedBlocks(t *testing.T) {
	input := `:PROCEDURE Test;:IF x;:IF y;z:=1;:ENDIF;:ENDIF;:ENDPROC;`

	opts := DefaultFormattingOptions()
	edits := FormatDocument(input, opts)
	formatted := edits[0].NewText

	lines := strings.Split(formatted, "\n")

	// Find the line with z:=1 and check indentation level
	// z:=1 is nested 3 levels deep (PROCEDURE > IF > IF), expect at least 2 tabs
	const minExpectedIndentLevels = 2
	for _, line := range lines {
		if strings.Contains(line, "z") {
			tabCount := strings.Count(line, "\t")
			if tabCount < minExpectedIndentLevels {
				t.Errorf("nested statement should have at least %d levels of indentation, got %d",
					minExpectedIndentLevels, tabCount)
			}
		}
	}

	t.Logf("Formatted output:\n%s", formatted)
}

func TestFormatDocument_LineLengthWrap(t *testing.T) {
	// Long function call with many parameters
	input := `result := MyFunction(param1, param2, param3, param4, param5, param6, param7, param8);`

	opts := DefaultFormattingOptions()
	opts.MaxLineLength = 60 // Set a short line length to trigger wrapping

	edits := FormatDocument(input, opts)
	formatted := edits[0].NewText

	lines := strings.Split(formatted, "\n")

	// Check that the output was wrapped (has multiple lines)
	nonEmptyLines := 0
	for _, line := range lines {
		if strings.TrimSpace(line) != "" {
			nonEmptyLines++
		}
	}

	if nonEmptyLines < 2 {
		t.Error("expected line wrapping for long function call")
	}

	// The wrap engine guarantees the limit (tabs counted as IndentSize
	// columns) except for atomic tokens; this fixture has none over budget.
	for i, line := range lines {
		if w := visualWidth(line, opts); w > opts.MaxLineLength {
			t.Errorf("line %d exceeds max length: %d columns (max %d): %s", i, w, opts.MaxLineLength, line)
		}
	}

	t.Logf("Formatted output:\n%s", formatted)
}

func TestFormatDocument_NoWrapWhenDisabled(t *testing.T) {
	// Long function call with many parameters
	input := `result := MyFunction(param1, param2, param3, param4, param5, param6, param7, param8);`

	opts := DefaultFormattingOptions()
	opts.MaxLineLength = 0 // Disable line length limit

	edits := FormatDocument(input, opts)
	formatted := edits[0].NewText

	// Should stay on one line (plus trailing newline)
	lines := strings.Split(formatted, "\n")
	nonEmptyLines := 0
	for _, line := range lines {
		if strings.TrimSpace(line) != "" {
			nonEmptyLines++
		}
	}

	if nonEmptyLines != 1 {
		t.Errorf("expected 1 line when wrapping is disabled, got %d", nonEmptyLines)
	}

	t.Logf("Formatted output:\n%s", formatted)
}

func TestFormatDocument_SemicolonEnforcement(t *testing.T) {
	// Input with missing semicolons
	input := `:PROCEDURE Test
:DECLARE x
x := 1
:IF x = 1
x := 2
:ENDIF
:ENDPROC`

	opts := DefaultFormattingOptions()
	edits := FormatDocument(input, opts)
	formatted := edits[0].NewText

	// Check that semicolons were added
	if !strings.Contains(formatted, ":PROCEDURE Test;") {
		t.Error("semicolon should be added after PROCEDURE")
	}

	if !strings.Contains(formatted, ":DECLARE x;") {
		t.Error("semicolon should be added after DECLARE")
	}

	if !strings.Contains(formatted, ":IF x = 1;") {
		t.Error("semicolon should be added after IF condition")
	}

	if !strings.Contains(formatted, ":ENDIF;") {
		t.Error("semicolon should be added after ENDIF")
	}

	t.Logf("Formatted output:\n%s", formatted)
}

func TestFormatDocument_SemicolonEnforcementDisabled(t *testing.T) {
	// Input with missing semicolons
	input := `:DECLARE x
x := 1
:IF x = 1`

	opts := DefaultFormattingOptions()
	opts.SemicolonEnforcement = false

	edits := FormatDocument(input, opts)
	formatted := edits[0].NewText

	// Count semicolons - should be none since enforcement is disabled
	semicolonCount := strings.Count(formatted, ";")
	if semicolonCount > 0 {
		t.Errorf("expected no semicolons when enforcement is disabled, got %d", semicolonCount)
	}

	t.Logf("Formatted output:\n%s", formatted)
}

// [spec feature.formatting/A4] — SQL function arguments are delegated to
// the SQL formatter.
func TestFormatDocument_SQLStringFormatting(t *testing.T) {
	// SSL code with SQL string in SQLExecute function. The query fits
	// within MaxLineLength, so per issue #64 the SQL formatter leaves the
	// already-uppercase single-line query byte-identical.
	input := `ds := SQLExecute("SELECT id, name FROM users WHERE status = 'active' AND role = 'admin'");`

	opts := DefaultFormattingOptions()
	opts.SQL.Enabled = true

	edits := FormatDocument(input, opts)
	formatted := edits[0].NewText

	want := "ds := SQLExecute(\"SELECT id, name FROM users WHERE status = 'active' AND role = 'admin'\");\n"
	if formatted != want {
		t.Errorf("fitting single-line SQL should pass through unchanged\n got: %q\nwant: %q", formatted, want)
	}
}

// [spec feature.formatting/A5]
func TestFormatDocument_SQLFormattingDisabled(t *testing.T) {
	// SSL code with SQL string
	input := `ds := SQLExecute("select id from users");`

	opts := DefaultFormattingOptions()
	opts.SQL.Enabled = false

	edits := FormatDocument(input, opts)
	formatted := edits[0].NewText

	// SQL should remain lowercase when formatting is disabled
	if strings.Contains(formatted, "SELECT") {
		t.Error("SQL should not be formatted when disabled")
	}

	t.Logf("Formatted output:\n%s", formatted)
}

// Range formatting tests

// [spec feature.formatting/A2] — range formatting edits only the requested
// lines.
func TestFormatDocumentRange_BasicRange(t *testing.T) {
	// Document with multiple lines, we'll format just a portion
	input := `:PROCEDURE Test;
:DECLARE x;
x:=1;y:=2;z:=3;
:IF x=1;
x:=2;
:ENDIF;
:ENDPROC;`

	opts := DefaultFormattingOptions()

	// Format just line 2 (the x:=1;y:=2;z:=3; line)
	edits := FormatDocumentRange(input, 2, 0, 2, 15, opts)

	if len(edits) != 1 {
		t.Fatalf("expected 1 edit, got %d", len(edits))
	}

	formatted := edits[0].NewText

	// Should have operator spacing applied
	if !strings.Contains(formatted, " := ") {
		t.Error("formatted range should have operator spacing")
	}

	t.Logf("Formatted range:\n%s", formatted)
}

// [spec feature.formatting/A2] — surrounding base indentation is preserved.
func TestFormatDocumentRange_PreservesBaseIndentation(t *testing.T) {
	// Document with indented code
	input := `:PROCEDURE Test;
	:IF x=1;
		y:=2;z:=3;
	:ENDIF;
:ENDPROC;`

	opts := DefaultFormattingOptions()

	// Format the indented assignment line (line 2)
	edits := FormatDocumentRange(input, 2, 0, 2, 20, opts)

	if len(edits) != 1 {
		t.Fatalf("expected 1 edit, got %d", len(edits))
	}

	formatted := edits[0].NewText

	// Should preserve the base indentation (tabs)
	if !strings.HasPrefix(formatted, "\t\t") {
		t.Errorf("formatted range should preserve base indentation, got: %q", formatted)
	}

	t.Logf("Formatted range:\n%s", formatted)
}

func TestFormatDocumentRange_MultiLineRange(t *testing.T) {
	input := `:PROCEDURE Test;
x:=1;
y:=2;
z:=3;
:ENDPROC;`

	opts := DefaultFormattingOptions()

	// Format lines 1-3 (the assignment lines)
	edits := FormatDocumentRange(input, 1, 0, 3, 4, opts)

	if len(edits) != 1 {
		t.Fatalf("expected 1 edit, got %d", len(edits))
	}

	formatted := edits[0].NewText

	// Each assignment should have proper spacing
	if !strings.Contains(formatted, " := ") {
		t.Error("formatted range should have operator spacing")
	}

	t.Logf("Formatted range:\n%s", formatted)
}

func TestFormatDocumentRange_InvalidRange(t *testing.T) {
	input := `:PROCEDURE Test;
:ENDPROC;`

	opts := DefaultFormattingOptions()

	// Invalid range (start > end)
	edits := FormatDocumentRange(input, 5, 0, 1, 0, opts)

	if edits != nil {
		t.Error("expected nil edits for invalid range")
	}
}

func TestFormatDocumentRange_EmptyLines(t *testing.T) {
	input := `:PROCEDURE Test;

x:=1;

:ENDPROC;`

	opts := DefaultFormattingOptions()

	// Format a range that includes empty lines
	edits := FormatDocumentRange(input, 1, 0, 3, 0, opts)

	if len(edits) != 1 {
		t.Fatalf("expected 1 edit, got %d", len(edits))
	}

	formatted := edits[0].NewText

	// Should handle empty lines gracefully
	if !strings.Contains(formatted, " := ") {
		t.Error("formatted range should have operator spacing")
	}

	t.Logf("Formatted range:\n%s", formatted)
}

func TestFormatDocumentRange_SpaceIndentation(t *testing.T) {
	// Document with space indentation
	input := `:PROCEDURE Test;
    :IF x=1;
        y:=2;z:=3;
    :ENDIF;
:ENDPROC;`

	opts := DefaultFormattingOptions()
	opts.IndentStyle = "space"
	opts.IndentSize = 4

	// Format the indented assignment line (line 2)
	edits := FormatDocumentRange(input, 2, 0, 2, 20, opts)

	if len(edits) != 1 {
		t.Fatalf("expected 1 edit, got %d", len(edits))
	}

	formatted := edits[0].NewText

	// Should preserve the base space indentation
	if !strings.HasPrefix(formatted, "        ") {
		t.Errorf("formatted range should preserve space indentation, got: %q", formatted)
	}

	t.Logf("Formatted range:\n%s", formatted)
}

// Configuration change tests

func TestFormattingOptions_AllDefaults(t *testing.T) {
	opts := DefaultFormattingOptions()

	// Verify all defaults are set correctly
	if opts.IndentStyle != "tab" {
		t.Errorf("expected indent style 'tab', got %q", opts.IndentStyle)
	}
	if opts.IndentSize != 4 {
		t.Errorf("expected indent size 4, got %d", opts.IndentSize)
	}
	if opts.MaxLineLength != 90 {
		t.Errorf("expected max line length 90, got %d", opts.MaxLineLength)
	}
	if !opts.OperatorSpacing {
		t.Error("expected operator spacing to be true")
	}
	if !opts.CommaSpacing {
		t.Error("expected comma spacing to be true")
	}
	if !opts.SemicolonEnforcement {
		t.Error("expected semicolon enforcement to be true")
	}
	if opts.BlankLinesBetweenProcs != 1 {
		t.Errorf("expected blank lines between procs 1, got %d", opts.BlankLinesBetweenProcs)
	}
}

func TestFormattingOptions_CustomIndentSize(t *testing.T) {
	// Document-level space indentation: exact output proves the configured
	// size (2 spaces) is used and no tabs appear anywhere.
	input := `:PROCEDURE Test;:DECLARE x;:ENDPROC;`

	opts := DefaultFormattingOptions()
	opts.IndentStyle = "space"
	opts.IndentSize = 2

	edits := FormatDocument(input, opts)
	formatted := edits[0].NewText

	want := ":PROCEDURE Test;\n  :DECLARE x;\n:ENDPROC;\n"
	if formatted != want {
		t.Errorf("2-space indentation output mismatch\n got: %q\nwant: %q", formatted, want)
	}
}

func TestFormattingOptions_NoOperatorSpacing(t *testing.T) {
	// Glued input: with OperatorSpacing=false the formatter must not
	// introduce spaces around := or + — the statement passes through
	// byte-identical (plus the trailing newline).
	input := `x:=1+2;`

	opts := DefaultFormattingOptions()
	opts.OperatorSpacing = false

	edits := FormatDocument(input, opts)
	formatted := edits[0].NewText

	want := "x:=1+2;\n"
	if formatted != want {
		t.Errorf("OperatorSpacing=false must not introduce operator spacing\n got: %q\nwant: %q", formatted, want)
	}
}

func TestFormattingOptions_NoCommaSpacing(t *testing.T) {
	input := `:DECLARE a,b,c;`

	opts := DefaultFormattingOptions()
	opts.CommaSpacing = false

	edits := FormatDocument(input, opts)
	formatted := edits[0].NewText

	// With comma spacing disabled, commas should not have trailing space added
	if strings.Contains(formatted, ", ") {
		t.Error("expected no space after comma when CommaSpacing=false")
	}
	// Verify the declaration is still present
	if !strings.Contains(formatted, "a") || !strings.Contains(formatted, "b") || !strings.Contains(formatted, "c") {
		t.Error("expected all declared variables to be present")
	}
	t.Logf("Formatted output:\n%s", formatted)
}

func TestFormattingOptions_BlankLinesBetweenProcs(t *testing.T) {
	// Note: Blank lines between procs are added when the formatter sees a new
	// :PROCEDURE keyword after an :ENDPROC. This requires proper token sequence.
	input := `:PROCEDURE Test1;
:ENDPROC;
:PROCEDURE Test2;
:ENDPROC;`

	opts := DefaultFormattingOptions()
	opts.BlankLinesBetweenProcs = 2

	edits := FormatDocument(input, opts)
	formatted := edits[0].NewText

	// Verify both procedures are in output
	if !strings.Contains(formatted, ":ENDPROC") || !strings.Contains(formatted, "Test2") {
		t.Fatal("expected both procedures in output")
	}

	// Count blank lines between :ENDPROC and :PROCEDURE Test2
	endprocIdx := strings.Index(formatted, ":ENDPROC;")
	test2Idx := strings.Index(formatted, ":PROCEDURE Test2")
	if endprocIdx == -1 || test2Idx == -1 {
		t.Fatal("expected both :ENDPROC and :PROCEDURE Test2 in output")
	}
	between := formatted[endprocIdx+len(":ENDPROC;") : test2Idx]
	// Count newlines in the separator (blank lines = newlines - 1 for the line break itself)
	newlineCount := strings.Count(between, "\n")
	// We expect at least BlankLinesBetweenProcs blank lines, which means newlineCount >= BlankLinesBetweenProcs + 1
	if newlineCount < opts.BlankLinesBetweenProcs {
		t.Errorf("expected at least %d blank lines between procs, got %d newlines",
			opts.BlankLinesBetweenProcs, newlineCount)
	}

	t.Logf("Formatted output:\n%s", formatted)
}

func TestFormattingOptions_SQLKeywordCaseUpper(t *testing.T) {
	// Issue #64: short single-line SQL is now left as-is. Use input long
	// enough to trigger the reformat path so the keyword-case option
	// actually has output to act on.
	input := `ds := SQLExecute("select id, full_name, mailing_address, phone, email from users where status = 'active' and role = 'ADMIN'");`

	opts := DefaultFormattingOptions()
	opts.SQL.Enabled = true
	opts.SQL.KeywordCase = "upper"

	edits := FormatDocument(input, opts)
	formatted := edits[0].NewText

	if !strings.Contains(formatted, "SELECT") {
		t.Errorf("expected SQL SELECT to be uppercase, got:\n%s", formatted)
	}

	t.Logf("Formatted output:\n%s", formatted)
}

func TestFormattingOptions_SQLKeywordCaseLower(t *testing.T) {
	// Issue #64: short single-line SQL is now left as-is, so to exercise
	// the keyword-case option we need a query long enough to trigger the
	// reformat path.
	input := `ds := SQLExecute("SELECT id, full_name, mailing_address, phone, email FROM users WHERE status = 'active' AND role = 'ADMIN'");`

	opts := DefaultFormattingOptions()
	opts.SQL.Enabled = true
	opts.SQL.KeywordCase = "lower"

	edits := FormatDocument(input, opts)
	formatted := edits[0].NewText

	if strings.Contains(formatted, "SELECT") || strings.Contains(formatted, "FROM") {
		t.Errorf("expected SQL keywords to be lowercase, got:\n%s", formatted)
	}
	if !strings.Contains(formatted, "select") || !strings.Contains(formatted, "from") {
		t.Errorf("expected SQL keywords to be lowercase, got:\n%s", formatted)
	}

	t.Logf("Formatted output:\n%s", formatted)
}

func TestFormatDocument_CaseStatementIndentation(t *testing.T) {
	// Test CASE statement formatting per docs/features/formatting.md Section 7.4
	input := `:BEGINCASE;
:CASE x=1;
DoOne();
:EXITCASE;
:OTHERWISE;
DoDefault();
:EXITCASE;
:ENDCASE;`

	opts := DefaultFormattingOptions()
	edits := FormatDocument(input, opts)
	formatted := edits[0].NewText

	// Expected: :CASE and :OTHERWISE at same level as :BEGINCASE
	// Content inside :CASE/:OTHERWISE indented one level
	// :EXITCASE indented at content level (inside CASE block)

	lines := strings.Split(formatted, "\n")

	// Track indentation levels
	var beginCaseIndent, caseIndent, caseContentIndent int
	var otherwiseIndent, exitCaseIndent, endCaseIndent int

	for _, line := range lines {
		trimmed := strings.TrimSpace(line)
		indent := len(line) - len(strings.TrimLeft(line, "\t"))

		switch {
		case strings.HasPrefix(trimmed, ":BEGINCASE"):
			beginCaseIndent = indent
		case strings.HasPrefix(trimmed, ":CASE"):
			caseIndent = indent
		case strings.HasPrefix(trimmed, "DoOne"):
			caseContentIndent = indent
		case strings.HasPrefix(trimmed, ":OTHERWISE"):
			otherwiseIndent = indent
		case strings.HasPrefix(trimmed, ":EXITCASE"):
			exitCaseIndent = indent
		case strings.HasPrefix(trimmed, ":ENDCASE"):
			endCaseIndent = indent
		}
	}

	// :CASE should be at same level as :BEGINCASE (dedent then indent pattern)
	if caseIndent != beginCaseIndent {
		t.Errorf(":CASE should be at same level as :BEGINCASE, got CASE=%d BEGINCASE=%d", caseIndent, beginCaseIndent)
	}

	// :OTHERWISE should be at same level as :CASE
	if otherwiseIndent != caseIndent {
		t.Errorf(":OTHERWISE should be at same level as :CASE, got OTHERWISE=%d CASE=%d", otherwiseIndent, caseIndent)
	}

	// Content inside CASE should be indented one level from CASE
	if caseContentIndent != caseIndent+1 {
		t.Errorf("CASE content should be indented one level from :CASE, got content=%d CASE=%d", caseContentIndent, caseIndent)
	}

	// :EXITCASE should be at content level (indented inside CASE)
	if exitCaseIndent != caseContentIndent {
		t.Errorf(":EXITCASE should be at content level, got EXITCASE=%d content=%d", exitCaseIndent, caseContentIndent)
	}

	// :ENDCASE should be at same level as :BEGINCASE
	if endCaseIndent != beginCaseIndent {
		t.Errorf(":ENDCASE should be at same level as :BEGINCASE, got ENDCASE=%d BEGINCASE=%d", endCaseIndent, beginCaseIndent)
	}

	t.Logf("Formatted output:\n%s", formatted)
}

func TestFormatDocument_TryCatchFinallyIndentation(t *testing.T) {
	// Test TRY/CATCH/FINALLY formatting per docs/features/formatting.md Section 7.5
	input := `:TRY;
DoRisky();
:CATCH;
HandleError();
:FINALLY;
Cleanup();
:ENDTRY;`

	opts := DefaultFormattingOptions()
	edits := FormatDocument(input, opts)
	formatted := edits[0].NewText

	lines := strings.Split(formatted, "\n")

	var tryIndent, tryContentIndent int
	var catchIndent, catchContentIndent int
	var finallyIndent, finallyContentIndent int
	var endTryIndent int

	for _, line := range lines {
		trimmed := strings.TrimSpace(line)
		indent := len(line) - len(strings.TrimLeft(line, "\t"))

		switch {
		case strings.HasPrefix(trimmed, ":TRY"):
			tryIndent = indent
		case strings.HasPrefix(trimmed, "DoRisky"):
			tryContentIndent = indent
		case strings.HasPrefix(trimmed, ":CATCH"):
			catchIndent = indent
		case strings.HasPrefix(trimmed, "HandleError"):
			catchContentIndent = indent
		case strings.HasPrefix(trimmed, ":FINALLY"):
			finallyIndent = indent
		case strings.HasPrefix(trimmed, "Cleanup"):
			finallyContentIndent = indent
		case strings.HasPrefix(trimmed, ":ENDTRY"):
			endTryIndent = indent
		}
	}

	// :CATCH should be at same level as :TRY (dedent then indent)
	if catchIndent != tryIndent {
		t.Errorf(":CATCH should be at same level as :TRY, got CATCH=%d TRY=%d", catchIndent, tryIndent)
	}

	// :FINALLY should be at same level as :TRY
	if finallyIndent != tryIndent {
		t.Errorf(":FINALLY should be at same level as :TRY, got FINALLY=%d TRY=%d", finallyIndent, tryIndent)
	}

	// Content inside TRY should be indented
	if tryContentIndent != tryIndent+1 {
		t.Errorf("TRY content should be indented, got content=%d TRY=%d", tryContentIndent, tryIndent)
	}

	// Content inside CATCH should be indented
	if catchContentIndent != catchIndent+1 {
		t.Errorf("CATCH content should be indented, got content=%d CATCH=%d", catchContentIndent, catchIndent)
	}

	// Content inside FINALLY should be indented
	if finallyContentIndent != finallyIndent+1 {
		t.Errorf("FINALLY content should be indented, got content=%d FINALLY=%d", finallyContentIndent, finallyIndent)
	}

	// :ENDTRY should be at same level as :TRY
	if endTryIndent != tryIndent {
		t.Errorf(":ENDTRY should be at same level as :TRY, got ENDTRY=%d TRY=%d", endTryIndent, tryIndent)
	}

	t.Logf("Formatted output:\n%s", formatted)
}

func TestFormatDocument_IfElseIndentation(t *testing.T) {
	// Test IF/ELSE formatting - ELSE should be at same level as IF
	input := `:IF x=1;
DoOne();
:ELSE;
DoTwo();
:ENDIF;`

	opts := DefaultFormattingOptions()
	edits := FormatDocument(input, opts)
	formatted := edits[0].NewText

	lines := strings.Split(formatted, "\n")

	var ifIndent, ifContentIndent int
	var elseIndent, elseContentIndent int
	var endIfIndent int

	for _, line := range lines {
		trimmed := strings.TrimSpace(line)
		indent := len(line) - len(strings.TrimLeft(line, "\t"))

		switch {
		case strings.HasPrefix(trimmed, ":IF"):
			ifIndent = indent
		case strings.HasPrefix(trimmed, "DoOne"):
			ifContentIndent = indent
		case strings.HasPrefix(trimmed, ":ELSE"):
			elseIndent = indent
		case strings.HasPrefix(trimmed, "DoTwo"):
			elseContentIndent = indent
		case strings.HasPrefix(trimmed, ":ENDIF"):
			endIfIndent = indent
		}
	}

	// :ELSE should be at same level as :IF
	if elseIndent != ifIndent {
		t.Errorf(":ELSE should be at same level as :IF, got ELSE=%d IF=%d", elseIndent, ifIndent)
	}

	// Content inside IF should be indented
	if ifContentIndent != ifIndent+1 {
		t.Errorf("IF content should be indented, got content=%d IF=%d", ifContentIndent, ifIndent)
	}

	// Content inside ELSE should be indented
	if elseContentIndent != elseIndent+1 {
		t.Errorf("ELSE content should be indented, got content=%d ELSE=%d", elseContentIndent, elseIndent)
	}

	// :ENDIF should be at same level as :IF
	if endIfIndent != ifIndent {
		t.Errorf(":ENDIF should be at same level as :IF, got ENDIF=%d IF=%d", endIfIndent, ifIndent)
	}

	t.Logf("Formatted output:\n%s", formatted)
}

func TestFormatDocument_NestedCaseInProcedure(t *testing.T) {
	// Test CASE nested inside a procedure
	input := `:PROCEDURE Test;
:BEGINCASE;
:CASE x=1;
DoOne();
:ENDCASE;
:ENDPROC;`

	opts := DefaultFormattingOptions()
	edits := FormatDocument(input, opts)
	formatted := edits[0].NewText

	lines := strings.Split(formatted, "\n")

	var procIndent, beginCaseIndent, caseIndent, contentIndent int

	for _, line := range lines {
		trimmed := strings.TrimSpace(line)
		indent := len(line) - len(strings.TrimLeft(line, "\t"))

		switch {
		case strings.HasPrefix(trimmed, ":PROCEDURE"):
			procIndent = indent
		case strings.HasPrefix(trimmed, ":BEGINCASE"):
			beginCaseIndent = indent
		case strings.HasPrefix(trimmed, ":CASE"):
			caseIndent = indent
		case strings.HasPrefix(trimmed, "DoOne"):
			contentIndent = indent
		}
	}

	// :BEGINCASE should be indented inside procedure
	if beginCaseIndent != procIndent+1 {
		t.Errorf(":BEGINCASE should be indented inside procedure, got BEGINCASE=%d PROC=%d", beginCaseIndent, procIndent)
	}

	// :CASE should be at same level as :BEGINCASE
	if caseIndent != beginCaseIndent {
		t.Errorf(":CASE should be at same level as :BEGINCASE, got CASE=%d BEGINCASE=%d", caseIndent, beginCaseIndent)
	}

	// Content should be indented inside CASE
	if contentIndent != caseIndent+1 {
		t.Errorf("Content should be indented inside :CASE, got content=%d CASE=%d", contentIndent, caseIndent)
	}

	t.Logf("Formatted output:\n%s", formatted)
}

// ============================================================================
// End-of-Line Comment Preservation Tests
// ============================================================================

// [spec feature.formatting/A3] — comment content is preserved.
func TestFormatDocument_EndOfLineCommentPreserved(t *testing.T) {
	input := `x := 5;  /* set x to 5;`

	opts := DefaultFormattingOptions()
	edits := FormatDocument(input, opts)
	formatted := edits[0].NewText

	// The comment should stay on the same line as the code
	lines := strings.Split(strings.TrimSuffix(formatted, "\n"), "\n")
	if len(lines) != 1 {
		t.Errorf("expected 1 line (comment on same line as code), got %d lines:\n%s", len(lines), formatted)
	}

	if !strings.Contains(formatted, "x := 5;") || !strings.Contains(formatted, "/* set x to 5;") {
		t.Errorf("expected code and comment on same line, got:\n%s", formatted)
	}

	t.Logf("Formatted output:\n%s", formatted)
}

func TestFormatDocument_EndOfLineCommentAfterStatement(t *testing.T) {
	input := `:PROCEDURE Test;
x := 5;  /* initialize x;
y := 10; /* initialize y;
:ENDPROC;`

	opts := DefaultFormattingOptions()
	edits := FormatDocument(input, opts)
	formatted := edits[0].NewText

	lines := strings.Split(strings.TrimSuffix(formatted, "\n"), "\n")

	// Check that comments appear on same lines as their code
	foundXComment := false
	foundYComment := false
	for _, line := range lines {
		if strings.Contains(line, "x := 5") && strings.Contains(line, "/* initialize x") {
			foundXComment = true
		}
		if strings.Contains(line, "y := 10") && strings.Contains(line, "/* initialize y") {
			foundYComment = true
		}
	}

	if !foundXComment {
		t.Error("comment for x should be on same line as x := 5")
	}
	if !foundYComment {
		t.Error("comment for y should be on same line as y := 10")
	}

	t.Logf("Formatted output:\n%s", formatted)
}

func TestFormatDocument_MultiLineCommentNotEndOfLine(t *testing.T) {
	input := `/* This is a block comment
that spans multiple lines;
x := 5;`

	opts := DefaultFormattingOptions()
	edits := FormatDocument(input, opts)
	formatted := edits[0].NewText

	// Multi-line block comments should be preserved as-is, not treated as end-of-line
	if !strings.Contains(formatted, "/* This is a block comment") {
		t.Error("multi-line comment should be preserved")
	}

	t.Logf("Formatted output:\n%s", formatted)
}

// ============================================================================
// Multi-Line Structure Preservation Tests
// ============================================================================

func TestFormatDocument_MultiLineStructurePreserved(t *testing.T) {
	input := `result := OuterFunction(
    InnerFunction(
        arg1,
        arg2
    ),
    arg3
);`

	opts := DefaultFormattingOptions()
	edits := FormatDocument(input, opts)
	formatted := edits[0].NewText

	// The multi-line structure should be preserved
	lines := strings.Split(strings.TrimSuffix(formatted, "\n"), "\n")
	if len(lines) < 5 {
		t.Errorf("expected multi-line structure to be preserved (at least 5 lines), got %d lines:\n%s", len(lines), formatted)
	}

	// Check that InnerFunction is on its own line
	foundInnerFunction := false
	for _, line := range lines {
		if strings.Contains(line, "InnerFunction") && !strings.Contains(line, "OuterFunction") {
			foundInnerFunction = true
			break
		}
	}
	if !foundInnerFunction {
		t.Error("InnerFunction should be on its own line, not collapsed with OuterFunction")
	}

	// Verify indentation levels (fixed 1-level continuation per schema):
	// Line 1: result := OuterFunction( - 0 tabs
	// Line 2: InnerFunction( - 1 tab (continuation inside parens)
	// Line 3: arg1, - 1 tab (continuation, fixed 1 level)
	// Line 4: arg2 - 1 tab (continuation, fixed 1 level)
	// Line 5: ), - 0 tabs (closing paren aligns with opening level)
	// Line 6: arg3 - 1 tab (continuation inside parens)
	// Line 7: ); - 0 tabs (closing outer paren)
	expectedIndents := []int{0, 1, 1, 1, 0, 1, 0}
	for i, line := range lines {
		if i >= len(expectedIndents) {
			break
		}
		tabCount := 0
		for _, r := range line {
			if r == '\t' {
				tabCount++
			} else {
				break
			}
		}
		if tabCount != expectedIndents[i] {
			t.Errorf("line %d: expected %d tabs, got %d: %q", i+1, expectedIndents[i], tabCount, line)
		}
	}

	t.Logf("Formatted output:\n%s", formatted)
}

func TestFormatDocument_MultiLineArrayPreserved(t *testing.T) {
	input := `arr := {
    "first",
    "second",
    "third"
};`

	opts := DefaultFormattingOptions()
	edits := FormatDocument(input, opts)
	formatted := edits[0].NewText

	// The multi-line array should be preserved
	lines := strings.Split(strings.TrimSuffix(formatted, "\n"), "\n")
	if len(lines) < 4 {
		t.Errorf("expected multi-line array to be preserved (at least 4 lines), got %d lines:\n%s", len(lines), formatted)
	}

	// Verify indentation levels:
	// Line 1: arr := { - 0 tabs
	// Line 2: "first", - 1 tab (inside brace)
	// Line 3: "second", - 1 tab (inside brace)
	// Line 4: "third" - 1 tab (inside brace)
	// Line 5: }; - 0 tabs (closing brace)
	expectedIndents := []int{0, 1, 1, 1, 0}
	for i, line := range lines {
		if i >= len(expectedIndents) {
			break
		}
		tabCount := 0
		for _, r := range line {
			if r == '\t' {
				tabCount++
			} else {
				break
			}
		}
		if tabCount != expectedIndents[i] {
			t.Errorf("line %d: expected %d tabs, got %d: %q", i+1, expectedIndents[i], tabCount, line)
		}
	}

	t.Logf("Formatted output:\n%s", formatted)
}

// ============================================================================
// SQL String Detection and Formatting Tests
// ============================================================================

// [spec feature.formatting/A4] — detected standalone SQL strings are
// formatted when detection is on.
func TestFormatDocument_DetectedSQLStringFormatted(t *testing.T) {
	// Issue #64: a single-line SQL assignment that already fits within
	// MaxLineLength must not be reflowed. Reformatting `sSQL := "..."`
	// across multiple lines breaks the surrounding SSL syntax.
	// To exercise SQL detection + reformat, we use input that genuinely
	// overflows the line — then the SQL formatter kicks in, keywords get
	// uppercased, and the result is multi-line.
	input := `sSQL := "select id, full_name, mailing_address, phone, email, status, created_dt from users where status = 'active' and role_code = 'ADMIN'";`

	opts := DefaultFormattingOptions()
	opts.SQL.Enabled = true
	opts.SQL.DetectSQLStrings = true

	edits := FormatDocument(input, opts)
	formatted := edits[0].NewText

	if !strings.Contains(formatted, "SELECT") {
		t.Error("expected detected SQL SELECT to be uppercase")
	}
	if !strings.Contains(formatted, "FROM") {
		t.Error("expected detected SQL FROM to be uppercase")
	}
	if !strings.Contains(formatted, "WHERE") {
		t.Error("expected detected SQL WHERE to be uppercase")
	}
	if !strings.Contains(formatted, "\n") {
		t.Error("expected overflowing detected SQL to be formatted as multi-line")
	}

	t.Logf("Formatted output:\n%s", formatted)
}

// [spec feature.formatting/A7] — with detection off, standalone strings are
// untouched.
func TestFormatDocument_DetectedSQLStringDisabled(t *testing.T) {
	// When DetectSQLStrings is false, only SQL function args should be formatted
	input := `sSQL := "select * from users where status = 'active'";`

	opts := DefaultFormattingOptions()
	opts.SQL.Enabled = true
	opts.SQL.DetectSQLStrings = false

	edits := FormatDocument(input, opts)
	formatted := edits[0].NewText

	// SQL should NOT be formatted when detection is disabled
	if strings.Contains(formatted, "SELECT") {
		t.Error("expected SQL to NOT be formatted when DetectSQLStrings=false")
	}

	// Original lowercase keywords should be preserved
	if !strings.Contains(formatted, "select") {
		t.Error("expected original 'select' to be preserved when DetectSQLStrings=false")
	}

	t.Logf("Formatted output:\n%s", formatted)
}

// [spec feature.formatting/A3] — non-SQL string content is preserved.
// [spec feature.formatting/A4] — plain-English strings are not treated as SQL.
func TestFormatDocument_NonSQLStringNotFormatted(t *testing.T) {
	// Regular English strings should not be touched by SQL detection
	input := `msg := "Hello world, this is a message";`

	opts := DefaultFormattingOptions()
	opts.SQL.Enabled = true
	opts.SQL.DetectSQLStrings = true

	edits := FormatDocument(input, opts)
	formatted := edits[0].NewText

	// String should be unchanged
	if !strings.Contains(formatted, `"Hello world, this is a message"`) {
		t.Errorf("non-SQL string should not be modified, got:\n%s", formatted)
	}

	t.Logf("Formatted output:\n%s", formatted)
}

// [spec feature.formatting/A7] — SQL function arguments still format when
// detection is off.
func TestFormatDocument_SQLFunctionArgStillFormattedWhenDetectionDisabled(t *testing.T) {
	// Issue #64: SQL inside SQLExecute should still be formatted when
	// detection is off, but only when the line genuinely overflows. Use a
	// long enough query to trigger reformat.
	input := `ds := SQLExecute("select id, full_name, mailing_address, phone, email from users where status = 'active' and role = 'ADMIN'");`

	opts := DefaultFormattingOptions()
	opts.SQL.Enabled = true
	opts.SQL.DetectSQLStrings = false // Detection off

	edits := FormatDocument(input, opts)
	formatted := edits[0].NewText

	if !strings.Contains(formatted, "SELECT") {
		t.Error("SQL inside SQLExecute should still be formatted (and uppercased) when overflowing, even with DetectSQLStrings=false")
	}

	t.Logf("Formatted output:\n%s", formatted)
}

func TestFormatDocument_SimpleSQLStaysOneLine(t *testing.T) {
	// Simple SQL like "SELECT 1" should stay on one line
	input := `x := "SELECT 1";`

	opts := DefaultFormattingOptions()
	opts.SQL.Enabled = true
	opts.SQL.DetectSQLStrings = true

	edits := FormatDocument(input, opts)
	formatted := edits[0].NewText

	// Simple SQL should NOT be multi-line
	lines := strings.Split(strings.TrimSpace(formatted), "\n")
	if len(lines) > 1 {
		t.Errorf("simple SQL should stay on one line, got:\n%s", formatted)
	}

	t.Logf("Formatted output:\n%s", formatted)
}

func TestFormatDocument_InvalidSQLNotFormatted(t *testing.T) {
	// "UPDATE your settings" should not be detected as SQL (no SET keyword)
	input := `msg := "Update your settings in the configuration";`

	opts := DefaultFormattingOptions()
	opts.SQL.Enabled = true
	opts.SQL.DetectSQLStrings = true

	edits := FormatDocument(input, opts)
	formatted := edits[0].NewText

	// Should NOT be formatted as SQL (no uppercase UPDATE)
	// The original casing should be preserved
	if strings.Contains(formatted, "UPDATE YOUR SETTINGS") {
		t.Errorf("'Update your settings' should not be detected as SQL, got:\n%s", formatted)
	}

	t.Logf("Formatted output:\n%s", formatted)
}

// --- TestFormatDocument_SkippedParameterCommas ---

func TestFormatDocument_SkippedParameterCommas(t *testing.T) {
	input := `DoProc("P",{a,,b,,c});`

	opts := DefaultFormattingOptions()
	edits := FormatDocument(input, opts)
	formatted := edits[0].NewText

	// Space after first comma (after "P") but adjacent commas (,,) stay together
	if !strings.Contains(formatted, ",,") {
		t.Error("adjacent commas (skipped parameters) should be preserved without space between them")
	}
	if !strings.Contains(formatted, `"P", {`) {
		t.Error("expected space after first comma separating arguments")
	}

	t.Logf("Formatted output:\n%s", formatted)
}

// --- TestFormatDocument_ClassBodyNotIndented ---

func TestFormatDocument_ClassBodyNotIndented(t *testing.T) {
	input := `:CLASS MyClass;
:DECLARE sField;
:PROCEDURE Test;
:ENDPROC;`

	opts := DefaultFormattingOptions()
	edits := FormatDocument(input, opts)
	formatted := edits[0].NewText

	lines := strings.Split(formatted, "\n")
	for _, line := range lines {
		trimmed := strings.TrimSpace(line)
		if trimmed == "" {
			continue
		}
		indent := len(line) - len(strings.TrimLeft(line, "\t "))
		// Top-level class members (:DECLARE, :PROCEDURE, :ENDPROC) should be at indent 0
		if strings.HasPrefix(trimmed, ":CLASS") || strings.HasPrefix(trimmed, ":DECLARE") ||
			strings.HasPrefix(trimmed, ":PROCEDURE") || strings.HasPrefix(trimmed, ":ENDPROC") {
			if indent != 0 {
				t.Errorf("expected %q at indent level 0, got %d", trimmed, indent)
			}
		}
	}

	t.Logf("Formatted output:\n%s", formatted)
}

// --- TestFormatDocument_WhileLoopIndentation ---

func TestFormatDocument_WhileLoopIndentation(t *testing.T) {
	input := `:WHILE x > 0;
x := x - 1;
:ENDWHILE;`

	opts := DefaultFormattingOptions()
	edits := FormatDocument(input, opts)
	formatted := edits[0].NewText

	lines := strings.Split(formatted, "\n")

	var whileIndent, bodyIndent, endWhileIndent int
	for _, line := range lines {
		trimmed := strings.TrimSpace(line)
		indent := len(line) - len(strings.TrimLeft(line, "\t"))
		switch {
		case strings.HasPrefix(trimmed, ":WHILE"):
			whileIndent = indent
		case strings.HasPrefix(trimmed, "x :=") || strings.HasPrefix(trimmed, "x:="):
			bodyIndent = indent
		case strings.HasPrefix(trimmed, ":ENDWHILE"):
			endWhileIndent = indent
		}
	}

	if bodyIndent != whileIndent+1 {
		t.Errorf("body should be indented one level from :WHILE, got body=%d while=%d", bodyIndent, whileIndent)
	}
	if endWhileIndent != whileIndent {
		t.Errorf(":ENDWHILE should be at same level as :WHILE, got endwhile=%d while=%d", endWhileIndent, whileIndent)
	}

	t.Logf("Formatted output:\n%s", formatted)
}

// --- TestFormatDocument_ForLoopIndentation ---

func TestFormatDocument_ForLoopIndentation(t *testing.T) {
	input := `:FOR i := 1 :TO 10;
x := i;
:NEXT;`

	opts := DefaultFormattingOptions()
	edits := FormatDocument(input, opts)
	formatted := edits[0].NewText

	lines := strings.Split(formatted, "\n")

	var forIndent, bodyIndent, nextIndent int
	for _, line := range lines {
		trimmed := strings.TrimSpace(line)
		indent := len(line) - len(strings.TrimLeft(line, "\t"))
		switch {
		case strings.HasPrefix(trimmed, ":FOR"):
			forIndent = indent
		case strings.HasPrefix(trimmed, "x :=") || strings.HasPrefix(trimmed, "x:="):
			bodyIndent = indent
		case strings.HasPrefix(trimmed, ":NEXT"):
			nextIndent = indent
		}
	}

	if bodyIndent != forIndent+1 {
		t.Errorf("body should be indented one level from :FOR, got body=%d for=%d", bodyIndent, forIndent)
	}
	if nextIndent != forIndent {
		t.Errorf(":NEXT should be at same level as :FOR, got next=%d for=%d", nextIndent, forIndent)
	}

	t.Logf("Formatted output:\n%s", formatted)
}

// --- TestFormatDocument_IncrementDecrementNoSpaces ---

func TestFormatDocument_IncrementDecrementNoSpaces(t *testing.T) {
	// Source of truth: increment/decrement operators should NOT have spaces around them
	input := `:DECLARE i;
i++;
--i;`

	opts := DefaultFormattingOptions()
	edits := FormatDocument(input, opts)
	formatted := edits[0].NewText

	// ++ and -- should stay attached to their operand, not get spaces
	if strings.Contains(formatted, "i ++") || strings.Contains(formatted, "++ ;") {
		t.Errorf("increment operator should not have spaces around it, got:\n%s", formatted)
	}
	if strings.Contains(formatted, "-- i") {
		t.Errorf("decrement operator should not have spaces around it, got:\n%s", formatted)
	}

	t.Logf("Formatted output:\n%s", formatted)
}

// --- TestFormatDocument_UnaryNotNoLeadingSpace ---

func TestFormatDocument_UnaryNotNoLeadingSpace(t *testing.T) {
	// Source of truth: ! is a unary prefix operator, no space after it
	input := `:DECLARE bFlag;
:IF !bFlag;
:ENDIF;`

	opts := DefaultFormattingOptions()
	edits := FormatDocument(input, opts)
	formatted := edits[0].NewText

	if strings.Contains(formatted, "! bFlag") {
		t.Errorf("unary ! should not have a space after it, got:\n%s", formatted)
	}
	if !strings.Contains(formatted, "!bFlag") {
		t.Errorf("expected !bFlag without space, got:\n%s", formatted)
	}

	t.Logf("Formatted output:\n%s", formatted)
}

// --- TestFormatDocument_LogicalOperatorSpacing ---

func TestFormatDocument_LogicalOperatorSpacing(t *testing.T) {
	// Source of truth: .AND., .OR., .NOT. must have spaces around them
	input := `:DECLARE a, b;
:IF a .AND. b;
:ENDIF;
:IF a .OR. b;
:ENDIF;`

	opts := DefaultFormattingOptions()
	edits := FormatDocument(input, opts)
	formatted := edits[0].NewText

	if !strings.Contains(formatted, "a .AND. b") {
		t.Errorf("expected spaces around .AND., got:\n%s", formatted)
	}
	if !strings.Contains(formatted, "a .OR. b") {
		t.Errorf("expected spaces around .OR., got:\n%s", formatted)
	}

	t.Logf("Formatted output:\n%s", formatted)
}

func TestFormatDocument_SQLInsideNestedProcedure(t *testing.T) {
	// Issue #64: a short single-line SQL string that already fits should
	// NOT be reflowed across multiple lines. It should be left as-is so
	// that `sSQL := "select ...";` stays on one line.
	input := `:PROCEDURE Test;
sSQL := "select id, name from users where active = 1";
:ENDPROC;`

	opts := DefaultFormattingOptions()
	opts.SQL.Enabled = true
	opts.SQL.DetectSQLStrings = true

	edits := FormatDocument(input, opts)
	formatted := edits[0].NewText

	// The SQL line should remain a single line, with the original quote and
	// trailing semicolon all on the same line.
	if !strings.Contains(formatted, `sSQL := "select id, name from users where active = 1";`) {
		t.Errorf("expected single-line SQL to be preserved, got:\n%s", formatted)
	}

	t.Logf("Formatted output:\n%s", formatted)
}

// --- Gap 2: .AND./.OR. line wrapping ---

func TestFormatDocument_WrapBeforeAndOr(t *testing.T) {
	input := `:IF longConditionVariableA = 1 .AND. longConditionVariableB = 2 .OR. longConditionVariableC = 3;
:ENDIF;`
	opts := DefaultFormattingOptions()
	opts.MaxLineLength = 50
	edits := FormatDocument(input, opts)
	formatted := edits[0].NewText
	// With before_operator wrapping, the line breaks before .AND. / .OR.
	// and each continuation gets one extra level of indent.
	want := ":IF longConditionVariableA = 1\n" +
		"\t.AND. longConditionVariableB = 2\n" +
		"\t.OR. longConditionVariableC = 3;\n" +
		":ENDIF;\n"
	if formatted != want {
		t.Errorf("wrapped condition mismatch\n got: %q\nwant: %q", formatted, want)
	}
}

func TestFormatDocument_WrapBeforeStringConcatOperator(t *testing.T) {
	input := `sMsg := "Hello " + "World " + "this is a very long concatenated string " + "that should wrap";`
	opts := DefaultFormattingOptions()
	opts.MaxLineLength = 60
	edits := FormatDocument(input, opts)
	formatted := edits[0].NewText
	foundWrap := false
	for _, line := range strings.Split(formatted, "\n") {
		trimmed := strings.TrimSpace(line)
		if strings.HasPrefix(trimmed, "+ ") || strings.HasPrefix(trimmed, "+\t") {
			foundWrap = true
		}
	}
	if !foundWrap {
		t.Errorf("expected wrap before '+' operator in long concatenation, got:\n%s", formatted)
	}
	t.Logf("Formatted output:\n%s", formatted)
}

func TestFormatDocument_WrapBeforeArithmeticOperator(t *testing.T) {
	input := `nResult := nLongVariableName + nAnotherLongVariableName * nYetAnotherVariable - nFinalLongVariable;`
	opts := DefaultFormattingOptions()
	opts.MaxLineLength = 60
	edits := FormatDocument(input, opts)
	formatted := edits[0].NewText
	foundWrap := false
	for _, line := range strings.Split(formatted, "\n") {
		trimmed := strings.TrimSpace(line)
		if strings.HasPrefix(trimmed, "+") || strings.HasPrefix(trimmed, "*") || strings.HasPrefix(trimmed, "-") {
			foundWrap = true
		}
	}
	if !foundWrap {
		t.Errorf("expected wrap before arithmetic operator, got:\n%s", formatted)
	}
	t.Logf("Formatted output:\n%s", formatted)
}

func TestFormatDocument_NoWrapBeforeComparisonOperator(t *testing.T) {
	// Comparison operators bind tightly to operands — "a = 1" should not split.
	input := `:IF longVariableNameA = 1 .AND. longVariableNameB = 2;
:ENDIF;`
	opts := DefaultFormattingOptions()
	opts.MaxLineLength = 50
	edits := FormatDocument(input, opts)
	formatted := edits[0].NewText
	for _, line := range strings.Split(formatted, "\n") {
		trimmed := strings.TrimSpace(line)
		// A line should never start with a bare comparison operator
		if strings.HasPrefix(trimmed, "= ") || strings.HasPrefix(trimmed, "!= ") ||
			strings.HasPrefix(trimmed, "> ") || strings.HasPrefix(trimmed, "< ") ||
			strings.HasPrefix(trimmed, ">= ") || strings.HasPrefix(trimmed, "<= ") {
			t.Errorf("comparison operator should not be a wrap point, got line: %q", trimmed)
		}
	}
	t.Logf("Formatted output:\n%s", formatted)
}

// --- Gap 1: Blank lines between :REGION blocks ---

func TestFormatDocument_BlankLinesBetweenRegions(t *testing.T) {
	input := `:REGION Reg1;
x := 1;
:ENDREGION;
:REGION Reg2;
y := 2;
:ENDREGION;`
	opts := DefaultFormattingOptions()
	opts.BlankLinesBetweenProcs = 1
	edits := FormatDocument(input, opts)
	formatted := edits[0].NewText
	// Between :ENDREGION; and :REGION should be at least 2 newlines (line break + blank line)
	if !strings.Contains(formatted, ":ENDREGION;\n\n:REGION") {
		t.Errorf("expected blank line between regions, got:\n%s", formatted)
	}
	t.Logf("Formatted output:\n%s", formatted)
}

// --- Gap 3: No space before ( ---

func TestFormatDocument_NoSpaceBeforeFunctionParen(t *testing.T) {
	input := `result := MyFunc (a, b);`
	opts := DefaultFormattingOptions()
	edits := FormatDocument(input, opts)
	formatted := edits[0].NewText
	if strings.Contains(formatted, "MyFunc (") {
		t.Errorf("expected no space before ( in function call, got:\n%s", formatted)
	}
	if !strings.Contains(formatted, "MyFunc(") {
		t.Errorf("expected MyFunc( without space, got:\n%s", formatted)
	}
	t.Logf("Formatted output:\n%s", formatted)
}

// --- Gap 4: Member access colon normalization ---

func TestFormatDocument_MemberAccessColonNoSpace(t *testing.T) {
	input := `x := obj : prop;`
	opts := DefaultFormattingOptions()
	edits := FormatDocument(input, opts)
	formatted := edits[0].NewText
	if !strings.Contains(formatted, "obj:prop") {
		t.Errorf("expected obj:prop with no spaces around colon, got:\n%s", formatted)
	}
	// Make sure := assignment is NOT affected
	if !strings.Contains(formatted, " := ") {
		t.Errorf("expected spaces around :=, got:\n%s", formatted)
	}
	t.Logf("Formatted output:\n%s", formatted)
}

func TestFormatDocument_ChainedMemberAccessNoSpaces(t *testing.T) {
	// Chained member access: obj:method():prop should have no spaces around either colon
	input := `x := obj : method() : prop;`
	opts := DefaultFormattingOptions()
	edits := FormatDocument(input, opts)
	formatted := edits[0].NewText
	if !strings.Contains(formatted, "obj:method():prop") {
		t.Errorf("expected obj:method():prop with no spaces around colons, got:\n%s", formatted)
	}
	t.Logf("Formatted output:\n%s", formatted)
}

// --- Gap 5: Mashed :LABELName normalization ---

func TestFormatDocument_MashedLabelNormalization(t *testing.T) {
	input := `:LABELMyLabel;
x := 1;`
	opts := DefaultFormattingOptions()
	edits := FormatDocument(input, opts)
	formatted := edits[0].NewText
	if strings.Contains(formatted, ":LABELMyLabel") {
		t.Errorf("expected mashed label to be split, got:\n%s", formatted)
	}
	if !strings.Contains(formatted, ":LABEL MyLabel") {
		t.Errorf("expected ':LABEL MyLabel', got:\n%s", formatted)
	}
	t.Logf("Formatted output:\n%s", formatted)
}

// --- Keyword casing normalization ---

func TestFormatDocument_KeywordCasingNormalized(t *testing.T) {
	input := `:if x = 1;
	y := 2;
:endif;`

	opts := DefaultFormattingOptions()
	edits := FormatDocument(input, opts)
	formatted := edits[0].NewText

	if strings.Contains(formatted, ":if ") {
		t.Errorf("expected :if to be normalized to :IF, got:\n%s", formatted)
	}
	if strings.Contains(formatted, ":endif") {
		t.Errorf("expected :endif to be normalized to :ENDIF, got:\n%s", formatted)
	}
	if !strings.Contains(formatted, ":IF") {
		t.Errorf("expected :IF in output, got:\n%s", formatted)
	}
	if !strings.Contains(formatted, ":ENDIF") {
		t.Errorf("expected :ENDIF in output, got:\n%s", formatted)
	}
	t.Logf("Formatted output:\n%s", formatted)
}

// --- Space inside parens stripped ---

func TestFormatDocument_SpaceInsideParensStripped(t *testing.T) {
	input := `Len( sValue );`

	opts := DefaultFormattingOptions()
	edits := FormatDocument(input, opts)
	formatted := edits[0].NewText

	if strings.Contains(formatted, "( ") {
		t.Errorf("expected space after ( to be stripped, got:\n%s", formatted)
	}
	if strings.Contains(formatted, " )") {
		t.Errorf("expected space before ) to be stripped, got:\n%s", formatted)
	}
	if !strings.Contains(formatted, "Len(sValue)") {
		t.Errorf("expected Len(sValue) with no inner spaces, got:\n%s", formatted)
	}
	t.Logf("Formatted output:\n%s", formatted)
}

// --- Space before semicolons stripped ---

func TestFormatDocument_SpaceBeforeSemicolonStripped(t *testing.T) {
	input := `x := 1 ;`

	opts := DefaultFormattingOptions()
	edits := FormatDocument(input, opts)
	formatted := edits[0].NewText

	if strings.Contains(formatted, " ;") {
		t.Errorf("expected space before semicolon to be stripped, got:\n%s", formatted)
	}
	t.Logf("Formatted output:\n%s", formatted)
}

// --- :ERROR/:RESUME indentation ---

func TestFormatDocument_ErrorResumeIndentation(t *testing.T) {
	input := `:PROCEDURE Test;
:ERROR;
x := 1;
:RESUME;
y := 2;
:ENDPROC;`

	opts := DefaultFormattingOptions()
	edits := FormatDocument(input, opts)
	formatted := edits[0].NewText

	lines := strings.Split(strings.TrimSuffix(formatted, "\n"), "\n")
	// Expected structure:
	// :PROCEDURE Test;        (0 tabs)
	// \t:ERROR;               (1 tab - inside PROCEDURE)
	// \t\tx := 1;             (2 tabs - inside ERROR body)
	// \t:RESUME;              (1 tab - middle keyword, dedented)
	// \t\ty := 2;             (2 tabs - inside RESUME body)
	// :ENDPROC;               (0 tabs)

	for _, line := range lines {
		if strings.Contains(line, "x := 1") {
			tabCount := 0
			for _, r := range line {
				if r == '\t' {
					tabCount++
				} else {
					break
				}
			}
			if tabCount < 2 {
				t.Errorf("expected :ERROR body indented 2 levels, got %d tabs: %q", tabCount, line)
			}
		}
		if strings.Contains(line, ":RESUME") {
			tabCount := 0
			for _, r := range line {
				if r == '\t' {
					tabCount++
				} else {
					break
				}
			}
			if tabCount != 1 {
				t.Errorf("expected :RESUME at 1 tab (middle keyword), got %d tabs: %q", tabCount, line)
			}
		}
	}
	t.Logf("Formatted output:\n%s", formatted)
}

func TestFormat_TrimTrailingWhitespace(t *testing.T) {
	src := ":PROCEDURE Test;   \nnX := 1;\t\t\n:ENDPROC;\n"
	opts := DefaultFormattingOptions()
	opts.TrimTrailingWhitespace = true
	out := applyPostFormatPasses(src, opts)
	for i, line := range strings.Split(out, "\n") {
		if line != strings.TrimRight(line, " \t") {
			t.Errorf("line %d still has trailing whitespace: %q", i, line)
		}
	}
}

func TestFormat_MemberAccessNeverSplitOnWrap(t *testing.T) {
	// vs-code-ssl-formatter#76 — wrapping a long line must keep member
	// access glued: `oVar:property`, chained `a:b:c`, assignment-LHS
	// receivers, and `oVar:method(args)` never break before or after the
	// member-access colon — while wrapping itself stays active at
	// legitimate break points.
	cases := []struct {
		name     string
		src      string
		maxLine  int
		glued    string // member-access span that must survive intact
		wantWrap bool   // output must still wrap at some other break point
	}{
		{
			name:    "call argument",
			src:     "DoSomething(a, b, c, oCurrentRequest:somewhatLongPropertyName, d);\n",
			maxLine: 50,
			glued:   "oCurrentRequest:somewhatLongPropertyName",
		},
		{
			name:    "chained member access",
			src:     "DoSomething(arg1, arg2, oRoot:childObject:deeplyNamedField, arg3);\n",
			maxLine: 40,
			glued:   "oRoot:childObject:deeplyNamedField",
		},
		{
			name:    "assignment LHS",
			src:     "oReceiverWithVeryLongName:propertyName := DoSomething(a, b, c);\n",
			maxLine: 30,
			glued:   "oReceiverWithVeryLongName:propertyName",
		},
		{
			// The fix must not silently disable wrapping — if there's a
			// valid break point elsewhere, the formatter still uses it.
			name:     "wrapping still active elsewhere",
			src:      "DoSomething(firstArg, secondArg, oVar:prop, thirdArg, fourthArg);\n",
			maxLine:  30,
			glued:    "oVar:prop",
			wantWrap: true,
		},
		{
			name:    "method call",
			src:     "DoSomething(arg1, arg2, oReceiver:doVeryLongMethodName(x), arg3);\n",
			maxLine: 50,
			glued:   "oReceiver:doVeryLongMethodName",
		},
	}
	for _, c := range cases {
		t.Run(c.name, func(t *testing.T) {
			opts := DefaultFormattingOptions()
			opts.MaxLineLength = c.maxLine
			out := FormatDocument(c.src, opts)[0].NewText
			if !strings.Contains(out, c.glued) {
				t.Errorf("member access %q was split:\n%s", c.glued, out)
			}
			if c.wantWrap && strings.Count(out, "\n") < 2 {
				t.Errorf("expected the long line to wrap at SOME other break point:\n%s", out)
			}
			for _, line := range strings.Split(out, "\n") {
				trimmed := strings.TrimSpace(line)
				if strings.HasSuffix(trimmed, ":") {
					t.Errorf("line ends with member-access colon (split member access): %q\nfull output:\n%s", line, out)
				}
				if strings.HasPrefix(trimmed, ":") {
					// Must be a keyword (:IF, :ENDIF, etc.) — keywords are uppercase letters.
					rest := trimmed[1:]
					if rest == "" || !(rest[0] >= 'A' && rest[0] <= 'Z') {
						t.Errorf("line starts with non-keyword colon (split member access): %q\nfull output:\n%s", line, out)
					}
				}
			}
		})
	}
}

func TestFormat_BlankLineBetweenSiblingBlocks(t *testing.T) {
	// Two adjacent :IF / :ENDIF blocks at the same indent should be separated
	// by a blank line so they read as distinct units (vs-code-ssl-formatter#77).
	src := ":IF a;\n\tx := 1;\n:ENDIF;\n:IF b;\n\ty := 2;\n:ENDIF;\n"
	opts := DefaultFormattingOptions()
	out := applyPostFormatPasses(src, opts)
	want := ":IF a;\n\tx := 1;\n:ENDIF;\n\n:IF b;\n\ty := 2;\n:ENDIF;\n"
	if out != want {
		t.Errorf("expected blank line between sibling :IF blocks\n got: %q\nwant: %q", out, want)
	}
}

func TestFormat_BlankLineBetweenSiblingBlocks_AlreadySeparated(t *testing.T) {
	// If the user already left a blank line, the post-pass must not add another.
	src := ":IF a;\n:ENDIF;\n\n:IF b;\n:ENDIF;\n"
	opts := DefaultFormattingOptions()
	out := applyPostFormatPasses(src, opts)
	if out != src {
		t.Errorf("blank-already-present output drifted\n got: %q\nwant: %q", out, src)
	}
}

func TestFormat_BlankLineBetweenSiblingBlocks_DifferentIndents(t *testing.T) {
	// When :ENDIF closes an inner block and the next :IF sits at an OUTER indent,
	// they aren't siblings — no blank line should be inserted.
	src := ":IF outer;\n\t:IF inner;\n\t:ENDIF;\n:ENDIF;\n:IF next;\n:ENDIF;\n"
	opts := DefaultFormattingOptions()
	out := applyPostFormatPasses(src, opts)
	// The outer :ENDIF and the next :IF ARE at the same indent (both 0), so
	// they should be separated. The inner pair must NOT be touched.
	want := ":IF outer;\n\t:IF inner;\n\t:ENDIF;\n:ENDIF;\n\n:IF next;\n:ENDIF;\n"
	if out != want {
		t.Errorf("indent-aware sibling detection failed\n got: %q\nwant: %q", out, want)
	}
}

func TestFormat_BlankLineBetweenSiblingBlocks_MixedFamilies(t *testing.T) {
	// :ENDIF immediately followed by :WHILE at the same indent counts as
	// adjacent sibling blocks even though the families differ.
	src := ":IF a;\n:ENDIF;\n:WHILE b;\n:ENDWHILE;\n"
	opts := DefaultFormattingOptions()
	out := applyPostFormatPasses(src, opts)
	want := ":IF a;\n:ENDIF;\n\n:WHILE b;\n:ENDWHILE;\n"
	if out != want {
		t.Errorf("mixed-family separation failed\n got: %q\nwant: %q", out, want)
	}
}

func TestFormat_BlankLineBetweenSiblingBlocks_Tabs(t *testing.T) {
	// The indent-equality check must compare the raw leading-whitespace
	// strings — two lines with different indent characters (tabs vs spaces)
	// are NOT considered siblings even if they render the same width.
	src := "\t:IF inner;\n\t:ENDIF;\n    :IF outer;\n    :ENDIF;\n"
	opts := DefaultFormattingOptions()
	out := applyPostFormatPasses(src, opts)
	if out != src {
		t.Errorf("expected no insertion when indent characters differ\n got: %q\nwant: %q", out, src)
	}
}

func TestFormat_BlankLineBetweenSiblingBlocks_TrailingComment(t *testing.T) {
	// A trailing inline comment on the closer line still counts as the
	// closer — the firstKeyword helper looks at the keyword at the start
	// of the trimmed line, not the whole line.
	src := ":IF a;\n:ENDIF;  /* note ;\n:IF b;\n:ENDIF;\n"
	opts := DefaultFormattingOptions()
	out := applyPostFormatPasses(src, opts)
	want := ":IF a;\n:ENDIF;  /* note ;\n\n:IF b;\n:ENDIF;\n"
	if out != want {
		t.Errorf("trailing-comment closer not recognised\n got: %q\nwant: %q", out, want)
	}
}

func TestFormat_BlankLineBetweenSiblingBlocks_FullPipeline(t *testing.T) {
	// Exercise FormatDocument end-to-end (not just the post-pass) so we
	// catch any interaction with the token streamer.
	src := ":PROCEDURE Demo;\n:IF a;\n\tx := 1;\n:ENDIF;\n:IF b;\n\ty := 2;\n:ENDIF;\n:ENDPROC;\n"
	opts := DefaultFormattingOptions()
	out := FormatDocument(src, opts)[0].NewText
	if !strings.Contains(out, ":ENDIF;\n\n\t:IF b;") {
		t.Errorf("expected blank line between the two :IF blocks in full pipeline output:\n%s", out)
	}
}

func TestFormat_BlankLineBetweenSiblingBlocks_ClosersFollowedByOrdinaryCode(t *testing.T) {
	// :ENDIF followed by a plain assignment statement at the same indent is
	// NOT a block boundary — the post-pass must leave it alone.
	src := ":IF a;\n:ENDIF;\nx := 1;\n"
	opts := DefaultFormattingOptions()
	out := applyPostFormatPasses(src, opts)
	if out != src {
		t.Errorf("expected no blank line before plain statement\n got: %q\nwant: %q", out, src)
	}
}

func TestFormat_BlankLineBetweenSiblingBlocks_CommentBetween(t *testing.T) {
	// When a stand-alone comment sits between the closer and the next
	// opener, prevContent becomes the comment line — so the post-pass
	// does not insert a blank line. (The blank-line concern was about
	// tightly-packed blocks; a comment between them already acts as a
	// visual separator.)
	src := ":IF a;\n:ENDIF;\n/* divider ;\n:IF b;\n:ENDIF;\n"
	opts := DefaultFormattingOptions()
	out := applyPostFormatPasses(src, opts)
	if out != src {
		t.Errorf("expected no insertion when a comment separates the blocks\n got: %q\nwant: %q", out, src)
	}
}

func TestFormat_BlankLineBetweenSiblingBlocks_SurvivesMaxConsecutiveCap(t *testing.T) {
	// MaxConsecutiveBlankLines runs after our pass. A cap of 1 must let
	// the single inserted blank line pass through unchanged.
	src := ":IF a;\n:ENDIF;\n:IF b;\n:ENDIF;\n"
	opts := DefaultFormattingOptions()
	opts.MaxConsecutiveBlankLines = 1
	out := applyPostFormatPasses(src, opts)
	want := ":IF a;\n:ENDIF;\n\n:IF b;\n:ENDIF;\n"
	if out != want {
		t.Errorf("blank line should survive cap of 1\n got: %q\nwant: %q", out, want)
	}
}

func TestFormat_BlankLineBetweenSiblingBlocks_Disabled(t *testing.T) {
	src := ":IF a;\n:ENDIF;\n:IF b;\n:ENDIF;\n"
	opts := DefaultFormattingOptions()
	opts.BlankLineBetweenBlocks = false
	out := applyPostFormatPasses(src, opts)
	if out != src {
		t.Errorf("disabled flag should be a no-op\n got: %q\nwant: %q", out, src)
	}
}

func TestFormat_BuiltinFunctionCase_PascalCase(t *testing.T) {
	// The published inventory uses canonical PascalCase. `len` and `empty`
	// should be rewritten to their canonical forms `Len` and `Empty`. A
	// non-builtin identifier `myUserFn` should be left alone. An identifier
	// not followed by `(` should also be left alone.
	src := "x := len(s);\ny := myUserFn(z);\nempty := emptyVar;\n"
	opts := DefaultFormattingOptions()
	opts.BuiltinFunctionCase = "PascalCase"
	out := applyPostFormatPasses(src, opts)
	if !strings.Contains(out, "Len(s)") {
		t.Errorf("expected canonical Len(s) in output: %q", out)
	}
	if !strings.Contains(out, "myUserFn(z)") {
		t.Errorf("user-defined call should be preserved: %q", out)
	}
	if !strings.Contains(out, "emptyVar") {
		t.Errorf("identifier not followed by '(' should be untouched: %q", out)
	}
}

// ---------------------------------------------------------------------------
// Regression tests for embedded-SQL string-quote placement (anonymized).
// These pin user-specified rules E and F and are expected to FAIL until the
// formatter is updated.
// ---------------------------------------------------------------------------

// Rule F: the opening '"' of a multi-line SQL string stays on the same line
// as its assignment / call argument. Currently it gets pushed to a new line.
func TestFormatDocument_RuleF_OpenQuoteStaysOnAssignmentLine(t *testing.T) {
	input := `sMySQL := "SELECT a.col1, a.col2 FROM my_table a JOIN other_table b ON b.id = a.id WHERE a.col3 = 'x'";`

	opts := DefaultFormattingOptions()
	opts.SQL.Enabled = true
	opts.SQL.DetectSQLStrings = true

	edits := FormatDocument(input, opts)
	if len(edits) == 0 {
		t.Fatalf("expected at least one edit")
	}
	out := edits[0].NewText

	for _, line := range strings.Split(out, "\n") {
		if strings.TrimSpace(line) == `"` {
			t.Errorf("Rule F: stranded opening '\"' on its own line; should hug `:=` on the previous line.\nfull output:\n%s", out)
		}
	}
	// The assignment line itself should end with the opening quote.
	first := strings.SplitN(out, "\n", 2)[0]
	if !strings.HasSuffix(strings.TrimRight(first, " \t"), `"`) {
		t.Errorf("Rule F: first line should end with opening '\"'. got: %q\nfull output:\n%s", first, out)
	}
}

// Rule E: do not insert a newline between the closing '"' of a multi-line SQL
// string and the trailing comma + remaining call args. The args follow inline.
func TestFormatDocument_RuleE_CloseQuoteHugsFollowingArgs(t *testing.T) {
	input := `x := MyFunc(commandString: "SELECT COUNT(*) FROM t1 a JOIN t2 b ON b.x = a.x WHERE a.col = ?", defaultValue: "0", friendlyName: "DB");`

	opts := DefaultFormattingOptions()
	opts.SQL.Enabled = true
	opts.SQL.DetectSQLStrings = true

	edits := FormatDocument(input, opts)
	if len(edits) == 0 {
		t.Fatalf("expected at least one edit")
	}
	out := edits[0].NewText

	for _, line := range strings.Split(out, "\n") {
		if strings.TrimSpace(line) == `",` {
			t.Errorf("Rule E: stranded '\",' on its own line; the comma + remaining args should follow inline.\nfull output:\n%s", out)
		}
	}
}

// Issue #64: short, already-fitting single-line SQL strings must NOT be
// reformatted — neither the assignment shape nor the call-site shape
// should be exploded across multiple lines.
func TestFormatDocument_Issue64_ShortSQLStringNotReformatted(t *testing.T) {
	cases := []struct {
		name  string
		input string
	}{
		{"assignment", `sVariantSQL := "SELECT * FROM DUAL";`},
		{"sqlexecute", `ds := SQLExecute("SELECT * FROM DUAL");`},
		{"runsql", `RunSQL("SELECT * FROM DUAL");`},
	}
	for _, c := range cases {
		t.Run(c.name, func(t *testing.T) {
			opts := DefaultFormattingOptions()
			opts.SQL.Enabled = true
			opts.SQL.DetectSQLStrings = true

			edits := FormatDocument(c.input, opts)
			if len(edits) == 0 {
				t.Fatalf("expected at least one edit")
			}
			out := edits[0].NewText

			// Output must be a single physical line (the trailing newline
			// added by the document formatter is fine, so trim it).
			trimmed := strings.TrimRight(out, "\n")
			if strings.Contains(trimmed, "\n") {
				t.Errorf("Issue #64: short SQL must stay single-line, got:\n%s", out)
			}
		})
	}
}

// TestFormatDocument_Idempotent pins the feature-level idempotence contract:
// formatting already-formatted output again under the same options must be
// byte-identical.
// [spec feature.formatting/A6]
func TestFormatDocument_Idempotent(t *testing.T) {
	input := `:PROCEDURE Test;
:PARAMETERS sName, nCount;
:DECLARE sResult;
sResult := "";
:IF nCount > 0;
	sResult := sName;
:ENDIF;
:RETURN sResult;
:ENDPROC;`

	opts := DefaultFormattingOptions()

	first := FormatDocument(input, opts)
	if len(first) != 1 {
		t.Fatalf("expected 1 edit, got %d", len(first))
	}
	once := first[0].NewText

	second := FormatDocument(once, opts)
	if len(second) != 1 {
		t.Fatalf("expected 1 edit on reformat, got %d", len(second))
	}
	twice := second[0].NewText

	if once != twice {
		t.Errorf("formatting is not idempotent:\n--- first pass ---\n%q\n--- second pass ---\n%q", once, twice)
	}
}

// ============================================================================
// Catalog formatter gap regressions (issues #33-#39)
// ============================================================================

// Issue #33 (fmt.blank_lines_between_procs) — the separator is normalized to
// exactly the configured count, replacing source blank lines rather than
// stacking on top of them.
func TestFormat_BlankLinesBetweenProcs_NormalizedNotAdditive(t *testing.T) {
	input := ":PROCEDURE First;\n:ENDPROC;\n\n\n\n:PROCEDURE Second;\n:ENDPROC;\n"
	opts := DefaultFormattingOptions()
	opts.BlankLinesBetweenProcs = 2
	out := FormatDocument(input, opts)[0].NewText
	want := ":PROCEDURE First;\n:ENDPROC;\n\n\n:PROCEDURE Second;\n:ENDPROC;\n"
	if out != want {
		t.Errorf("expected exactly 2 blank lines between procs\n got: %q\nwant: %q", out, want)
	}
}

// Issue #33 (fmt.blank_lines_between_procs) — a doc-comment block attached to
// the next procedure stays attached; the separating blank line is placed
// above the comment block.
func TestFormat_BlankLinesBetweenProcs_DocCommentStaysAttached(t *testing.T) {
	input := ":PROCEDURE First;\n:ENDPROC;\n\n/* First line of doc;\n/* second line of doc;\n:PROCEDURE Second;\n:ENDPROC;\n"
	opts := DefaultFormattingOptions()
	out := FormatDocument(input, opts)[0].NewText
	want := ":PROCEDURE First;\n:ENDPROC;\n\n/* First line of doc;\n/* second line of doc;\n:PROCEDURE Second;\n:ENDPROC;\n"
	if out != want {
		t.Errorf("doc comment should stay attached to its procedure\n got: %q\nwant: %q", out, want)
	}
}

// Issue #34 (fmt.builtin_function_case) — built-in names inside string
// literals and comments are literal text and must not be re-cased.
func TestFormat_BuiltinFunctionCase_SkipsStringsAndComments(t *testing.T) {
	input := "sMsg := \"please call upper(sInput) first\";\n/* call len(a) before use;\nnSize := len(sMsg);\n"
	opts := DefaultFormattingOptions()
	opts.BuiltinFunctionCase = "PascalCase"
	out := FormatDocument(input, opts)[0].NewText
	if !strings.Contains(out, "\"please call upper(sInput) first\"") {
		t.Errorf("string literal content was re-cased:\n%s", out)
	}
	if !strings.Contains(out, "/* call len(a) before use;") {
		t.Errorf("comment content was re-cased:\n%s", out)
	}
	if !strings.Contains(out, "nSize := Len(sMsg);") {
		t.Errorf("real call site should still be re-cased:\n%s", out)
	}
}

// Issue #35 (fmt.comma_spacing) — a stray space before a comma is removed;
// a space between adjacent commas collapses to the tight skipped-parameter
// form (last comma of the run keeps its trailing space).
func TestFormat_CommaSpacing_SpaceBeforeCommaRemoved(t *testing.T) {
	input := "aValues := {nFirst ,nSecond};\naSkips := {nFirst , ,nThird};\n"
	opts := DefaultFormattingOptions()
	out := FormatDocument(input, opts)[0].NewText
	want := "aValues := {nFirst, nSecond};\naSkips := {nFirst,, nThird};\n"
	if out != want {
		t.Errorf("space before comma should be removed\n got: %q\nwant: %q", out, want)
	}
}

// Issue #35 — with commaSpacing off, comma spacing (before and after) is
// left as written.
func TestFormat_CommaSpacing_Disabled_PreservesSpaceBeforeComma(t *testing.T) {
	input := "aValues := {nFirst ,nSecond};\n"
	opts := DefaultFormattingOptions()
	opts.CommaSpacing = false
	out := FormatDocument(input, opts)[0].NewText
	if !strings.Contains(out, "{nFirst ,nSecond}") {
		t.Errorf("with commaSpacing off the stray space should be preserved:\n%q", out)
	}
}

// Issue #36 (fmt.indent_style) — standalone comments are indented at the
// enclosing block depth like statements, including nested blocks.
func TestFormat_StandaloneCommentIndentedWithBlock(t *testing.T) {
	input := ":PROCEDURE Demo;\n/* outer comment;\n:IF bReady;\n/* inner comment;\nnValue := 1;\n:ENDIF;\n:ENDPROC;\n"
	opts := DefaultFormattingOptions()
	out := FormatDocument(input, opts)[0].NewText
	want := ":PROCEDURE Demo;\n\t/* outer comment;\n\t:IF bReady;\n\t\t/* inner comment;\n\t\tnValue := 1;\n\t:ENDIF;\n:ENDPROC;\n"
	if out != want {
		t.Errorf("comments should take the block indent\n got: %q\nwant: %q", out, want)
	}
}

// Issue #37 (fmt.max_consecutive_blank_lines) — the default 0 preserves
// source blank-line runs through a full format.
func TestFormat_MaxConsecutiveBlankLines_ZeroPreservesSourceRuns(t *testing.T) {
	input := "nFirst := 1;\n\n\nnSecond := 2;\n"
	opts := DefaultFormattingOptions() // MaxConsecutiveBlankLines: 0
	out := FormatDocument(input, opts)[0].NewText
	if out != input {
		t.Errorf("cap 0 must preserve source blank runs\n got: %q\nwant: %q", out, input)
	}
}

// Issue #37 — an intermediate cap (2) is reachable: five blank lines become
// two, an existing two-blank run is untouched.
func TestFormat_MaxConsecutiveBlankLines_CapTwo(t *testing.T) {
	input := "nFirst := 1;\n\n\n\n\n\nnSecond := 2;\n\n\nnThird := 3;\n"
	opts := DefaultFormattingOptions()
	opts.MaxConsecutiveBlankLines = 2
	out := FormatDocument(input, opts)[0].NewText
	want := "nFirst := 1;\n\n\nnSecond := 2;\n\n\nnThird := 3;\n"
	if out != want {
		t.Errorf("cap 2 should allow exactly two blank lines\n got: %q\nwant: %q", out, want)
	}
}

// Issue #38 (fmt.semicolon_enforcement) — a final statement with no trailing
// newline is terminated at end-of-file.
func TestFormat_SemicolonEnforcement_AtEOFWithoutNewline(t *testing.T) {
	input := ":DECLARE nValue;\nnValue := 1"
	opts := DefaultFormattingOptions()
	out := FormatDocument(input, opts)[0].NewText
	want := ":DECLARE nValue;\nnValue := 1;\n"
	if out != want {
		t.Errorf("final statement should get its semicolon at EOF\n got: %q\nwant: %q", out, want)
	}
}

// Issue #38 — the continuation guards still apply at EOF: a document ending
// mid-expression gets no semicolon.
func TestFormat_SemicolonEnforcement_NoSemicolonMidExpressionAtEOF(t *testing.T) {
	input := "nValue := 1 +"
	opts := DefaultFormattingOptions()
	out := FormatDocument(input, opts)[0].NewText
	if strings.Contains(out, ";") {
		t.Errorf("no semicolon may be added after a trailing operator at EOF: %q", out)
	}
}

// Issue #39 (fmt.trim_trailing_whitespace) — with the option off, line-end
// whitespace inside multi-line comment content survives the format.
func TestFormat_TrimTrailingWhitespaceDisabled_PreservesCommentInterior(t *testing.T) {
	input := "/* first line   \nsecond line;\nnValue := 1;\n"
	opts := DefaultFormattingOptions()
	opts.TrimTrailingWhitespace = false
	out := FormatDocument(input, opts)[0].NewText
	if !strings.Contains(out, "/* first line   \n") {
		t.Errorf("trailing whitespace inside comment should be preserved when trim is off:\n%q", out)
	}

	opts.TrimTrailingWhitespace = true
	trimmed := FormatDocument(input, opts)[0].NewText
	if !strings.Contains(trimmed, "/* first line\n") {
		t.Errorf("trailing whitespace inside comment should be trimmed when trim is on:\n%q", trimmed)
	}
}

// Issue #81: a bracket-quoted SQL string that overflows the line reflows with
// a ']' closer and formats idempotently — the second pass must not swallow the
// remainder of the document into the string.
func TestFormatDocument_BracketQuotedSQLStringRoundTrip(t *testing.T) {
	input := ":PROCEDURE BracketSql;\n" +
		":DECLARE sSql, aRes;\n" +
		"sSql := [SELECT sample_id, sample_name, sample_status FROM samples WHERE owner_name = 'O''Brien' AND sample_status = ?status? ORDER BY sample_id];\n" +
		"aRes := SQLExecute(sSql);\n" +
		":RETURN aRes;\n" +
		":ENDPROC;\n"
	opts := DefaultFormattingOptions()

	once := FormatDocument(input, opts)[0].NewText
	if !strings.Contains(once, "];") {
		t.Fatalf("reflowed bracket string must close with ']':\n%s", once)
	}
	if strings.Contains(once, "[;") {
		t.Fatalf("reflowed bracket string must not close with '[':\n%s", once)
	}
	if !strings.Contains(once, ":ENDPROC;") {
		t.Fatalf("document structure lost after formatting:\n%s", once)
	}

	twice := FormatDocument(once, opts)[0].NewText
	if once != twice {
		t.Errorf("bracket-quoted SQL formatting is not idempotent.\nfirst:\n%s\nsecond:\n%s", once, twice)
	}
}

// Issue #82: over-90-col English message strings must be byte-preserved
// even though they contain SQL trigger words. [spec fmt.sql_in_strings]
func TestFormatDocument_EnglishStringsNotRewrittenAsSQL(t *testing.T) {
	input := ":PROCEDURE Messages;\n" +
		":DECLARE sMsgA, sMsgB, sMsgC;\n" +
		"sMsgA := \"Select the samples from the rack and update the status column before continuing with the run\";\n" +
		"sMsgB := \"Update your password and set a reminder so that it does not expire while you are away on leave\";\n" +
		"sMsgC := \"Delete old records from the archive folder after you have exported them to the backup share\";\n" +
		":RETURN .T.;\n" +
		":ENDPROC;\n"
	out := FormatDocument(input, DefaultFormattingOptions())[0].NewText
	for _, want := range []string{
		`"Select the samples from the rack and update the status column before continuing with the run"`,
		`"Update your password and set a reminder so that it does not expire while you are away on leave"`,
		`"Delete old records from the archive folder after you have exported them to the backup share"`,
	} {
		if !strings.Contains(out, want) {
			t.Errorf("message string was rewritten; want byte-preserved %s\ngot:\n%s", want, out)
		}
	}
}

// Issue #82: only argument 0 of a SQL function is a SQL candidate — the
// LSearch default value (argument 1) must never be reformatted, while the
// SQL argument still is.
func TestFormatDocument_SQLFunctionNonFirstArgPreserved(t *testing.T) {
	input := ":PROCEDURE ArgTest;\n" +
		":DECLARE sRes;\n" +
		"sRes := LSearch(\"SELECT name FROM samples WHERE id = ?\", \"Select a valid sample from the list and update your filter settings before retrying the search\", \"\", {nId});\n" +
		":RETURN sRes;\n" +
		":ENDPROC;\n"
	out := FormatDocument(input, DefaultFormattingOptions())[0].NewText
	if !strings.Contains(out, `"Select a valid sample from the list and update your filter settings before retrying the search"`) {
		t.Errorf("LSearch default value was rewritten:\n%s", out)
	}

	// Argument 0 still reflows when it overflows.
	input2 := ":PROCEDURE SqlTest;\n" +
		":DECLARE aRes;\n" +
		"aRes := SQLExecute(\"SELECT sample_id, sample_name, sample_status FROM samples WHERE sample_status = ?status? ORDER BY sample_id\");\n" +
		":RETURN aRes;\n" +
		":ENDPROC;\n"
	out2 := FormatDocument(input2, DefaultFormattingOptions())[0].NewText
	if !strings.Contains(out2, "\n\t    SELECT sample_id, sample_name, sample_status\n") {
		t.Errorf("SQL argument 0 should still reflow:\n%s", out2)
	}
}

// Issue #82: a nested call inside a SQL function's argument list must not
// end the SQL-function state early — the argument counter has to survive
// `Left(sX, 3)` appearing as argument 1.
func TestFormatDocument_SQLFunctionStateSurvivesNestedCalls(t *testing.T) {
	input := ":PROCEDURE Nested;\n" +
		":DECLARE sRes;\n" +
		"sRes := LSearch(\"SELECT name FROM samples WHERE id = ?\", Left(sFallbackValueName, 3), \"\", {\"Select one from the list and update it before you retry the whole search once more please\"});\n" +
		":RETURN sRes;\n" +
		":ENDPROC;\n"
	out := FormatDocument(input, DefaultFormattingOptions())[0].NewText
	if !strings.Contains(out, "Select one from the list and update it") {
		t.Errorf("string inside argument array was altered:\n%s", out)
	}
}

// Issue #85: an over-long atomic string is neither split nor moved — the
// line stays long (fmt.max_line_length) instead of wrapping to a
// continuation that still exceeds the limit and growing a blank line on
// every subsequent pass.
func TestFormatDocument_OverlongAtomicStringStaysPut(t *testing.T) {
	input := ":PROCEDURE LongString;\n" +
		":DECLARE sMsg;\n" +
		"sMsg := \"This message is deliberately long but is definitely not structured query language at all okay\";\n" +
		":RETURN sMsg;\n" +
		":ENDPROC;\n"
	opts := DefaultFormattingOptions()
	out := FormatDocument(input, opts)[0].NewText
	if !strings.Contains(out, "\tsMsg := \"This message is deliberately long") {
		t.Errorf("over-long string was moved off its assignment line:\n%s", out)
	}
	if FormatDocument(out, opts)[0].NewText != out {
		t.Errorf("not idempotent:\n%s", out)
	}
}

// Issue #86: a wrapped operator continuation keeps its one-level extra
// indent across re-formats — a line starting with a binary operator is a
// continuation of the previous expression.
func TestFormatDocument_OperatorContinuationIndentStable(t *testing.T) {
	input := ":PROCEDURE OpWrap;\n" +
		":DECLARE bResult;\n" +
		"bResult := bFirstConditionFlag .AND. bSecondConditionFlag .AND. bThirdConditionFlag .AND. bFourthFlag;\n" +
		":RETURN bResult;\n" +
		":ENDPROC;\n"
	opts := DefaultFormattingOptions()
	once := FormatDocument(input, opts)[0].NewText
	if !strings.Contains(once, "\n\t\t.AND. bFourthFlag;") {
		t.Errorf("wrapped continuation should sit one level past the statement:\n%s", once)
	}
	if FormatDocument(once, opts)[0].NewText != once {
		t.Errorf("continuation indent lost on re-format:\n%s", once)
	}
}

// Issue #87: a document ending in an unterminated string gets no edits —
// formatting used to append another semicolon on every pass.
func TestFormatDocument_UnterminatedStringNoEdits(t *testing.T) {
	inputs := []string{
		":PROCEDURE U;\nsX := \"never closed\nnY := 2;\n",
		"sX := 'half open",
		"sSql := [SELECT 1 FROM DUAL",
	}
	for _, input := range inputs {
		if edits := FormatDocument(input, DefaultFormattingOptions()); len(edits) != 0 {
			t.Errorf("expected no edits for unterminated string %q, got:\n%s", input, edits[0].NewText)
		}
	}
	// A terminated multi-line string still formats.
	ok := "sX := \"line one\nline two\";\nnY:=2;\n"
	edits := FormatDocument(ok, DefaultFormattingOptions())
	if len(edits) == 0 || !strings.Contains(edits[0].NewText, "nY := 2;") {
		t.Error("terminated multi-line string should still format")
	}
}

// Issue #88: operators glued to a preceding operator get exactly one space —
// the previous operator's trailing space is not doubled.
func TestFormatDocument_GluedOperatorsSingleSpace(t *testing.T) {
	input := "bFlag:=.not.bFlag;\nnC**=2;\nnB := nA - -3;\n"
	opts := DefaultFormattingOptions()
	out := FormatDocument(input, opts)[0].NewText
	if strings.Contains(out, "  ") {
		t.Errorf("double space in operator output:\n%q", out)
	}
	if !strings.Contains(out, "bFlag := .NOT. bFlag;") {
		t.Errorf("expected single-spaced glued operators:\n%s", out)
	}
	if !strings.Contains(out, "nB := nA - -3;") {
		t.Errorf("unary minus after binary minus must stay tight:\n%s", out)
	}
	if FormatDocument(out, opts)[0].NewText != out {
		t.Errorf("not idempotent:\n%s", out)
	}
}

// Issue #98: a range selection whose lines mix tab and space indentation
// keeps its anchor — the base indent falls back to the first non-blank
// line's indent instead of collapsing to column 0. [spec feature.formatting/A2]
func TestFormatDocumentRange_MixedIndentKeepsAnchor(t *testing.T) {
	doc := ":PROCEDURE P;\n    :IF bGo;\n\tnX:=1;\n    :ENDIF;\n:ENDPROC;\n"
	edits := FormatDocumentRange(doc, 1, 0, 3, 0, DefaultFormattingOptions())
	if len(edits) != 1 {
		t.Fatalf("expected one edit, got %d", len(edits))
	}
	got := edits[0].NewText
	if !strings.HasPrefix(got, "    :IF bGo;") {
		t.Errorf("selection lost its 4-space anchor:\n%q", got)
	}
	if strings.Contains(got, "\n:") {
		t.Errorf("a line was dedented to column 0:\n%q", got)
	}
}

// Issue #99: postfix increment/decrement end a complete statement and get
// semicolon enforcement.
func TestFormatDocument_IncrementDecrementSemicolonEnforced(t *testing.T) {
	input := ":PROCEDURE Inc;\n:DECLARE nX;\nnX++\nnX--\n:RETURN nX;\n:ENDPROC;\n"
	out := FormatDocument(input, DefaultFormattingOptions())[0].NewText
	if !strings.Contains(out, "nX++;") || !strings.Contains(out, "nX--;") {
		t.Errorf("expected semicolons on postfix inc/dec statements:\n%s", out)
	}
}

// Issue #90: dot logical operators are canonicalized to uppercase, and
// Me/Base receivers take canonical casing; a plain variable named me is
// left alone. [spec fmt.keyword_case]
func TestFormatDocument_DotOperatorAndReceiverCasing(t *testing.T) {
	input := ":CLASS C;\n:PROCEDURE M;\n:DECLARE bA;\nbA := bX .and. bY .or. .not. bZ;\nme:Helper();\nnT := base:Compute(1);\n:RETURN bA;\n:ENDPROC;\n"
	out := FormatDocument(input, DefaultFormattingOptions())[0].NewText
	for _, want := range []string{".AND.", ".OR.", ".NOT.", "Me:Helper()", "Base:Compute(1)"} {
		if !strings.Contains(out, want) {
			t.Errorf("missing canonical form %q in:\n%s", want, out)
		}
	}

	// Not receivers — untouched.
	plain := "me := 1;\nnX := me + 1;\n"
	outPlain := FormatDocument(plain, DefaultFormattingOptions())[0].NewText
	if !strings.Contains(outPlain, "me := 1;") || !strings.Contains(outPlain, "me + 1") {
		t.Errorf("identifier merely named me must not be recased:\n%s", outPlain)
	}
}

// Issue #89: the wrap engine's conformance guarantee — after formatting, a
// line exceeds the limit only when a single atomic token exceeds the
// budget. Runs the corpus and asserts every over-limit output line has no
// viable break candidate (approximated: contains a token wider than the
// remaining budget).
func TestWrapEngine_ConformanceGuaranteeOnCorpus(t *testing.T) {
	dir := filepath.Join("testdata", "idempotence")
	entries, err := os.ReadDir(dir)
	if err != nil {
		t.Fatalf("reading corpus: %v", err)
	}
	opts := DefaultFormattingOptions()
	for _, e := range entries {
		if e.IsDir() || !strings.HasSuffix(e.Name(), ".ssl") {
			continue
		}
		raw, _ := os.ReadFile(filepath.Join(dir, e.Name()))
		out := formatAll(string(raw), opts)

		// Widest single-line token per output line (atomic units).
		widest := map[int]int{}
		multi := map[int]bool{}
		for _, tok := range lexer.NewLexer(out).Tokenize() {
			if tok.Type == lexer.TokenEOF || tok.Type == lexer.TokenWhitespace {
				continue
			}
			li := tok.Line - 1
			if strings.Contains(tok.Text, "\n") {
				for l := li; l <= li+strings.Count(tok.Text, "\n"); l++ {
					multi[l] = true
				}
				continue
			}
			if w := visualWidth(tok.Text, opts); w > widest[li] {
				widest[li] = w
			}
		}

		// Corpus files allowed to contain over-limit lines: each holds an
		// unsplittable span (a long atomic string, a member-chain
		// comparison with no break points, or a callable glued to a long
		// string argument). Anything else exceeding the limit is a wrap
		// regression.
		allowed := map[string]bool{
			"english_overlong_strings.ssl": true,
			"overlong_string_wrap.ssl":     true,
			"sql_function_default_arg.ssl": true,
			"string_preservation.ssl":      true,
			"wrap_string_arguments.ssl":    true,
		}
		for ln, line := range strings.Split(out, "\n") {
			if visualWidth(line, opts) <= opts.MaxLineLength || multi[ln] {
				continue
			}
			budget := opts.MaxLineLength - (visualWidth(leadingIndentString(line), opts) + opts.IndentSize)
			if widest[ln] >= budget {
				continue // an atomic token alone exceeds the budget
			}
			if !allowed[e.Name()] {
				t.Errorf("%s:%d unexplained over-limit line (widest atom %d, budget %d):\n%s",
					e.Name(), ln+1, widest[ln], budget, line)
			}
		}
	}
}

// Issue #89: subscripts are atomic — a break never lands inside [...].
func TestWrapEngine_SubscriptNeverSplit(t *testing.T) {
	input := "vRes := CreateUdObject(\"MyNamespace.MyClassName\", {oParentObject:ChildCollection[nChildIndex], sConfigurationKey});\n"
	out := formatAll(input, DefaultFormattingOptions())
	if !strings.Contains(out, "ChildCollection[nChildIndex]") {
		t.Errorf("subscript was split:\n%s", out)
	}
	for _, line := range strings.Split(out, "\n") {
		if visualWidth(line, DefaultFormattingOptions()) > 90 {
			t.Errorf("line exceeds 90: %q", line)
		}
	}
}

// Issue #89: lines inside reflowed multi-line SQL are never re-wrapped.
func TestWrapEngine_MultilineSQLUntouched(t *testing.T) {
	input := "aRes := SQLExecute(\"SELECT sample_id, sample_name, sample_status FROM samples WHERE sample_status = ?status? ORDER BY sample_id\");\n"
	opts := DefaultFormattingOptions()
	once := formatAll(input, opts)
	if !strings.Contains(once, "\n    SELECT sample_id, sample_name, sample_status\n") {
		t.Fatalf("SQL should reflow:\n%s", once)
	}
	if formatAll(once, opts) != once {
		t.Errorf("multi-line SQL block not stable under wrap pass:\n%s", once)
	}
}

// CRLF input normalizes to LF-only output, stable on the second pass
// (schema files.line_endings). [spec feature.formatting/A10]
func TestFormatDocument_CRLFNormalizedToLF(t *testing.T) {
	input := ":PROCEDURE CrlfTest;\r\n:DECLARE nX;\r\nnX := 1;\r\n:ENDPROC;\r\n"
	opts := DefaultFormattingOptions()
	out := FormatDocument(input, opts)[0].NewText
	if strings.Contains(out, "\r") {
		t.Errorf("output must not contain CR bytes:\n%q", out)
	}
	if FormatDocument(out, opts)[0].NewText != out {
		t.Errorf("not idempotent after CRLF normalization")
	}
}

// Issue #101: a statement following a standalone comment on the same source
// line moves to its own line (one_statement_per_line) — it must not hide
// behind the comment. End-of-line comments after code stay attached.
func TestFormatDocument_CommentThenCodeSplits(t *testing.T) {
	input := ":PROCEDURE MixedLine;\n/* leading; nX := 1;\nnY := 2;  /* trailing;\n:RETURN nY;\n:ENDPROC;\n"
	opts := DefaultFormattingOptions()
	out := FormatDocument(input, opts)[0].NewText
	if !strings.Contains(out, "/* leading;\n\tnX := 1;") {
		t.Errorf("statement after standalone comment should move to its own line:\n%s", out)
	}
	if !strings.Contains(out, "nY := 2;  /* trailing;") {
		t.Errorf("end-of-line comment must stay attached to its statement:\n%s", out)
	}
	if FormatDocument(out, opts)[0].NewText != out {
		t.Errorf("not idempotent:\n%s", out)
	}
}

// Issue #91 (schema R42): code-block literals canonicalize to
// `{|params| expression}` — comma-space params, one space after the closing
// '|', configured spacing in the body. Unary signs stay glued; malformed or
// multi-line blocks pass through verbatim. [spec fmt.code_block_literals]
func TestFormatDocument_CodeBlockLiteralNormalized(t *testing.T) {
	opts := DefaultFormattingOptions()
	input := "fnAdd := {|a,b|a+b};\nfnT := {|x| x * 2};\nfnNeg := {|n| -n};\nfnStr := {|s| s + \"a,b  glued\"};\n"
	out := FormatDocument(input, opts)[0].NewText
	for _, want := range []string{
		"fnAdd := {|a, b| a + b};",
		"fnT := {|x| x * 2};",
		"fnNeg := {|n| -n};",
		"fnStr := {|s| s + \"a,b  glued\"};",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("missing %q in:\n%s", want, out)
		}
	}
	if FormatDocument(out, opts)[0].NewText != out {
		t.Errorf("not idempotent:\n%s", out)
	}
}

// BlankLinesBetweenProcs=0 disables proc-boundary normalization entirely:
// blank runs between procedures are preserved exactly as written, and no
// separator is inserted where none exists.
func TestFormat_BlankLinesBetweenProcs_ZeroDisablesNormalization(t *testing.T) {
	opts := DefaultFormattingOptions()
	opts.BlankLinesBetweenProcs = 0

	withBlanks := ":PROCEDURE First;\n:ENDPROC;\n\n\n\n:PROCEDURE Second;\n:ENDPROC;\n"
	out := FormatDocument(withBlanks, opts)[0].NewText
	if out != withBlanks {
		t.Errorf("blank run between procs should be preserved as written\n got: %q\nwant: %q", out, withBlanks)
	}

	packed := ":PROCEDURE First;\n:ENDPROC;\n:PROCEDURE Second;\n:ENDPROC;\n"
	outPacked := FormatDocument(packed, opts)[0].NewText
	if outPacked != packed {
		t.Errorf("no separator should be inserted between packed procs\n got: %q\nwant: %q", outPacked, packed)
	}
}

// Range-formatting a multi-line selection nested two levels deep: the
// selection keeps its two-tab anchor while statements are split one per
// line and operator spacing is normalized.
func TestFormatDocumentRange_NestedBlockMultiLine(t *testing.T) {
	doc := ":PROCEDURE Test;\n\t:IF bGo;\n\t\tx:=1;y:=2;\n\t\tz   :=3;\n\t:ENDIF;\n:ENDPROC;\n"
	opts := DefaultFormattingOptions()

	edits := FormatDocumentRange(doc, 2, 0, 3, 12, opts)
	if len(edits) != 1 {
		t.Fatalf("expected 1 edit, got %d", len(edits))
	}

	want := "\t\tx := 1;\n\t\ty := 2;\n\t\tz := 3;"
	if got := edits[0].NewText; got != want {
		t.Errorf("nested range output mismatch\n got: %q\nwant: %q", got, want)
	}
}

// Issue #92: builtinFunctionCase defaults to PascalCase — call sites take
// the canonical inventory casing out of the box; strings and comments stay
// untouched (issue #34 fences). [spec fmt.builtin_function_case]
func TestFormatDocument_BuiltinCasingDefaultPascalCase(t *testing.T) {
	input := "x := iif(bFlag, alltrim(sName), 2);\ns := \"call alltrim(x) here\";  /* alltrim(y);\n"
	out := FormatDocument(input, DefaultFormattingOptions())[0].NewText
	if !strings.Contains(out, "IIf(bFlag, AllTrim(sName), 2)") {
		t.Errorf("builtins should canonicalize by default:\n%s", out)
	}
	if !strings.Contains(out, `"call alltrim(x) here"`) || !strings.Contains(out, "/* alltrim(y);") {
		t.Errorf("strings/comments must stay untouched:\n%s", out)
	}
}

// Issue #164: region bodies are opaque payload — the formatter passes them
// through verbatim (no reindent, no semicolon enforcement, no SQL reflow)
// and the result is idempotent.
func TestFormatDocument_RegionBodyVerbatim(t *testing.T) {
	input := ":PROCEDURE P;\n:REGION Html;\n  <div onclick=\"if(a && b[0]) x.go()\">\n      raw   spacing kept\n  </div>\n:ENDREGION;\n:RETURN GetRegion(\"Html\");\n:ENDPROC;\n"

	opts := DefaultFormattingOptions()
	edits := FormatDocument(input, opts)
	if len(edits) != 1 {
		t.Fatalf("expected 1 edit, got %d", len(edits))
	}
	formatted := edits[0].NewText

	for _, line := range []string{
		"  <div onclick=\"if(a && b[0]) x.go()\">",
		"      raw   spacing kept",
		"  </div>",
	} {
		if !strings.Contains(formatted, line+"\n") {
			t.Errorf("region body line not preserved verbatim:\nwant line %q\ngot:\n%s", line, formatted)
		}
	}

	second := FormatDocument(formatted, opts)
	if len(second) == 1 && second[0].NewText != formatted {
		t.Errorf("region formatting not idempotent:\n--- first ---\n%s\n--- second ---\n%s", formatted, second[0].NewText)
	}
}

// Multiple consecutive end-of-line comments on one line all survive
// formatting, in order (issue #215 — the pending-EOL-comment slot merged
// by clobbering, silently deleting all but the last).
// [spec feature.formatting/A3]
func TestFormatDocument_MultipleEOLCommentsPreserved(t *testing.T) {
	input := `sPwd := ""; /*Encrypt(sU, sU); /* keep it simple;
sPath := sBase;/*+sWorkingDir; /*always pass without final slash;`
	got := input
	if edits := FormatDocument(input, DefaultFormattingOptions()); len(edits) > 0 {
		got = edits[0].NewText
	}
	for _, want := range []string{
		"/*Encrypt(sU, sU);", "/* keep it simple;",
		"/*+sWorkingDir;", "/*always pass without final slash;",
	} {
		if !strings.Contains(got, want) {
			t.Errorf("comment %q missing from formatted output:\n%s", want, got)
		}
	}
	again := got
	if edits := FormatDocument(got, DefaultFormattingOptions()); len(edits) > 0 {
		again = edits[0].NewText
	}
	if again != got {
		t.Errorf("multi-EOL-comment output not idempotent:\n%s\n--- second pass ---\n%s", got, again)
	}
}

// Detected-SQL strings with an unbalanced single-quote count are
// byte-preserved: they end/begin inside an open SQL character literal
// continued across concatenation, and respacing rewrites literal content
// (issue #216 — {d '...'} escapes, IN/LIKE patterns).
// [spec feature.formatting/A3]
func TestFormatDocument_OddQuoteSQLStringsBytePreserved(t *testing.T) {
	inputs := []string{
		`sSql := "update t set d = {d '" + sDate + "'} where id = ?";`,
		`sSql := "delete from t where name not in('" + sList + "')";`,
		`sSql := "select x from t where note like ('" + sPat + "%')";`,
	}
	for _, input := range inputs {
		got := input
		if edits := FormatDocument(input, DefaultFormattingOptions()); len(edits) > 0 {
			got = edits[0].NewText
		}
		orig := lexer.NewLexer(input).Tokenize()
		fmtd := lexer.NewLexer(got).Tokenize()
		var a, b []string
		for _, tok := range orig {
			if tok.Type == lexer.TokenString {
				a = append(a, tok.Text)
			}
		}
		for _, tok := range fmtd {
			if tok.Type == lexer.TokenString {
				b = append(b, tok.Text)
			}
		}
		if len(a) != len(b) {
			t.Fatalf("%q: string token count changed %d -> %d", input, len(a), len(b))
		}
		for i := range a {
			if a[i] != b[i] {
				t.Errorf("%q: string mutated:\n  orig %q\n  fmtd %q", input, a[i], b[i])
			}
		}
	}
}

// A declaration keyword ending its line takes its operand list from the
// following lines — semicolon enforcement must not truncate it
// (production-corpus shape; fmt.semicolon_enforcement).
func TestFormatDocument_NoForcedSemicolonAfterDeclarationKeyword(t *testing.T) {
	input := ":PARAMETERS \n\tchartNo, strRules\n\t, STD;\n:DEFAULT chartNo, \"\";"
	got := input
	if edits := FormatDocument(input, DefaultFormattingOptions()); len(edits) > 0 {
		got = edits[0].NewText
	}
	if strings.Contains(got, ":PARAMETERS;") {
		t.Fatalf("forced semicolon truncated the parameter list:\n%s", got)
	}
	diags := GetDiagnostics(got, DefaultDiagnosticOptions())
	for _, d := range diags {
		if d.Code == CodeDefaultAfterParameters {
			t.Errorf("formatting introduced default_after_parameters: %s", d.Message)
		}
	}
}

// Idempotence regressions from the production-corpus sweep (issue #218).
// Each case pins format(format(x)) == format(x) for a class that
// oscillated. [spec feature.formatting/A6]
func TestFormatDocument_IdempotenceRegressions218(t *testing.T) {
	cases := []struct {
		name  string
		input string
	}{
		{"comma-before-closer", `dbResponse := RunSQL("Delete from T where X in " + sOrigrec,,);`},
		{"comma-before-brace", `aRes := {DoProc("A.B.C", {uResult}),,,,,, };`},
		{"wrapped-call-continuation", `DoProc("Category.Script.Procedure", {LimsString(sContent), LimsString(sApp), LimsString(sGuid), LimsString(sLogFile), sMode, sFileName});`},
		{"wrapped-declare-list", `:DECLARE sFirstThing, sSecondThing, sThirdThing, sFourthThing, sFifthThing, sSixthThing, sSeventhLongThing;`},
		{"eol-comment-spacing", ":IF (sPlatform == \"ORACLE\" .AND. oErr:GenCode != 2443) .OR. /*cannot drop constraint - nonexistent;\n\t(sPlatform == \"MSSQL\" .AND. oErr:GenCode != 3728);/*is not a constraint per the runtime message text;\n\tErrorMes(\"cannot drop\", oErr:Message);\n:ENDIF;"},
		{"sql-line-comment", `aRows := SQLExecute("select r.id from rules r where r.active = 1 and ( -- rules with no mappings
    not exists (select m.id from mappings m where m.ruleid = r.id)) and r.site = ?sSite?");`},
	}
	for _, tc := range cases {
		f1 := tc.input
		if e := FormatDocument(tc.input, DefaultFormattingOptions()); len(e) > 0 {
			f1 = e[0].NewText
		}
		f2 := f1
		if e := FormatDocument(f1, DefaultFormattingOptions()); len(e) > 0 {
			f2 = e[0].NewText
		}
		if f1 != f2 {
			t.Errorf("%s: not idempotent\n--- pass1 ---\n%s\n--- pass2 ---\n%s", tc.name, f1, f2)
		}
	}
}

// A SQL '--' line comment must end its output line — gluing the next
// token into it hands the DBMS a query with the following code swallowed
// by the comment (issue #218 sweep finding).
func TestFormatDocument_SQLLineCommentForcesBreak(t *testing.T) {
	input := `aRows := SQLExecute("select r.id from rules r where r.active = 1 and ( -- rules with no mappings
    not exists (select m.id from mappings m where m.ruleid = r.id)) and r.site = ?sSite?");`
	got := input
	if e := FormatDocument(input, DefaultFormattingOptions()); len(e) > 0 {
		got = e[0].NewText
	}
	for _, line := range strings.Split(got, "\n") {
		if idx := strings.Index(line, "--"); idx >= 0 {
			rest := line[idx:]
			if strings.Contains(strings.ToUpper(rest), "NOT EXISTS") {
				t.Fatalf("code glued into SQL line comment: %q", line)
			}
		}
	}
}

// ODBC escapes are atomic spans (issue #217): '}' separates from a
// following alias, the scalar-function name after {fn and SQL_* tokens
// uppercase, interior identifiers keep the author's casing, and a
// placeholder's interior — quoted content included — is never respaced.
func TestFormatDocument_ODBCEscapesAndPlaceholders(t *testing.T) {
	input := `aRows := SQLExecute("select {fn ifnull(sc.owner,'')} as owner, {fn convert(SC.ITEMID, SQL_VARCHAR)}itemid from limssourcecontrol sc where sc.owner = ?sUser? and sc.moddate > {fn timestampadd(SQL_TSI_DAY, -30, current_timestamp)} and sc.created = ?'<<username>>'?");`
	got := input
	if e := FormatDocument(input, DefaultFormattingOptions()); len(e) > 0 {
		got = e[0].NewText
	}
	for _, want := range []string{
		"{fn IFNULL(sc.owner, '')} AS owner",
		"{fn CONVERT(SC.ITEMID, SQL_VARCHAR)} itemid",
		"{fn TIMESTAMPADD(SQL_TSI_DAY, -30, current_timestamp)}",
		"?'<<username>>'?",
	} {
		if !strings.Contains(got, want) {
			t.Errorf("missing %q in:\n%s", want, got)
		}
	}
	for _, bad := range []string{"}AS", "}itemid", "sql_varchar", "? '<<username>>' ?"} {
		if strings.Contains(got, bad) {
			t.Errorf("found %q (should not appear) in:\n%s", bad, got)
		}
	}
	again := got
	if e := FormatDocument(got, DefaultFormattingOptions()); len(e) > 0 {
		again = e[0].NewText
	}
	if again != got {
		t.Errorf("not idempotent:\n%s\n--- pass2 ---\n%s", got, again)
	}
}

// A rewrite of a detected-SQL string always takes the rule-F multi-line
// form — never an in-place padded single line (issue #219 decision).
// Unchanged short strings stay inline byte-identical.
func TestFormatDocument_SQLRewriteAlwaysRuleF(t *testing.T) {
	// Deep SSL indent forces the overflow path for a single-line string.
	input := ":IF a;\n:IF b;\n:IF c;\n\t\t\tsResult := SQLExecute(\"select   ordno,  folderno from orders where fldsts = 'Done' and dept = ?sDept?\");\n:ENDIF;\n:ENDIF;\n:ENDIF;"
	got := input
	if e := FormatDocument(input, DefaultFormattingOptions()); len(e) > 0 {
		got = e[0].NewText
	}
	if strings.Contains(got, `" select`) || strings.Contains(got, `?sDept? "`) {
		t.Errorf("padded single-line rewrite (should be rule-F multi-line):\n%s", got)
	}
	// Unchanged short SQL stays inline.
	short := `x := SQLExecute("select 1 from dual");`
	got2 := short
	if e := FormatDocument(short, DefaultFormattingOptions()); len(e) > 0 {
		got2 = e[0].NewText
	}
	if strings.TrimRight(got2, "\n") != short {
		t.Errorf("short unchanged SQL should stay inline:\n%s", got2)
	}
}
