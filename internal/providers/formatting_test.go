package providers

import (
	"strings"
	"testing"
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

func TestFormatDocument_SpaceIndentation(t *testing.T) {
	input := `:PROCEDURE Test;:DECLARE x;:ENDPROC;`

	opts := DefaultFormattingOptions()
	opts.IndentStyle = "space"
	opts.IndentSize = 4

	edits := FormatDocument(input, opts)
	formatted := edits[0].NewText

	// Check that spaces are used instead of tabs
	if strings.Contains(formatted, "\t") {
		t.Error("formatted output should not contain tabs when using space indentation")
	}

	// Check that spaces are present for indentation
	if !strings.Contains(formatted, "    ") {
		t.Error("formatted output should contain 4 spaces for indentation")
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

	// Check that no line exceeds the max length significantly
	for i, line := range lines {
		// Allow some tolerance for continuation indent
		if len(line) > opts.MaxLineLength+10 && strings.TrimSpace(line) != "" {
			t.Errorf("line %d exceeds max length: %d chars (max %d): %s", i, len(line), opts.MaxLineLength, line)
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
	// SSL code with SQL string in SQLExecute function
	input := `ds := SQLExecute("SELECT id, name FROM users WHERE status = 'active' AND role = 'admin'");`

	opts := DefaultFormattingOptions()
	opts.SQL.Enabled = true

	edits := FormatDocument(input, opts)
	formatted := edits[0].NewText

	// SQL should be formatted with keywords uppercase and proper structure
	if !strings.Contains(formatted, "SELECT") {
		t.Error("SQL SELECT should be uppercase")
	}

	// Complex SQL should have line breaks
	if !strings.Contains(formatted, "\n") {
		t.Log("Note: SQL formatting may not produce line breaks for simple queries")
	}

	t.Logf("Formatted output:\n%s", formatted)
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
	input := `:PROCEDURE Test;:DECLARE x;:ENDPROC;`

	opts := DefaultFormattingOptions()
	opts.IndentStyle = "space"
	opts.IndentSize = 2

	edits := FormatDocument(input, opts)
	formatted := edits[0].NewText

	// Check that 2 spaces are used for indentation
	if !strings.Contains(formatted, "  :DECLARE") {
		t.Error("expected 2-space indentation")
	}
	if strings.Contains(formatted, "    :DECLARE") {
		t.Error("should not have 4-space indentation")
	}

	t.Logf("Formatted output:\n%s", formatted)
}

func TestFormattingOptions_NoOperatorSpacing(t *testing.T) {
	input := `x := 1 + 2;`

	opts := DefaultFormattingOptions()
	opts.OperatorSpacing = false

	edits := FormatDocument(input, opts)
	formatted := edits[0].NewText

	// With operator spacing disabled, the formatter should not add or normalize
	// spaces around operators. Verify the output contains the assignment.
	if !strings.Contains(formatted, ":=") {
		t.Error("formatted output should contain assignment operator")
	}
	// The formatter should not add extra spacing when disabled
	// Note: existing spaces in input may be preserved, but no new spacing should be enforced
	t.Logf("Formatted output:\n%s", formatted)
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
	// With before_operator wrapping, any operator (=, .AND., .OR.) is a valid wrap point.
	// The formatter wraps before the first operator that would cause overflow.
	lines := strings.Split(formatted, "\n")
	if len(lines) < 3 {
		t.Errorf("expected at least 3 lines from wrapping long condition, got %d:\n%s", len(lines), formatted)
	}
	t.Logf("Formatted output:\n%s", formatted)
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

// --- Trailing whitespace trimming ---

func TestFormatDocument_TrailingWhitespaceTrimmed(t *testing.T) {
	input := "x := 1;   \ny := 2;\t\t\n"

	opts := DefaultFormattingOptions()
	edits := FormatDocument(input, opts)
	formatted := edits[0].NewText

	lines := strings.Split(formatted, "\n")
	for i, line := range lines {
		if line != strings.TrimRight(line, " \t") {
			t.Errorf("line %d has trailing whitespace: %q", i+1, line)
		}
	}
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

func TestFormat_MaxConsecutiveBlankLines(t *testing.T) {
	src := "a\n\n\n\n\nb\n"
	opts := DefaultFormattingOptions()
	opts.MaxConsecutiveBlankLines = 2
	out := applyPostFormatPasses(src, opts)
	want := "a\n\n\nb\n" // a + 2 blank lines + b + trailing newline
	if out != want {
		t.Errorf("got %q, want %q", out, want)
	}
}

func TestFormat_DoesNotSplitMemberAccessAcrossLines(t *testing.T) {
	// vs-code-ssl-formatter#76 — wrapping a long line must keep `oVar:property`
	// together; never break before or after the member-access colon.
	src := "DoSomething(a, b, c, oCurrentRequest:somewhatLongPropertyName, d);\n"
	opts := DefaultFormattingOptions()
	opts.MaxLineLength = 50
	out := FormatDocument(src, opts)[0].NewText
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
}

func TestFormat_DoesNotSplitChainedMemberAccess(t *testing.T) {
	// `a:b:c` must remain intact when the line is too long.
	src := "DoSomething(arg1, arg2, oRoot:childObject:deeplyNamedField, arg3);\n"
	opts := DefaultFormattingOptions()
	opts.MaxLineLength = 40
	out := FormatDocument(src, opts)[0].NewText
	if !strings.Contains(out, "oRoot:childObject:deeplyNamedField") {
		t.Errorf("expected chained member access to stay together:\n%s", out)
	}
}

func TestFormat_DoesNotSplitMemberAccess_AssignmentLHS(t *testing.T) {
	// Even when `oVar:property` is the assignment target, it must not be
	// split.
	src := "oReceiverWithVeryLongName:propertyName := DoSomething(a, b, c);\n"
	opts := DefaultFormattingOptions()
	opts.MaxLineLength = 30
	out := FormatDocument(src, opts)[0].NewText
	if !strings.Contains(out, "oReceiverWithVeryLongName:propertyName") {
		t.Errorf("expected LHS member access to stay together:\n%s", out)
	}
}

func TestFormat_StillWrapsLongLinesWithMemberAccess(t *testing.T) {
	// The fix must not silently disable wrapping — if there's a valid
	// break point elsewhere, the formatter should still use it.
	src := "DoSomething(firstArg, secondArg, oVar:prop, thirdArg, fourthArg);\n"
	opts := DefaultFormattingOptions()
	opts.MaxLineLength = 30
	out := FormatDocument(src, opts)[0].NewText
	if strings.Count(out, "\n") < 2 {
		t.Errorf("expected the long line to wrap at SOME comma boundary:\n%s", out)
	}
}

func TestFormat_DoesNotSplitMethodCall(t *testing.T) {
	// `oVar:method(args)` uses the same member-access colon. The wrap
	// rule must keep the receiver, colon, and method name together.
	src := "DoSomething(arg1, arg2, oReceiver:doVeryLongMethodName(x), arg3);\n"
	opts := DefaultFormattingOptions()
	opts.MaxLineLength = 50
	out := FormatDocument(src, opts)[0].NewText
	if !strings.Contains(out, "oReceiver:doVeryLongMethodName") {
		t.Errorf("expected method call to stay glued:\n%s", out)
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
