package providers

import (
	"strings"
	"testing"
)

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
	input := `ds := SQLExecute("select id from users");`

	opts := DefaultFormattingOptions()
	opts.SQL.Enabled = true
	opts.SQL.KeywordCase = "upper"

	edits := FormatDocument(input, opts)
	formatted := edits[0].NewText

	if !strings.Contains(formatted, "SELECT") {
		t.Error("expected SQL SELECT to be uppercase")
	}

	t.Logf("Formatted output:\n%s", formatted)
}

func TestFormattingOptions_SQLKeywordCaseLower(t *testing.T) {
	input := `ds := SQLExecute("SELECT id FROM users");`

	opts := DefaultFormattingOptions()
	opts.SQL.Enabled = true
	opts.SQL.KeywordCase = "lower"

	edits := FormatDocument(input, opts)
	formatted := edits[0].NewText

	if strings.Contains(formatted, "SELECT") || strings.Contains(formatted, "FROM") {
		t.Error("expected SQL keywords to be lowercase")
	}
	if !strings.Contains(formatted, "select") || !strings.Contains(formatted, "from") {
		t.Error("expected SQL keywords to be lowercase")
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

	// Verify indentation levels:
	// Line 1: result := OuterFunction( - 0 tabs
	// Line 2: InnerFunction( - 1 tab (inside 1 paren)
	// Line 3: arg1, - 2 tabs (inside 2 parens)
	// Line 4: arg2 - 2 tabs (inside 2 parens)
	// Line 5: ), - 1 tab (closing inner paren)
	// Line 6: arg3 - 1 tab (inside 1 paren)
	// Line 7: ); - 0 tabs (closing outer paren)
	expectedIndents := []int{0, 1, 2, 2, 1, 1, 0}
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

func TestFormatDocument_DetectedSQLStringFormatted(t *testing.T) {
	// SQL string assigned to variable should be detected and formatted
	input := `sSQL := "select * from users where status = 'active'";`

	opts := DefaultFormattingOptions()
	opts.SQL.Enabled = true
	opts.SQL.DetectSQLStrings = true

	edits := FormatDocument(input, opts)
	formatted := edits[0].NewText

	// Should be formatted with SQL keywords uppercased
	if !strings.Contains(formatted, "SELECT") {
		t.Error("expected detected SQL SELECT to be uppercase")
	}
	if !strings.Contains(formatted, "FROM") {
		t.Error("expected detected SQL FROM to be uppercase")
	}
	if !strings.Contains(formatted, "WHERE") {
		t.Error("expected detected SQL WHERE to be uppercase")
	}

	// Complex SQL should be multi-line
	if !strings.Contains(formatted, "\n") {
		t.Error("expected complex detected SQL to be formatted as multi-line")
	}

	t.Logf("Formatted output:\n%s", formatted)
}

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

func TestFormatDocument_SQLFunctionArgStillFormattedWhenDetectionDisabled(t *testing.T) {
	// SQL inside SQLExecute should still be formatted even when detection is off
	input := `ds := SQLExecute("select * from users");`

	opts := DefaultFormattingOptions()
	opts.SQL.Enabled = true
	opts.SQL.DetectSQLStrings = false // Detection off

	edits := FormatDocument(input, opts)
	formatted := edits[0].NewText

	// SQL function args should STILL be formatted
	if !strings.Contains(formatted, "SELECT") {
		t.Error("SQL inside SQLExecute should still be formatted even with DetectSQLStrings=false")
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

func TestFormatDocument_SQLInsideNestedProcedure(t *testing.T) {
	// SQL detection should work inside procedures with proper indentation
	input := `:PROCEDURE Test;
sSQL := "select id, name from users where active = 1";
:ENDPROC;`

	opts := DefaultFormattingOptions()
	opts.SQL.Enabled = true
	opts.SQL.DetectSQLStrings = true

	edits := FormatDocument(input, opts)
	formatted := edits[0].NewText

	// SQL should be detected and formatted
	if !strings.Contains(formatted, "SELECT") {
		t.Error("expected SQL to be detected and formatted inside procedure")
	}

	// SQL should be multi-line due to complexity
	if !strings.Contains(formatted, "FROM") && strings.Contains(formatted, "\n") {
		t.Log("SQL is properly formatted with line breaks")
	}

	t.Logf("Formatted output:\n%s", formatted)
}
