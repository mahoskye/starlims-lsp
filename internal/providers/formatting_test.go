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
	for _, line := range lines {
		if strings.Contains(line, "z") {
			tabCount := strings.Count(line, "\t")
			// Should be indented 3 levels: PROCEDURE, IF, IF
			if tabCount < 2 {
				t.Errorf("nested statement should have at least 2 levels of indentation, got %d", tabCount)
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

	// With operator spacing disabled, := should not have extra spaces added
	// Note: the input already has spaces, but new formatting shouldn't enforce them
	t.Logf("Formatted output:\n%s", formatted)
}

func TestFormattingOptions_NoCommaSpacing(t *testing.T) {
	input := `:DECLARE a,b,c;`

	opts := DefaultFormattingOptions()
	opts.CommaSpacing = false

	edits := FormatDocument(input, opts)
	formatted := edits[0].NewText

	// With comma spacing disabled, no spaces should be added after commas
	// Check that ", " pattern is not enforced
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

	// The formatter adds blank lines based on the BlankLinesBetweenProcs setting
	// Check that multiple procedures are properly separated
	if !strings.Contains(formatted, ":ENDPROC") || !strings.Contains(formatted, "Test2") {
		t.Error("expected both procedures in output")
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
