package providers

import (
	"strings"
	"testing"
)

func TestFormatDocument_EdgeCase_Unicode(t *testing.T) {
	input := `:PROCEDURE Café;
:DECLARE δοκιμή;
value := "こんにちは";
:ENDPROC;`

	edits := FormatDocument(input, DefaultFormattingOptions())
	if len(edits) != 1 {
		t.Fatalf("expected 1 edit, got %d", len(edits))
	}
	formatted := edits[0].NewText
	for _, token := range []string{"Café", "δοκιμή", "こんにちは"} {
		if !strings.Contains(formatted, token) {
			t.Errorf("expected formatted output to contain %q", token)
		}
	}
}

func TestFormatDocument_EdgeCase_MixedLineEndings(t *testing.T) {
	input := ":PROCEDURE Test;\r\n:DECLARE x;\n:ENDPROC;\r\n"

	edits := FormatDocument(input, DefaultFormattingOptions())
	formatted := edits[0].NewText
	if strings.Contains(formatted, "\r") {
		t.Error("expected formatted output to normalize line endings")
	}
	if !strings.Contains(formatted, "\n") {
		t.Error("expected formatted output to contain newlines")
	}
}

func TestFormatDocument_EdgeCase_TabsAndSpaces(t *testing.T) {
	input := ":PROCEDURE Test;\n\t:DECLARE x;\n\tvalue := 1;\n:ENDPROC;"

	opts := DefaultFormattingOptions()
	opts.IndentStyle = "space"
	opts.IndentSize = 2

	edits := FormatDocument(input, opts)
	formatted := edits[0].NewText
	if strings.Contains(formatted, "\t") {
		t.Error("expected spaces instead of tabs for indentation")
	}
	if !strings.Contains(formatted, "  ") {
		t.Error("expected spaces for indentation")
	}
}

func TestFormatDocument_EdgeCase_VeryLongLine(t *testing.T) {
	input := `result := MyFunction(param1, param2, param3, param4, param5, param6, param7, param8);`

	opts := DefaultFormattingOptions()
	opts.MaxLineLength = 40

	edits := FormatDocument(input, opts)
	formatted := edits[0].NewText
	if len(strings.Split(strings.TrimSpace(formatted), "\n")) < 2 {
		t.Error("expected long line to be wrapped")
	}
}

func TestFormatDocument_EdgeCase_EmptyInput(t *testing.T) {
	edits := FormatDocument("", DefaultFormattingOptions())
	if len(edits) != 1 {
		t.Fatalf("expected 1 edit, got %d", len(edits))
	}
	if edits[0].NewText != "" {
		t.Errorf("expected empty formatted output, got %q", edits[0].NewText)
	}
}

func TestFormatDocument_EdgeCase_SingleToken(t *testing.T) {
	edits := FormatDocument("x", DefaultFormattingOptions())
	if len(edits) != 1 {
		t.Fatalf("expected 1 edit, got %d", len(edits))
	}
	if !strings.Contains(edits[0].NewText, "x") {
		t.Errorf("expected formatted output to contain token, got %q", edits[0].NewText)
	}
}

func TestSQLFormatter_EdgeCase_NestedSubquery(t *testing.T) {
	sql := "SELECT id FROM users WHERE id IN (SELECT user_id FROM orders WHERE total > 10)"

	formatter := NewSQLFormatter(DefaultSQLFormattingOptions())
	formatted := formatter.FormatSQL(sql, "")
	if strings.Count(formatted, "SELECT") < 2 {
		t.Errorf("expected multiple SELECT keywords, got %q", formatted)
	}
	if !strings.Contains(formatted, "\n") {
		t.Error("expected formatted SQL to contain newlines")
	}
}

func TestSQLFormatter_EdgeCase_Placeholders(t *testing.T) {
	sql := "SELECT * FROM users WHERE id = ?varName?"

	formatter := NewSQLFormatter(DefaultSQLFormattingOptions())
	formatted := formatter.FormatSQL(sql, "")
	if !strings.Contains(formatted, "?varName?") {
		t.Errorf("expected placeholder to remain, got %q", formatted)
	}
}

func TestSQLFormatter_EdgeCase_MalformedSQL(t *testing.T) {
	sql := "SELECT ( FROM"

	formatter := NewSQLFormatter(DefaultSQLFormattingOptions())
	formatted := formatter.FormatSQL(sql, "")
	if strings.TrimSpace(formatted) == "" {
		t.Error("expected formatted output even for malformed SQL")
	}
}
