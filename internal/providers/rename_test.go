package providers

import (
	"strings"
	"testing"

	"starlims-lsp/internal/lexer"
	"starlims-lsp/internal/parser"
)

// Helper function to parse text and extract procedures/variables
func parseText(text string) ([]parser.ProcedureInfo, []parser.VariableInfo) {
	lex := lexer.NewLexer(text)
	tokens := lex.Tokenize()
	p := parser.NewParser(tokens)
	ast := p.Parse()
	return p.ExtractProcedures(ast), p.ExtractVariables(ast)
}

// ==================== PrepareRename Tests ====================

func TestPrepareRename_Variable(t *testing.T) {
	text := `:PROCEDURE Test;
:DECLARE sName;
sName := "test";
:ENDPROC;`

	procedures, variables := parseText(text)

	// Position on sName in assignment (line 3, column 1)
	result := PrepareRename(text, 3, 1, "file:///test.ssl", procedures, variables)

	if result == nil {
		t.Fatal("expected prepare rename to succeed for variable")
	}

	if result.Placeholder != "sName" {
		t.Errorf("expected placeholder 'sName', got '%s'", result.Placeholder)
	}
}

func TestPrepareRename_Procedure(t *testing.T) {
	text := `:PROCEDURE MyProc;
x := 1;
:ENDPROC;

:PROCEDURE Caller;
DoProc("MyProc");
:ENDPROC;`

	procedures, variables := parseText(text)

	// Position on MyProc in procedure declaration (line 1, column 12)
	result := PrepareRename(text, 1, 12, "file:///test.ssl", procedures, variables)

	if result == nil {
		t.Fatal("expected prepare rename to succeed for procedure")
	}

	if result.Placeholder != "MyProc" {
		t.Errorf("expected placeholder 'MyProc', got '%s'", result.Placeholder)
	}
}

// [spec feature.rename/A3]
func TestPrepareRename_Keyword_NotAllowed(t *testing.T) {
	text := `:PROCEDURE Test;
:IF .T.;
x := 1;
:ENDIF;
:ENDPROC;`

	procedures, variables := parseText(text)

	// Position on IF keyword (line 2, column 2)
	result := PrepareRename(text, 2, 2, "file:///test.ssl", procedures, variables)

	if result != nil {
		t.Error("expected prepare rename to fail for keyword")
	}
}

// [spec feature.rename/A3]
func TestPrepareRename_BuiltinFunction_NotAllowed(t *testing.T) {
	text := `:PROCEDURE Test;
x := Len("hello");
:ENDPROC;`

	procedures, variables := parseText(text)

	// Position on Len function (line 2, column 6)
	result := PrepareRename(text, 2, 6, "file:///test.ssl", procedures, variables)

	if result != nil {
		t.Error("expected prepare rename to fail for built-in function")
	}
}

// [spec feature.rename/A4]
func TestPrepareRename_InsideString_NotAllowed(t *testing.T) {
	text := `:PROCEDURE Test;
x := "hello world";
:ENDPROC;`

	procedures, variables := parseText(text)

	// Position inside string (line 2, column 10)
	result := PrepareRename(text, 2, 10, "file:///test.ssl", procedures, variables)

	if result != nil {
		t.Error("expected prepare rename to fail inside string")
	}
}

// [spec feature.rename/A4]
func TestPrepareRename_InsideComment_NotAllowed(t *testing.T) {
	text := `:PROCEDURE Test;
/* This is a comment;
x := 1;
:ENDPROC;`

	procedures, variables := parseText(text)

	// Position inside comment (line 2, column 10)
	result := PrepareRename(text, 2, 10, "file:///test.ssl", procedures, variables)

	if result != nil {
		t.Error("expected prepare rename to fail inside comment")
	}
}

// [spec feature.rename/A3]
func TestPrepareRename_MeKeyword_NotAllowed(t *testing.T) {
	text := `:CLASS MyClass;
:PROCEDURE Init;
Me:Name := "test";
:ENDPROC;`

	procedures, variables := parseText(text)

	// Position on Me keyword (line 3, column 1)
	result := PrepareRename(text, 3, 1, "file:///test.ssl", procedures, variables)

	if result != nil {
		t.Error("expected prepare rename to fail for Me keyword")
	}
}

// ==================== Rename Tests ====================

// [spec feature.rename/A1]
func TestRename_Variable(t *testing.T) {
	text := `:PROCEDURE Test;
:DECLARE sName;
sName := "test";
x := sName;
:ENDPROC;`

	procedures, variables := parseText(text)

	// Rename sName to sNewName (line 3, column 1)
	result := Rename(text, 3, 1, "sNewName", "file:///test.ssl", procedures, variables)

	if result == nil {
		t.Fatal("expected rename to succeed")
	}

	edits, ok := result.Changes["file:///test.ssl"]
	if !ok {
		t.Fatal("expected edits for test.ssl")
	}

	// Should have 3 edits: declaration, assignment, usage
	if len(edits) != 3 {
		t.Errorf("expected 3 edits, got %d", len(edits))
	}

	// All edits should have the new name
	for _, edit := range edits {
		if edit.NewText != "sNewName" {
			t.Errorf("expected NewText 'sNewName', got '%s'", edit.NewText)
		}
	}
}

// [spec feature.rename/A6]
func TestRename_Procedure(t *testing.T) {
	text := `:PROCEDURE MyProc;
x := 1;
:ENDPROC;

:PROCEDURE Caller;
MyProc();
:ENDPROC;`

	procedures, variables := parseText(text)

	// Rename MyProc to NewProc (line 1, column 12)
	result := Rename(text, 1, 12, "NewProc", "file:///test.ssl", procedures, variables)

	if result == nil {
		t.Fatal("expected rename to succeed")
	}

	edits, ok := result.Changes["file:///test.ssl"]
	if !ok {
		t.Fatal("expected edits for test.ssl")
	}

	// Should have 2 edits: declaration and call
	if len(edits) != 2 {
		t.Errorf("expected 2 edits, got %d", len(edits))
	}
}

// [spec feature.rename/A2]
func TestRename_ScopedVariable(t *testing.T) {
	text := `:PROCEDURE Proc1;
:DECLARE sValue;
sValue := "one";
:ENDPROC;

:PROCEDURE Proc2;
:DECLARE sValue;
sValue := "two";
:ENDPROC;`

	procedures, variables := parseText(text)

	// Rename sValue in Proc1 only (line 3, column 1)
	result := Rename(text, 3, 1, "sNewValue", "file:///test.ssl", procedures, variables)

	if result == nil {
		t.Fatal("expected rename to succeed")
	}

	edits, ok := result.Changes["file:///test.ssl"]
	if !ok {
		t.Fatal("expected edits for test.ssl")
	}

	// Should only rename in Proc1 (2 occurrences), not in Proc2
	if len(edits) != 2 {
		t.Errorf("expected 2 edits (scoped to Proc1), got %d", len(edits))
	}

	// Verify edits are in Proc1 (lines 2 and 3)
	for _, edit := range edits {
		if edit.Range.Start.Line > 3 {
			t.Errorf("expected edits in Proc1 only, got edit on line %d", edit.Range.Start.Line)
		}
	}
}

// [spec feature.rename/A5]
func TestRename_InvalidNewName_Keyword(t *testing.T) {
	text := `:PROCEDURE Test;
:DECLARE sName;
sName := "test";
:ENDPROC;`

	procedures, variables := parseText(text)

	// Try to rename to a keyword
	result := Rename(text, 3, 1, "IF", "file:///test.ssl", procedures, variables)

	if result != nil {
		t.Error("expected rename to fail for keyword as new name")
	}
}

// [spec feature.rename/A5]
func TestRename_InvalidNewName_BuiltinFunction(t *testing.T) {
	text := `:PROCEDURE Test;
:DECLARE sName;
sName := "test";
:ENDPROC;`

	procedures, variables := parseText(text)

	// Try to rename to a built-in function
	result := Rename(text, 3, 1, "Len", "file:///test.ssl", procedures, variables)

	if result != nil {
		t.Error("expected rename to fail for built-in function as new name")
	}
}

// [spec feature.rename/A5]
func TestRename_InvalidNewName_InvalidChars(t *testing.T) {
	text := `:PROCEDURE Test;
:DECLARE sName;
sName := "test";
:ENDPROC;`

	procedures, variables := parseText(text)

	// Try to rename with invalid characters
	result := Rename(text, 3, 1, "my-name", "file:///test.ssl", procedures, variables)

	if result != nil {
		t.Error("expected rename to fail for invalid identifier")
	}
}

// [spec feature.rename/A1]
func TestRename_CaseInsensitive(t *testing.T) {
	text := `:PROCEDURE Test;
:DECLARE sName;
sname := "test";
SNAME := sname;
:ENDPROC;`

	procedures, variables := parseText(text)

	// Rename sName (line 2, column 10)
	result := Rename(text, 2, 10, "sNewName", "file:///test.ssl", procedures, variables)

	if result == nil {
		t.Fatal("expected rename to succeed")
	}

	edits, ok := result.Changes["file:///test.ssl"]
	if !ok {
		t.Fatal("expected edits for test.ssl")
	}

	// Should find all case variations: sName, sname, SNAME (at least 4 occurrences)
	if len(edits) < 4 {
		t.Errorf("expected at least 4 edits for case-insensitive rename, got %d", len(edits))
	}
}

// [spec feature.rename/A3]
func TestPrepareRename_BaseKeyword_NotAllowed(t *testing.T) {
	text := `:CLASS MyClass;
:INHERIT ParentClass;
:PROCEDURE Init;
Base:Init();
:ENDPROC;`

	procedures, variables := parseText(text)

	// Position on Base keyword (line 4, column 1)
	result := PrepareRename(text, 4, 1, "file:///test.ssl", procedures, variables)

	if result != nil {
		t.Error("expected prepare rename to fail for Base keyword")
	}
}

// [spec feature.rename/A3]
func TestPrepareRename_ConstructorKeyword_NotAllowed(t *testing.T) {
	text := `:CLASS MyClass;
:PROCEDURE Constructor;
:ENDPROC;`

	procedures, variables := parseText(text)

	// Position on Constructor (line 2, column 12)
	result := PrepareRename(text, 2, 12, "file:///test.ssl", procedures, variables)

	if result != nil {
		t.Error("expected prepare rename to fail for Constructor keyword")
	}
}

// ==================== Validation Tests ====================

func TestIsValidIdentifier_Valid(t *testing.T) {
	validNames := []string{
		"sName",
		"_private",
		"myVar123",
		"X",
		"some_long_variable_name",
	}

	for _, name := range validNames {
		if !isValidIdentifier(name) {
			t.Errorf("expected '%s' to be a valid identifier", name)
		}
	}
}

func TestIsValidIdentifier_Invalid(t *testing.T) {
	invalidNames := []string{
		"",
		"123abc",
		"my-name",
		"my name",
		"IF",          // keyword
		"DECLARE",     // keyword
		"Me",          // class-context form
		"Base",        // class-context form
		"Constructor", // class-context form
	}

	for _, name := range invalidNames {
		if isValidIdentifier(name) {
			t.Errorf("expected '%s' to be an invalid identifier", name)
		}
	}
}

func TestIsRenameableSymbol_Keywords(t *testing.T) {
	keywords := []string{"IF", "WHILE", "FOR", "DECLARE", "PROCEDURE", "ENDPROC"}

	for _, kw := range keywords {
		if isRenameableSymbol(kw, 1, 1, "", nil, nil) {
			t.Errorf("expected keyword '%s' to not be renameable", kw)
		}
	}
}

func TestIsRenameableSymbol_BuiltinFunctions(t *testing.T) {
	functions := []string{"Len", "Trim", "Upper", "Lower", "SQLExecute"}

	for _, fn := range functions {
		if isRenameableSymbol(fn, 1, 1, "", nil, nil) {
			t.Errorf("expected built-in function '%s' to not be renameable", fn)
		}
	}
}

func TestIsRenameableSymbol_UserVariable(t *testing.T) {
	variables := []parser.VariableInfo{
		{Name: "sUserVar", Line: 1, Column: 1},
	}

	if !isRenameableSymbol("sUserVar", 1, 1, "", nil, variables) {
		t.Error("expected user variable to be renameable")
	}
}

func TestIsRenameableSymbol_UserProcedure(t *testing.T) {
	procedures := []parser.ProcedureInfo{
		{Name: "MyCustomProc", StartLine: 1, EndLine: 5},
	}

	if !isRenameableSymbol("MyCustomProc", 1, 1, "", procedures, nil) {
		t.Error("expected user procedure to be renameable")
	}
}

// ==================== getWordRange Tests ====================

func TestGetWordRange_Basic(t *testing.T) {
	text := "sName := value"
	result := getWordRange(text, 1, 1, "sName")

	if result == nil {
		t.Fatal("expected to find word range")
	}

	if result.Start.Character != 0 || result.End.Character != 5 {
		t.Errorf("expected range [0, 5], got [%d, %d]", result.Start.Character, result.End.Character)
	}
}

func TestGetWordRange_MiddleOfLine(t *testing.T) {
	text := "x := sName + 5"
	result := getWordRange(text, 1, 8, "sName")

	if result == nil {
		t.Fatal("expected to find word range")
	}

	if result.Start.Character != 5 || result.End.Character != 10 {
		t.Errorf("expected range [5, 10], got [%d, %d]", result.Start.Character, result.End.Character)
	}
}

func TestGetWordRange_NotWholeWord(t *testing.T) {
	text := "sNamePrefix := 1"
	// Cursor is at 'sName' but it's part of 'sNamePrefix'
	result := getWordRange(text, 1, 3, "sName")

	if result != nil {
		t.Error("expected not to find word range when not a whole word")
	}
}

// ==================== Integration Tests ====================

// [spec feature.rename/A1]
func TestRename_EndToEnd(t *testing.T) {
	text := `:PROCEDURE ProcessData;
:PARAMETERS sInput;
:DECLARE sOutput;
sOutput := Upper(sInput);
:RETURN sOutput;
:ENDPROC;`

	procedures, variables := parseText(text)

	// First, prepare rename on sOutput
	prepResult := PrepareRename(text, 4, 1, "file:///test.ssl", procedures, variables)
	if prepResult == nil {
		t.Fatal("prepare rename should succeed")
	}

	// Then perform rename
	renameResult := Rename(text, 4, 1, "sResult", "file:///test.ssl", procedures, variables)
	if renameResult == nil {
		t.Fatal("rename should succeed")
	}

	edits := renameResult.Changes["file:///test.ssl"]

	// Apply edits to verify result (simplified - just count)
	// sOutput appears: declaration (line 3), assignment (line 4), return (line 5) = 3 times
	if len(edits) != 3 {
		t.Errorf("expected 3 edits, got %d", len(edits))
	}
}

// [spec feature.rename/A1]
func TestRename_ParameterVariable(t *testing.T) {
	text := `:PROCEDURE Calculate;
:PARAMETERS nValue;
result := nValue * 2;
:RETURN result + nValue;
:ENDPROC;`

	procedures, variables := parseText(text)

	// Rename parameter nValue
	result := Rename(text, 2, 13, "nInput", "file:///test.ssl", procedures, variables)

	if result == nil {
		t.Fatal("expected rename to succeed for parameter")
	}

	edits := result.Changes["file:///test.ssl"]

	// nValue appears 3 times: parameter, line 3, line 4
	if len(edits) != 3 {
		t.Errorf("expected 3 edits for parameter rename, got %d", len(edits))
	}
}

// [spec feature.rename/A6]
func TestRename_PublicVariable(t *testing.T) {
	text := `:PUBLIC gCounter;
gCounter := 0;

:PROCEDURE Increment;
gCounter := gCounter + 1;
:ENDPROC;

:PROCEDURE GetCounter;
:RETURN gCounter;
:ENDPROC;`

	procedures, variables := parseText(text)

	// Rename public variable gCounter
	result := Rename(text, 1, 10, "gCount", "file:///test.ssl", procedures, variables)

	if result == nil {
		t.Fatal("expected rename to succeed for public variable")
	}

	edits := result.Changes["file:///test.ssl"]

	// gCounter appears 5 times across the file
	if len(edits) < 5 {
		t.Errorf("expected at least 5 edits for public variable rename, got %d", len(edits))
	}
}

// TestRename_Procedure_UpdatesDoProcStringTarget: renaming a procedure also
// updates the dispatch string in DoProc("Name") so the call keeps working —
// the string target is a real reference (see feature.references/A7).
// [spec feature.rename/A7]
func TestRename_Procedure_UpdatesDoProcStringTarget(t *testing.T) {
	text := `:PROCEDURE CalcTotal;
:RETURN 100;
:ENDPROC;

:PROCEDURE Main;
result := DoProc("CalcTotal", {});
:ENDPROC;`

	procedures, variables := parseText(text)

	// Rename CalcTotal at its declaration (line 1, column 12)
	result := Rename(text, 1, 12, "CalculateTotal", "file:///test.ssl", procedures, variables)
	if result == nil {
		t.Fatal("expected rename to succeed")
	}

	edits := result.Changes["file:///test.ssl"]
	if len(edits) != 2 {
		t.Fatalf("expected 2 edits (declaration + DoProc string target), got %d", len(edits))
	}

	foundStringTarget := false
	for _, edit := range edits {
		if edit.NewText != "CalculateTotal" {
			t.Errorf("expected NewText 'CalculateTotal', got %q", edit.NewText)
		}
		if edit.Range.Start.Line == 5 { // 0-based: the DoProc call line
			foundStringTarget = true
			// The edit must cover exactly the name inside the quotes.
			if edit.Range.Start.Character != 18 || edit.Range.End.Character != 27 {
				t.Errorf("expected string-target edit at [18,27), got [%d,%d)",
					edit.Range.Start.Character, edit.Range.End.Character)
			}
		}
	}
	if !foundStringTarget {
		t.Error("expected the DoProc string target to be updated")
	}
}

// TestRename_LeavesCommentsAndUnrelatedStringsUntouched: renaming a symbol
// must not edit unrelated string literal or comment content that merely
// mentions the old name (issue #43). This test replaces the misnamed
// TestRename_DoesNotAffectStrings, which pinned the defect that string
// content WAS renamed. Dispatch strings are still renamed — see
// TestRename_Procedure_UpdatesDoProcStringTarget (feature.rename/A7).
// [spec feature.rename/A8]
func TestRename_LeavesCommentsAndUnrelatedStringsUntouched(t *testing.T) {
	text := `:PROCEDURE Test;
:DECLARE sName;
sName := "sName is a variable";
/* sName appears in this comment too;
:ENDPROC;`

	procedures, variables := parseText(text)

	result := Rename(text, 2, 10, "sNewName", "file:///test.ssl", procedures, variables)

	if result == nil {
		t.Fatal("expected rename to succeed")
	}

	edits := result.Changes["file:///test.ssl"]

	// Exactly 2 edits: the declaration and the assignment target — nothing
	// inside the string literal or the comment.
	if len(edits) != 2 {
		t.Errorf("expected exactly 2 edits (declaration + assignment), got %d", len(edits))
	}
	for _, edit := range edits {
		if edit.Range.Start.Line == 2 && edit.Range.Start.Character > 0 {
			t.Error("unrelated string content must not be edited by rename")
		}
		if edit.Range.Start.Line == 3 {
			t.Error("comment content must not be edited by rename")
		}
	}
}

// --- Dispatch-target prepare-rename (issue #125) ---

// [spec feature.rename/A9]
func TestPrepareRename_DispatchLastSegment(t *testing.T) {
	text := `:PROCEDURE Caller;
:RETURN ExecFunction("CAT.SCRIPT.CalcTotal", {});
:ENDPROC;`
	procedures, variables := parseText(text)

	lineText := `:RETURN ExecFunction("CAT.SCRIPT.CalcTotal", {});`
	segStart := strings.Index(lineText, "CalcTotal") // 0-based
	result := PrepareRename(text, 2, segStart+3, "file:///test.ssl", procedures, variables)
	if result == nil {
		t.Fatal("expected prepare rename to succeed on the dispatch last segment")
	}
	if result.Placeholder != "CalcTotal" {
		t.Errorf("placeholder = %q, want CalcTotal", result.Placeholder)
	}
	if result.Range.Start.Line != 1 || result.Range.Start.Character != segStart {
		t.Errorf("range start = %+v, want line 1 char %d", result.Range.Start, segStart)
	}
	if result.Range.End.Character != segStart+len("CalcTotal") {
		t.Errorf("range end char = %d, want %d", result.Range.End.Character, segStart+len("CalcTotal"))
	}
}

// [spec feature.rename/A15]
func TestPrepareRename_DispatchEarlierSegments_Rejected(t *testing.T) {
	text := `:PROCEDURE Caller;
:RETURN ExecFunction("CAT.SCRIPT.CalcTotal", {});
:ENDPROC;`
	procedures, variables := parseText(text)

	lineText := `:RETURN ExecFunction("CAT.SCRIPT.CalcTotal", {});`
	for _, seg := range []string{"CAT", "SCRIPT"} {
		col := strings.Index(lineText, seg) + 2 // 1-based, inside the segment
		if result := PrepareRename(text, 2, col, "file:///test.ssl", procedures, variables); result != nil {
			t.Errorf("expected rejection on the %s segment, got %+v", seg, result)
		}
	}
}

// [spec feature.rename/A4] — 1-part dispatch strings keep the string
// rejection; same-file rename starts from the identifier.
func TestPrepareRename_OnePartDispatchString_Rejected(t *testing.T) {
	text := `:PROCEDURE CalcTotal;
:ENDPROC;
:PROCEDURE Caller;
:RETURN DoProc("CalcTotal");
:ENDPROC;`
	procedures, variables := parseText(text)

	col := strings.Index(`:RETURN DoProc("CalcTotal");`, "CalcTotal") + 3
	if result := PrepareRename(text, 4, col, "file:///test.ssl", procedures, variables); result != nil {
		t.Errorf("expected rejection inside a 1-part dispatch string, got %+v", result)
	}
}
