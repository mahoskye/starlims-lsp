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
	return p.ExtractProcedures(ast), p.ExtractVariables()
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

// [spec feature.rename/A5] — NIL and built-in class names are not legal new
// names either (issue #258).
func TestRename_InvalidNewName_LiteralAndClass(t *testing.T) {
	text := `:PROCEDURE Test;
:DECLARE sName;
sName := "test";
:ENDPROC;`
	procedures, variables := parseText(text)
	for _, bad := range []string{"NIL", "nil", "Email", "SSLDataset"} {
		if result := Rename(text, 3, 1, bad, "file:///test.ssl", procedures, variables); result != nil {
			t.Errorf("expected rename to %q to be rejected", bad)
		}
	}
}

// [spec feature.rename/A18] — a dispatch string naming the like-spelled
// procedure is a procedure call site: renaming the variable (or a member)
// leaves `DoProc("Calc")` untouched (issue #258).
func TestRename_VariableLeavesLikeNamedDispatchString(t *testing.T) {
	text := `:PROCEDURE Calc;
:RETURN 1;
:ENDPROC;
:PROCEDURE Main;
:DECLARE Calc, nX, oObj;
Calc := 1;
nX := DoProc("Calc");
nX := oObj:Calc();
:ENDPROC;`
	procedures, variables := parseText(text)

	res := Rename(text, 6, 1, "nCalc", "file:///t.ssl", procedures, variables)
	if res == nil {
		t.Fatal("expected the variable rename to produce edits")
	}
	for _, line := range editedLines(res) {
		if line == 6 || line == 0 || line == 7 {
			t.Errorf("variable rename must not edit the dispatch string, the procedure, or the member (edited line %d)", line)
		}
	}
}

// [spec feature.rename/A21] — edit ranges are character (rune) columns: a
// non-ASCII character earlier on the line never skews the edit (issue #258).
func TestRename_NonASCIIBeforeOccurrence(t *testing.T) {
	text := `:PROCEDURE Test;
:DECLARE sName;
sName := "Größe" + sName;
:ENDPROC;`
	procedures, variables := parseText(text)

	res := Rename(text, 3, 1, "sNew", "file:///t.ssl", procedures, variables)
	if res == nil {
		t.Fatal("expected edits")
	}
	got := applyEdits(text, res.Changes["file:///t.ssl"])
	want := `:PROCEDURE Test;
:DECLARE sNew;
sNew := "Größe" + sNew;
:ENDPROC;`
	if got != want {
		t.Errorf("rename produced:\n%s\nwant:\n%s", got, want)
	}

	// Prepare-rename on the second occurrence ranges over exactly the name.
	prep := PrepareRename(text, 3, 22, "file:///t.ssl", procedures, variables)
	if prep == nil || prep.Range.Start.Character != 19 || prep.Range.End.Character != 24 {
		t.Errorf("expected prepare range 19-24 on the second sName, got %+v", prep)
	}
}

// [spec feature.rename/A22] — renaming a file-wide variable leaves alone a
// procedure that declares its own local of the same name (issue #258).
func TestRename_FileWideSkipsShadowingProcedure(t *testing.T) {
	text := `:PUBLIC gCount;
gCount := 0;
:PROCEDURE Uses;
gCount := gCount + 1;
:ENDPROC;
:PROCEDURE Shadows;
:DECLARE gCount;
gCount := 5;
:ENDPROC;`
	procedures, variables := parseText(text)

	res := Rename(text, 2, 1, "gTotal", "file:///t.ssl", procedures, variables)
	if res == nil {
		t.Fatal("expected edits")
	}
	lines := editedLines(res)
	for _, want := range []int{0, 1, 3} {
		if !containsLine(lines, want) {
			t.Errorf("expected the public's occurrence on line %d to be edited, got %v", want, lines)
		}
	}
	if containsLine(lines, 6) || containsLine(lines, 7) {
		t.Errorf("the shadowing procedure's own local must not be edited, got %v", lines)
	}
}

// [spec feature.rename/A23] — Rename repeats prepare's guards: a request
// positioned inside a comment or string produces no edits (issue #258).
func TestRename_FromCommentOrStringProducesNoEdits(t *testing.T) {
	text := `:PROCEDURE Test;
:DECLARE sName;
/* sName is documented here;
sName := "sName";
:ENDPROC;`
	procedures, variables := parseText(text)
	if res := Rename(text, 3, 4, "sNew", "file:///t.ssl", procedures, variables); res != nil {
		t.Errorf("rename from a comment must produce no edits, got %+v", res)
	}
	if res := Rename(text, 4, 12, "sNew", "file:///t.ssl", procedures, variables); res != nil {
		t.Errorf("rename from a string must produce no edits, got %+v", res)
	}
}

func applyEdits(text string, edits []TextEdit) string {
	lines := strings.Split(text, "\n")
	// Apply right-to-left per line so earlier columns stay valid.
	byLine := map[int][]TextEdit{}
	for _, e := range edits {
		byLine[e.Range.Start.Line] = append(byLine[e.Range.Start.Line], e)
	}
	for ln, es := range byLine {
		runes := []rune(lines[ln])
		for i := 0; i < len(es); i++ {
			for j := i + 1; j < len(es); j++ {
				if es[j].Range.Start.Character > es[i].Range.Start.Character {
					es[i], es[j] = es[j], es[i]
				}
			}
		}
		for _, e := range es {
			runes = append(append(append([]rune{}, runes[:e.Range.Start.Character]...), []rune(e.NewText)...), runes[e.Range.End.Character:]...)
		}
		lines[ln] = string(runes)
	}
	return strings.Join(lines, "\n")
}

// [spec feature.rename/A21] — getWordRange matches on the original line by
// characters: a non-ASCII character whose case mapping changes length
// (`İ`) earlier on the line neither panics nor shifts the range (issue #255).
func TestGetWordRange_NonASCIICaseMapping(t *testing.T) {
	text := "x := \"İİİ\" + SNAME;"
	r := getWordRange(text, 1, 15, "sName")
	start := len([]rune("x := \"İİİ\" + "))
	if r == nil || r.Start.Character != start || r.End.Character != start+5 {
		t.Errorf("expected range %d-%d, got %+v", start, start+5, r)
	}
}

// dottedTokenScript is issue #378's reproduction: every dotted literal and
// operator, NIL, and two plain parameters.
const dottedTokenScript = `:PROCEDURE Check;
	:PARAMETERS bA, bB;
	:IF bA .AND. .T.;
		:RETURN .T.;
	:ENDIF;
	:IF .NOT. bB .OR. bA;
		:RETURN NIL;
	:ENDIF;
	:RETURN .F.;
:ENDPROC;`

// [spec feature.rename/A25] — the letters between the dots of a literal or
// operator are part of the token. Prepare-rename rejects every cursor
// position on the token, and a rename there (prepare skipped) produces no
// edits (issue #378).
func TestRename_DottedLiteralsAndOperatorsRejected(t *testing.T) {
	procedures, variables := parseText(dottedTokenScript)
	lines := strings.Split(dottedTokenScript, "\n")

	for _, tok := range []string{".T.", ".F.", ".AND.", ".OR.", ".NOT.", "NIL"} {
		found := false
		for i, lineText := range lines {
			at := strings.Index(lineText, tok)
			if at < 0 {
				continue
			}
			found = true
			// Every cursor position touching the token: before its first
			// character through after its last (1-based columns).
			for col := at + 1; col <= at+len(tok)+1; col++ {
				if res := PrepareRename(dottedTokenScript, i+1, col, "file:///t.ssl", procedures, variables); res != nil {
					t.Errorf("%s line %d col %d: prepare-rename must reject, got placeholder %q", tok, i+1, col, res.Placeholder)
				}
				if res := Rename(dottedTokenScript, i+1, col, "zNew", "file:///t.ssl", procedures, variables); res != nil {
					t.Errorf("%s line %d col %d: rename must produce no edits, got %+v", tok, i+1, col, res.Changes)
				}
			}
		}
		if !found {
			t.Fatalf("fixture lost its %s", tok)
		}
	}

	// Lowercase spellings are the same tokens.
	lower := ":PROCEDURE P;\n\t:PARAMETERS bA;\n\t:RETURN bA .and. .t.;\n:ENDPROC;"
	procedures, variables = parseText(lower)
	for _, col := range []int{14, 15, 16, 17, 20, 21} {
		if res := PrepareRename(lower, 3, col, "file:///t.ssl", procedures, variables); res != nil {
			t.Errorf("lowercase col %d: prepare-rename must reject, got placeholder %q", col, res.Placeholder)
		}
	}

	// The neighbours still rename: bA on the same line as `.AND. .T.`.
	procedures, variables = parseText(dottedTokenScript)
	res := PrepareRename(dottedTokenScript, 3, 7, "file:///t.ssl", procedures, variables)
	if res == nil || res.Placeholder != "bA" {
		t.Fatalf("bA beside .AND. must stay renameable, got %+v", res)
	}
}

// [spec feature.rename/A25] — a cursor at the end of `.NOT.` that is also
// the start of an identifier written against it names the identifier.
func TestRename_IdentifierAgainstDottedOperatorStillRenames(t *testing.T) {
	text := ":PROCEDURE P;\n\t:PARAMETERS bDone, bA;\n\t:RETURN .NOT.bDone .AND.bA;\n:ENDPROC;"
	procedures, variables := parseText(text)
	line := strings.Split(text, "\n")[2]

	// 1-based column of the `b` of bDone == the position just after `.NOT.`.
	col := strings.Index(line, "bDone") + 1
	res := PrepareRename(text, 3, col, "file:///t.ssl", procedures, variables)
	if res == nil || res.Placeholder != "bDone" {
		t.Fatalf("cursor between .NOT. and bDone must offer bDone, got %+v", res)
	}
	renamed := Rename(text, 3, col, "bFinished", "file:///t.ssl", procedures, variables)
	if renamed == nil {
		t.Fatal("expected edits for bDone")
	}
	got := applyEdits(text, renamed.Changes["file:///t.ssl"])
	want := ":PROCEDURE P;\n\t:PARAMETERS bFinished, bA;\n\t:RETURN .NOT.bFinished .AND.bA;\n:ENDPROC;"
	if got != want {
		t.Errorf("got:\n%s\nwant:\n%s", got, want)
	}
}

// [spec feature.rename/A26] — a variable spelled like the letters of a
// dotted token is renamed without touching the token: the shared reference
// search drops a word match inside a keyword, literal or operator token
// (issue #378).
func TestRename_VariableSpelledLikeDottedToken(t *testing.T) {
	text := `:PROCEDURE P;
	:DECLARE t, f, and, or, not;
	t := .T.;
	f := .F.;
	and := t .AND. f;
	or := t .OR. f;
	not := .NOT. t;
	:RETURN and .AND. or .OR. .NOT. not;
:ENDPROC;`
	procedures, variables := parseText(text)

	// Rename each from its assignment (lines 3-7, column 2), feeding the
	// result of one rename into the next.
	renames := []struct {
		line    int
		newName string
	}{
		{3, "bTrue"}, {4, "bFalse"}, {5, "bBoth"}, {6, "bEither"}, {7, "bNeither"},
	}
	for _, r := range renames {
		res := Rename(text, r.line, 2, r.newName, "file:///t.ssl", procedures, variables)
		if res == nil {
			t.Fatalf("line %d: expected edits", r.line)
		}
		text = applyEdits(text, res.Changes["file:///t.ssl"])
		procedures, variables = parseText(text)
	}

	want := `:PROCEDURE P;
	:DECLARE bTrue, bFalse, bBoth, bEither, bNeither;
	bTrue := .T.;
	bFalse := .F.;
	bBoth := bTrue .AND. bFalse;
	bEither := bTrue .OR. bFalse;
	bNeither := .NOT. bTrue;
	:RETURN bBoth .AND. bEither .OR. .NOT. bNeither;
:ENDPROC;`
	if text != want {
		t.Errorf("got:\n%s\nwant:\n%s", text, want)
	}
}

// [spec feature.rename/A25] — the server takes the workspace-wide procedure
// path when the cursor may name a procedure. The letters of `.T.` never do,
// even in a file that declares a procedure spelled `T` (issue #378).
func TestCursorMayNameProcedure_DottedTokenNeverDoes(t *testing.T) {
	text := ":PROCEDURE T;\n\t:RETURN .T.;\n:ENDPROC;"
	if !CursorMayNameProcedure(text, 1, 12) {
		t.Error("the procedure header must still name the procedure")
	}
	// `.T.` on line 2: the dot is at 1-based column 10, the T at 11.
	for _, col := range []int{11, 12} {
		if CursorMayNameProcedure(text, 2, col) {
			t.Errorf("col %d: the T of .T. must not name the procedure T", col)
		}
	}
}
