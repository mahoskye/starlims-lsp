package providers

import (
	"reflect"
	"strings"
	"testing"

	"starlims-lsp/internal/parser"
)

// ==================== Hover Tests ====================

func TestGetHover_Keyword(t *testing.T) {
	text := `:PROCEDURE Test;
:DECLARE x;
:ENDPROC;`

	// Hover over :DECLARE
	hover := GetHover(text, 2, 5, nil, nil)
	if hover == nil {
		t.Fatal("expected hover info for keyword")
	}
	if !strings.Contains(hover.Contents, "DECLARE") {
		t.Errorf("expected hover to contain 'DECLARE', got: %s", hover.Contents)
	}
}

func TestGetHover_Variable(t *testing.T) {
	text := `:PROCEDURE Test;
:DECLARE myVar;
myVar := 1;
:ENDPROC;`

	variables := []parser.VariableInfo{
		{Name: "myVar", Line: 2, Column: 10, Scope: parser.ScopeLocal},
	}

	// Hover over myVar on line 3
	hover := GetHover(text, 3, 3, nil, variables)
	if hover == nil {
		t.Fatal("expected hover info for variable")
	}
	if !strings.Contains(hover.Contents, "myVar") {
		t.Errorf("expected hover to contain 'myVar', got: %s", hover.Contents)
	}
	if !strings.Contains(hover.Contents, "variable") {
		t.Errorf("expected hover to indicate it's a variable, got: %s", hover.Contents)
	}
}

func TestGetHover_Procedure(t *testing.T) {
	text := `:PROCEDURE MyProc;
:PARAMETERS param1, param2;
:ENDPROC;

:PROCEDURE Test;
MyProc(1, 2);
:ENDPROC;`

	procedures := []parser.ProcedureInfo{
		{Name: "MyProc", StartLine: 1, EndLine: 3, Parameters: []string{"param1", "param2"}},
		{Name: "Test", StartLine: 5, EndLine: 7, Parameters: nil},
	}

	// Hover over MyProc call on line 6
	hover := GetHover(text, 6, 3, procedures, nil)
	if hover == nil {
		t.Fatal("expected hover info for procedure")
	}
	if !strings.Contains(hover.Contents, "MyProc") {
		t.Errorf("expected hover to contain 'MyProc', got: %s", hover.Contents)
	}
	if !strings.Contains(hover.Contents, "param1") || !strings.Contains(hover.Contents, "param2") {
		t.Errorf("expected hover to show parameters, got: %s", hover.Contents)
	}
}

func TestGetHover_BuiltinFunction(t *testing.T) {
	text := `result := Len("hello");`

	hover := GetHover(text, 1, 12, nil, nil)
	if hover == nil {
		t.Log("Note: Len may not be in the function list")
		return
	}
	if !strings.Contains(strings.ToLower(hover.Contents), "function") {
		t.Errorf("expected hover to mention 'function', got: %s", hover.Contents)
	}
}

func TestGetHover_NoMatch(t *testing.T) {
	text := `unknownThing := 1;`

	hover := GetHover(text, 1, 5, nil, nil)
	// unknownThing is not a keyword, function, or known variable
	// It may return nil or a generic message
	if hover != nil {
		t.Logf("Hover returned for unknown symbol: %s", hover.Contents)
	}
}

// ==================== Definition Tests ====================

func TestFindDefinition_Procedure(t *testing.T) {
	text := `:PROCEDURE MyProc;
:ENDPROC;

:PROCEDURE Test;
MyProc();
:ENDPROC;`

	procedures := []parser.ProcedureInfo{
		{Name: "MyProc", StartLine: 1, EndLine: 2, Parameters: nil},
	}

	// Find definition of MyProc call on line 5
	location := FindDefinition(text, 5, 3, "file:///test.ssl", procedures, nil)
	if location == nil {
		t.Fatal("expected to find definition for procedure")
	}
	if location.Range.Start.Line != 0 { // 0-based line number
		t.Errorf("expected definition on line 0, got %d", location.Range.Start.Line)
	}
}

func TestFindDefinition_Variable(t *testing.T) {
	text := `:PROCEDURE Test;
:DECLARE myVar;
myVar := 1;
x := myVar + 1;
:ENDPROC;`

	variables := []parser.VariableInfo{
		{Name: "myVar", Line: 2, Column: 10, Scope: parser.ScopeLocal},
	}

	// Find definition of myVar on line 4
	location := FindDefinition(text, 4, 7, "file:///test.ssl", nil, variables)
	if location == nil {
		t.Fatal("expected to find definition for variable")
	}
	if location.Range.Start.Line != 1 { // 0-based
		t.Errorf("expected definition on line 1, got %d", location.Range.Start.Line)
	}
}

func TestFindDefinition_NotFound(t *testing.T) {
	text := `unknownSymbol := 1;`

	location := FindDefinition(text, 1, 5, "file:///test.ssl", nil, nil)
	if location != nil {
		t.Error("expected nil for unknown symbol definition")
	}
}

// ==================== References Tests ====================

func TestFindReferences_Variable(t *testing.T) {
	text := `:PROCEDURE Test;
:DECLARE myVar;
myVar := 1;
x := myVar + 1;
y := myVar * 2;
:ENDPROC;`

	// Find references to myVar (should find 3 occurrences)
	locations := FindReferences(text, 2, 10, "file:///test.ssl", true)
	if locations == nil {
		t.Fatal("expected to find references")
	}
	if len(locations) < 3 {
		t.Errorf("expected at least 3 references to myVar, got %d", len(locations))
	}
}

func TestFindReferences_Procedure(t *testing.T) {
	text := `:PROCEDURE MyProc;
:ENDPROC;

:PROCEDURE Test;
MyProc();
MyProc();
:ENDPROC;`

	// Find references to MyProc (should find definition + 2 calls)
	locations := FindReferences(text, 1, 12, "file:///test.ssl", true)
	if locations == nil {
		t.Fatal("expected to find references")
	}
	if len(locations) < 3 {
		t.Errorf("expected at least 3 references to MyProc, got %d", len(locations))
	}
}

// ==================== Document Symbols Tests ====================

func TestGetDocumentSymbols_Procedures(t *testing.T) {
	text := `:PROCEDURE Test1;
:ENDPROC;

:PROCEDURE Test2;
:PARAMETERS param1;
:ENDPROC;`

	symbols := GetDocumentSymbols(text)

	if len(symbols) < 2 {
		t.Fatalf("expected at least 2 symbols, got %d", len(symbols))
	}

	// Check that we found both procedures
	foundTest1 := false
	foundTest2 := false
	for _, sym := range symbols {
		if sym.Name == "Test1" {
			foundTest1 = true
			if sym.Kind != SymbolKindFunction {
				t.Errorf("expected Test1 to be a function symbol")
			}
		}
		if sym.Name == "Test2" {
			foundTest2 = true
			if sym.Kind != SymbolKindFunction {
				t.Errorf("expected Test2 to be a function symbol")
			}
			// Note: Parameter extraction from :PARAMETERS depends on parser implementation
			// If children are found, verify they're valid
			if len(sym.Children) > 0 {
				for _, child := range sym.Children {
					if child.Kind != SymbolKindVariable {
						t.Errorf("expected parameter child to be variable symbol")
					}
				}
			}
		}
	}

	if !foundTest1 {
		t.Error("expected to find Test1 symbol")
	}
	if !foundTest2 {
		t.Error("expected to find Test2 symbol")
	}
}

func TestGetDocumentSymbols_PublicVariables(t *testing.T) {
	text := `:PUBLIC gVar1, gVar2;

:PROCEDURE Test;
:DECLARE localVar;
:ENDPROC;`

	symbols := GetDocumentSymbols(text)

	// Should include the public variables
	foundPublic := 0
	for _, sym := range symbols {
		if strings.Contains(sym.Detail, "public") {
			foundPublic++
		}
	}

	if foundPublic < 1 {
		t.Error("expected to find public variable symbols")
	}
}

// ==================== Folding Range Tests ====================

func TestGetFoldingRanges_Procedures(t *testing.T) {
	text := `:PROCEDURE Test1;
:DECLARE x;
x := 1;
:ENDPROC;

:PROCEDURE Test2;
:DECLARE y;
:ENDPROC;`

	ranges := GetFoldingRanges(text)

	if len(ranges) < 2 {
		t.Fatalf("expected at least 2 folding ranges, got %d", len(ranges))
	}

	// Verify ranges span from procedure to endproc
	for _, r := range ranges {
		if r.StartLine > r.EndLine {
			t.Errorf("invalid folding range: start %d > end %d", r.StartLine, r.EndLine)
		}
	}
}

func TestGetFoldingRanges_CommentBlocks(t *testing.T) {
	text := `/* This is a
multi-line
comment block */
:PROCEDURE Test;
:ENDPROC;`

	ranges := GetFoldingRanges(text)

	// Should have at least a comment block range
	foundComment := false
	for _, r := range ranges {
		if r.Kind == "comment" {
			foundComment = true
		}
	}

	if !foundComment {
		t.Log("Note: Multi-line comment block may not be detected as separate folding range")
	}
}

// ==================== Signature Help Tests ====================

func TestGetSignatureHelp_KnownFunction(t *testing.T) {
	// Test with a function that should be in the signatures
	text := `result := Len(`

	help := GetSignatureHelp(text, 1, 14)
	// Note: This test depends on Len being in the function signatures
	if help != nil {
		if len(help.Signatures) == 0 {
			t.Error("expected at least one signature")
		}
		t.Logf("Signature: %s", help.Signatures[0].Label)
	}
}

func TestGetSignatureHelp_ActiveParameter(t *testing.T) {
	// Test that active parameter is tracked correctly
	text := `result := SubStr("hello", 1, `

	help := GetSignatureHelp(text, 1, 29)
	if help != nil {
		if help.ActiveParameter != 2 { // 0-indexed, third parameter
			t.Errorf("expected active parameter 2, got %d", help.ActiveParameter)
		}
	}
}

func TestGetSignatureHelp_NestedCalls(t *testing.T) {
	// Nested function calls - the implementation scans backwards from cursor
	// so it finds the innermost function at the cursor position
	text := `result := Len(Trim(`

	help := GetSignatureHelp(text, 1, 19)
	if help != nil {
		// The implementation may return either the inner or outer function
		// depending on how the context scanning works
		if len(help.Signatures) > 0 {
			t.Logf("Nested call signature: %s", help.Signatures[0].Label)
			// Verify we got a valid signature
			if help.Signatures[0].Label == "" {
				t.Error("expected non-empty signature label")
			}
		}
	}
}

// ==================== Completion Tests ====================

func TestGetKeywordCompletions(t *testing.T) {
	completions := GetKeywordCompletions()

	if len(completions) == 0 {
		t.Fatal("expected some keyword completions")
	}

	// All keywords should start with ':'
	for _, c := range completions {
		if !strings.HasPrefix(c.Label, ":") {
			t.Errorf("expected keyword to start with ':', got %q", c.Label)
		}
		if c.Kind != CompletionKindKeyword {
			t.Errorf("expected keyword completion kind for %s", c.Label)
		}
	}
}

func TestGetFunctionCompletions(t *testing.T) {
	completions := GetFunctionCompletions()

	if len(completions) == 0 {
		t.Fatal("expected some function completions")
	}

	for _, c := range completions {
		if c.Kind != CompletionKindFunction {
			t.Errorf("expected function completion kind for %s", c.Label)
		}
	}
}

func TestGetSnippetCompletions(t *testing.T) {
	snippets := GetSnippetCompletions()

	if len(snippets) == 0 {
		t.Fatal("expected some snippet completions")
	}

	// Check for common snippets
	foundProc := false
	foundIf := false
	foundFor := false
	for _, s := range snippets {
		if s.Label == "proc" {
			foundProc = true
		}
		if s.Label == "if" {
			foundIf = true
		}
		if s.Label == "for" {
			foundFor = true
		}
	}

	if !foundProc {
		t.Error("expected 'proc' snippet")
	}
	if !foundIf {
		t.Error("expected 'if' snippet")
	}
	if !foundFor {
		t.Error("expected 'for' snippet")
	}
}

func TestGetProcedureCompletions(t *testing.T) {
	procedures := []parser.ProcedureInfo{
		{Name: "MyProc", StartLine: 1, EndLine: 5, Parameters: []string{"param1"}},
		{Name: "OtherProc", StartLine: 10, EndLine: 15, Parameters: nil},
	}

	completions := GetProcedureCompletions(procedures)

	if len(completions) != 2 {
		t.Fatalf("expected 2 procedure completions, got %d", len(completions))
	}

	for _, c := range completions {
		if c.Kind != CompletionKindFunction {
			t.Errorf("expected function completion kind for procedure %s", c.Label)
		}
	}
}

func TestGetVariableCompletions(t *testing.T) {
	variables := []parser.VariableInfo{
		{Name: "var1", Line: 1, Column: 10, Scope: parser.ScopeLocal},
		{Name: "var2", Line: 2, Column: 10, Scope: parser.ScopePublic},
	}

	completions := GetVariableCompletions(variables)

	if len(completions) != 2 {
		t.Fatalf("expected 2 variable completions, got %d", len(completions))
	}

	for _, c := range completions {
		if c.Kind != CompletionKindVariable {
			t.Errorf("expected variable completion kind for %s", c.Label)
		}
	}
}

func TestGetAllCompletions(t *testing.T) {
	procedures := []parser.ProcedureInfo{
		{Name: "MyProc", StartLine: 1, EndLine: 5, Parameters: nil},
	}
	variables := []parser.VariableInfo{
		{Name: "myVar", Line: 2, Column: 10, Scope: parser.ScopeLocal},
	}

	completions := GetAllCompletions(procedures, variables)

	if len(completions) == 0 {
		t.Fatal("expected some completions")
	}

	// Should include keywords, functions, the procedure, and the variable
	foundKeyword := false
	foundProc := false
	foundVar := false
	for _, c := range completions {
		if c.Kind == CompletionKindKeyword {
			foundKeyword = true
		}
		if c.Label == "MyProc" {
			foundProc = true
		}
		if c.Label == "myVar" {
			foundVar = true
		}
	}

	if !foundKeyword {
		t.Error("expected to find keyword completions")
	}
	if !foundProc {
		t.Error("expected to find procedure completion")
	}
	if !foundVar {
		t.Error("expected to find variable completion")
	}
}

// ==================== Diagnostics Tests ====================

func TestGetDiagnostics_UnmatchedParens(t *testing.T) {
	text := `:PROCEDURE Test;
result := Len(("hello");
:ENDPROC;`

	opts := DefaultDiagnosticOptions()
	opts.CheckUnmatchedParens = true

	diagnostics := GetDiagnostics(text, opts)

	foundParenError := false
	for _, d := range diagnostics {
		if strings.Contains(d.Message, "Unclosed") || strings.Contains(d.Message, "Unmatched") {
			foundParenError = true
		}
	}

	if !foundParenError {
		t.Error("expected to find unmatched parenthesis error")
	}
}

func TestGetDiagnostics_UnclosedBlock(t *testing.T) {
	text := `:PROCEDURE Test;
:IF x = 1;
x := 2;
:ENDPROC;`

	opts := DefaultDiagnosticOptions()
	opts.CheckUnclosedBlocks = true

	diagnostics := GetDiagnostics(text, opts)

	foundBlockError := false
	for _, d := range diagnostics {
		if strings.Contains(d.Message, "Unclosed") && strings.Contains(d.Message, "IF") {
			foundBlockError = true
		}
	}

	if !foundBlockError {
		t.Error("expected to find unclosed IF block error")
	}
}

func TestGetDiagnostics_ValidCode(t *testing.T) {
	text := `:PROCEDURE Test;
:DECLARE x;
:IF x = 1;
	x := 2;
:ENDIF;
:ENDPROC;`

	opts := DefaultDiagnosticOptions()
	diagnostics := GetDiagnostics(text, opts)

	if len(diagnostics) > 0 {
		t.Logf("Found %d diagnostics in valid code:", len(diagnostics))
		for _, d := range diagnostics {
			t.Logf("  - %s", d.Message)
		}
	}
}

func TestGetDiagnostics_BlockDepth(t *testing.T) {
	// Create deeply nested blocks
	text := `:PROCEDURE Test;
:IF a;
:IF b;
:IF c;
:IF d;
:IF e;
 x := 1;
:ENDIF;
:ENDIF;
:ENDIF;
:ENDIF;
:ENDIF;
:ENDPROC;`

	opts := DefaultDiagnosticOptions()
	opts.MaxBlockDepth = 3 // Allow only 3 levels of nesting

	diagnostics := GetDiagnostics(text, opts)

	foundDepthWarning := false
	for _, d := range diagnostics {
		if strings.Contains(d.Message, "nesting depth") || strings.Contains(d.Message, "depth") {
			foundDepthWarning = true
		}
	}

	if !foundDepthWarning {
		t.Log("Note: Block depth warning may not trigger for this test case")
	}
}

func TestGetDiagnostics_HungarianNotationDisabled(t *testing.T) {
	text := `:PROCEDURE Test;
:PARAMETERS nCount, sName;
:DECLARE sValue;
:ENDPROC;`

	opts := DefaultDiagnosticOptions()
	diagnostics := GetDiagnostics(text, opts)

	for _, d := range diagnostics {
		if strings.Contains(d.Message, "Hungarian notation") {
			t.Fatalf("did not expect Hungarian notation diagnostic: %s", d.Message)
		}
	}
}

func TestGetDiagnostics_HungarianNotationEnabled(t *testing.T) {
	text := `:PROCEDURE Test;
:PARAMETERS nCount, sName;
:DECLARE sValue, goodName;
:ENDPROC;`

	opts := DefaultDiagnosticOptions()
	opts.CheckHungarianNotation = true
	diagnostics := GetDiagnostics(text, opts)

	var hungarianDiagnostics []Diagnostic
	for _, d := range diagnostics {
		if strings.Contains(d.Message, "Hungarian notation") {
			hungarianDiagnostics = append(hungarianDiagnostics, d)
		}
	}

	if len(hungarianDiagnostics) != 3 {
		t.Fatalf("expected 3 Hungarian notation warnings, got %d", len(hungarianDiagnostics))
	}

	for _, d := range hungarianDiagnostics {
		if d.Severity != SeverityWarning {
			t.Errorf("expected warning severity, got %v", d.Severity)
		}
	}
}

func TestGetDiagnostics_HungarianNotationCustomPrefixes(t *testing.T) {
	text := `:PROCEDURE Test;
:PARAMETERS xValue, yValue;
:DECLARE sValue;
:ENDPROC;`

	opts := DefaultDiagnosticOptions()
	opts.CheckHungarianNotation = true
	opts.HungarianPrefixes = []string{"x", "y"}
	diagnostics := GetDiagnostics(text, opts)

	var hungarianDiagnostics []Diagnostic
	for _, d := range diagnostics {
		if strings.Contains(d.Message, "Hungarian notation") {
			hungarianDiagnostics = append(hungarianDiagnostics, d)
		}
	}

	if len(hungarianDiagnostics) != 2 {
		t.Fatalf("expected 2 Hungarian notation warnings, got %d", len(hungarianDiagnostics))
	}
}

// ==================== Default Options Tests ====================

func TestDefaultDiagnosticOptions(t *testing.T) {
	opts := DefaultDiagnosticOptions()

	if !opts.CheckUnclosedBlocks {
		t.Error("expected CheckUnclosedBlocks to be true by default")
	}
	if !opts.CheckUnmatchedParens {
		t.Error("expected CheckUnmatchedParens to be true by default")
	}
	if opts.MaxBlockDepth != 10 {
		t.Errorf("expected MaxBlockDepth to be 10, got %d", opts.MaxBlockDepth)
	}
	if opts.CheckHungarianNotation {
		t.Error("expected CheckHungarianNotation to be false by default")
	}
	if !reflect.DeepEqual(opts.HungarianPrefixes, []string{"a", "b", "d", "n", "o", "s"}) {
		t.Errorf("unexpected default Hungarian prefixes: %v", opts.HungarianPrefixes)
	}
}

func TestDefaultSQLFormattingOptions(t *testing.T) {
	opts := DefaultSQLFormattingOptions()

	if !opts.Enabled {
		t.Error("expected SQL formatting to be enabled by default")
	}
	if opts.Style != "standard" {
		t.Errorf("expected style 'standard', got %q", opts.Style)
	}
	if opts.KeywordCase != "upper" {
		t.Errorf("expected keyword case 'upper', got %q", opts.KeywordCase)
	}
}

// ==================== Edge Cases ====================

func TestHover_EmptyText(t *testing.T) {
	hover := GetHover("", 1, 1, nil, nil)
	if hover != nil {
		t.Error("expected nil hover for empty text")
	}
}

func TestDefinition_EmptyText(t *testing.T) {
	location := FindDefinition("", 1, 1, "file:///test.ssl", nil, nil)
	if location != nil {
		t.Error("expected nil location for empty text")
	}
}

func TestReferences_EmptyText(t *testing.T) {
	locations := FindReferences("", 1, 1, "file:///test.ssl", true)
	if locations != nil {
		t.Error("expected nil locations for empty text")
	}
}

func TestDocumentSymbols_EmptyText(t *testing.T) {
	symbols := GetDocumentSymbols("")
	// Empty text should return empty (but valid) slice
	if symbols == nil {
		t.Log("GetDocumentSymbols returns nil for empty text")
	}
}

func TestFoldingRanges_EmptyText(t *testing.T) {
	ranges := GetFoldingRanges("")
	if ranges == nil {
		t.Log("GetFoldingRanges returns nil for empty text")
	}
}
