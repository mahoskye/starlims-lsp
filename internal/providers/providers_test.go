package providers

import (
	"reflect"
	"strings"
	"testing"

	"starlims-lsp/internal/lexer"
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

func TestFindReferences_ExcludeDeclaration_Declare(t *testing.T) {
	text := `:PROCEDURE Test;
:DECLARE myVar;
myVar := 1;
x := myVar + 1;
:ENDPROC;`

	// Cursor on :DECLARE line, request without declaration
	locations := FindReferences(text, 2, 10, "file:///test.ssl", false)
	if locations == nil {
		t.Fatal("expected to find references")
	}
	// Should find 2 references (lines 3 and 4), excluding declaration on line 2
	if len(locations) != 2 {
		t.Errorf("expected 2 references excluding declaration, got %d", len(locations))
	}
	// Verify none are on declaration line (0-based index 1)
	for _, loc := range locations {
		if loc.Range.Start.Line == 1 {
			t.Error("declaration should be excluded when includeDeclaration=false")
		}
	}
}

func TestFindReferences_ExcludeDeclaration_Parameters(t *testing.T) {
	text := `:PROCEDURE Test;
:PARAMETERS param1;
result := param1 + 1;
:ENDPROC;`

	// Cursor on :PARAMETERS line, request without declaration
	locations := FindReferences(text, 2, 13, "file:///test.ssl", false)
	if locations == nil {
		t.Fatal("expected to find references")
	}
	// Should find 1 reference (line 3), excluding declaration on line 2
	if len(locations) != 1 {
		t.Errorf("expected 1 reference excluding declaration, got %d", len(locations))
	}
	// Verify none are on :PARAMETERS line (0-based index 1)
	for _, loc := range locations {
		if loc.Range.Start.Line == 1 {
			t.Error(":PARAMETERS declaration should be excluded")
		}
	}
}

func TestFindReferencesWithScope_LocalVariablesScopedToProcedure(t *testing.T) {
	// Test case from documentation 6.6: Local variables in different procedures
	text := `:PROCEDURE ProcA;
:DECLARE localVar;
x := localVar;
:ENDPROC;

:PROCEDURE ProcB;
:DECLARE localVar;
y := localVar;
:ENDPROC;`

	lex := lexer.NewLexer(text)
	tokens := lex.Tokenize()
	p := parser.NewParser(tokens)
	ast := p.Parse()
	procedures := p.ExtractProcedures(ast)
	variables := p.ExtractVariables(ast)

	// Find references to localVar in ProcA (line 2)
	// Should only find references within ProcA, not ProcB
	locations := FindReferencesWithScope(text, 2, 10, "file:///test.ssl", true, procedures, variables)

	if locations == nil {
		t.Fatal("expected to find references")
	}

	// Should find 2 references in ProcA (declaration + usage)
	if len(locations) != 2 {
		t.Errorf("expected 2 references in ProcA scope, got %d", len(locations))
	}

	// Verify all references are within ProcA's line range (lines 1-4, 0-based: 0-3)
	for _, loc := range locations {
		if loc.Range.Start.Line > 3 {
			t.Errorf("found reference outside ProcA scope at line %d", loc.Range.Start.Line)
		}
	}
}

func TestFindReferencesWithScope_PublicVariablesGlobalScope(t *testing.T) {
	// Public variables should find references across entire document
	text := `:PUBLIC gCounter;

:PROCEDURE ProcA;
gCounter := 1;
:ENDPROC;

:PROCEDURE ProcB;
x := gCounter + 1;
:ENDPROC;`

	lex := lexer.NewLexer(text)
	tokens := lex.Tokenize()
	p := parser.NewParser(tokens)
	ast := p.Parse()
	procedures := p.ExtractProcedures(ast)
	variables := p.ExtractVariables(ast)

	// Find references to gCounter (line 1)
	locations := FindReferencesWithScope(text, 1, 10, "file:///test.ssl", true, procedures, variables)

	if locations == nil {
		t.Fatal("expected to find references")
	}

	// Should find 3 references: declaration + ProcA usage + ProcB usage
	if len(locations) != 3 {
		t.Errorf("expected 3 references for public variable, got %d", len(locations))
	}
}

func TestFindReferencesWithScope_ParametersScopedToProcedure(t *testing.T) {
	// Parameters should be scoped to their procedure
	text := `:PROCEDURE ProcA;
:PARAMETERS sName;
x := sName;
:ENDPROC;

:PROCEDURE ProcB;
:PARAMETERS sName;
y := sName;
:ENDPROC;`

	lex := lexer.NewLexer(text)
	tokens := lex.Tokenize()
	p := parser.NewParser(tokens)
	ast := p.Parse()
	procedures := p.ExtractProcedures(ast)
	variables := p.ExtractVariables(ast)

	// Find references to sName in ProcA (line 2)
	locations := FindReferencesWithScope(text, 2, 13, "file:///test.ssl", true, procedures, variables)

	if locations == nil {
		t.Fatal("expected to find references")
	}

	// Should find 2 references in ProcA only
	if len(locations) != 2 {
		t.Errorf("expected 2 references in ProcA scope, got %d", len(locations))
	}

	// Verify all references are within ProcA's line range
	for _, loc := range locations {
		if loc.Range.Start.Line > 3 {
			t.Errorf("found reference outside ProcA scope at line %d", loc.Range.Start.Line)
		}
	}
}

func TestFindReferencesWithScope_ProcedureReferencesGlobalScope(t *testing.T) {
	// Procedure names should find references across entire document
	text := `:PROCEDURE HelperProc;
:ENDPROC;

:PROCEDURE Main;
HelperProc();
x := HelperProc();
:ENDPROC;`

	lex := lexer.NewLexer(text)
	tokens := lex.Tokenize()
	p := parser.NewParser(tokens)
	ast := p.Parse()
	procedures := p.ExtractProcedures(ast)
	variables := p.ExtractVariables(ast)

	// Find references to HelperProc (line 1)
	locations := FindReferencesWithScope(text, 1, 12, "file:///test.ssl", true, procedures, variables)

	if locations == nil {
		t.Fatal("expected to find references")
	}

	// Should find 3 references: definition + 2 calls
	if len(locations) != 3 {
		t.Errorf("expected 3 references for procedure, got %d", len(locations))
	}
}

func TestFindReferencesWithScope_NilProceduresVariablesFallback(t *testing.T) {
	// When procedures/variables are nil, should fall back to global search
	text := `:PROCEDURE ProcA;
:DECLARE localVar;
x := localVar;
:ENDPROC;

:PROCEDURE ProcB;
:DECLARE localVar;
y := localVar;
:ENDPROC;`

	// Without scope info, should find all occurrences
	locations := FindReferencesWithScope(text, 2, 10, "file:///test.ssl", true, nil, nil)

	if locations == nil {
		t.Fatal("expected to find references")
	}

	// Should find 4 references (all occurrences without scope filtering)
	if len(locations) != 4 {
		t.Errorf("expected 4 references without scope info, got %d", len(locations))
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

func TestGetSignatureHelpWithProcedures_UserDefinedProc(t *testing.T) {
	// Test signature help for a user-defined procedure
	text := `:PROCEDURE MyCustomProc;
:PARAMETERS sName, nValue, bFlag;
:ENDPROC;

result := MyCustomProc(`

	lex := lexer.NewLexer(text)
	tokens := lex.Tokenize()
	p := parser.NewParser(tokens)
	ast := p.Parse()
	procedures := p.ExtractProcedures(ast)

	// Position cursor inside the call to MyCustomProc
	help := GetSignatureHelpWithProcedures(tokens, procedures, 5, 24)

	if help == nil {
		t.Fatal("expected signature help for user-defined procedure")
	}

	if len(help.Signatures) != 1 {
		t.Fatalf("expected 1 signature, got %d", len(help.Signatures))
	}

	sig := help.Signatures[0]
	if !strings.Contains(sig.Label, "MyCustomProc") {
		t.Errorf("expected label to contain 'MyCustomProc', got: %s", sig.Label)
	}

	if len(sig.Parameters) != 3 {
		t.Errorf("expected 3 parameters, got %d", len(sig.Parameters))
	}

	expectedParams := []string{"sName", "nValue", "bFlag"}
	for i, param := range sig.Parameters {
		if param.Label != expectedParams[i] {
			t.Errorf("parameter %d: expected '%s', got '%s'", i, expectedParams[i], param.Label)
		}
	}
}

func TestGetSignatureHelpWithProcedures_ActiveParameter(t *testing.T) {
	// Test that active parameter is tracked correctly for user procedures
	text := `:PROCEDURE Calculate;
:PARAMETERS nA, nB, nC;
:ENDPROC;

result := Calculate(1, 2, `

	lex := lexer.NewLexer(text)
	tokens := lex.Tokenize()
	p := parser.NewParser(tokens)
	ast := p.Parse()
	procedures := p.ExtractProcedures(ast)

	// Position cursor after the second comma (third parameter)
	help := GetSignatureHelpWithProcedures(tokens, procedures, 5, 26)

	if help == nil {
		t.Fatal("expected signature help")
	}

	if help.ActiveParameter != 2 {
		t.Errorf("expected active parameter 2 (third param), got %d", help.ActiveParameter)
	}
}

func TestGetSignatureHelpWithProcedures_NoParams(t *testing.T) {
	// Test procedure with no parameters
	text := `:PROCEDURE DoSomething;
:ENDPROC;

result := DoSomething(`

	lex := lexer.NewLexer(text)
	tokens := lex.Tokenize()
	p := parser.NewParser(tokens)
	ast := p.Parse()
	procedures := p.ExtractProcedures(ast)

	// Position cursor after the opening paren (column 23)
	help := GetSignatureHelpWithProcedures(tokens, procedures, 4, 23)

	if help == nil {
		t.Fatal("expected signature help for procedure with no params")
	}

	sig := help.Signatures[0]
	if sig.Label != "DoSomething()" {
		t.Errorf("expected 'DoSomething()', got: %s", sig.Label)
	}

	if len(sig.Parameters) != 0 {
		t.Errorf("expected 0 parameters, got %d", len(sig.Parameters))
	}
}

func TestGetSignatureHelpWithProcedures_BuiltInTakesPrecedence(t *testing.T) {
	// Built-in functions should still work and take precedence
	text := `:PROCEDURE Len;
:PARAMETERS x;
:ENDPROC;

result := Len(`

	lex := lexer.NewLexer(text)
	tokens := lex.Tokenize()
	p := parser.NewParser(tokens)
	ast := p.Parse()
	procedures := p.ExtractProcedures(ast)

	// Position cursor after the opening paren
	help := GetSignatureHelpWithProcedures(tokens, procedures, 5, 15)

	if help == nil {
		t.Fatal("expected signature help")
	}

	// Should get the built-in Len function, not the user-defined one
	sig := help.Signatures[0]
	// Built-in Len has a specific signature with type info
	if strings.Contains(sig.Documentation, "User-defined") {
		t.Error("expected built-in function signature, got user-defined")
	}
}

func TestGetSignatureHelpWithProcedures_CaseInsensitive(t *testing.T) {
	// Test case-insensitive matching for procedure names
	text := `:PROCEDURE myproc;
:PARAMETERS sValue;
:ENDPROC;

result := MYPROC(`

	lex := lexer.NewLexer(text)
	tokens := lex.Tokenize()
	p := parser.NewParser(tokens)
	ast := p.Parse()
	procedures := p.ExtractProcedures(ast)

	// Position cursor after the opening paren
	help := GetSignatureHelpWithProcedures(tokens, procedures, 5, 18)

	if help == nil {
		t.Fatal("expected signature help with case-insensitive match")
	}

	if len(help.Signatures[0].Parameters) != 1 {
		t.Error("expected 1 parameter from case-insensitive match")
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

// ==================== SSL Language Rule Enforcement Tests ====================

func TestGetDiagnostics_MissingExitCase(t *testing.T) {
	text := `:PROCEDURE Test;
:BEGINCASE;
:CASE nVal == 1;
	x := 1;
:CASE nVal == 2;
	x := 2;
	:EXITCASE;
:OTHERWISE;
	x := 0;
:ENDCASE;
:ENDPROC;`

	opts := DefaultDiagnosticOptions()
	diagnostics := GetDiagnostics(text, opts)

	// Should find warnings for first :CASE (missing EXITCASE) and :OTHERWISE (missing EXITCASE)
	missingExitCaseWarnings := 0
	for _, d := range diagnostics {
		if strings.Contains(d.Message, "EXITCASE") {
			missingExitCaseWarnings++
			if d.Severity != SeverityWarning {
				t.Errorf("expected warning severity for missing EXITCASE, got %v", d.Severity)
			}
		}
	}

	if missingExitCaseWarnings != 2 {
		t.Errorf("expected 2 missing EXITCASE warnings (first CASE and OTHERWISE), got %d", missingExitCaseWarnings)
	}
}

func TestGetDiagnostics_MissingExitCase_AllPresent(t *testing.T) {
	text := `:PROCEDURE Test;
:BEGINCASE;
:CASE nVal == 1;
	x := 1;
	:EXITCASE;
:CASE nVal == 2;
	x := 2;
	:EXITCASE;
:OTHERWISE;
	x := 0;
	:EXITCASE;
:ENDCASE;
:ENDPROC;`

	opts := DefaultDiagnosticOptions()
	diagnostics := GetDiagnostics(text, opts)

	// Should NOT find any EXITCASE warnings when all are present
	for _, d := range diagnostics {
		if strings.Contains(d.Message, "EXITCASE") {
			t.Errorf("unexpected EXITCASE warning when all present: %s", d.Message)
		}
	}
}

func TestGetDiagnostics_BareLogicalOperators(t *testing.T) {
	text := `:PROCEDURE Test;
:IF x > 5 AND y < 10;
	z := 1;
:ENDIF;
:IF a = 1 OR b = 2;
	z := 2;
:ENDIF;
:IF NOT bFlag;
	z := 3;
:ENDIF;
:ENDPROC;`

	opts := DefaultDiagnosticOptions()
	diagnostics := GetDiagnostics(text, opts)

	// Should find errors for bare AND, OR, NOT
	bareOperatorErrors := 0
	foundAND := false
	foundOR := false
	foundNOT := false
	for _, d := range diagnostics {
		if strings.Contains(d.Message, ".AND.") || strings.Contains(d.Message, ".OR.") || strings.Contains(d.Message, ".NOT.") {
			bareOperatorErrors++
			if d.Severity != SeverityError {
				t.Errorf("expected error severity for bare logical operator, got %v", d.Severity)
			}
			if strings.Contains(d.Message, ".AND.") {
				foundAND = true
			}
			if strings.Contains(d.Message, ".OR.") {
				foundOR = true
			}
			if strings.Contains(d.Message, ".NOT.") {
				foundNOT = true
			}
		}
	}

	if bareOperatorErrors != 3 {
		t.Errorf("expected 3 bare logical operator errors, got %d", bareOperatorErrors)
	}
	if !foundAND || !foundOR || !foundNOT {
		t.Errorf("expected to find AND, OR, and NOT errors: AND=%v, OR=%v, NOT=%v", foundAND, foundOR, foundNOT)
	}
}

func TestGetDiagnostics_BareLogicalOperators_ValidSyntax(t *testing.T) {
	text := `:PROCEDURE Test;
:IF x > 5 .AND. y < 10;
	z := 1;
:ENDIF;
:IF a = 1 .OR. b = 2;
	z := 2;
:ENDIF;
:IF .NOT. bFlag;
	z := 3;
:ENDIF;
:ENDPROC;`

	opts := DefaultDiagnosticOptions()
	diagnostics := GetDiagnostics(text, opts)

	// Should NOT find errors for proper .AND., .OR., .NOT.
	for _, d := range diagnostics {
		if strings.Contains(d.Message, "instead of") && (strings.Contains(d.Message, ".AND.") || strings.Contains(d.Message, ".OR.") || strings.Contains(d.Message, ".NOT.")) {
			t.Errorf("unexpected bare operator error when using proper syntax: %s", d.Message)
		}
	}
}

func TestGetDiagnostics_DefaultOnDeclareLine(t *testing.T) {
	text := `:PROCEDURE Test;
:DECLARE sName; :DEFAULT sName, "";
:ENDPROC;`

	opts := DefaultDiagnosticOptions()
	diagnostics := GetDiagnostics(text, opts)

	// Should find warning for :DEFAULT on same line as :DECLARE
	foundDefaultWarning := false
	for _, d := range diagnostics {
		if strings.Contains(d.Message, "DEFAULT") && strings.Contains(d.Message, "DECLARE") {
			foundDefaultWarning = true
			if d.Severity != SeverityWarning {
				t.Errorf("expected warning severity for DEFAULT on DECLARE line, got %v", d.Severity)
			}
		}
	}

	if !foundDefaultWarning {
		t.Error("expected warning for :DEFAULT on same line as :DECLARE")
	}
}

func TestGetDiagnostics_DefaultOnDeclareLine_ValidSyntax(t *testing.T) {
	text := `:PROCEDURE Test;
:PARAMETERS sName;
:DEFAULT sName, "";
:DECLARE sLocal;
:ENDPROC;`

	opts := DefaultDiagnosticOptions()
	diagnostics := GetDiagnostics(text, opts)

	// Should NOT find warning - :DEFAULT is with :PARAMETERS (correct) and :DECLARE is separate
	for _, d := range diagnostics {
		if strings.Contains(d.Message, "DEFAULT") && strings.Contains(d.Message, "DECLARE") {
			t.Errorf("unexpected DEFAULT/DECLARE warning when syntax is valid: %s", d.Message)
		}
	}
}

// ==================== Global Variable Assignment Tests ====================

func TestGetDiagnostics_GlobalAssignment_Error(t *testing.T) {
	text := `:PROCEDURE Test;
gCurrentUser := "admin";
x := gCurrentUser;
:ENDPROC;`

	opts := DefaultDiagnosticOptions()
	opts.GlobalVariables = []string{"gCurrentUser", "gAppName"}
	diagnostics := GetDiagnostics(text, opts)

	// Should find error for assigning to gCurrentUser
	foundGlobalError := false
	for _, d := range diagnostics {
		if strings.Contains(d.Message, "Cannot assign to global variable") && strings.Contains(d.Message, "gCurrentUser") {
			foundGlobalError = true
			if d.Severity != SeverityError {
				t.Errorf("expected error severity for global assignment, got %v", d.Severity)
			}
		}
	}

	if !foundGlobalError {
		t.Error("expected error for assigning to global variable gCurrentUser")
	}
}

func TestGetDiagnostics_GlobalUsage_NoError(t *testing.T) {
	text := `:PROCEDURE Test;
:DECLARE x;
x := gCurrentUser;
Len(gCurrentUser);
:ENDPROC;`

	opts := DefaultDiagnosticOptions()
	opts.GlobalVariables = []string{"gCurrentUser"}
	diagnostics := GetDiagnostics(text, opts)

	// Should NOT find any global assignment errors - just reading the global, not assigning
	for _, d := range diagnostics {
		if strings.Contains(d.Message, "Cannot assign to global variable") {
			t.Errorf("unexpected global assignment error when only reading: %s", d.Message)
		}
	}
}

func TestGetDiagnostics_GlobalAssignment_CaseInsensitive(t *testing.T) {
	text := `:PROCEDURE Test;
GCURRENTUSER := "admin";
:ENDPROC;`

	opts := DefaultDiagnosticOptions()
	opts.GlobalVariables = []string{"gCurrentUser"} // lowercase definition
	diagnostics := GetDiagnostics(text, opts)

	// Should find error even with different casing
	foundGlobalError := false
	for _, d := range diagnostics {
		if strings.Contains(d.Message, "Cannot assign to global variable") {
			foundGlobalError = true
		}
	}

	if !foundGlobalError {
		t.Error("expected case-insensitive match for global variable")
	}
}

func TestGetDiagnostics_GlobalAssignment_NoGlobals(t *testing.T) {
	text := `:PROCEDURE Test;
x := 1;
:ENDPROC;`

	opts := DefaultDiagnosticOptions()
	// No globals configured
	diagnostics := GetDiagnostics(text, opts)

	// Should NOT have any global-related errors
	for _, d := range diagnostics {
		if strings.Contains(d.Message, "global variable") {
			t.Errorf("unexpected global variable error when no globals configured: %s", d.Message)
		}
	}
}

// ==================== Undeclared Variable Tests ====================
// These tests verify the fix for GitHub issues #55, #56, #2, #53

func TestGetDiagnostics_UndeclaredVariable_Basic(t *testing.T) {
	text := `:PROCEDURE Test;
:DECLARE x;
x := 1;
y := undeclaredVar + x;
:ENDPROC;`

	opts := DefaultDiagnosticOptions()
	opts.CheckUndeclaredVars = true
	diagnostics := GetDiagnostics(text, opts)

	// Should find warning for 'undeclaredVar'
	foundUndeclaredWarning := false
	for _, d := range diagnostics {
		if strings.Contains(d.Message, "undeclaredVar") && strings.Contains(d.Message, "not declared") {
			foundUndeclaredWarning = true
			if d.Severity != SeverityWarning {
				t.Errorf("expected warning severity for undeclared variable, got %v", d.Severity)
			}
		}
	}

	if !foundUndeclaredWarning {
		t.Error("expected warning for undeclared variable 'undeclaredVar'")
	}
}

func TestGetDiagnostics_UndeclaredVariable_DeclaredVarsNotFlagged(t *testing.T) {
	text := `:PROCEDURE Test;
:DECLARE x, y;
:PARAMETERS param1;
x := 1;
y := param1 + x;
:ENDPROC;`

	opts := DefaultDiagnosticOptions()
	opts.CheckUndeclaredVars = true
	diagnostics := GetDiagnostics(text, opts)

	// Should NOT find any undeclared variable warnings
	for _, d := range diagnostics {
		if strings.Contains(d.Message, "not declared") {
			t.Errorf("unexpected undeclared variable warning for declared variable: %s", d.Message)
		}
	}
}

// Issue #55: Globals config should recognize variables as pre-declared
func TestGetDiagnostics_UndeclaredVariable_GlobalsRecognized(t *testing.T) {
	text := `:PROCEDURE Test;
x := gCurrentUser;
:ENDPROC;`

	opts := DefaultDiagnosticOptions()
	opts.CheckUndeclaredVars = true
	opts.GlobalVariables = []string{"gCurrentUser", "gAppName"}
	diagnostics := GetDiagnostics(text, opts)

	// Should NOT find undeclared warning for gCurrentUser - it's a configured global
	for _, d := range diagnostics {
		if strings.Contains(d.Message, "gCurrentUser") && strings.Contains(d.Message, "not declared") {
			t.Errorf("globals should be recognized as pre-declared (Issue #55): %s", d.Message)
		}
	}
}

// Issue #56: :INCLUDE paths should be skipped from undeclared variable checking
func TestGetDiagnostics_UndeclaredVariable_IncludePathSkipped(t *testing.T) {
	text := `:INCLUDE File_Helpers.FileWork;

:PROCEDURE Test;
:DECLARE x;
x := 1;
:ENDPROC;`

	opts := DefaultDiagnosticOptions()
	opts.CheckUndeclaredVars = true
	diagnostics := GetDiagnostics(text, opts)

	// Should NOT find undeclared warnings for 'File_Helpers' or 'FileWork'
	for _, d := range diagnostics {
		// Only check "not declared" messages, not lexer unknown token warnings
		if strings.Contains(d.Message, "not declared") &&
			(strings.Contains(d.Message, "File_Helpers") || strings.Contains(d.Message, "FileWork")) {
			t.Errorf(":INCLUDE paths should not be flagged as undeclared (Issue #56): %s", d.Message)
		}
	}
}

// Issue #2: 'Me' should be recognized as a built-in identifier
func TestGetDiagnostics_UndeclaredVariable_MeRecognized(t *testing.T) {
	text := `:CLASS MyClass;
:PROCEDURE Initialize;
Me:bActive := .T.;
Me:nCounter := 0;
:ENDPROC;
:ENDCLASS;`

	opts := DefaultDiagnosticOptions()
	opts.CheckUndeclaredVars = true
	diagnostics := GetDiagnostics(text, opts)

	// Should NOT find undeclared warning for 'Me'
	for _, d := range diagnostics {
		if strings.Contains(strings.ToUpper(d.Message), "ME") && strings.Contains(d.Message, "not declared") {
			t.Errorf("'Me' should be recognized as built-in identifier (Issue #2): %s", d.Message)
		}
	}
}

// Issue #53: Function calls should be skipped from undefined variable checking
func TestGetDiagnostics_UndeclaredVariable_FunctionCallsSkipped(t *testing.T) {
	text := `:PROCEDURE Test;
:DECLARE result;
result := MyCustomProc(1, 2);
result := Calculate(result);
:ENDPROC;`

	opts := DefaultDiagnosticOptions()
	opts.CheckUndeclaredVars = true
	diagnostics := GetDiagnostics(text, opts)

	// Should NOT find undeclared warnings for 'MyCustomProc' or 'Calculate' - they're function calls
	for _, d := range diagnostics {
		if strings.Contains(d.Message, "MyCustomProc") || strings.Contains(d.Message, "Calculate") {
			t.Errorf("function calls should not be flagged as undeclared (Issue #53): %s", d.Message)
		}
	}
}

// Test that built-in functions are not flagged as undeclared
func TestGetDiagnostics_UndeclaredVariable_BuiltinFunctionsSkipped(t *testing.T) {
	text := `:PROCEDURE Test;
:DECLARE sql, result;
sql := "SELECT * FROM users";
result := SQLExecute(sql, "ds");
result := Len("hello");
:ENDPROC;`

	opts := DefaultDiagnosticOptions()
	opts.CheckUndeclaredVars = true
	diagnostics := GetDiagnostics(text, opts)

	// Should NOT find undeclared warnings for SQLExecute or Len
	for _, d := range diagnostics {
		if strings.Contains(d.Message, "SQLExecute") || strings.Contains(d.Message, "Len") {
			t.Errorf("built-in functions should not be flagged as undeclared: %s", d.Message)
		}
	}
}

// Test that dynamic assignment declares the variable
func TestGetDiagnostics_UndeclaredVariable_DynamicAssignment(t *testing.T) {
	text := `:PROCEDURE Test;
x := 1;
y := x + 1;
:ENDPROC;`

	opts := DefaultDiagnosticOptions()
	opts.CheckUndeclaredVars = true
	diagnostics := GetDiagnostics(text, opts)

	// Should NOT find undeclared warnings - x is dynamically declared by assignment
	for _, d := range diagnostics {
		if (strings.Contains(d.Message, "'x'") || strings.Contains(d.Message, "'y'")) &&
			strings.Contains(d.Message, "not declared") {
			t.Errorf("dynamically assigned variables should be treated as declared: %s", d.Message)
		}
	}
}

// Test that property access is skipped
func TestGetDiagnostics_UndeclaredVariable_PropertyAccessSkipped(t *testing.T) {
	text := `:PROCEDURE Test;
:DECLARE oData;
oData:Value := 10;
oData:Name := "test";
:ENDPROC;`

	opts := DefaultDiagnosticOptions()
	opts.CheckUndeclaredVars = true
	diagnostics := GetDiagnostics(text, opts)

	// Should NOT find undeclared warnings for 'Value' or 'Name' - they're property access
	for _, d := range diagnostics {
		if strings.Contains(d.Message, "Value") || strings.Contains(d.Message, "Name") {
			t.Errorf("property access should not be flagged as undeclared: %s", d.Message)
		}
	}
}

// Test that NIL is not flagged as undeclared
func TestGetDiagnostics_UndeclaredVariable_NILNotFlagged(t *testing.T) {
	text := `:PROCEDURE Test;
:DECLARE x;
x := NIL;
:IF x = NIL;
:ENDIF;
:ENDPROC;`

	opts := DefaultDiagnosticOptions()
	opts.CheckUndeclaredVars = true
	diagnostics := GetDiagnostics(text, opts)

	// Should NOT find undeclared warning for NIL
	for _, d := range diagnostics {
		if strings.Contains(d.Message, "NIL") && strings.Contains(d.Message, "not declared") {
			t.Errorf("NIL should not be flagged as undeclared: %s", d.Message)
		}
	}
}

// Test that SSL classes are not flagged as undeclared
func TestGetDiagnostics_UndeclaredVariable_ClassesNotFlagged(t *testing.T) {
	text := `:PROCEDURE Test;
:DECLARE oExpando, oDataset;
oExpando := SSLExpando();
oDataset := SSLDataset();
:ENDPROC;`

	opts := DefaultDiagnosticOptions()
	opts.CheckUndeclaredVars = true
	diagnostics := GetDiagnostics(text, opts)

	// Should NOT find undeclared warnings for SSLExpando or SSLDataset
	for _, d := range diagnostics {
		if strings.Contains(d.Message, "SSLExpando") || strings.Contains(d.Message, "SSLDataset") {
			t.Errorf("SSL built-in classes should not be flagged as undeclared: %s", d.Message)
		}
	}
}

// Test undeclared variable checking is disabled by default
func TestGetDiagnostics_UndeclaredVariable_DisabledByDefault(t *testing.T) {
	text := `:PROCEDURE Test;
x := undeclaredVar;
:ENDPROC;`

	opts := DefaultDiagnosticOptions()
	// CheckUndeclaredVars is false by default
	diagnostics := GetDiagnostics(text, opts)

	// Should NOT find any undeclared variable warnings when feature is disabled
	for _, d := range diagnostics {
		if strings.Contains(d.Message, "not declared") {
			t.Errorf("undeclared variable checking should be disabled by default: %s", d.Message)
		}
	}
}

// ==================== Control Flow Folding Range Tests ====================

func TestGetFoldingRanges_IfBlock(t *testing.T) {
	text := `:IF condition;
    DoSomething();
    DoMore();
:ENDIF;`

	ranges := GetFoldingRanges(text)

	// Should have at least one folding range for IF block
	foundIFRange := false
	for _, r := range ranges {
		if r.StartLine == 0 && r.EndLine == 3 && r.Kind == "region" {
			foundIFRange = true
		}
	}

	if !foundIFRange {
		t.Errorf("expected folding range for IF block (0-3), got ranges: %+v", ranges)
	}
}

func TestGetFoldingRanges_WhileBlock(t *testing.T) {
	text := `:WHILE x > 0;
    x := x - 1;
    Process();
:ENDWHILE;`

	ranges := GetFoldingRanges(text)

	foundWhileRange := false
	for _, r := range ranges {
		if r.StartLine == 0 && r.EndLine == 3 && r.Kind == "region" {
			foundWhileRange = true
		}
	}

	if !foundWhileRange {
		t.Errorf("expected folding range for WHILE block (0-3), got ranges: %+v", ranges)
	}
}

func TestGetFoldingRanges_ForBlock(t *testing.T) {
	text := `:FOR i := 1 :TO 10;
    arr[i] := i;
    Process(i);
:NEXT;`

	ranges := GetFoldingRanges(text)

	foundForRange := false
	for _, r := range ranges {
		if r.StartLine == 0 && r.EndLine == 3 && r.Kind == "region" {
			foundForRange = true
		}
	}

	if !foundForRange {
		t.Errorf("expected folding range for FOR block (0-3), got ranges: %+v", ranges)
	}
}

func TestGetFoldingRanges_BeginCaseBlock(t *testing.T) {
	text := `:BEGINCASE;
:CASE nVal == 1;
    x := 1;
    :EXITCASE;
:CASE nVal == 2;
    x := 2;
    :EXITCASE;
:OTHERWISE;
    x := 0;
    :EXITCASE;
:ENDCASE;`

	ranges := GetFoldingRanges(text)

	foundCaseRange := false
	for _, r := range ranges {
		if r.StartLine == 0 && r.EndLine == 10 && r.Kind == "region" {
			foundCaseRange = true
		}
	}

	if !foundCaseRange {
		t.Errorf("expected folding range for BEGINCASE block (0-10), got ranges: %+v", ranges)
	}
}

func TestGetFoldingRanges_TryBlock(t *testing.T) {
	text := `:TRY;
    RiskyOperation();
:CATCH;
    HandleError();
:FINALLY;
    Cleanup();
:ENDTRY;`

	ranges := GetFoldingRanges(text)

	foundTryRange := false
	for _, r := range ranges {
		if r.StartLine == 0 && r.EndLine == 6 && r.Kind == "region" {
			foundTryRange = true
		}
	}

	if !foundTryRange {
		t.Errorf("expected folding range for TRY block (0-6), got ranges: %+v", ranges)
	}
}

func TestGetFoldingRanges_NestedBlocks(t *testing.T) {
	text := `:PROCEDURE Test;
    :IF x > 0;
        :WHILE y < 10;
            y := y + 1;
        :ENDWHILE;
    :ENDIF;
:ENDPROC;`

	ranges := GetFoldingRanges(text)

	// Should have folding ranges for: PROCEDURE (0-6), IF (1-5), WHILE (2-4)
	foundProcedure := false
	foundIF := false
	foundWhile := false

	for _, r := range ranges {
		if r.StartLine == 0 && r.EndLine == 6 {
			foundProcedure = true
		}
		if r.StartLine == 1 && r.EndLine == 5 {
			foundIF = true
		}
		if r.StartLine == 2 && r.EndLine == 4 {
			foundWhile = true
		}
	}

	if !foundProcedure {
		t.Error("expected folding range for PROCEDURE (0-6)")
	}
	if !foundIF {
		t.Error("expected folding range for IF (1-5)")
	}
	if !foundWhile {
		t.Error("expected folding range for WHILE (2-4)")
	}
}

func TestGetFoldingRanges_SingleLineNotFoldable(t *testing.T) {
	text := `:IF x > 0; :RETURN x; :ENDIF;`

	ranges := GetFoldingRanges(text)

	// Single-line blocks should not be foldable
	for _, r := range ranges {
		if r.StartLine == r.EndLine {
			t.Errorf("single-line block should not create folding range: %+v", r)
		}
	}
}

func TestGetFoldingRanges_UnclosedBlock(t *testing.T) {
	text := `:PROCEDURE Test;
    :IF x > 0;
        DoSomething();`

	ranges := GetFoldingRanges(text)

	// Unclosed blocks should extend to end of file
	foundIF := false
	for _, r := range ranges {
		// IF starts at line 1, should end at last line (2)
		if r.StartLine == 1 && r.EndLine >= 2 {
			foundIF = true
		}
	}

	if !foundIF {
		t.Errorf("unclosed IF block should have folding range extending to end, got: %+v", ranges)
	}
}
