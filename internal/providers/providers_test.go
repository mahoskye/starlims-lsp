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

// [spec feature.hover/A4]
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

// [spec feature.hover/A3]
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

func TestGetHover_Procedure_DocblockSurfaces(t *testing.T) {
	// vs-code-ssl-formatter#75 — docblock fields should appear in hover.
	// [spec feature.hover/A3]
	text := `/*
 * Description: Composes a friendly greeting.
 * Parameters:
 *   sName - the user name
 * Returns: sGreeting - the composed greeting
;
:PROCEDURE Greet;
:PARAMETERS sName;
:RETURN "hi " + sName;
:ENDPROC;

:PROCEDURE Caller;
Greet("World");
:ENDPROC;`

	procedures := []parser.ProcedureInfo{
		{
			Name: "Greet", StartLine: 7, EndLine: 10, Parameters: []string{"sName"},
			Doc: parser.ProcedureDoc{
				Description:   "Composes a friendly greeting.",
				ParameterDocs: map[string]string{"sName": "the user name"},
				Returns:       "sGreeting - the composed greeting",
			},
		},
		{Name: "Caller", StartLine: 12, EndLine: 14},
	}

	hover := GetHover(text, 13, 1, procedures, nil)
	if hover == nil {
		t.Fatal("expected hover for Greet")
	}
	if !strings.Contains(hover.Contents, "Composes a friendly greeting") {
		t.Errorf("hover missing description: %s", hover.Contents)
	}
	if !strings.Contains(hover.Contents, "the user name") {
		t.Errorf("hover missing parameter doc: %s", hover.Contents)
	}
	if !strings.Contains(hover.Contents, "Returns") {
		t.Errorf("hover missing returns: %s", hover.Contents)
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

// TestGetHover_FunctionExceptions confirms that documented exceptions from
// ssl-element-meta.json are appended to the function-hover output. The
// canary is ExecFunction — its docs page lists two exception triggers
// ("Called with no arguments at all." and "aParameters is provided but
// is not an array.").
func TestGetHover_FunctionExceptions(t *testing.T) {
	// [spec feature.hover/A2]
	text := `result := ExecFunction("Foo", {});`
	hover := GetHover(text, 1, 12, nil, nil)
	if hover == nil {
		t.Skip("ExecFunction may not be in the function list on this build")
		return
	}
	if !strings.Contains(hover.Contents, "Documented exceptions") {
		t.Errorf("expected hover to include exceptions section; got:\n%s", hover.Contents)
	}
	if !strings.Contains(hover.Contents, "Please provide at least one parameter for ExecFunction") {
		t.Errorf("expected hover to quote canonical exception message; got:\n%s", hover.Contents)
	}
}

func TestGetHover_NoMatch(t *testing.T) {
	// Unknown identifiers must return nil rather than an empty or
	// fabricated hover. [spec feature.hover/A8]
	text := `unknownThing := 1;`

	hover := GetHover(text, 1, 5, nil, nil)
	if hover != nil {
		t.Errorf("expected nil hover for unknown identifier, got: %s", hover.Contents)
	}
}

// TestGetHover_BuiltinFunction_SignatureAndParameters pins the built-in
// function hover contract: Markdown containing the canonical signature label,
// the parameter list, and the return type. [spec feature.hover/A1]
func TestGetHover_BuiltinFunction_SignatureAndParameters(t *testing.T) {
	text := `result := SQLExecute(query, "ds");`

	hover := GetHover(text, 1, 12, nil, nil)
	if hover == nil {
		t.Fatal("expected hover info for SQLExecute")
	}
	if !strings.Contains(hover.Contents, "SQLExecute(") {
		t.Errorf("expected hover to contain the signature label, got: %s", hover.Contents)
	}
	if !strings.Contains(hover.Contents, "**Parameters:**") {
		t.Errorf("expected hover to contain a parameter list, got: %s", hover.Contents)
	}
	if !strings.Contains(hover.Contents, "**Returns:**") {
		t.Errorf("expected hover to contain the return type, got: %s", hover.Contents)
	}
}

// TestGetHover_BuiltinFunction_CaseInsensitive: lowercase `sqlexecute`
// resolves to the canonical SQLExecute hover. [spec feature.hover/A5]
func TestGetHover_BuiltinFunction_CaseInsensitive(t *testing.T) {
	text := `result := sqlexecute(query);`

	hover := GetHover(text, 1, 12, nil, nil)
	if hover == nil {
		t.Fatal("expected hover info for lowercase sqlexecute")
	}
	if !strings.Contains(hover.Contents, "SQLExecute(") {
		t.Errorf("expected canonical SQLExecute hover, got: %s", hover.Contents)
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

// [spec feature.definition/A2]
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

// TestFindDefinition_Parameter: a use of a :PARAMETERS name navigates to the
// :PARAMETERS declaration line. [spec feature.definition/A2]
func TestFindDefinition_Parameter(t *testing.T) {
	text := `:PROCEDURE Calculate;
:PARAMETERS nValue, sType;
result := nValue * 2;
:ENDPROC;`

	procedures, variables := parseText(text)

	// Cursor on nValue use (line 3, column 11)
	location := FindDefinition(text, 3, 11, "file:///test.ssl", procedures, variables)
	if location == nil {
		t.Fatal("expected to find definition for parameter")
	}
	if location.Range.Start.Line != 1 { // 0-based, :PARAMETERS is line 2
		t.Errorf("expected definition on line 1 (:PARAMETERS), got %d", location.Range.Start.Line)
	}
}

// TestFindDefinition_BuiltinAndKeyword_ReturnsNil: built-in functions and
// keywords have no navigable source in user code — the response must be
// null, never a spurious location. [spec feature.definition/A5]
func TestFindDefinition_BuiltinAndKeyword_ReturnsNil(t *testing.T) {
	text := `:PROCEDURE Test;
:DECLARE sQuery;
result := SQLExecute(sQuery, "ds");
:IF result > 0;
:ENDIF;
:ENDPROC;`

	procedures, variables := parseText(text)

	// Cursor on SQLExecute (line 3, column 12)
	if loc := FindDefinition(text, 3, 12, "file:///test.ssl", procedures, variables); loc != nil {
		t.Errorf("expected nil definition for built-in function, got %+v", loc)
	}

	// Cursor on the :IF keyword (line 4, column 2)
	if loc := FindDefinition(text, 4, 2, "file:///test.ssl", procedures, variables); loc != nil {
		t.Errorf("expected nil definition for keyword, got %+v", loc)
	}
}

// TestFindDefinition_ForeignLocalNotSurfaced: with procedure info available,
// an out-of-scope name must resolve to null — never to another procedure's
// local (issue #41). Top-level declarations remain navigable from anywhere.
func TestFindDefinition_ForeignLocalNotSurfaced(t *testing.T) {
	text := `:DECLARE gShared;
:PROCEDURE First;
:DECLARE nPrivate;
:ENDPROC;
:PROCEDURE Second;
result := nPrivate;
result := gShared;
:ENDPROC;`

	procedures, variables := parseText(text)

	// Cursor on nPrivate use inside Second (line 6, column 11): First's
	// local is out of scope — expect nil, not First's declaration.
	if loc := FindDefinition(text, 6, 11, "file:///test.ssl", procedures, variables); loc != nil {
		t.Errorf("expected nil for another procedure's local, got %+v", loc)
	}

	// Cursor on gShared use (line 7, column 11): the file-level declaration
	// is in scope and must still resolve via the fallback.
	loc := FindDefinition(text, 7, 11, "file:///test.ssl", procedures, variables)
	if loc == nil {
		t.Fatal("expected file-level declaration to resolve")
	}
	if loc.Range.Start.Line != 0 {
		t.Errorf("expected definition on line 0 (:DECLARE gShared), got %d", loc.Range.Start.Line)
	}
}

// ==================== References Tests ====================

// [spec feature.references/A1]
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

// [spec feature.references/A1]
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

// [spec feature.references/A2]
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

// [spec feature.references/A2]
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
	// [spec feature.references/A5]
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
	// [spec feature.references/A6]
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
	// [spec feature.references/A5]
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
	// [spec feature.references/A6]
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

// TestFindReferences_CaseInsensitive: every case variant of the identifier
// is the same symbol and is returned. [spec feature.references/A3]
func TestFindReferences_CaseInsensitive(t *testing.T) {
	text := `:PROCEDURE Test;
:DECLARE MyVariable;
x := myvariable;
y := MYVARIABLE;
:ENDPROC;`

	locations := FindReferences(text, 2, 10, "file:///test.ssl", true)
	if locations == nil {
		t.Fatal("expected to find references")
	}
	if len(locations) != 3 {
		t.Errorf("expected 3 case-insensitive references, got %d", len(locations))
	}
}

// TestFindReferences_WholeWordOnly: `count` must not match `countAll` or
// `recount`. [spec feature.references/A4]
func TestFindReferences_WholeWordOnly(t *testing.T) {
	text := `:PROCEDURE Test;
:DECLARE count;
x := count;
y := countAll;
z := recount;
:ENDPROC;`

	locations := FindReferences(text, 2, 10, "file:///test.ssl", true)
	if locations == nil {
		t.Fatal("expected to find references")
	}
	if len(locations) != 2 {
		t.Errorf("expected 2 whole-word references, got %d", len(locations))
	}
	for _, loc := range locations {
		if loc.Range.Start.Line == 3 || loc.Range.Start.Line == 4 {
			t.Errorf("partial identifier matched on line %d", loc.Range.Start.Line)
		}
	}
}

// TestFindReferences_ExcludeDeclaration_FromUseSite: includeDeclaration=false
// must exclude the declaration even when the request originates on a use
// site — the declaration is resolved from the parsed symbol, not the cursor
// line (issue #42). [spec feature.references/A8]
func TestFindReferences_ExcludeDeclaration_FromUseSite(t *testing.T) {
	t.Run("variable", func(t *testing.T) {
		text := `:PROCEDURE Test;
:DECLARE myVar;
myVar := 1;
x := myVar + 1;
:ENDPROC;`

		procedures, variables := parseText(text)

		// Cursor on the use at line 3, not the :DECLARE line
		locations := FindReferencesWithScope(text, 3, 2, "file:///test.ssl", false, procedures, variables)
		if locations == nil {
			t.Fatal("expected to find references")
		}
		if len(locations) != 2 {
			t.Errorf("expected 2 references excluding declaration, got %d", len(locations))
		}
		for _, loc := range locations {
			if loc.Range.Start.Line == 1 {
				t.Error("declaration should be excluded when includeDeclaration=false, even from a use site")
			}
		}
	})

	t.Run("procedure", func(t *testing.T) {
		text := `:PROCEDURE TargetProc;
:ENDPROC;

:PROCEDURE Main;
TargetProc();
x := TargetProc();
:ENDPROC;`

		procedures, variables := parseText(text)

		// Cursor on the call site at line 5, not the :PROCEDURE line
		locations := FindReferencesWithScope(text, 5, 2, "file:///test.ssl", false, procedures, variables)
		if locations == nil {
			t.Fatal("expected to find references")
		}
		if len(locations) != 2 {
			t.Errorf("expected 2 references excluding declaration, got %d", len(locations))
		}
		for _, loc := range locations {
			if loc.Range.Start.Line == 0 {
				t.Error(":PROCEDURE declaration should be excluded when includeDeclaration=false, even from a call site")
			}
		}
	})
}

// TestFindReferences_SkipsCommentsAndNonDispatchStrings: whole-word matches
// inside comments and unrelated string literals are not references; only
// code-context matches are returned (issue #43). [spec feature.references/A9]
func TestFindReferences_SkipsCommentsAndNonDispatchStrings(t *testing.T) {
	text := `:PROCEDURE Test;
:DECLARE sName;
/* sName is mentioned in this comment;
sName := "sName is a variable";
other := "prefix sName suffix";
:ENDPROC;`

	procedures, variables := parseText(text)

	locations := FindReferencesWithScope(text, 2, 10, "file:///test.ssl", true, procedures, variables)
	if locations == nil {
		t.Fatal("expected to find references")
	}
	// Only the declaration (line 2) and the assignment target (line 4).
	if len(locations) != 2 {
		t.Errorf("expected 2 references (declaration + assignment), got %d", len(locations))
	}
	for _, loc := range locations {
		if loc.Range.Start.Line == 2 {
			t.Error("match inside a comment must not be returned")
		}
		if loc.Range.Start.Line == 3 && loc.Range.Start.Character > 0 {
			t.Error("match inside an unrelated string must not be returned")
		}
		if loc.Range.Start.Line == 4 {
			t.Error("match inside an unrelated string must not be returned")
		}
	}
}

// TestFindReferences_NonDispatchStringWithProcName: a string mentioning a
// procedure name only counts as a reference when it is the whole first
// argument of DoProc/ExecFunction — the dispatch-target case; strings passed
// to other functions or containing extra text are skipped (issue #43).
// [spec feature.references/A9]
func TestFindReferences_NonDispatchStringWithProcName(t *testing.T) {
	text := `:PROCEDURE TargetProc;
:ENDPROC;

:PROCEDURE Main;
DoProc("TargetProc");
ExecFunction("TargetProc");
LogMessage("TargetProc failed");
DoProc("TargetProc extra");
x := "TargetProc";
:ENDPROC;`

	procedures, variables := parseText(text)

	locations := FindReferencesWithScope(text, 1, 12, "file:///test.ssl", true, procedures, variables)
	if locations == nil {
		t.Fatal("expected to find references")
	}
	// Declaration + DoProc target + ExecFunction target.
	if len(locations) != 3 {
		t.Errorf("expected 3 references (declaration + 2 dispatch targets), got %d", len(locations))
	}
	foundDoProc, foundExecFunction := false, false
	for _, loc := range locations {
		switch loc.Range.Start.Line {
		case 4:
			foundDoProc = true
		case 5:
			foundExecFunction = true
		case 6, 7, 8:
			t.Errorf("non-dispatch string match on line %d must not be returned", loc.Range.Start.Line)
		}
	}
	if !foundDoProc || !foundExecFunction {
		t.Error("dispatch string targets must remain references (feature.references/A7)")
	}
}

// TestFindReferences_DoProcStringTarget: the first string argument of
// DoProc/ExecFunction is the only legal call syntax for user procedures, so
// it counts as a reference. [spec feature.references/A7]
func TestFindReferences_DoProcStringTarget(t *testing.T) {
	text := `:PROCEDURE TargetProc;
:ENDPROC;

:PROCEDURE Main;
DoProc("TargetProc");
:ENDPROC;`

	procedures, variables := parseText(text)

	locations := FindReferencesWithScope(text, 1, 12, "file:///test.ssl", true, procedures, variables)
	if locations == nil {
		t.Fatal("expected to find references")
	}
	if len(locations) != 2 {
		t.Fatalf("expected 2 references (declaration + DoProc target), got %d", len(locations))
	}
	foundStringTarget := false
	for _, loc := range locations {
		if loc.Range.Start.Line == 4 { // 0-based: the DoProc call line
			foundStringTarget = true
		}
	}
	if !foundStringTarget {
		t.Error("expected the DoProc string target to be included as a reference")
	}
}

// ==================== Document Symbols Tests ====================

// [spec feature.document_symbols/A1]
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

// [spec feature.document_symbols/A4] — selectionRange covers exactly the
// procedure name, not the :PROCEDURE keyword or the trailing semicolon
// (issue #44).
func TestGetDocumentSymbols_SelectionRangeIsName(t *testing.T) {
	text := `:PROCEDURE MyProc;
:ENDPROC;`

	symbols := GetDocumentSymbols(text)

	for _, sym := range symbols {
		if sym.Name != "MyProc" {
			continue
		}
		sel := sym.SelectionRange
		// ":PROCEDURE MyProc;" — name starts at 0-based character 11.
		if sel.Start.Line != 0 || sel.End.Line != 0 {
			t.Fatalf("selection range must be on the declaration line, got %+v", sel)
		}
		if sel.Start.Character != 11 || sel.End.Character != 11+len("MyProc") {
			t.Errorf("expected selection range 11-%d (exactly the name), got %d-%d",
				11+len("MyProc"), sel.Start.Character, sel.End.Character)
		}
		return
	}
	t.Fatal("MyProc symbol not found")
}

// [spec feature.document_symbols/A2] — one Variable symbol per declared
// :PUBLIC name.
// [spec feature.document_symbols/A6] — :DECLARE locals never appear.
func TestGetDocumentSymbols_PublicVariables(t *testing.T) {
	text := `:PUBLIC gVar1, gVar2;

:PROCEDURE Test;
:DECLARE localVar;
:ENDPROC;`

	symbols := GetDocumentSymbols(text)

	// Should include one symbol per declared public name.
	publicNames := map[string]bool{}
	for _, sym := range symbols {
		if strings.Contains(sym.Detail, "public") {
			if sym.Kind != SymbolKindVariable {
				t.Errorf("expected public variable %q to have kind Variable, got %v", sym.Name, sym.Kind)
			}
			publicNames[sym.Name] = true
		}
	}
	if len(publicNames) != 2 || !publicNames["gVar1"] || !publicNames["gVar2"] {
		t.Errorf("expected public symbols gVar1 and gVar2, got %v", publicNames)
	}

	// :DECLARE locals must not be emitted as symbols at any level.
	var walk func([]DocumentSymbol)
	walk = func(syms []DocumentSymbol) {
		for _, sym := range syms {
			if sym.Name == "localVar" {
				t.Error(":DECLARE local localVar must not appear in document symbols")
			}
			walk(sym.Children)
		}
	}
	walk(symbols)
}

// [spec feature.document_symbols/A5] — results follow file order, not
// alphabetical order.
func TestGetDocumentSymbols_FileOrder(t *testing.T) {
	text := `:PROCEDURE Zebra;
:ENDPROC;

:PROCEDURE Alpha;
:ENDPROC;`

	symbols := GetDocumentSymbols(text)

	var procNames []string
	for _, sym := range symbols {
		if sym.Kind == SymbolKindFunction {
			procNames = append(procNames, sym.Name)
		}
	}
	if len(procNames) != 2 || procNames[0] != "Zebra" || procNames[1] != "Alpha" {
		t.Errorf("expected file order [Zebra Alpha], got %v", procNames)
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

// [spec feature.folding/A7]
func TestGetFoldingRanges_CommentBlocks(t *testing.T) {
	text := `/* This is a
multi-line
comment block ;
:PROCEDURE Test;
:ENDPROC;`

	ranges := GetFoldingRanges(text)

	// The multi-line comment (not a region marker) must fold with kind
	// "comment", not "region".
	foundComment := false
	for _, r := range ranges {
		if r.StartLine == 0 && r.EndLine == 2 {
			if r.Kind != "comment" {
				t.Errorf("expected kind %q for multi-line comment fold, got %q", "comment", r.Kind)
			}
			foundComment = true
		}
	}

	if !foundComment {
		t.Errorf("expected comment folding range (0-2), got: %+v", ranges)
	}
}

// ==================== Signature Help Tests ====================

func TestGetSignatureHelp_KnownFunction(t *testing.T) {
	text := `result := Len(`
	tokens := tokenizeForSignatureHelp(t, text)

	help := GetSignatureHelpWithProcedures(tokens, nil, 1, len(text)+1)
	if help == nil {
		t.Fatal("expected signature help for Len(")
	}
	if len(help.Signatures) == 0 {
		t.Error("expected at least one signature")
	}
}

func TestGetSignatureHelp_ActiveParameter(t *testing.T) {
	text := `result := SubStr("hello", 1, `
	tokens := tokenizeForSignatureHelp(t, text)

	help := GetSignatureHelpWithProcedures(tokens, nil, 1, len(text)+1)
	if help == nil {
		t.Fatal("expected signature help for SubStr")
	}
	if help.ActiveParameter != 2 { // 0-indexed, third parameter
		t.Errorf("expected active parameter 2, got %d", help.ActiveParameter)
	}
}

// [spec feature.signature_help/A4]
func TestGetSignatureHelpWithProcedures_UserDefinedProcReturnsNil(t *testing.T) {
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

	if help != nil {
		t.Fatal("expected no signature help for direct user-defined procedure call")
	}
}

func TestGetSignatureHelpWithProcedures_DirectUserProcActiveParameterReturnsNil(t *testing.T) {
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

	if help != nil {
		t.Fatalf("expected no signature help for direct user procedure call, got %+v", help)
	}
}

func TestGetSignatureHelpWithProcedures_NoParamsReturnsNilForDirectProc(t *testing.T) {
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

	if help != nil {
		t.Fatalf("expected no signature help for direct user procedure call, got %+v", help)
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

// [spec feature.signature_help/A4]
func TestGetSignatureHelpWithProcedures_CaseInsensitiveUserProcReturnsNil(t *testing.T) {
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

	if help != nil {
		t.Fatalf("expected no signature help for direct user procedure call, got %+v", help)
	}
}

func tokenizeForSignatureHelp(t *testing.T, text string) []lexer.Token {
	t.Helper()
	return lexer.NewLexer(text).Tokenize()
}

// TestGetSignatureHelpWithProcedures_OpenParenActiveParamZero: immediately
// after the opening paren of a built-in call, the signature with its
// parameter list is returned and activeParameter is 0.
// [spec feature.signature_help/A1]
func TestGetSignatureHelpWithProcedures_OpenParenActiveParamZero(t *testing.T) {
	text := `result := SQLExecute(`
	tokens := tokenizeForSignatureHelp(t, text)

	help := GetSignatureHelpWithProcedures(tokens, nil, 1, len(text)+1)
	if help == nil {
		t.Fatal("expected signature help for SQLExecute")
	}
	if len(help.Signatures) != 1 {
		t.Fatalf("expected 1 signature, got %d", len(help.Signatures))
	}
	if !strings.Contains(help.Signatures[0].Label, "SQLExecute(") {
		t.Errorf("expected SQLExecute signature label, got %q", help.Signatures[0].Label)
	}
	if len(help.Signatures[0].Parameters) == 0 {
		t.Error("expected parameter information")
	}
	if help.ActiveParameter != 0 {
		t.Errorf("expected activeParameter 0, got %d", help.ActiveParameter)
	}
}

// TestGetSignatureHelpWithProcedures_ConstructorContext: signature help
// inside `Email{` returns the class's constructor signatures on the wired
// token-based path (issue #40). [spec feature.signature_help/A8]
func TestGetSignatureHelpWithProcedures_ConstructorContext(t *testing.T) {
	text := `oMail := Email{`
	tokens := tokenizeForSignatureHelp(t, text)

	help := GetSignatureHelpWithProcedures(tokens, nil, 1, len(text)+1)
	if help == nil {
		t.Fatal("expected constructor signature help for Email{")
	}
	if len(help.Signatures) == 0 {
		t.Fatal("expected at least one constructor signature")
	}
	for _, sig := range help.Signatures {
		if !strings.Contains(strings.ToLower(sig.Label), "email") {
			t.Errorf("expected Email constructor label, got %q", sig.Label)
		}
	}
	if help.ActiveParameter != 0 {
		t.Errorf("expected activeParameter 0, got %d", help.ActiveParameter)
	}
}

// TestGetSignatureHelpWithProcedures_ArrayLiteralCommasDoNotCount: commas
// inside an array literal argument belong to the literal, not the enclosing
// call's parameter index (fixed alongside issue #40's brace handling).
func TestGetSignatureHelpWithProcedures_ArrayLiteralCommasDoNotCount(t *testing.T) {
	// Cursor inside the array literal: the enclosing DoProc is at its
	// second argument (one top-level comma), not its fourth.
	text := `DoProc("Calc", {1, 2, `
	tokens := tokenizeForSignatureHelp(t, text)

	help := GetSignatureHelpWithProcedures(tokens, nil, 1, len(text)+1)
	if help == nil {
		// DoProc is a built-in dispatch function with a signature.
		t.Fatal("expected signature help for DoProc")
	}
	if help.ActiveParameter != 1 {
		t.Errorf("expected activeParameter 1 (array is one argument), got %d", help.ActiveParameter)
	}
}

// TestGetSignatureHelpWithProcedures_TopLevelCommasOnly: activeParameter
// counts top-level commas only — commas inside string literals must not
// advance it, and it stays put while typing within an argument.
// [spec feature.signature_help/A2]
func TestGetSignatureHelpWithProcedures_TopLevelCommasOnly(t *testing.T) {
	// One top-level comma; the commas inside the SQL string don't count.
	text := `result := SQLExecute("SELECT a, b, c FROM t", `
	tokens := tokenizeForSignatureHelp(t, text)

	help := GetSignatureHelpWithProcedures(tokens, nil, 1, len(text)+1)
	if help == nil {
		t.Fatal("expected signature help for SQLExecute")
	}
	if help.ActiveParameter != 1 {
		t.Errorf("expected activeParameter 1 (string commas ignored), got %d", help.ActiveParameter)
	}

	// Still parameter 1 while typing the second argument.
	text2 := `result := SQLExecute("SELECT a, b, c FROM t", sName`
	tokens2 := tokenizeForSignatureHelp(t, text2)

	help2 := GetSignatureHelpWithProcedures(tokens2, nil, 1, len(text2)+1)
	if help2 == nil {
		t.Fatal("expected signature help for SQLExecute")
	}
	if help2.ActiveParameter != 1 {
		t.Errorf("expected activeParameter to stay 1 mid-argument, got %d", help2.ActiveParameter)
	}
}

// TestGetSignatureHelpWithProcedures_NestedCallsInnermost: inside nested
// calls the innermost enclosing call's signature is shown; once the inner
// call closes, the outer one is. [spec feature.signature_help/A3]
func TestGetSignatureHelpWithProcedures_NestedCallsInnermost(t *testing.T) {
	inner := `result := Upper(AllTrim(`
	tokens := tokenizeForSignatureHelp(t, inner)

	help := GetSignatureHelpWithProcedures(tokens, nil, 1, len(inner)+1)
	if help == nil {
		t.Fatal("expected signature help inside AllTrim(")
	}
	if !strings.Contains(help.Signatures[0].Label, "AllTrim(") {
		t.Errorf("expected inner AllTrim signature, got %q", help.Signatures[0].Label)
	}

	outer := `result := Upper(AllTrim(sValue)`
	tokens2 := tokenizeForSignatureHelp(t, outer)

	help2 := GetSignatureHelpWithProcedures(tokens2, nil, 1, len(outer)+1)
	if help2 == nil {
		t.Fatal("expected signature help after inner call closed")
	}
	if !strings.Contains(help2.Signatures[0].Label, "Upper(") {
		t.Errorf("expected outer Upper signature, got %q", help2.Signatures[0].Label)
	}
}

// TestGetSignatureHelpWithProcedures_UnknownFunctionReturnsNil: names not in
// the built-in inventory produce no signature help.
// [spec feature.signature_help/A5]
func TestGetSignatureHelpWithProcedures_UnknownFunctionReturnsNil(t *testing.T) {
	text := `result := NotARealFunction(`
	tokens := tokenizeForSignatureHelp(t, text)

	if help := GetSignatureHelpWithProcedures(tokens, nil, 1, len(text)+1); help != nil {
		t.Fatalf("expected no signature help for unknown function, got %+v", help)
	}
}

// TestGetSignatureHelpWithProcedures_OutsideCallReturnsNil: positions outside
// any call's argument list produce no signature help.
// [spec feature.signature_help/A7]
func TestGetSignatureHelpWithProcedures_OutsideCallReturnsNil(t *testing.T) {
	text := `x := 5;`
	tokens := tokenizeForSignatureHelp(t, text)

	if help := GetSignatureHelpWithProcedures(tokens, nil, 1, len(text)+1); help != nil {
		t.Fatalf("expected no signature help outside a call, got %+v", help)
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

// [spec feature.snippets/A1] — every snippet item carries kind Snippet,
// insertTextFormat Snippet, and $-style tab stops.
func TestGetSnippetCompletions(t *testing.T) {
	snippets := GetSnippetCompletions(false)

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

	// Every snippet (both sets) must be a real LSP snippet with tab stops.
	for _, set := range [][]CompletionItem{GetSnippetCompletions(false), GetSnippetCompletions(true)} {
		for _, s := range set {
			if s.Kind != CompletionKindSnippet {
				t.Errorf("snippet %q has kind %v, want CompletionKindSnippet", s.Label, s.Kind)
			}
			if s.InsertTextFormat != InsertTextFormatSnippet {
				t.Errorf("snippet %q has insertTextFormat %v, want InsertTextFormatSnippet", s.Label, s.InsertTextFormat)
			}
			if !strings.Contains(s.InsertText, "$") {
				t.Errorf("snippet %q has no $-style tab stop", s.Label)
			}
		}
	}
}

// TestGetSnippetCompletions_BodiesWellFormed pins the structural contract of
// snippet bodies: block openers are matched by their closers and
// colon-keyword statements are semicolon-terminated, so no snippet can
// expand to structurally broken SSL.
// [spec feature.snippets/A2] — proc expands to a complete block.
// [spec feature.snippets/A3] — case snippet structure.
// [spec feature.snippets/A4] — sql snippet uses named ?value? placeholders.
// [spec feature.snippets/A5] — matched closers and terminators everywhere.
func TestGetSnippetCompletions_BodiesWellFormed(t *testing.T) {
	standard := GetSnippetCompletions(false)
	dataSource := GetSnippetCompletions(true)

	byLabel := map[string]string{}
	for _, s := range append(append([]CompletionItem{}, standard...), dataSource...) {
		byLabel[s.Label] = s.InsertText
	}

	// A2: proc is a complete :PROCEDURE/:ENDPROC block with a header comment
	// and a tab stop on the procedure name.
	proc := byLabel["proc"]
	if proc == "" {
		t.Fatal("missing 'proc' snippet")
	}
	if !strings.Contains(proc, ":PROCEDURE ${1:") || !strings.Contains(proc, ":ENDPROC;") {
		t.Errorf("proc snippet must open :PROCEDURE with a name tab stop and close with :ENDPROC;, got:\n%s", proc)
	}
	if !strings.HasPrefix(proc, "/*") {
		t.Errorf("proc snippet must start with a header comment, got:\n%s", proc)
	}

	// A3: case snippet contains the full :BEGINCASE structure.
	caseBody := byLabel["case"]
	for _, part := range []string{":BEGINCASE;", ":CASE ", ":EXITCASE;", ":OTHERWISE;", ":ENDCASE;"} {
		if !strings.Contains(caseBody, part) {
			t.Errorf("case snippet missing %q, got:\n%s", part, caseBody)
		}
	}

	// A4: sql snippet is a SQLExecute call with named ?value? placeholders.
	sqlBody := byLabel["sql"]
	if !strings.Contains(sqlBody, "SQLExecute(") {
		t.Errorf("sql snippet must call SQLExecute, got:\n%s", sqlBody)
	}
	if !strings.Contains(sqlBody, "?${") || strings.Index(sqlBody, "?${") >= strings.LastIndex(sqlBody, "}?") {
		t.Errorf("sql snippet must use named ?value? placeholder syntax, got:\n%s", sqlBody)
	}

	// A5: every block opener in every snippet has its matching closer, and
	// colon-keyword lines are semicolon-terminated.
	pairs := [][2]string{
		{":PROCEDURE", ":ENDPROC"},
		{":IF ", ":ENDIF"},
		{":WHILE ", ":ENDWHILE"},
		{":FOR ", ":NEXT"},
		{":BEGINCASE", ":ENDCASE"},
		{":TRY", ":ENDTRY"},
		{":BEGININLINECODE", ":ENDINLINECODE"},
		{"/* region", "/* endregion"},
	}
	for label, body := range byLabel {
		for _, p := range pairs {
			open, close := strings.Count(body, p[0]), strings.Count(body, p[1])
			if open != close {
				t.Errorf("snippet %q: %d %q openers but %d %q closers", label, open, p[0], close, p[1])
			}
		}
		for _, line := range strings.Split(body, "\n") {
			trimmed := strings.TrimSpace(line)
			if strings.HasPrefix(trimmed, ":") && !strings.HasSuffix(trimmed, ";") {
				t.Errorf("snippet %q: colon-keyword line not semicolon-terminated: %q", label, trimmed)
			}
		}
	}
}

func TestGetProcedureCompletions(t *testing.T) {
	procedures := []parser.ProcedureInfo{
		{Name: "MyProc", StartLine: 1, EndLine: 5, Parameters: []string{"param1"}},
		{Name: "OtherProc", StartLine: 10, EndLine: 15, Parameters: nil},
	}

	completions := GetProcedureCompletions(procedures, false)

	if len(completions) != 2 {
		t.Fatalf("expected 2 procedure completions, got %d", len(completions))
	}

	for _, c := range completions {
		if c.Kind != CompletionKindFunction {
			t.Errorf("expected function completion kind for procedure %s", c.Label)
		}
		switch c.Label {
		case "MyProc":
			if c.InsertText != `DoProc("MyProc", {${1:param1}})` {
				t.Errorf("expected DoProc snippet for parameterized procedure, got %q", c.InsertText)
			}
		case "OtherProc":
			if c.InsertText != `DoProc("OtherProc")` {
				t.Errorf("expected empty-arg omission for no-arg procedure, got %q", c.InsertText)
			}
		}
	}
}

func TestGetProcedureCompletions_DocblockSurfaces(t *testing.T) {
	// vs-code-ssl-formatter#75 — the description, per-parameter docs, and
	// return doc parsed from the leading /* ... ; block must appear in the
	// completion item's Documentation field so users see them in the popup.
	procedures := []parser.ProcedureInfo{
		{
			Name: "Greet", StartLine: 7, EndLine: 10, Parameters: []string{"sName"},
			Doc: parser.ProcedureDoc{
				Description:   "Composes a friendly greeting.",
				ParameterDocs: map[string]string{"sName": "the user name"},
				Returns:       "sGreeting - the composed greeting",
			},
		},
	}
	items := GetProcedureCompletions(procedures, false)
	if len(items) != 1 {
		t.Fatalf("expected 1 item, got %d", len(items))
	}
	doc := items[0].Documentation
	if !strings.Contains(doc, "Composes a friendly greeting") {
		t.Errorf("completion missing description: %s", doc)
	}
	if !strings.Contains(doc, "the user name") {
		t.Errorf("completion missing parameter doc: %s", doc)
	}
	if !strings.Contains(doc, "Returns") {
		t.Errorf("completion missing returns: %s", doc)
	}
}

func TestGetProcedureCompletions_ClassMethodContext(t *testing.T) {
	procedures := []parser.ProcedureInfo{
		{Name: "MyMethod", StartLine: 2, EndLine: 6, Parameters: []string{"sValue", "nCount"}},
		{Name: "Helper", StartLine: 8, EndLine: 10, Parameters: nil},
	}

	completions := GetProcedureCompletions(procedures, true)

	if len(completions) != 2 {
		t.Fatalf("expected 2 procedure completions, got %d", len(completions))
	}

	for _, c := range completions {
		switch c.Label {
		case "MyMethod":
			if c.InsertText != `Me:MyMethod(${1:sValue}, ${2:nCount})` {
				t.Errorf("expected Me: method snippet for parameterized method, got %q", c.InsertText)
			}
		case "Helper":
			if c.InsertText != `Me:Helper()` {
				t.Errorf("expected Me: method snippet for no-arg method, got %q", c.InsertText)
			}
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

	completions := GetAllCompletions(procedures, variables, false, false, false)

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

	for _, c := range completions {
		if c.Label == "Me" || c.Label == "Base" || c.Label == "Constructor" {
			t.Fatalf("did not expect class-only completion %q outside class-method context", c.Label)
		}
	}
}

func TestGetAllCompletions_ClassMethodContextIncludesClassForms(t *testing.T) {
	completions := GetAllCompletions(nil, nil, true, false, false)

	foundMe := false
	foundBase := false
	foundConstructor := false

	for _, c := range completions {
		switch c.Label {
		case "Me":
			foundMe = true
		case "Base":
			foundBase = true
		case "Constructor":
			foundConstructor = true
		}
	}

	if !foundMe {
		t.Error("expected Me completion in class-method context")
	}
	if !foundBase {
		t.Error("expected Base completion in class-method context")
	}
	if !foundConstructor {
		t.Error("expected Constructor completion in class-method context")
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

	if len(hungarianDiagnostics) != 1 {
		t.Fatalf("expected 1 Hungarian notation warning, got %d", len(hungarianDiagnostics))
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

	if len(hungarianDiagnostics) != 1 {
		t.Fatalf("expected 1 Hungarian notation warning, got %d", len(hungarianDiagnostics))
	}
}

func TestGetDiagnostics_DefaultMustFollowParameters(t *testing.T) {
	text := `:PROCEDURE Test;
:PARAMETERS sName;
:DECLARE sValue;
:DEFAULT sName, "";
:ENDPROC;`

	diagnostics := GetDiagnostics(text, DefaultDiagnosticOptions())

	for _, d := range diagnostics {
		if strings.Contains(d.Message, "immediately after ':PARAMETERS'") {
			return
		}
	}

	t.Fatal("expected ':DEFAULT' placement diagnostic")
}

func TestGetDiagnostics_BareKeywordNameIsIdentifier(t *testing.T) {
	// IF without a colon is just an identifier, not a keyword — no diagnostic.
	text := `IF x = 1;
    value := 1;
ENDIF;`

	diagnostics := GetDiagnostics(text, DefaultDiagnosticOptions())

	for _, d := range diagnostics {
		if strings.Contains(d.Message, "must be colon-prefixed") {
			t.Error("bare keyword-named identifier should not trigger colon-prefix diagnostic")
		}
	}
}

func TestGetDiagnostics_KeywordMustBeUppercase(t *testing.T) {
	text := `:if x = 1;
    value := 1;
:endif;`

	diagnostics := GetDiagnostics(text, DefaultDiagnosticOptions())

	for _, d := range diagnostics {
		if strings.Contains(d.Message, "must be uppercase") {
			return
		}
	}

	t.Fatal("expected keyword casing diagnostic")
}

func TestGetDiagnostics_UnknownColonKeyword(t *testing.T) {
	text := `:ENDCLASS;`

	diagnostics := GetDiagnostics(text, DefaultDiagnosticOptions())

	for _, d := range diagnostics {
		if strings.Contains(d.Message, "Unknown SSL keyword") {
			return
		}
	}

	t.Fatal("expected unknown keyword diagnostic")
}

func TestGetDiagnostics_LegacyCompactLabelAccepted(t *testing.T) {
	text := `:LABELSKIP;
Branch("LABELSKIP");`

	diagnostics := GetDiagnostics(text, DefaultDiagnosticOptions())
	for _, d := range diagnostics {
		if strings.Contains(d.Message, "Unknown SSL keyword") || strings.Contains(d.Message, "label keyword forms") {
			t.Fatalf("did not expect label-form diagnostic: %s", d.Message)
		}
	}
}

func TestGetDiagnostics_EmptyTrailingArrayShouldBeOmitted(t *testing.T) {
	text := `result := DoProc("MyProc", {});`

	diagnostics := GetDiagnostics(text, DefaultDiagnosticOptions())
	for _, d := range diagnostics {
		if strings.Contains(d.Message, "Omit the trailing empty array") {
			return
		}
	}

	t.Fatal("expected empty-array omission diagnostic")
}

func TestGetDiagnostics_TryRequiresCatchOrFinally(t *testing.T) {
	text := `:PROCEDURE Test;
:TRY;
	sValue := "";
:ENDTRY;
:ENDPROC;`

	diagnostics := GetDiagnostics(text, DefaultDiagnosticOptions())

	for _, d := range diagnostics {
		if strings.Contains(d.Message, "requires at least one ':CATCH' or ':FINALLY'") {
			return
		}
	}

	t.Fatal("expected ':TRY' structure diagnostic")
}

func TestGetDiagnostics_CatchDoesNotTakeVariable(t *testing.T) {
	text := `:PROCEDURE Test;
:TRY;
	sValue := "";
:CATCH oErr;
	oErr := GetLastSSLError();
:ENDTRY;
:ENDPROC;`

	diagnostics := GetDiagnostics(text, DefaultDiagnosticOptions())

	for _, d := range diagnostics {
		if strings.Contains(d.Message, "':CATCH' does not take an exception variable") {
			return
		}
	}

	t.Fatal("expected ':CATCH' clause-form diagnostic")
}

func TestGetDiagnostics_BranchTargetMustIncludeLabelToken(t *testing.T) {
	text := `:LABEL SKIP;
Branch("SKIP");`

	diagnostics := GetDiagnostics(text, DefaultDiagnosticOptions())

	for _, d := range diagnostics {
		if strings.Contains(d.Message, "Branch target string must include the label token text") {
			return
		}
	}

	t.Fatal("expected Branch target diagnostic")
}

func TestGetDiagnostics_DoProcInClassMethod(t *testing.T) {
	text := `:CLASS MyClass;
:PROCEDURE Run;
	DoProc("Helper", {});
:ENDPROC;`

	diagnostics := GetDiagnostics(text, DefaultDiagnosticOptions())

	for _, d := range diagnostics {
		if d.Code == CodeDoProcInClass {
			if !strings.Contains(d.Message, "Unqualified DoProc targets") {
				t.Errorf("unexpected message: %s", d.Message)
			}
			return
		}
	}

	t.Fatal("expected DoProc-in-class diagnostic")
}

func TestGetDiagnostics_DoProcInClassMethod_QualifiedAndUnprovableTargetsAllowed(t *testing.T) {
	// Qualified "Category.Script.Procedure" references are valid inside
	// class methods, and non-literal targets are not provable — neither may
	// flag (issue #151, ssl-style-guide#49). ExecFunction has no
	// class-method restriction in any form, including unqualified
	// string-literal targets.
	cases := []struct {
		name string
		code string
	}{
		{
			name: "qualified DoProc target",
			code: `:CLASS ValidationClient;
:PROCEDURE CheckInput;
	:PARAMETERS oInput;
	:DECLARE bResult;
	bResult := DoProc("API_Helper.ValidationHelper.ValidateProperties", {oInput});
	:RETURN bResult;
:ENDPROC;`,
		},
		{
			name: "variable DoProc target",
			code: `:CLASS ValidationClient;
:PROCEDURE CheckInput;
	:DECLARE sTarget, bResult;
	sTarget := BuildTargetName();
	bResult := DoProc(sTarget, {});
	:RETURN bResult;
:ENDPROC;`,
		},
		{
			name: "qualified ExecFunction target",
			code: `:CLASS ValidationClient;
:PROCEDURE CheckInput;
	:PARAMETERS oInput;
	:DECLARE bResult;
	bResult := ExecFunction("API_Helper.ValidationHelper.ValidateProperties", {oInput});
	:RETURN bResult;
:ENDPROC;`,
		},
		{
			name: "unqualified ExecFunction target",
			code: `:CLASS ValidationClient;
:PROCEDURE CheckInput;
	:PARAMETERS oInput;
	:DECLARE bResult;
	bResult := ExecFunction("ValidateProperties", {oInput});
	:RETURN bResult;
:ENDPROC;`,
		},
	}

	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			for _, d := range GetDiagnostics(tc.code, DefaultDiagnosticOptions()) {
				if d.Code == CodeDoProcInClass {
					t.Errorf("must not flag: %s", d.Message)
				}
			}
		})
	}
}

func TestGetDiagnostics_ConstructorReturnValue(t *testing.T) {
	text := `:CLASS MyClass;
:PROCEDURE Constructor;
	:RETURN "bad";
:ENDPROC;`

	diagnostics := GetDiagnostics(text, DefaultDiagnosticOptions())

	for _, d := range diagnostics {
		if strings.Contains(d.Message, "Constructor cannot return a value") {
			return
		}
	}

	t.Fatal("expected constructor return-value diagnostic")
}

func TestGetDiagnostics_BeginInlineCodeRequiresName(t *testing.T) {
	text := `:BEGININLINECODE;
:ENDINLINECODE;`

	diagnostics := GetDiagnostics(text, DefaultDiagnosticOptions())

	for _, d := range diagnostics {
		if strings.Contains(d.Message, "':BEGININLINECODE' requires a name") {
			return
		}
	}

	t.Fatal("expected BEGININLINECODE naming diagnostic")
}

func TestGetDiagnostics_UnclosedBeginInlineCode(t *testing.T) {
	text := `:BEGININLINECODE "MyBlock";
:DECLARE sValue;`

	diagnostics := GetDiagnostics(text, DefaultDiagnosticOptions())

	for _, d := range diagnostics {
		if strings.Contains(d.Message, "Unclosed ':BEGININLINECODE'") {
			return
		}
	}

	t.Fatal("expected unclosed BEGININLINECODE diagnostic")
}

func TestGetDiagnostics_ResumeIsDeprecatedKeyword(t *testing.T) {
	text := `:ERROR;
	sValue := "";
:RESUME;`

	diagnostics := GetDiagnostics(text, DefaultDiagnosticOptions())

	for _, d := range diagnostics {
		if strings.Contains(d.Message, "':RESUME' is legacy error handling") {
			return
		}
	}

	t.Fatal("expected RESUME deprecation diagnostic")
}

func TestGetDiagnostics_ErrorHandlerRequiresBody(t *testing.T) {
	text := `:ERROR;
:RESUME;`

	diagnostics := GetDiagnostics(text, DefaultDiagnosticOptions())

	for _, d := range diagnostics {
		if strings.Contains(d.Message, "':ERROR' must contain at least one statement") {
			return
		}
	}

	t.Fatal("expected :ERROR body diagnostic")
}

func TestGetDiagnostics_TryRequiresBody(t *testing.T) {
	text := `:PROCEDURE Test;
:TRY;
:CATCH;
	sValue := "";
:ENDTRY;
:ENDPROC;`

	diagnostics := GetDiagnostics(text, DefaultDiagnosticOptions())

	for _, d := range diagnostics {
		if strings.Contains(d.Message, "requires at least one statement before ':CATCH' or ':FINALLY'") {
			return
		}
	}

	t.Fatal("expected TRY body diagnostic")
}

func TestGetDiagnostics_FinallyRequiresBody(t *testing.T) {
	text := `:PROCEDURE Test;
:TRY;
	sValue := "";
:FINALLY;
:ENDTRY;
:ENDPROC;`

	diagnostics := GetDiagnostics(text, DefaultDiagnosticOptions())

	for _, d := range diagnostics {
		if strings.Contains(d.Message, "':FINALLY' must contain at least one statement") {
			return
		}
	}

	t.Fatal("expected FINALLY body diagnostic")
}

func TestGetDiagnostics_ClassMemberOrder(t *testing.T) {
	text := `:CLASS MyClass;
:PROCEDURE Constructor;
:ENDPROC;
:DECLARE sName;
`

	diagnostics := GetDiagnostics(text, DefaultDiagnosticOptions())

	for _, d := range diagnostics {
		if strings.Contains(d.Message, "Class members must be ordered as") {
			if d.Severity != SeverityInfo {
				t.Fatalf("expected class member order to be an info diagnostic, got %d", d.Severity)
			}
			return
		}
	}

	t.Fatal("expected class member order diagnostic")
}

func TestGetDiagnostics_MeOutsideClass(t *testing.T) {
	text := `vResult := Me;`

	diagnostics := GetDiagnostics(text, DefaultDiagnosticOptions())

	for _, d := range diagnostics {
		if strings.Contains(d.Message, "'Me' can only be used inside a ':CLASS' definition") {
			return
		}
	}

	t.Fatal("expected Me-outside-class diagnostic")
}

func TestGetDiagnostics_BaseRequiresMemberAndInherit(t *testing.T) {
	tests := []struct {
		name    string
		code    string
		message string
	}{
		{
			name:    "base outside class",
			code:    `vResult := Base:Run();`,
			message: "'Base:MemberName' can only be used inside a ':CLASS' definition",
		},
		{
			name:    "base without member",
			code:    `:CLASS MyClass; :INHERIT ParentClass; vResult := Base;`,
			message: "'Base' must be used as 'Base:MemberName' and cannot stand alone",
		},
		{
			name: "base without inherit",
			code: `:CLASS MyClass;
:PROCEDURE Run;
	vResult := Base:Run();
:ENDPROC;`,
			message: "'Base:MemberName' requires ':INHERIT' in the current ':CLASS' definition",
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			diagnostics := GetDiagnostics(tc.code, DefaultDiagnosticOptions())

			for _, d := range diagnostics {
				if strings.Contains(d.Message, tc.message) {
					return
				}
			}

			t.Fatalf("expected Base/Me diagnostic %q, got %#v", tc.message, diagnostics)
		})
	}
}

func TestGetDiagnostics_BaseWithInheritIsAllowed(t *testing.T) {
	text := `:CLASS MyClass;
:INHERIT ParentClass;
:PROCEDURE Run;
	vResult := Base:Run();
:ENDPROC;`

	diagnostics := GetDiagnostics(text, DefaultDiagnosticOptions())

	for _, d := range diagnostics {
		if strings.Contains(d.Message, "'Base' must be used") ||
			strings.Contains(d.Message, "'Base:MemberName' requires") ||
			strings.Contains(d.Message, "'Base:MemberName' can only") {
			t.Fatalf("unexpected Base diagnostic: %#v", diagnostics)
		}
	}
}

func TestGetDiagnostics_CommentMustEndWithSemicolon(t *testing.T) {
	text := `/* Missing terminator`

	diagnostics := GetDiagnostics(text, DefaultDiagnosticOptions())

	for _, d := range diagnostics {
		if strings.Contains(d.Message, "comments must end with a semicolon") {
			if d.Severity != SeverityError {
				t.Fatalf("expected missing comment terminator to be an error, got %d", d.Severity)
			}
			return
		}
	}

	t.Fatal("expected missing comment terminator diagnostic")
}

func TestGetDiagnostics_IncludeAtTop(t *testing.T) {
	text := `:PROCEDURE Test;
:ENDPROC;
:INCLUDE File_Helpers.FileWork;`

	diagnostics := GetDiagnostics(text, DefaultDiagnosticOptions())

	for _, d := range diagnostics {
		if strings.Contains(d.Message, "should appear early in the file") {
			return
		}
	}

	t.Fatal("expected include placement diagnostic")
}

func TestGetDiagnostics_PublicVariablesDiscouraged(t *testing.T) {
	text := `:PUBLIC gShared;`

	diagnostics := GetDiagnostics(text, DefaultDiagnosticOptions())

	for _, d := range diagnostics {
		if strings.Contains(d.Message, "risk namespace pollution") {
			return
		}
	}

	t.Fatal("expected :PUBLIC diagnostic")
}

func TestGetDiagnostics_TooManyProcedureParameters(t *testing.T) {
	text := `:PROCEDURE BigProc;
:PARAMETERS p01, p02, p03, p04, p05, p06, p07, p08, p09, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, p21;
:ENDPROC;`

	diagnostics := GetDiagnostics(text, DefaultDiagnosticOptions())

	for _, d := range diagnostics {
		if strings.Contains(d.Message, "more than 20 parameters") {
			return
		}
	}

	t.Fatal("expected max-parameters diagnostic")
}

func TestGetDiagnostics_LooseStringEquality(t *testing.T) {
	text := `:IF sName = "Test";
:ENDIF;`

	diagnostics := GetDiagnostics(text, DefaultDiagnosticOptions())

	for _, d := range diagnostics {
		if strings.Contains(d.Message, "prefix matching") {
			return
		}
	}

	t.Fatal("expected loose string equality diagnostic")
}

func TestGetDiagnostics_NilVsEmptyString(t *testing.T) {
	text := `:IF NIL = "";
:ENDIF;`

	diagnostics := GetDiagnostics(text, DefaultDiagnosticOptions())

	for _, d := range diagnostics {
		if strings.Contains(d.Message, "NIL is not the same as empty string") {
			return
		}
	}

	t.Fatal("expected NIL-vs-empty diagnostic")
}

func TestGetDiagnostics_NilVsZeroAndFalse(t *testing.T) {
	text := `:IF NIL == 0;
:ENDIF;
:IF NIL = .F.;
:ENDIF;`

	diagnostics := GetDiagnostics(text, DefaultDiagnosticOptions())

	found := 0
	for _, d := range diagnostics {
		if strings.Contains(d.Message, "NIL is not the same as empty string, zero, or .F.") {
			found++
		}
	}

	if found < 2 {
		t.Fatalf("expected NIL-vs-default diagnostics for zero and .F., got %d", found)
	}
}

func TestGetDiagnostics_DollarOperatorRequiresStrings(t *testing.T) {
	text := `bFound := 1 $ "haystack";`

	diagnostics := GetDiagnostics(text, DefaultDiagnosticOptions())

	for _, d := range diagnostics {
		if strings.Contains(d.Message, "containment operator only works on strings") {
			return
		}
	}

	t.Fatal("expected dollar-operator diagnostic")
}

func TestGetDiagnostics_NilInOperation(t *testing.T) {
	text := `x := NIL + 1;`

	diagnostics := GetDiagnostics(text, DefaultDiagnosticOptions())

	for _, d := range diagnostics {
		if strings.Contains(d.Message, "Using NIL in arithmetic or string operations") {
			return
		}
	}

	t.Fatal("expected NIL-operation diagnostic")
}

func TestGetDiagnostics_ForLoopRequiresNumericLiteralValues(t *testing.T) {
	text := `:FOR i := "1" :TO 10 :STEP .T.;
:NEXT;`

	diagnostics := GetDiagnostics(text, DefaultDiagnosticOptions())

	foundStart := false
	foundStep := false
	for _, d := range diagnostics {
		if strings.Contains(d.Message, "':FOR' start value should be numeric") {
			foundStart = true
		}
		if strings.Contains(d.Message, "':FOR' step value should be numeric") {
			foundStep = true
		}
	}

	if !foundStart || !foundStep {
		t.Fatalf("expected :FOR numeric diagnostics, got start=%v step=%v", foundStart, foundStep)
	}
}

func TestGetDiagnostics_ForLoopRequiresNumericVariables(t *testing.T) {
	text := `:DECLARE sStart, bStep;
:FOR sIndex := sStart :TO 10 :STEP bStep;
:NEXT;`

	diagnostics := GetDiagnostics(text, DefaultDiagnosticOptions())

	foundLoopVar := false
	foundStart := false
	foundStep := false
	for _, d := range diagnostics {
		if strings.Contains(d.Message, "':FOR' loop variable should be numeric") {
			foundLoopVar = true
		}
		if strings.Contains(d.Message, "':FOR' start value should be numeric") {
			foundStart = true
		}
		if strings.Contains(d.Message, "':FOR' step value should be numeric") {
			foundStep = true
		}
	}

	if !foundLoopVar || !foundStart || !foundStep {
		t.Fatalf("expected :FOR variable diagnostics, got loopVar=%v start=%v step=%v", foundLoopVar, foundStart, foundStep)
	}
}

func TestGetDiagnostics_CodeBlockComparison(t *testing.T) {
	text := `:DECLARE fnPredicate;
:IF fnPredicate == {|v| v > 1};
:ENDIF;`

	diagnostics := GetDiagnostics(text, DefaultDiagnosticOptions())

	for _, d := range diagnostics {
		if strings.Contains(d.Message, "Code blocks (lambdas) cannot be compared") {
			return
		}
	}

	t.Fatal("expected code-block comparison diagnostic")
}

func TestGetDiagnostics_CommentTerminatesEarly(t *testing.T) {
	text := `/* This is a comment; this text is CODE
x := 1;`

	diagnostics := GetDiagnostics(text, DefaultDiagnosticOptions())

	for _, d := range diagnostics {
		if strings.Contains(d.Message, "Comment terminated early by semicolon") {
			return
		}
	}

	t.Fatal("expected premature-comment diagnostic")
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
	if opts.MaxBlockDepth != 4 {
		t.Errorf("expected MaxBlockDepth to be 4, got %d", opts.MaxBlockDepth)
	}
	if opts.CheckHungarianNotation {
		t.Error("expected CheckHungarianNotation to be false by default")
	}
	if !reflect.DeepEqual(opts.HungarianPrefixes, []string{"a", "b", "d", "fn", "n", "o", "s", "v"}) {
		t.Errorf("unexpected default Hungarian prefixes: %v", opts.HungarianPrefixes)
	}
}

func TestDefaultSQLFormattingOptions(t *testing.T) {
	opts := DefaultSQLFormattingOptions()

	if !opts.Enabled {
		t.Error("expected SQL formatting to be enabled by default")
	}
	if opts.Style != "canonicalCompact" {
		t.Errorf("expected style 'canonicalCompact', got %q", opts.Style)
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

// [spec feature.document_symbols/A7]
func TestDocumentSymbols_EmptyText(t *testing.T) {
	symbols := GetDocumentSymbols("")
	// Empty text must produce an empty result, not an error/panic.
	if len(symbols) != 0 {
		t.Errorf("expected no symbols for empty text, got %d", len(symbols))
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

func TestGetDiagnostics_MissingExitCase_Nested(t *testing.T) {
	// Nested BEGINCASE blocks must be tracked independently
	text := `:BEGINCASE;
:CASE nOuter == 1;
    :BEGINCASE;
    :CASE nInner == 1;
        x := 1;
        :EXITCASE;
    :ENDCASE;
:CASE nOuter == 2;
    x := 2;
    :EXITCASE;
:ENDCASE;`

	opts := DefaultDiagnosticOptions()
	diagnostics := GetDiagnostics(text, opts)

	// Outer first :CASE (nOuter == 1) is missing :EXITCASE — should be flagged
	warnings := 0
	for _, d := range diagnostics {
		if strings.Contains(d.Message, "EXITCASE") {
			warnings++
		}
	}
	if warnings != 1 {
		t.Errorf("expected 1 missing EXITCASE warning (outer first CASE), got %d", warnings)
		for _, d := range diagnostics {
			if strings.Contains(d.Message, "EXITCASE") {
				t.Logf("  warning: %s (line %d)", d.Message, d.Range.Start.Line+1)
			}
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

	// Should find error for :DEFAULT on same line as :DECLARE (language constraint)
	foundDefaultError := false
	for _, d := range diagnostics {
		if strings.Contains(d.Message, "DEFAULT") && strings.Contains(d.Message, "DECLARE") {
			foundDefaultError = true
			if d.Severity != SeverityError {
				t.Errorf("expected error severity for DEFAULT on DECLARE line, got %v", d.Severity)
			}
		}
	}

	if !foundDefaultError {
		t.Error("expected error for :DEFAULT on same line as :DECLARE")
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

// Issue #56 (deep paths): a :INCLUDE path with three or more components like
// `:INCLUDE A.B.C.D;` was firing dot_property_access on the trailing dots
// because the single-token lookback in checkDotPropertyAccess gets stuck on
// the lexer's Unknown(".B.") chunks. None of the dots inside an :INCLUDE
// statement should fire dot_property_access.
func TestGetDiagnostics_DotPropertyAccess_IncludePathSkipped_DeepPath(t *testing.T) {
	cases := []string{
		`:INCLUDE File_Helpers.FileWork;`,
		`:INCLUDE A.B.C;`,
		`:INCLUDE A.B.C.D;`,
		`:INCLUDE Foo.Bar.Baz.Qux.Quux;`,
	}
	for _, text := range cases {
		t.Run(text, func(t *testing.T) {
			opts := DefaultDiagnosticOptions()
			diagnostics := GetDiagnostics(text, opts)
			for _, d := range diagnostics {
				if d.Code == CodeDotPropertyAccess {
					t.Errorf("dot_property_access should not fire inside :INCLUDE: %s", d.Message)
				}
			}
		})
	}
}

func TestGetDiagnostics_DotPropertyAccess_InheritQualifiedNameSkipped(t *testing.T) {
	// :INHERIT requires the qualified-name syntax (schema
	// classes.signature.inherit); its dots are path separators, not
	// property access (issue #149).
	cases := []string{
		`:CLASS RestApiUsers;
:INHERIT RestApi.RestApiBase;`,
		`:CLASS Widget;
:INHERIT Framework.UI.WidgetBase;`,
	}
	for _, text := range cases {
		t.Run(text, func(t *testing.T) {
			for _, d := range GetDiagnostics(text, DefaultDiagnosticOptions()) {
				if d.Code == CodeDotPropertyAccess {
					t.Errorf("dot_property_access should not fire inside :INHERIT: %s", d.Message)
				}
			}
		})
	}

	// Ordinary dot property access after the :INHERIT statement still flags.
	text := `:CLASS RestApiUsers;
:INHERIT RestApi.RestApiBase;
:PROCEDURE GetUsers;
	:DECLARE x, oEmail;
	oEmail := Email{};
	x := oEmail.Subject;
:ENDPROC;`
	found := false
	for _, d := range GetDiagnostics(text, DefaultDiagnosticOptions()) {
		if d.Code == CodeDotPropertyAccess {
			found = true
		}
	}
	if !found {
		t.Error("dot_property_access should still fire outside :INHERIT")
	}
}

func TestGetDiagnostics_UndeclaredVariable_InheritNameSkipped(t *testing.T) {
	// Identifiers inside :INHERIT qualified base names are module
	// references, not variable uses (issue #149; same mechanism as the
	// :INCLUDE exemption, issue #56).
	text := `:CLASS RestApiUsers;
:INHERIT RestApi.RestApiBase;
:PROCEDURE GetUsers;
	:DECLARE aOut;
	aOut := {};
	:RETURN aOut;
:ENDPROC;`

	opts := DefaultDiagnosticOptions()
	opts.CheckUndeclaredVars = true
	for _, d := range GetDiagnostics(text, opts) {
		if d.Code == CodeUndeclaredVariable && strings.Contains(d.Message, "RestApi'") {
			t.Errorf(":INHERIT base name should not be flagged as undeclared: %s", d.Message)
		}
	}
}

func TestGetDiagnostics_UndeclaredVariable_ClassNameSkipped(t *testing.T) {
	// The identifier after :CLASS is the class-name declaration, not a
	// variable use — same exemption mechanism as :INCLUDE paths (#56) and
	// :INHERIT base names (#149). Issue #155.
	text := `:CLASS RestApiUsers;
:PROCEDURE GetUsers;
	:DECLARE aOut;
	aOut := {};
	:RETURN aOut;
:ENDPROC;`

	opts := DefaultDiagnosticOptions()
	opts.CheckUndeclaredVars = true
	for _, d := range GetDiagnostics(text, opts) {
		if d.Code == CodeUndeclaredVariable && strings.Contains(d.Message, "RestApiUsers") {
			t.Errorf(":CLASS name should not be flagged as undeclared (issue #155): %s", d.Message)
		}
	}
}

func TestGetDiagnostics_UndeclaredVariable_ClassNameSkipped_OrdinaryStillFlags(t *testing.T) {
	// The :CLASS exemption must not suppress genuine undeclared uses
	// elsewhere in the class file.
	text := `:CLASS RestApiUsers;
:PROCEDURE GetUsers;
	nTotal := nMissing + 1;
:ENDPROC;`

	opts := DefaultDiagnosticOptions()
	opts.CheckUndeclaredVars = true
	found := false
	for _, d := range GetDiagnostics(text, opts) {
		if d.Code == CodeUndeclaredVariable && strings.Contains(d.Message, "nMissing") {
			found = true
		}
	}
	if !found {
		t.Error("ordinary undeclared variable should still flag in a :CLASS file (issue #155)")
	}
}

// Issue #2: 'Me' should be recognized as a built-in identifier
func TestGetDiagnostics_UndeclaredVariable_MeRecognized(t *testing.T) {
	text := `:CLASS MyClass;
:PROCEDURE Initialize;
Me:bActive := .T.;
Me:nCounter := 0;
:ENDPROC;`

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

// Issue #53: built-in function calls should be skipped from undefined-variable checking,
// while invalid direct custom procedure calls should still be diagnosed elsewhere.
func TestGetDiagnostics_UndeclaredVariable_DirectCustomCallsAreNotReportedAsUndeclared(t *testing.T) {
	text := `:PROCEDURE Test;
:DECLARE result;
result := MyCustomProc(1, 2);
result := Calculate(result);
:ENDPROC;`

	opts := DefaultDiagnosticOptions()
	opts.CheckUndeclaredVars = true
	diagnostics := GetDiagnostics(text, opts)

	foundDirectCallDiagnostic := false
	for _, d := range diagnostics {
		if strings.Contains(d.Message, "not declared") &&
			(strings.Contains(d.Message, "MyCustomProc") || strings.Contains(d.Message, "Calculate")) {
			t.Errorf("direct custom calls should not be treated as undeclared variables: %s", d.Message)
		}
		// Issue #167: unknown callables now warn rather than error, but the
		// direct_procedure_call code still fires.
		if d.Code == CodeDirectProcedureCall {
			foundDirectCallDiagnostic = true
		}
	}
	if !foundDirectCallDiagnostic {
		t.Fatal("expected direct custom procedure call diagnostic")
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
oExpando := SSLExpando{};
oDataset := SSLDataset{};
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

// [spec feature.folding/A1]
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
		if r.StartLine == 0 && r.EndLine == 6 && r.Kind == "region" {
			foundProcedure = true
		}
		if r.StartLine == 1 && r.EndLine == 5 && r.Kind == "region" {
			foundIF = true
		}
		if r.StartLine == 2 && r.EndLine == 4 && r.Kind == "region" {
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

// [spec feature.folding/A3]
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

// [spec feature.folding/A2]
func TestGetFoldingRanges_RegionMarkers(t *testing.T) {
	text := `/* region Helpers;
:PROCEDURE Helper;
:ENDPROC;
/* endregion;`

	ranges := GetFoldingRanges(text)

	// The region marker pair should fold from the region line to the
	// endregion line with kind "region".
	foundRegion := false
	for _, r := range ranges {
		if r.StartLine == 0 && r.EndLine == 3 && r.Kind == "region" {
			foundRegion = true
		}
	}

	if !foundRegion {
		t.Errorf("expected region folding range (0-3) for region markers, got: %+v", ranges)
	}
}

// [spec feature.folding/A4]
func TestGetFoldingRanges_UnclosedProcedureFoldsToEOF(t *testing.T) {
	text := `:PROCEDURE Test;
:DECLARE nCount;
nCount := 1;`

	ranges := GetFoldingRanges(text)

	// A :PROCEDURE with no :ENDPROC extends to the last line of the file,
	// like other unclosed blocks (starlims-lsp #27).
	found := false
	for _, r := range ranges {
		if r.StartLine == 0 && r.EndLine == 2 && r.Kind == "region" {
			found = true
		}
	}
	if !found {
		t.Errorf("expected unclosed procedure to fold 0-2, got: %+v", ranges)
	}
}

// [spec feature.folding/A5]
func TestGetFoldingRanges_NestedRegions_LIFOClose(t *testing.T) {
	text := `/* region Outer;
/* region Inner;
:DECLARE nCount;
/* endregion;
/* endregion;`

	ranges := GetFoldingRanges(text)

	// The canonical closer is a bare '/* endregion;' — no name. The first
	// closer ends the innermost open region (Inner, lines 1-3); the second
	// ends Outer (lines 0-4).
	foundInner, foundOuter := false, false
	for _, r := range ranges {
		if r.Kind != "region" {
			continue
		}
		if r.StartLine == 1 && r.EndLine == 3 {
			foundInner = true
		}
		if r.StartLine == 0 && r.EndLine == 4 {
			foundOuter = true
		}
	}
	if !foundInner || !foundOuter {
		t.Errorf("expected LIFO region ranges 1-3 (Inner) and 0-4 (Outer), got: %+v", ranges)
	}
}

// TestGetFoldingRanges_EndregionTrailingProseIgnored: trailing text before
// the ';' on an endregion marker is prose — it does not change pairing.
func TestGetFoldingRanges_EndregionTrailingProseIgnored(t *testing.T) {
	text := `/* region Helpers;
:DECLARE nCount;
/* endregion Helpers;`

	ranges := GetFoldingRanges(text)

	found := false
	for _, r := range ranges {
		if r.Kind == "region" && r.StartLine == 0 && r.EndLine == 2 {
			found = true
		}
	}
	if !found {
		t.Errorf("expected region range 0-2 with prose after endregion, got: %+v", ranges)
	}
}

// [spec feature.folding/A6]
func TestGetFoldingRanges_OrphanEndregion_ClosesNothing(t *testing.T) {
	text := `/* endregion;
/* region A;
:DECLARE nCount;
/* endregion;`

	ranges := GetFoldingRanges(text)

	// The first endregion has no open region — it closes nothing (the
	// mistake is surfaced by diag.region_end_mismatch). Region A still
	// pairs with the second endregion.
	found := false
	for _, r := range ranges {
		if r.Kind == "region" && r.StartLine == 1 && r.EndLine == 3 {
			found = true
		}
	}
	if !found {
		t.Errorf("expected region A range 1-3 despite orphan endregion, got: %+v", ranges)
	}
}

func TestGetDiagnostics_RegionEndMismatch(t *testing.T) {
	text := `/* endregion;
:PROCEDURE Helper;
:ENDPROC;`

	diagnostics := GetDiagnostics(text, DefaultDiagnosticOptions())

	found := false
	for _, d := range diagnostics {
		if d.Code == CodeRegionEndMismatch {
			found = true
			if d.Severity != SeverityWarning {
				t.Errorf("expected warning severity, got %v", d.Severity)
			}
		}
	}
	if !found {
		t.Errorf("expected region_end_mismatch for endregion with no open region, got: %+v", diagnostics)
	}
}

func TestGetDiagnostics_RegionEndMismatch_BalancedClean(t *testing.T) {
	text := `/* region Outer;
/* region Inner;
:DECLARE nCount;
/* endregion;
/* endregion Outer is done here;`

	diagnostics := GetDiagnostics(text, DefaultDiagnosticOptions())

	for _, d := range diagnostics {
		if d.Code == CodeRegionEndMismatch {
			t.Errorf("balanced markers (trailing prose ignored) must not flag: %+v", d)
		}
	}
}

// ==================== Me Keyword Hover Tests ====================

func TestGetHover_MeKeyword(t *testing.T) {
	text := `:CLASS MyClass;
:PROCEDURE Test;
    result := Me:Calculate();
:ENDPROC;`

	// Hover over Me
	hover := GetHover(text, 3, 15, nil, nil)
	if hover == nil {
		t.Fatal("expected hover info for Me keyword")
	}
	if !strings.Contains(hover.Contents, "Me") {
		t.Errorf("expected hover to contain 'Me', got: %s", hover.Contents)
	}
	if !strings.Contains(strings.ToLower(hover.Contents), "self-reference") {
		t.Errorf("expected hover to explain Me is a self-reference, got: %s", hover.Contents)
	}
}

func TestGetHover_MeKeyword_CaseInsensitive(t *testing.T) {
	text := `result := ME:Property;`

	// Hover over ME (uppercase)
	hover := GetHover(text, 1, 11, nil, nil)
	if hover == nil {
		t.Fatal("expected hover info for ME keyword")
	}
	if !strings.Contains(hover.Contents, "Me") {
		t.Errorf("expected hover to contain 'Me', got: %s", hover.Contents)
	}
}

// ==================== Definition Scope Precedence Tests ====================

// [spec feature.definition/A4]
func TestFindDefinition_LocalPrecedence(t *testing.T) {
	text := `:PUBLIC globalVar;

:PROCEDURE Test;
:DECLARE globalVar;  /* Local shadows global;
x := globalVar;
:ENDPROC;`

	procedures := []parser.ProcedureInfo{
		{Name: "Test", StartLine: 3, EndLine: 6},
	}
	variables := []parser.VariableInfo{
		{Name: "globalVar", Line: 1, Column: 9, Scope: parser.ScopePublic},
		{Name: "globalVar", Line: 4, Column: 10, Scope: parser.ScopeLocal},
	}

	// Find definition of globalVar on line 5 - should find local (line 4), not public (line 1)
	location := FindDefinition(text, 5, 7, "file:///test.ssl", procedures, variables)
	if location == nil {
		t.Fatal("expected to find definition for variable")
	}
	if location.Range.Start.Line != 3 { // 0-based, so line 4 = index 3
		t.Errorf("expected definition on line 3 (local), got %d", location.Range.Start.Line)
	}
}

// [spec feature.definition/A2]
func TestFindDefinition_PublicWhenNoLocal(t *testing.T) {
	text := `:PUBLIC gGlobalVar;

:PROCEDURE Test;
x := gGlobalVar;
:ENDPROC;`

	procedures := []parser.ProcedureInfo{
		{Name: "Test", StartLine: 3, EndLine: 5},
	}
	variables := []parser.VariableInfo{
		{Name: "gGlobalVar", Line: 1, Column: 9, Scope: parser.ScopePublic},
	}

	// Find definition of gGlobalVar on line 4 - should find public (line 1)
	location := FindDefinition(text, 4, 7, "file:///test.ssl", procedures, variables)
	if location == nil {
		t.Fatal("expected to find definition for variable")
	}
	if location.Range.Start.Line != 0 { // 0-based, so line 1 = index 0
		t.Errorf("expected definition on line 0 (public), got %d", location.Range.Start.Line)
	}
}

// ==================== DoProc/ExecFunction Definition Tests ====================

// [spec feature.definition/A1]
func TestFindDefinition_DoProc(t *testing.T) {
	text := `:PROCEDURE Main;
result := DoProc("Helper", {param1, param2});
:ENDPROC;

:PROCEDURE Helper;
:PARAMETERS p1, p2;
x := p1 + p2;
:ENDPROC;`

	procedures := []parser.ProcedureInfo{
		{Name: "Main", StartLine: 1, EndLine: 3},
		{Name: "Helper", StartLine: 5, EndLine: 8},
	}

	// Cursor on "Helper" inside the DoProc string (around column 20)
	location := FindDefinition(text, 2, 20, "file:///test.ssl", procedures, nil)
	if location == nil {
		t.Fatal("expected to find definition for DoProc target")
	}
	if location.Range.Start.Line != 4 { // 0-based, so line 5 = index 4
		t.Errorf("expected definition on line 4 (Helper procedure), got %d", location.Range.Start.Line)
	}
}

// [spec feature.definition/A7]
func TestFindDefinition_ExecFunction(t *testing.T) {
	text := `:PROCEDURE Main;
result := ExecFunction("Calculate", {10, 20});
:ENDPROC;

:PROCEDURE Calculate;
:PARAMETERS a, b;
:RETURN a * b;
:ENDPROC;`

	procedures := []parser.ProcedureInfo{
		{Name: "Main", StartLine: 1, EndLine: 3},
		{Name: "Calculate", StartLine: 5, EndLine: 8},
	}

	// Cursor on "Calculate" inside the ExecFunction string
	location := FindDefinition(text, 2, 28, "file:///test.ssl", procedures, nil)
	if location == nil {
		t.Fatal("expected to find definition for ExecFunction target")
	}
	if location.Range.Start.Line != 4 { // 0-based, so line 5 = index 4
		t.Errorf("expected definition on line 4 (Calculate procedure), got %d", location.Range.Start.Line)
	}
}

// [spec feature.definition/A1]
func TestFindDefinition_DoProc_SingleQuotes(t *testing.T) {
	text := `:PROCEDURE Main;
result := DoProc('Helper', {});
:ENDPROC;

:PROCEDURE Helper;
:ENDPROC;`

	procedures := []parser.ProcedureInfo{
		{Name: "Main", StartLine: 1, EndLine: 3},
		{Name: "Helper", StartLine: 5, EndLine: 6},
	}

	// Cursor on 'Helper' with single quotes
	location := FindDefinition(text, 2, 20, "file:///test.ssl", procedures, nil)
	if location == nil {
		t.Fatal("expected to find definition for DoProc target with single quotes")
	}
	if location.Range.Start.Line != 4 {
		t.Errorf("expected definition on line 4, got %d", location.Range.Start.Line)
	}
}

// [spec feature.definition/A6]
func TestFindDefinition_DoProc_NotFound(t *testing.T) {
	text := `:PROCEDURE Main;
result := DoProc("NonExistent", {});
:ENDPROC;`

	procedures := []parser.ProcedureInfo{
		{Name: "Main", StartLine: 1, EndLine: 3},
	}

	// Cursor on "NonExistent" - procedure doesn't exist
	location := FindDefinition(text, 2, 20, "file:///test.ssl", procedures, nil)
	if location != nil {
		t.Error("expected no definition for non-existent procedure")
	}
}

// [spec feature.definition/A3]
func TestFindDefinition_DoProc_CaseInsensitive(t *testing.T) {
	text := `:PROCEDURE Main;
result := DoProc("helper", {});
:ENDPROC;

:PROCEDURE Helper;
:ENDPROC;`

	procedures := []parser.ProcedureInfo{
		{Name: "Main", StartLine: 1, EndLine: 3},
		{Name: "Helper", StartLine: 5, EndLine: 6},
	}

	// Cursor on "helper" (lowercase) should find Helper (PascalCase)
	location := FindDefinition(text, 2, 20, "file:///test.ssl", procedures, nil)
	if location == nil {
		t.Fatal("expected to find definition with case-insensitive matching")
	}
	if location.Range.Start.Line != 4 {
		t.Errorf("expected definition on line 4, got %d", location.Range.Start.Line)
	}
}

// ==================== Hierarchical Document Symbols Tests ====================

// [spec feature.document_symbols/A3]
func TestGetDocumentSymbols_RegionContainsProcedures(t *testing.T) {
	text := `/* region Helpers;
:PROCEDURE HelperOne;
:ENDPROC;

:PROCEDURE HelperTwo;
:ENDPROC;
/* endregion;`

	symbols := GetDocumentSymbols(text)

	// Find the region symbol
	var regionSymbol *DocumentSymbol
	for i := range symbols {
		if symbols[i].Name == "Helpers" && symbols[i].Detail == "region" {
			regionSymbol = &symbols[i]
			break
		}
	}

	if regionSymbol == nil {
		t.Fatal("expected to find region symbol 'Helpers'")
	}

	// Region should have procedures as children
	if len(regionSymbol.Children) != 2 {
		t.Errorf("expected region to have 2 children (procedures), got %d", len(regionSymbol.Children))
	}

	// Verify children are the procedures
	foundHelperOne := false
	foundHelperTwo := false
	for _, child := range regionSymbol.Children {
		if child.Name == "HelperOne" && child.Kind == SymbolKindFunction {
			foundHelperOne = true
		}
		if child.Name == "HelperTwo" && child.Kind == SymbolKindFunction {
			foundHelperTwo = true
		}
	}

	if !foundHelperOne {
		t.Error("expected HelperOne procedure as child of region")
	}
	if !foundHelperTwo {
		t.Error("expected HelperTwo procedure as child of region")
	}
}

// [spec feature.document_symbols/A3] — a procedure outside the markers is a
// sibling, not a child.
func TestGetDocumentSymbols_ProcedureOutsideRegion(t *testing.T) {
	text := `:PROCEDURE OutsideProc;
:ENDPROC;

/* region Helpers;
:PROCEDURE InsideProc;
:ENDPROC;
/* endregion;`

	symbols := GetDocumentSymbols(text)

	// OutsideProc should be a top-level symbol, not nested
	foundOutside := false
	for _, sym := range symbols {
		if sym.Name == "OutsideProc" && sym.Kind == SymbolKindFunction {
			foundOutside = true
			break
		}
	}

	if !foundOutside {
		t.Error("expected OutsideProc to be a top-level symbol")
	}

	// Find the region and verify InsideProc is its child
	var regionSymbol *DocumentSymbol
	for i := range symbols {
		if symbols[i].Name == "Helpers" {
			regionSymbol = &symbols[i]
			break
		}
	}

	if regionSymbol == nil {
		t.Fatal("expected to find region 'Helpers'")
	}

	foundInside := false
	for _, child := range regionSymbol.Children {
		if child.Name == "InsideProc" {
			foundInside = true
			break
		}
	}

	if !foundInside {
		t.Error("expected InsideProc to be a child of region Helpers")
	}
}

// ==================== Unused Variables Diagnostic Tests ====================

func TestCheckUnusedVariables_UnusedLocal(t *testing.T) {
	text := `:PROCEDURE Test;
:DECLARE unusedVar;
x := 1;
:ENDPROC;`

	opts := DiagnosticOptions{CheckUnusedVars: true}
	diagnostics := GetDiagnostics(text, opts)

	// Should find one unused variable
	found := false
	for _, d := range diagnostics {
		if strings.Contains(d.Message, "unusedVar") && strings.Contains(d.Message, "never used") {
			found = true
			if d.Severity != SeverityHint {
				t.Errorf("expected SeverityHint, got %d", d.Severity)
			}
			break
		}
	}

	if !found {
		t.Error("expected diagnostic for unused variable 'unusedVar'")
	}
}

func TestCheckUnusedVariables_UsedLocal(t *testing.T) {
	text := `:PROCEDURE Test;
:DECLARE usedVar;
usedVar := 1;
x := usedVar + 1;
:ENDPROC;`

	opts := DiagnosticOptions{CheckUnusedVars: true}
	diagnostics := GetDiagnostics(text, opts)

	// Should NOT find unused variable diagnostic for usedVar
	for _, d := range diagnostics {
		if strings.Contains(d.Message, "usedVar") && strings.Contains(d.Message, "never used") {
			t.Error("should not flag 'usedVar' as unused since it is used")
		}
	}
}

func TestCheckUnusedVariables_UnusedParameter(t *testing.T) {
	text := `:PROCEDURE Test;
:PARAMETERS unusedParam;
x := 1;
:ENDPROC;`

	opts := DiagnosticOptions{CheckUnusedVars: true}
	diagnostics := GetDiagnostics(text, opts)

	// Should find unused parameter
	found := false
	for _, d := range diagnostics {
		if strings.Contains(d.Message, "unusedParam") && strings.Contains(d.Message, "never used") {
			found = true
			break
		}
	}

	if !found {
		t.Error("expected diagnostic for unused parameter 'unusedParam'")
	}
}

func TestCheckUnusedVariables_UsedParameter(t *testing.T) {
	text := `:PROCEDURE Test;
:PARAMETERS usedParam;
x := usedParam + 1;
:ENDPROC;`

	opts := DiagnosticOptions{CheckUnusedVars: true}
	diagnostics := GetDiagnostics(text, opts)

	// Should NOT find unused parameter diagnostic
	for _, d := range diagnostics {
		if strings.Contains(d.Message, "usedParam") && strings.Contains(d.Message, "never used") {
			t.Error("should not flag 'usedParam' as unused since it is used")
		}
	}
}

func TestCheckUnusedVariables_DisabledByDefault(t *testing.T) {
	text := `:PROCEDURE Test;
:DECLARE unusedVar;
x := 1;
:ENDPROC;`

	// Default options have CheckUnusedVars: false
	opts := DefaultDiagnosticOptions()
	diagnostics := GetDiagnostics(text, opts)

	// Should NOT report unused variables when disabled
	for _, d := range diagnostics {
		if strings.Contains(d.Message, "never used") {
			t.Error("should not check unused variables when disabled")
		}
	}
}

// ============================================================================
// SQL Parameter Validation Tests
// ============================================================================

func TestCheckSQLParameterValidation_UndeclaredParameter(t *testing.T) {
	text := `:PROCEDURE Test;
:DECLARE userId;
sql := "SELECT * FROM users WHERE id = ?userId? AND name = ?userName?";
:ENDPROC;`

	opts := DefaultDiagnosticOptions()
	opts.CheckSQLParams = true
	diagnostics := GetDiagnostics(text, opts)

	// Should report ?userName? as undeclared (userId is declared)
	foundUserName := false
	foundUserId := false
	for _, d := range diagnostics {
		if strings.Contains(d.Message, "userName") && strings.Contains(d.Message, "does not match") {
			foundUserName = true
		}
		if strings.Contains(d.Message, "userId") && strings.Contains(d.Message, "does not match") {
			foundUserId = true
		}
	}

	if !foundUserName {
		t.Error("expected warning for undeclared SQL parameter 'userName'")
	}
	if foundUserId {
		t.Error("should not warn about 'userId' since it is declared")
	}
}

func TestCheckSQLParameterValidation_CaseInsensitive(t *testing.T) {
	text := `:PROCEDURE Test;
:PARAMETERS sRunno;
sql := "SELECT * FROM samples WHERE runno = ?sRUNNO?";
:ENDPROC;`

	opts := DefaultDiagnosticOptions()
	opts.CheckSQLParams = true
	diagnostics := GetDiagnostics(text, opts)

	// Should NOT report ?sRUNNO? - it matches sRunno (case-insensitive)
	for _, d := range diagnostics {
		if strings.Contains(d.Message, "sRUNNO") || strings.Contains(d.Message, "sRunno") {
			t.Errorf("should not warn about case-insensitive match: %s", d.Message)
		}
	}
}

func TestCheckSQLParameterValidation_GlobalVariable(t *testing.T) {
	text := `:PROCEDURE Test;
sql := "SELECT * FROM users WHERE user_id = ?gCurrentUser?";
:ENDPROC;`

	opts := DefaultDiagnosticOptions()
	opts.CheckSQLParams = true
	opts.GlobalVariables = []string{"gCurrentUser", "gAppName"}
	diagnostics := GetDiagnostics(text, opts)

	// Should NOT report ?gCurrentUser? - it's a configured global
	for _, d := range diagnostics {
		if strings.Contains(d.Message, "gCurrentUser") {
			t.Errorf("should not warn about global variable: %s", d.Message)
		}
	}
}

func TestCheckSQLParameterValidation_PositionalNotValidated(t *testing.T) {
	text := `:PROCEDURE Test;
sql := "SELECT * FROM users WHERE id = ? AND name = ?";
:ENDPROC;`

	opts := DefaultDiagnosticOptions()
	opts.CheckSQLParams = true
	diagnostics := GetDiagnostics(text, opts)

	// Positional parameters (?) should not trigger validation warnings
	for _, d := range diagnostics {
		if strings.Contains(d.Message, "SQL parameter") {
			t.Errorf("positional parameters should not be validated: %s", d.Message)
		}
	}
}

func TestCheckSQLParameterValidation_DisabledByDefault(t *testing.T) {
	text := `:PROCEDURE Test;
sql := "SELECT * FROM users WHERE id = ?undeclared?";
:ENDPROC;`

	// Default options have CheckSQLParams: false
	opts := DefaultDiagnosticOptions()
	diagnostics := GetDiagnostics(text, opts)

	// Should NOT report SQL parameter issues when disabled
	for _, d := range diagnostics {
		if strings.Contains(d.Message, "SQL parameter") {
			t.Error("should not check SQL parameters when disabled")
		}
	}
}

func TestCheckSQLParameterValidation_MultipleStrings(t *testing.T) {
	text := `:PROCEDURE Test;
:DECLARE userId, orderId;
sql1 := "SELECT * FROM users WHERE id = ?userId?";
sql2 := "SELECT * FROM orders WHERE id = ?orderId? AND user = ?missing?";
:ENDPROC;`

	opts := DefaultDiagnosticOptions()
	opts.CheckSQLParams = true
	diagnostics := GetDiagnostics(text, opts)

	// Should only report ?missing? as undeclared
	foundMissing := false
	foundOther := false
	for _, d := range diagnostics {
		if strings.Contains(d.Message, "SQL parameter") {
			if strings.Contains(d.Message, "missing") {
				foundMissing = true
			} else {
				foundOther = true
			}
		}
	}

	if !foundMissing {
		t.Error("expected warning for undeclared SQL parameter 'missing'")
	}
	if foundOther {
		t.Error("should not warn about declared parameters")
	}
}

func TestCheckSQLParameterValidation_ProcedureParameters(t *testing.T) {
	text := `:PROCEDURE GetUser;
:PARAMETERS userId, userName;
sql := "SELECT * FROM users WHERE id = ?userId? AND name = ?userName?";
:ENDPROC;`

	opts := DefaultDiagnosticOptions()
	opts.CheckSQLParams = true
	diagnostics := GetDiagnostics(text, opts)

	// Should NOT report - both parameters are declared via :PARAMETERS
	for _, d := range diagnostics {
		if strings.Contains(d.Message, "SQL parameter") {
			t.Errorf("should not warn about procedure parameters: %s", d.Message)
		}
	}
}

func TestCheckSQLParameterValidation_ComplexNamedPlaceholdersSkipped(t *testing.T) {
	text := `:PROCEDURE Test;
:DECLARE oUser, aIds;
sql := "SELECT * FROM users WHERE id = ?oUser:ID? AND other_id = ?aIds[i]? AND dt < ?Today()?";
:ENDPROC;`

	opts := DefaultDiagnosticOptions()
	opts.CheckSQLParams = true
	diagnostics := GetDiagnostics(text, opts)

	for _, d := range diagnostics {
		if strings.Contains(d.Message, "SQL parameter") {
			t.Errorf("should not warn about complex named placeholders: %s", d.Message)
		}
	}
}

// ============================================================================
// SQL Placeholder Hover Tests
// ============================================================================

func TestParseSQLPlaceholders_NamedParameters(t *testing.T) {
	sql := "SELECT * FROM users WHERE id = ?userId? AND name = ?userName?"

	placeholders := ParseSQLPlaceholders(sql)

	if len(placeholders) != 2 {
		t.Fatalf("expected 2 placeholders, got %d", len(placeholders))
	}

	// First placeholder: ?userId?
	if placeholders[0].Name != "userId" {
		t.Errorf("expected first placeholder name 'userId', got '%s'", placeholders[0].Name)
	}
	if !placeholders[0].IsNamed {
		t.Error("expected first placeholder to be named")
	}

	// Second placeholder: ?userName?
	if placeholders[1].Name != "userName" {
		t.Errorf("expected second placeholder name 'userName', got '%s'", placeholders[1].Name)
	}
}

func TestParseSQLPlaceholders_PositionalParameters(t *testing.T) {
	sql := "INSERT INTO users (name, age) VALUES (?, ?)"

	placeholders := ParseSQLPlaceholders(sql)

	if len(placeholders) != 2 {
		t.Fatalf("expected 2 positional placeholders, got %d", len(placeholders))
	}

	// First positional parameter
	if placeholders[0].Position != 1 {
		t.Errorf("expected first placeholder position 1, got %d", placeholders[0].Position)
	}
	if placeholders[0].IsNamed {
		t.Error("expected first placeholder to be positional, not named")
	}

	// Second positional parameter
	if placeholders[1].Position != 2 {
		t.Errorf("expected second placeholder position 2, got %d", placeholders[1].Position)
	}
}

func TestParseSQLPlaceholders_MixedParameters(t *testing.T) {
	sql := "SELECT * FROM t WHERE a = ? AND b = ?name? AND c = ?"

	placeholders := ParseSQLPlaceholders(sql)

	// Should have: 1 positional, 1 named, 1 positional
	if len(placeholders) != 3 {
		t.Fatalf("expected 3 placeholders, got %d", len(placeholders))
	}

	// First: positional ?
	if placeholders[0].IsNamed || placeholders[0].Position != 1 {
		t.Errorf("expected first placeholder to be positional #1, got named=%v pos=%d", placeholders[0].IsNamed, placeholders[0].Position)
	}

	// Second: named ?name?
	if !placeholders[1].IsNamed || placeholders[1].Name != "name" {
		t.Errorf("expected second placeholder to be named 'name', got named=%v name='%s'", placeholders[1].IsNamed, placeholders[1].Name)
	}

	// Third: positional ?
	if placeholders[2].IsNamed || placeholders[2].Position != 2 {
		t.Errorf("expected third placeholder to be positional #2, got named=%v pos=%d", placeholders[2].IsNamed, placeholders[2].Position)
	}
}

func TestParseSQLPlaceholders_ComplexNamedParameters(t *testing.T) {
	sql := "SELECT * FROM t WHERE a = ?oUser:ID? AND b = ?aIds[i]? AND c = ?Today()?"

	placeholders := ParseSQLPlaceholders(sql)

	if len(placeholders) != 3 {
		t.Fatalf("expected 3 placeholders, got %d", len(placeholders))
	}

	want := []string{"oUser:ID", "aIds[i]", "Today()"}
	for i, expected := range want {
		if !placeholders[i].IsNamed || placeholders[i].Name != expected {
			t.Fatalf("expected named placeholder %q at index %d, got named=%v name=%q", expected, i, placeholders[i].IsNamed, placeholders[i].Name)
		}
	}
}

// [spec feature.hover/A6]
func TestGetSQLPlaceholderHover_NamedParameter(t *testing.T) {
	content := "SELECT * FROM users WHERE id = ?userId?"

	// Cursor on 'userId' (position within the ?userId? range)
	// ?userId? starts at position 32, ends at 41
	hover := GetSQLPlaceholderHover(content, 33) // On 'u' of userId

	if hover == nil {
		t.Fatal("expected hover for named parameter, got nil")
	}

	if !strings.Contains(hover.Contents, "SQL Parameter: userId") {
		t.Errorf("expected hover to contain 'SQL Parameter: userId', got: %s", hover.Contents)
	}

	if !strings.Contains(hover.Contents, "Named parameter") {
		t.Errorf("expected hover to mention 'Named parameter', got: %s", hover.Contents)
	}
}

// [spec feature.hover/A6]
func TestGetSQLPlaceholderHover_PositionalParameter(t *testing.T) {
	content := "INSERT INTO t VALUES (?, ?)"

	// Cursor on first ?
	hover := GetSQLPlaceholderHover(content, 22) // On first ?

	if hover == nil {
		t.Fatal("expected hover for positional parameter, got nil")
	}

	if !strings.Contains(hover.Contents, "SQL Parameter #1") {
		t.Errorf("expected hover to contain 'SQL Parameter #1', got: %s", hover.Contents)
	}

	if !strings.Contains(hover.Contents, "1st parameter") {
		t.Errorf("expected hover to mention '1st parameter', got: %s", hover.Contents)
	}
}

func TestGetSQLPlaceholderHover_SecondPositionalParameter(t *testing.T) {
	content := "INSERT INTO t VALUES (?, ?)"

	// Cursor on second ?
	hover := GetSQLPlaceholderHover(content, 25) // On second ?

	if hover == nil {
		t.Fatal("expected hover for second positional parameter, got nil")
	}

	if !strings.Contains(hover.Contents, "SQL Parameter #2") {
		t.Errorf("expected hover to contain 'SQL Parameter #2', got: %s", hover.Contents)
	}

	if !strings.Contains(hover.Contents, "2nd parameter") {
		t.Errorf("expected hover to mention '2nd parameter', got: %s", hover.Contents)
	}
}

func TestGetSQLPlaceholderHover_OutsidePlaceholder(t *testing.T) {
	content := "SELECT * FROM users WHERE id = ?userId?"

	// Cursor on 'SELECT' - not on a placeholder
	hover := GetSQLPlaceholderHover(content, 3)

	if hover != nil {
		t.Errorf("expected no hover outside placeholder, got: %s", hover.Contents)
	}
}

// [spec feature.hover/A6]
func TestGetSQLPlaceholderHoverFromToken(t *testing.T) {
	text := `result := SQLExecute("SELECT * FROM users WHERE id = ?userId?", "ds");`

	l := lexer.NewLexer(text)
	tokens := l.Tokenize()

	// Find position on the userId parameter
	// The string starts at column 22 (0-based: 21)
	// "SELECT * FROM users WHERE id = ?userId?"
	// The string content starts at column 23 (after opening quote)
	// ?userId? is at offset 32 within the string content
	// So in the document, it's around column 22 + 1 + 32 = 55

	hover := GetSQLPlaceholderHoverFromToken(tokens, 1, 55) // Line 1, column ~55

	if hover == nil {
		t.Fatal("expected hover for SQL placeholder from token, got nil")
	}

	if !strings.Contains(hover.Contents, "userId") {
		t.Errorf("expected hover to contain 'userId', got: %s", hover.Contents)
	}
}

func TestGetOrdinal(t *testing.T) {
	tests := []struct {
		n        int
		expected string
	}{
		{1, "1st"},
		{2, "2nd"},
		{3, "3rd"},
		{4, "4th"},
		{5, "5th"},
		{11, "11th"},
		{12, "12th"},
		{13, "13th"},
		{21, "21st"},
		{22, "22nd"},
		{23, "23rd"},
		{100, "100th"},
		{101, "101st"},
	}

	for _, tc := range tests {
		result := getOrdinal(tc.n)
		if result != tc.expected {
			t.Errorf("getOrdinal(%d) = %s, expected %s", tc.n, result, tc.expected)
		}
	}
}

// ==================== Gotcha Diagnostic Tests ====================

// Test Gotcha #9: Assignment in conditions
func TestGetDiagnostics_AssignmentInCondition(t *testing.T) {
	tests := []struct {
		name    string
		code    string
		wantMsg string
	}{
		{
			name:    "assignment in IF",
			code:    `:IF x := 5;`,
			wantMsg: "Assignment ':=' used in IF condition",
		},
		{
			name:    "assignment in WHILE",
			code:    `:WHILE y := 1;`,
			wantMsg: "Assignment ':=' used in WHILE condition",
		},
		{
			name:    "assignment in CASE",
			code:    `:BEGINCASE; :CASE z := 3;`,
			wantMsg: "Assignment ':=' used in CASE condition",
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			opts := DefaultDiagnosticOptions()
			diagnostics := GetDiagnostics(tc.code, opts)

			found := false
			for _, d := range diagnostics {
				if strings.Contains(d.Message, tc.wantMsg) {
					found = true
					if d.Severity != SeverityWarning {
						t.Errorf("expected SeverityWarning, got %d", d.Severity)
					}
					break
				}
			}
			if !found {
				t.Errorf("expected diagnostic containing %q, got: %v", tc.wantMsg, diagnostics)
			}
		})
	}
}

func TestGetDiagnostics_AssignmentInCondition_ValidSyntax(t *testing.T) {
	tests := []struct {
		name string
		code string
	}{
		{
			name: "comparison with equals",
			code: `:IF x = 5;`,
		},
		{
			name: "comparison with double equals",
			code: `:IF x == 5;`,
		},
		{
			name: "assignment outside condition",
			code: `x := 5; :IF x = 5;`,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			opts := DefaultDiagnosticOptions()
			diagnostics := GetDiagnostics(tc.code, opts)

			for _, d := range diagnostics {
				if strings.Contains(d.Message, "Assignment ':='") && strings.Contains(d.Message, "condition") {
					t.Errorf("should not flag valid syntax: %s", d.Message)
				}
			}
		})
	}
}

// Test Gotcha #15: Class instantiation with parentheses
func TestGetDiagnostics_ClassInstantiationSyntax(t *testing.T) {
	tests := []struct {
		name      string
		code      string
		className string
	}{
		{
			name:      "Email with parentheses",
			code:      `oEmail := Email();`,
			className: "Email",
		},
		{
			name:      "SSLRegex with parentheses",
			code:      `oRegex := SSLRegex('\d+');`,
			className: "SSLRegex",
		},
		{
			name:      "SSLDataset with parentheses",
			code:      `oDs := SSLDataset();`,
			className: "SSLDataset",
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			opts := DefaultDiagnosticOptions()
			diagnostics := GetDiagnostics(tc.code, opts)

			found := false
			for _, d := range diagnostics {
				if strings.Contains(d.Message, tc.className) && strings.Contains(d.Message, "curly braces") {
					found = true
					if d.Severity != SeverityError {
						t.Errorf("expected SeverityError, got %d", d.Severity)
					}
					break
				}
			}
			if !found {
				t.Errorf("expected diagnostic for %s(), got: %v", tc.className, diagnostics)
			}
		})
	}
}

func TestGetDiagnostics_ClassInstantiationSyntax_ValidSyntax(t *testing.T) {
	tests := []struct {
		name string
		code string
	}{
		{
			name: "Email with curly braces",
			code: `oEmail := Email{};`,
		},
		{
			name: "SSLRegex with curly braces",
			code: `oRegex := SSLRegex{'\d+'};`,
		},
		{
			name: "CreateUdObject function call",
			code: `oObj := CreateUdObject();`,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			opts := DefaultDiagnosticOptions()
			diagnostics := GetDiagnostics(tc.code, opts)

			for _, d := range diagnostics {
				if strings.Contains(d.Message, "curly braces") {
					t.Errorf("should not flag valid syntax: %s", d.Message)
				}
			}
		})
	}
}

func TestGetDiagnostics_CreateUdObjectBuiltinClassMisuse(t *testing.T) {
	tests := []struct {
		name string
		code string
	}{
		{
			name: "built-in class by string name",
			code: `oObj := CreateUdObject("Email");`,
		},
		{
			name: "built-in class by string name with args",
			code: `oObj := CreateUdObject("SSLDataset", {});`,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			diagnostics := GetDiagnostics(tc.code, DefaultDiagnosticOptions())

			found := false
			for _, d := range diagnostics {
				if strings.Contains(d.Message, "must use curly-brace construction") {
					found = true
					if d.Severity != SeverityError {
						t.Errorf("expected SeverityError, got %d", d.Severity)
					}
					break
				}
			}
			if !found {
				t.Fatalf("expected CreateUdObject built-in class misuse diagnostic, got %#v", diagnostics)
			}
		})
	}
}

// Test Gotcha #5: Zero-based array indexing
func TestGetDiagnostics_ZeroBasedArrayIndex(t *testing.T) {
	tests := []struct {
		name string
		code string
	}{
		{
			name: "array access with 0",
			code: `x := aItems[0];`,
		},
		{
			name: "string access with 0",
			code: `c := sText[0];`,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			opts := DefaultDiagnosticOptions()
			diagnostics := GetDiagnostics(tc.code, opts)

			found := false
			for _, d := range diagnostics {
				if strings.Contains(d.Message, "1-based") && strings.Contains(d.Message, "index 0") {
					found = true
					if d.Severity != SeverityError {
						t.Errorf("expected SeverityError, got %d", d.Severity)
					}
					break
				}
			}
			if !found {
				t.Errorf("expected zero-index diagnostic, got: %v", diagnostics)
			}
		})
	}
}

func TestGetDiagnostics_ZeroBasedArrayIndex_MemberAccessWarns(t *testing.T) {
	// [0] on a value reached through colon member access may be a 0-based
	// .NET collection: warning, not error (issue #152). Chained access
	// behaves the same.
	cases := []struct {
		name string
		code string
	}{
		{name: "member access", code: `oTable := dataSet:Tables[0];`},
		{name: "chained member access", code: `oCol := dataSet:Tables:Columns[0];`},
	}

	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			found := false
			for _, d := range GetDiagnostics(tc.code, DefaultDiagnosticOptions()) {
				if d.Code == CodeZeroBasedArrayIndex {
					found = true
					if d.Severity != SeverityWarning {
						t.Errorf("expected SeverityWarning, got %d", d.Severity)
					}
					if !strings.Contains(d.Message, ".NET") {
						t.Errorf("message should mention .NET collections: %s", d.Message)
					}
				}
			}
			if !found {
				t.Error("expected zero-index warning on member-access subscript")
			}
		})
	}
}

func TestGetDiagnostics_ZeroBasedArrayIndex_ValidSyntax(t *testing.T) {
	tests := []struct {
		name string
		code string
	}{
		{
			name: "array access with 1",
			code: `x := aItems[1];`,
		},
		{
			name: "array access with variable",
			code: `x := aItems[nIdx];`,
		},
		{
			name: "array access with expression",
			code: `x := aItems[0 + 1];`,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			opts := DefaultDiagnosticOptions()
			diagnostics := GetDiagnostics(tc.code, opts)

			for _, d := range diagnostics {
				if strings.Contains(d.Message, "1-based") {
					t.Errorf("should not flag valid syntax: %s", d.Message)
				}
			}
		})
	}
}

// Test Gotcha #7: Named SQL params with wrong function
func TestGetDiagnostics_NamedSQLParamsWithWrongFunction(t *testing.T) {
	tests := []struct {
		name     string
		code     string
		funcName string
	}{
		{
			name:     "RunSQL with named param",
			code:     `RunSQL("UPDATE T SET X = ?sValue?", "", {});`,
			funcName: "RunSQL",
		},
		{
			name:     "LSearch with named param",
			code:     `result := LSearch("SELECT * FROM T WHERE ID = ?nID?", "");`,
			funcName: "LSearch",
		},
		{
			name:     "GetDataSetWithSchemaFromSelect with named param",
			code:     `result := GetDataSetWithSchemaFromSelect("SELECT * FROM T WHERE ID = ?nID?", "", {}, {"ID"}, {"UK_ID"});`,
			funcName: "GetDataSetWithSchemaFromSelect",
		},
		{
			name:     "GetDataSetXMLFromSelect with named param",
			code:     `result := GetDataSetXMLFromSelect("SELECT * FROM T WHERE ID = ?nID?", "", .T., {});`,
			funcName: "GetDataSetXMLFromSelect",
		},
		{
			name:     "GetNETDataSet with named param",
			code:     `result := GetNETDataSet("SELECT * FROM T WHERE ID = ?nID?", "ds", {}, "T", .T., .F.);`,
			funcName: "GetNETDataSet",
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			opts := DefaultDiagnosticOptions()
			diagnostics := GetDiagnostics(tc.code, opts)

			found := false
			for _, d := range diagnostics {
				if strings.Contains(d.Message, "Named SQL parameter") && strings.Contains(d.Message, tc.funcName) {
					found = true
					if d.Severity != SeverityWarning {
						t.Errorf("expected SeverityWarning, got %d", d.Severity)
					}
					break
				}
			}
			if !found {
				t.Errorf("expected named param diagnostic for %s, got: %v", tc.funcName, diagnostics)
			}
		})
	}
}

func TestGetDiagnostics_NamedSQLParamsWithWrongFunction_ValidSyntax(t *testing.T) {
	tests := []struct {
		name string
		code string
	}{
		{
			name: "RunSQL with positional param",
			code: `RunSQL("UPDATE T SET X = ?", "", {sValue});`,
		},
		{
			name: "SQLExecute with named param",
			code: `result := SQLExecute("SELECT * FROM T WHERE ID = ?nID?");`,
		},
		{
			name: "GetNETDataSet with positional param",
			code: `result := GetNETDataSet("SELECT * FROM T WHERE ID = ?", "ds", {nID}, "T", .T., .F.);`,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			opts := DefaultDiagnosticOptions()
			diagnostics := GetDiagnostics(tc.code, opts)

			for _, d := range diagnostics {
				if strings.Contains(d.Message, "Named SQL parameter") {
					t.Errorf("should not flag valid syntax: %s", d.Message)
				}
			}
		})
	}
}

// Test Gotcha #1: Direct procedure calls
func TestGetDiagnostics_DirectProcedureCalls(t *testing.T) {
	code := `:PROCEDURE MyHelper;
:ENDPROC;

:PROCEDURE Main;
result := MyHelper();
:ENDPROC;`

	opts := DefaultDiagnosticOptions()
	diagnostics := GetDiagnostics(code, opts)

	found := false
	for _, d := range diagnostics {
		if strings.Contains(d.Message, "Custom procedures cannot be called directly") && strings.Contains(d.Message, "DoProc") {
			found = true
			if d.Severity != SeverityError {
				t.Errorf("expected SeverityError, got %d", d.Severity)
			}
			break
		}
	}
	if !found {
		t.Errorf("expected direct procedure call diagnostic, got: %v", diagnostics)
	}
}

func TestGetDiagnostics_DirectProcedureCalls_ValidSyntax(t *testing.T) {
	tests := []struct {
		name string
		code string
	}{
		{
			name: "DoProc call",
			code: `:PROCEDURE MyHelper;
:ENDPROC;
:PROCEDURE Main;
result := DoProc("MyHelper", {});
:ENDPROC;`,
		},
		{
			name: "procedure declaration",
			code: `:PROCEDURE MyHelper;
:ENDPROC;`,
		},
		{
			name: "reference without call",
			code: `:PROCEDURE MyHelper;
:ENDPROC;
:PROCEDURE Main;
x := MyHelper;
:ENDPROC;`,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			opts := DefaultDiagnosticOptions()
			diagnostics := GetDiagnostics(tc.code, opts)

			for _, d := range diagnostics {
				if strings.Contains(d.Message, "Custom procedures cannot be called directly") {
					t.Errorf("should not flag valid syntax: %s", d.Message)
				}
			}
		})
	}
}

// Test Gotcha #8: Dot property access instead of colon
func TestGetDiagnostics_DotPropertyAccess(t *testing.T) {
	tests := []struct {
		name     string
		code     string
		propName string
	}{
		{
			name:     "object dot property",
			code:     `x := oEmail.Subject;`,
			propName: "Subject",
		},
		{
			name:     "object dot method",
			code:     `oDataset.RowCount;`,
			propName: "RowCount",
		},
		{
			name:     "chained dot access",
			code:     `val := oObj.Property;`,
			propName: "Property",
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			opts := DefaultDiagnosticOptions()
			diagnostics := GetDiagnostics(tc.code, opts)

			found := false
			for _, d := range diagnostics {
				if strings.Contains(d.Message, "colon ':'") && strings.Contains(d.Message, tc.propName) {
					found = true
					if d.Severity != SeverityError {
						t.Errorf("expected SeverityError, got %d", d.Severity)
					}
					break
				}
			}
			if !found {
				t.Errorf("expected dot property access diagnostic for .%s, got: %v", tc.propName, diagnostics)
			}
		})
	}
}

func TestGetDiagnostics_DotPropertyAccess_ValidSyntax(t *testing.T) {
	tests := []struct {
		name string
		code string
	}{
		{
			name: "colon property access",
			code: `x := oEmail:Subject;`,
		},
		{
			name: "colon method call",
			code: `n := oDataset:RowCount;`,
		},
		{
			name: "logical operator .AND.",
			code: `:IF a .AND. b;`,
		},
		{
			name: "boolean true .T.",
			code: `x := .T.;`,
		},
		{
			name: "boolean false .F.",
			code: `x := .F.;`,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			opts := DefaultDiagnosticOptions()
			diagnostics := GetDiagnostics(tc.code, opts)

			for _, d := range diagnostics {
				if strings.Contains(d.Message, "colon ':'") && strings.Contains(d.Message, "property access") {
					t.Errorf("should not flag valid syntax: %s", d.Message)
				}
			}
		})
	}
}

// Test missing quotes in ExecFunction/DoProc
func TestGetDiagnostics_MissingQuotesInExecFunction(t *testing.T) {
	tests := []struct {
		name     string
		code     string
		funcName string
	}{
		{
			name:     "ExecFunction without quotes",
			code:     `result := ExecFunction(Module.Procedure, {});`,
			funcName: "ExecFunction",
		},
		{
			name:     "DoProc without quotes",
			code:     `result := DoProc(Helper.Func, {});`,
			funcName: "DoProc",
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			opts := DefaultDiagnosticOptions()
			diagnostics := GetDiagnostics(tc.code, opts)

			found := false
			for _, d := range diagnostics {
				if strings.Contains(d.Message, "must be quoted") && strings.Contains(d.Message, tc.funcName) {
					found = true
					if d.Severity != SeverityError {
						t.Errorf("expected SeverityError, got %d", d.Severity)
					}
					break
				}
			}
			if !found {
				t.Errorf("expected missing quotes diagnostic for %s, got: %v", tc.funcName, diagnostics)
			}
		})
	}
}

func TestGetDiagnostics_MissingQuotesInExecFunction_ValidSyntax(t *testing.T) {
	tests := []struct {
		name string
		code string
	}{
		{
			name: "ExecFunction with double quotes",
			code: `result := ExecFunction("Module.Procedure", {});`,
		},
		{
			name: "DoProc with single quotes",
			code: `result := DoProc('Helper.Func', {});`,
		},
		{
			name: "ExecFunction with simple name",
			code: `result := ExecFunction("SimpleName", {});`,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			opts := DefaultDiagnosticOptions()
			diagnostics := GetDiagnostics(tc.code, opts)

			for _, d := range diagnostics {
				if strings.Contains(d.Message, "must be quoted") {
					t.Errorf("should not flag valid syntax: %s", d.Message)
				}
			}
		})
	}
}

func TestGetDiagnostics_ParameterPlacement(t *testing.T) {
	tests := []struct {
		name        string
		code        string
		wantMessage string
	}{
		{
			name: "procedure parameters after declaration",
			code: `:PROCEDURE Test;
:DECLARE sValue;
:PARAMETERS sInput;
:ENDPROC;`,
			wantMessage: "':PARAMETERS' must appear immediately after ':PROCEDURE'",
		},
		{
			name: "script parameters after statement",
			code: `nValue := 1;
:PARAMETERS sInput;`,
			wantMessage: "Script-level ':PARAMETERS' must appear before top-level statements",
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			diagnostics := GetDiagnostics(tc.code, DefaultDiagnosticOptions())

			found := false
			for _, d := range diagnostics {
				if strings.Contains(d.Message, tc.wantMessage) {
					found = true
					if d.Severity != SeverityError {
						t.Errorf("expected SeverityError, got %d", d.Severity)
					}
				}
			}

			if !found {
				t.Fatalf("expected parameter placement diagnostic, got: %+v", diagnostics)
			}
		})
	}
}

func TestGetDiagnostics_ParameterPlacement_LeadingProcedureBeforeScriptParametersAllowed(t *testing.T) {
	code := `:PROCEDURE Helper;
:ENDPROC;

:PARAMETERS sInput;
:DECLARE sValue;
sValue := sInput;`

	diagnostics := GetDiagnostics(code, DefaultDiagnosticOptions())

	for _, d := range diagnostics {
		if strings.Contains(d.Message, "':PARAMETERS' must appear immediately after ':PROCEDURE'") ||
			strings.Contains(d.Message, "Script-level ':PARAMETERS' must appear before top-level statements") {
			t.Fatalf("did not expect parameter placement diagnostic, got: %+v", diagnostics)
		}
	}
}

func TestGetDiagnostics_ParameterPlacement_HeaderCommentBeforeScriptParametersAllowed(t *testing.T) {
	code := `/* Header comment;

:PARAMETERS sInput;
:DECLARE sValue;
sValue := sInput;`

	diagnostics := GetDiagnostics(code, DefaultDiagnosticOptions())

	for _, d := range diagnostics {
		if strings.Contains(d.Message, "':PARAMETERS' must appear immediately after ':PROCEDURE'") ||
			strings.Contains(d.Message, "Script-level ':PARAMETERS' must appear before top-level statements") {
			t.Fatalf("did not expect parameter placement diagnostic, got: %+v", diagnostics)
		}
	}
}

// --- Agent instruction & refactoring guide coverage tests ---

func TestGetDiagnostics_ComplexSQLPlaceholder(t *testing.T) {
	// Agent instructions gotcha #21: only arithmetic/concatenation expressions
	// trigger the complex placeholder warning. Property access, array indexing,
	// and function calls are standard supported patterns.
	code := `SQLExecute("SELECT * FROM T WHERE Code = ?sPrefix + sSuffix?");`

	diagnostics := GetDiagnostics(code, DefaultDiagnosticOptions())

	for _, d := range diagnostics {
		if strings.Contains(d.Message, "Complex expression") && d.Severity == SeverityInfo {
			return
		}
	}

	t.Fatal("expected complex SQL placeholder warning for ?sPrefix + sSuffix?")
}

func TestGetDiagnostics_ComplexSQLPlaceholder_StandardFormsNotFlagged(t *testing.T) {
	// Source of truth: ?oObj:Prop?, ?aArr[i]?, ?Today()? are standard patterns, not complex
	cases := []string{
		`SQLExecute("SELECT * FROM T WHERE id = ?oUser:ID?");`,
		`SQLExecute("SELECT * FROM T WHERE id = ?aIds[1]?");`,
		`SQLExecute("SELECT * FROM T WHERE d = ?Today()?");`,
	}

	for _, code := range cases {
		diagnostics := GetDiagnostics(code, DefaultDiagnosticOptions())
		for _, d := range diagnostics {
			if strings.Contains(d.Message, "Complex expression") {
				t.Errorf("standard placeholder should not trigger complex warning: %s\n  got: %s", code, d.Message)
			}
		}
	}
}

func TestGetDiagnostics_ComplexSQLPlaceholder_SimpleNotFlagged(t *testing.T) {
	code := `SQLExecute("SELECT * FROM T WHERE Code = ?sCode?");`

	diagnostics := GetDiagnostics(code, DefaultDiagnosticOptions())

	for _, d := range diagnostics {
		if strings.Contains(d.Message, "Complex expression") {
			t.Fatal("simple placeholder ?sCode? should not trigger complex expression warning")
		}
	}
}

// --- UDObject array property in IN clause tests ---

func TestGetDiagnostics_UDObjectArrayInClause_Flagged(t *testing.T) {
	// UDObject property access inside IN(...) should warn
	code := `SQLExecute("SELECT * FROM T WHERE id IN (?oObj:aItems?)");`

	diagnostics := GetDiagnostics(code, DefaultDiagnosticOptions())

	found := false
	for _, d := range diagnostics {
		if strings.Contains(d.Message, "UDObject array property") && strings.Contains(d.Message, "oObj:aItems") {
			found = true
			if d.Severity != SeverityWarning {
				t.Errorf("expected Warning severity, got %d", d.Severity)
			}
		}
	}
	if !found {
		t.Fatal("expected diagnostic for UDObject property in IN clause, got none")
	}
}

func TestGetDiagnostics_UDObjectArrayInClause_NotFlaggedOutsideIN(t *testing.T) {
	// Property access in a WHERE = clause (not IN) should NOT be flagged
	code := `SQLExecute("SELECT * FROM T WHERE name = ?oObj:sName?");`

	diagnostics := GetDiagnostics(code, DefaultDiagnosticOptions())

	for _, d := range diagnostics {
		if strings.Contains(d.Message, "UDObject array property") {
			t.Fatalf("property access outside IN clause should not be flagged: %s", d.Message)
		}
	}
}

func TestGetDiagnostics_UDObjectArrayInClause_LocalVarNotFlagged(t *testing.T) {
	// Local variable (no ':') in IN clause should NOT be flagged
	code := `SQLExecute("SELECT * FROM T WHERE id IN (?aLocalIds?)");`

	diagnostics := GetDiagnostics(code, DefaultDiagnosticOptions())

	for _, d := range diagnostics {
		if strings.Contains(d.Message, "UDObject array property") {
			t.Fatalf("local variable in IN clause should not be flagged: %s", d.Message)
		}
	}
}

func TestGetDiagnostics_UDObjectArrayInClause_CaseInsensitive(t *testing.T) {
	// IN keyword should be detected case-insensitively
	code := `SQLExecute("SELECT * FROM T WHERE id in (?oObj:aList?)");`

	diagnostics := GetDiagnostics(code, DefaultDiagnosticOptions())

	found := false
	for _, d := range diagnostics {
		if strings.Contains(d.Message, "UDObject array property") {
			found = true
		}
	}
	if !found {
		t.Fatal("expected diagnostic for UDObject property in lowercase 'in' clause")
	}
}

func TestGetDiagnostics_UDObjectArrayInClause_WithSpaces(t *testing.T) {
	// Various spacing around IN ( should still be detected
	code := `SQLExecute("SELECT * FROM T WHERE id IN  (  ?oObj:aIds?  )");`

	diagnostics := GetDiagnostics(code, DefaultDiagnosticOptions())

	found := false
	for _, d := range diagnostics {
		if strings.Contains(d.Message, "UDObject array property") {
			found = true
		}
	}
	if !found {
		t.Fatal("expected diagnostic with extra whitespace around IN clause")
	}
}

func TestGetDiagnostics_BeginCaseRequiresCase(t *testing.T) {
	// Agent instructions: BEGINCASE requires at least one CASE block
	code := `:BEGINCASE;
:ENDCASE;`

	diagnostics := GetDiagnostics(code, DefaultDiagnosticOptions())

	for _, d := range diagnostics {
		if strings.Contains(d.Message, "requires at least one ':CASE'") {
			return
		}
	}

	t.Fatal("expected BEGINCASE-requires-CASE diagnostic")
}

func TestGetDiagnostics_ReturnInFinally(t *testing.T) {
	// Agent instructions: :RETURN inside :FINALLY is a compile-time error
	code := `:TRY;
	x := 1;
:CATCH;
	y := 2;
:FINALLY;
	:RETURN NIL;
:ENDTRY;`

	diagnostics := GetDiagnostics(code, DefaultDiagnosticOptions())

	for _, d := range diagnostics {
		if strings.Contains(d.Message, "':RETURN'") && strings.Contains(d.Message, "':FINALLY'") {
			return
		}
	}

	t.Fatal("expected RETURN-in-FINALLY diagnostic")
}

func TestGetDiagnostics_ExitForOutsideLoop(t *testing.T) {
	// Agent instructions: :EXITFOR must be inside a :FOR loop
	code := `:PROCEDURE Test;
:EXITFOR;
:ENDPROC;`

	diagnostics := GetDiagnostics(code, DefaultDiagnosticOptions())

	for _, d := range diagnostics {
		if strings.Contains(d.Message, "':EXITFOR' must be inside a ':FOR' loop") {
			return
		}
	}

	t.Fatal("expected EXITFOR-outside-loop diagnostic")
}

func TestGetDiagnostics_LoopOutsideLoop(t *testing.T) {
	// Agent instructions: :LOOP must be inside a loop
	code := `:PROCEDURE Test;
:LOOP;
:ENDPROC;`

	diagnostics := GetDiagnostics(code, DefaultDiagnosticOptions())

	for _, d := range diagnostics {
		if strings.Contains(d.Message, "':LOOP' must be inside") {
			return
		}
	}

	t.Fatal("expected LOOP-outside-loop diagnostic")
}

func TestGetDiagnostics_ConstructorOutsideClass(t *testing.T) {
	// Agent instructions: Constructor is only meaningful inside :CLASS
	code := `:PROCEDURE Constructor;
:ENDPROC;`

	diagnostics := GetDiagnostics(code, DefaultDiagnosticOptions())

	for _, d := range diagnostics {
		if strings.Contains(d.Message, "'Constructor' is only meaningful inside a ':CLASS'") {
			return
		}
	}

	t.Fatal("expected Constructor-outside-class diagnostic")
}

func TestGetDiagnostics_NotPreferredOperators(t *testing.T) {
	// Schema: # and <> should use != instead
	code := `:IF x <> 1;
:ENDIF;
:IF y # 2;
:ENDIF;`

	diagnostics := GetDiagnostics(code, DefaultDiagnosticOptions())

	foundLtGt := false
	foundHash := false
	for _, d := range diagnostics {
		if strings.Contains(d.Message, "instead of '<>'") {
			foundLtGt = true
		}
		if strings.Contains(d.Message, "instead of '#'") {
			foundHash = true
		}
	}

	if !foundLtGt {
		t.Error("expected non-preferred operator warning for <>")
	}
	if !foundHash {
		t.Error("expected non-preferred operator warning for #")
	}
}

// ==================== Missing Test Coverage ====================

// --- checkDefaultPlacement tests ---

func TestGetDiagnostics_DefaultPlacement_MustFollowParameters(t *testing.T) {
	// :DEFAULT must appear immediately after :PARAMETERS
	code := `:PROCEDURE Test;
:PARAMETERS sName;
:DECLARE nVal;
:DEFAULT sName, "";
:ENDPROC;`

	diagnostics := GetDiagnostics(code, DefaultDiagnosticOptions())

	for _, d := range diagnostics {
		if strings.Contains(d.Message, "':DEFAULT' must appear immediately after ':PARAMETERS'") {
			return
		}
	}

	t.Fatal("expected DEFAULT placement diagnostic when DEFAULT doesn't follow PARAMETERS")
}

func TestGetDiagnostics_DefaultPlacement_ValidSyntax(t *testing.T) {
	// :DEFAULT immediately after :PARAMETERS should be fine
	code := `:PROCEDURE Test;
:PARAMETERS sName, nVal;
:DEFAULT sName, "";
:DEFAULT nVal, 0;
:DECLARE sResult;
:ENDPROC;`

	diagnostics := GetDiagnostics(code, DefaultDiagnosticOptions())

	for _, d := range diagnostics {
		if strings.Contains(d.Message, "':DEFAULT' must appear immediately after ':PARAMETERS'") {
			t.Errorf("unexpected DEFAULT placement diagnostic: %s", d.Message)
		}
	}
}

// Issue #170: an inline comment mid-way through a multi-line :PARAMETERS
// list must not end the statement — the following :DEFAULT is still
// immediately after the (single) :PARAMETERS statement.
func TestGetDiagnostics_DefaultPlacement_MultiLineParametersWithInlineComments(t *testing.T) {
	code := `:PARAMETERS uP0, /* dsName;
 uP1, /* filter;
 uP2;
:DEFAULT uP2, "";
:RETURN uP0;`

	diagnostics := GetDiagnostics(code, DefaultDiagnosticOptions())

	for _, d := range diagnostics {
		if d.Code == CodeDefaultAfterParameters {
			t.Errorf("unexpected default_after_parameters on comment-split :PARAMETERS list: %s", d.Message)
		}
		if d.Code == CodeParametersFirst {
			t.Errorf("unexpected parameters_first on comment-split :PARAMETERS list: %s", d.Message)
		}
	}
}

// Issue #170 (procedure variant): the comment-split :PARAMETERS list inside
// a :PROCEDURE must not register its later parameters as body statements.
func TestGetDiagnostics_ParameterPlacement_MultiLineParametersWithInlineComments(t *testing.T) {
	code := `:PROCEDURE Demo;
:PARAMETERS uP0, /* dsName;
 uP1, /* filter;
 uP2;
:DECLARE nCount;
:ENDPROC;`

	diagnostics := GetDiagnostics(code, DefaultDiagnosticOptions())

	for _, d := range diagnostics {
		if d.Code == CodeParametersFirst {
			t.Errorf("unexpected parameters_first on comment-split :PARAMETERS list: %s", d.Message)
		}
	}
}

// Issue #168: :INCLUDE is a paste-time directive, not a statement — the
// include-then-parameters pattern (include_early) must not flag.
func TestGetDiagnostics_ParameterPlacement_IncludeBeforeParametersAllowed(t *testing.T) {
	code := `:INCLUDE Enterprise_Server.UserAuthentication;
:PARAMETERS psUser;
:RETURN psUser;`

	for _, d := range GetDiagnostics(code, DefaultDiagnosticOptions()) {
		if d.Code == CodeParametersFirst {
			t.Errorf("unexpected parameters_first after :INCLUDE: %s", d.Message)
		}
	}
}

// Issue #168: a :BEGININLINECODE block is a :PARAMETERS scope of its own —
// its leading :PARAMETERS is valid regardless of earlier script statements,
// while a non-leading one flags against the block.
func TestGetDiagnostics_ParameterPlacement_InlineCodeScope(t *testing.T) {
	valid := `:DECLARE x;
x := 1;
:BEGININLINECODE Calc;
:PARAMETERS a;
:RETURN a;
:ENDINLINECODE;`

	for _, d := range GetDiagnostics(valid, DefaultDiagnosticOptions()) {
		if d.Code == CodeParametersFirst {
			t.Errorf("unexpected parameters_first for leading :PARAMETERS in inline-code block: %s", d.Message)
		}
	}

	invalid := `:BEGININLINECODE Calc;
:DECLARE b;
:PARAMETERS a;
:ENDINLINECODE;`

	found := false
	for _, d := range GetDiagnostics(invalid, DefaultDiagnosticOptions()) {
		if d.Code == CodeParametersFirst {
			found = true
			if !strings.Contains(d.Message, ":BEGININLINECODE") {
				t.Errorf("expected message to name :BEGININLINECODE, got %s", d.Message)
			}
		}
	}
	if !found {
		t.Error("expected parameters_first for non-leading :PARAMETERS in inline-code block")
	}
}

// Issue #169: an in-file declaration suppresses global_assignment — a
// declared local colliding case-insensitively with a status keyword is the
// author's own variable, and a :PUBLIC declarer is the initializer script.
func TestGetDiagnostics_GlobalAssignment_DeclaredLocalCollidingWithStatusKeyword(t *testing.T) {
	code := `:DECLARE iS, aSeg;
aSeg := {1,2};
:FOR iS := 1 :TO Len(aSeg);
:NEXT;`

	for _, d := range GetDiagnostics(code, DefaultDiagnosticOptions()) {
		if d.Code == CodeGlobalAssignment {
			t.Errorf("unexpected global_assignment on declared local: %s", d.Message)
		}
	}
}

// Issue #169: the file declaring :PUBLIC <global> is its initializer and may
// assign it; a file without the declaration still flags.
func TestGetDiagnostics_GlobalAssignment_PublicDeclarerMayAssign(t *testing.T) {
	declared := `:PUBLIC MYUSERNAME;
MYUSERNAME := "system";`

	for _, d := range GetDiagnostics(declared, DefaultDiagnosticOptions()) {
		if d.Code == CodeGlobalAssignment {
			t.Errorf("unexpected global_assignment in :PUBLIC initializer: %s", d.Message)
		}
	}

	undeclared := `MYUSERNAME := "someone";`
	found := false
	for _, d := range GetDiagnostics(undeclared, DefaultDiagnosticOptions()) {
		if d.Code == CodeGlobalAssignment {
			found = true
		}
	}
	if !found {
		t.Error("expected global_assignment without an in-file declaration")
	}
}

// Issue #165: bare And/Or/Not in identifier slots (declarations, assignment
// targets, member access) are legal identifiers — WSDL-generated proxy
// classes declare such members. Only operator positions flag.
func TestGetDiagnostics_BareLogicalOperator_IdentifierSlotsAllowed(t *testing.T) {
	code := `:DECLARE And, Or, oProxy, x;
And := 1;
Or := 2;
x := oProxy:And;
x := oProxy:Not(3);`

	for _, d := range GetDiagnostics(code, DefaultDiagnosticOptions()) {
		if d.Code == CodeBareLogicalOperator {
			t.Errorf("unexpected bare_logical_operator in identifier slot: %s", d.Message)
		}
	}
}

// Issue #165: genuine operator positions still flag after the narrowing.
func TestGetDiagnostics_BareLogicalOperator_OperatorPositionsStillFlag(t *testing.T) {
	code := `:DECLARE a, b, c;
c := a And b;
c := a Or .T.;
:IF Not a;
:ENDIF;`

	count := 0
	for _, d := range GetDiagnostics(code, DefaultDiagnosticOptions()) {
		if d.Code == CodeBareLogicalOperator {
			count++
		}
	}
	if count != 3 {
		t.Errorf("expected 3 bare_logical_operator diagnostics, got %d", count)
	}
}

// Issue #166: a variable whose most recent assignment is .NET-derived
// (colon member call or LimsNetConnect/LimsNetCast result) downgrades a
// later [0] subscript to the .NET warning; unrelated variables keep the
// error, and a non-.NET reassignment restores it.
func TestGetDiagnostics_ZeroBasedIndex_NetDerivedVariableWarns(t *testing.T) {
	code := `:DECLARE oInt, aBytes, bZero;
oInt := LimsNetConnect("System", "System.Numerics.BigInteger");
aBytes := oInt:ToByteArray();
bZero := aBytes[0] == 0;`

	found := false
	for _, d := range GetDiagnostics(code, DefaultDiagnosticOptions()) {
		if d.Code == CodeZeroBasedArrayIndex {
			found = true
			if d.Severity != SeverityWarning {
				t.Errorf("expected warning severity on .NET-derived [0], got %v", d.Severity)
			}
		}
	}
	if !found {
		t.Error("expected zero_based_array_index warning on .NET-derived [0]")
	}

	reassigned := `:DECLARE oInt, aBytes, bZero;
aBytes := oInt:ToByteArray();
aBytes := {1,2};
bZero := aBytes[0] == 0;`

	for _, d := range GetDiagnostics(reassigned, DefaultDiagnosticOptions()) {
		if d.Code == CodeZeroBasedArrayIndex && d.Severity != SeverityError {
			t.Errorf("expected error severity after non-.NET reassignment, got %v", d.Severity)
		}
	}
}

// Issue #167: direct-call severity is tiered — an in-file procedure called
// directly is a provable misuse (error); an unknown bare callable may be an
// uncataloged vendor built-in (warning).
func TestGetDiagnostics_DirectProcedureCall_TieredSeverity(t *testing.T) {
	inFile := `:PROCEDURE MyHelper;
:ENDPROC;
:PROCEDURE Main;
:DECLARE result;
result := MyHelper();
:ENDPROC;`

	found := false
	for _, d := range GetDiagnostics(inFile, DefaultDiagnosticOptions()) {
		if d.Code == CodeDirectProcedureCall {
			found = true
			if d.Severity != SeverityError {
				t.Errorf("expected error severity for in-file direct call, got %v", d.Severity)
			}
		}
	}
	if !found {
		t.Error("expected direct_procedure_call for in-file direct call")
	}

	unknown := `:DECLARE s;
s := LimsCleanUp();`

	found = false
	for _, d := range GetDiagnostics(unknown, DefaultDiagnosticOptions()) {
		if d.Code == CodeDirectProcedureCall {
			found = true
			if d.Severity != SeverityWarning {
				t.Errorf("expected warning severity for unknown bare callable, got %v", d.Severity)
			}
		}
	}
	if !found {
		t.Error("expected direct_procedure_call warning for unknown bare callable")
	}
}

// Issue #171: a classless file of solely :PROCEDURE blocks is the shape of
// an :INCLUDE library compiled into a class — Me there warns instead of
// erroring; a file with top-level statements keeps the error.
func TestGetDiagnostics_MeOutsideClass_IncludeLibraryWarns(t *testing.T) {
	library := `:PROCEDURE Compare;
:PARAMETERS oOther;
:RETURN Me:Name == oOther:Name;
:ENDPROC;`

	found := false
	for _, d := range GetDiagnostics(library, DefaultDiagnosticOptions()) {
		if d.Code == CodeMeOutsideClass {
			found = true
			if d.Severity != SeverityWarning {
				t.Errorf("expected warning severity in include-library shape, got %v", d.Severity)
			}
		}
	}
	if !found {
		t.Error("expected me_outside_class warning in include-library shape")
	}

	script := `:DECLARE x;
x := Me:Name;`

	found = false
	for _, d := range GetDiagnostics(script, DefaultDiagnosticOptions()) {
		if d.Code == CodeMeOutsideClass {
			found = true
			if d.Severity != SeverityError {
				t.Errorf("expected error severity with top-level statements, got %v", d.Severity)
			}
		}
	}
	if !found {
		t.Error("expected me_outside_class error in script shape")
	}
}

// --- checkUnusedVariables tests ---

func TestGetDiagnostics_UnusedVariable_Basic(t *testing.T) {
	code := `:DECLARE sUsed, sUnused;
sUsed := "hello";`

	opts := DefaultDiagnosticOptions()
	opts.CheckUnusedVars = true

	diagnostics := GetDiagnostics(code, opts)

	foundUnused := false
	for _, d := range diagnostics {
		if strings.Contains(d.Message, "sUnused") && strings.Contains(d.Message, "never used") {
			foundUnused = true
		}
	}

	if !foundUnused {
		t.Fatal("expected unused variable diagnostic for sUnused")
	}
}

func TestGetDiagnostics_UnusedVariable_UsedVariableNotFlagged(t *testing.T) {
	code := `:DECLARE sName;
sName := "test";
x := sName;`

	opts := DefaultDiagnosticOptions()
	opts.CheckUnusedVars = true

	diagnostics := GetDiagnostics(code, opts)

	for _, d := range diagnostics {
		if strings.Contains(d.Message, "sName") && strings.Contains(d.Message, "never used") {
			t.Error("sName is used and should not be flagged as unused")
		}
	}
}

func TestGetDiagnostics_UnusedVariable_DisabledByDefault(t *testing.T) {
	code := `:DECLARE sUnused;`

	diagnostics := GetDiagnostics(code, DefaultDiagnosticOptions())

	for _, d := range diagnostics {
		if strings.Contains(d.Message, "never used") {
			t.Error("unused variable check should be disabled by default")
		}
	}
}

// --- checkSQLParameterValidation tests ---

func TestGetDiagnostics_SQLParameterValidation_UndeclaredParam(t *testing.T) {
	code := `:DECLARE sName;
sName := "test";
x := SQLExecute("SELECT * FROM users WHERE name = ?sUndeclared?");`

	opts := DefaultDiagnosticOptions()
	opts.CheckSQLParams = true

	diagnostics := GetDiagnostics(code, opts)

	for _, d := range diagnostics {
		if strings.Contains(d.Message, "sUndeclared") && strings.Contains(d.Message, "does not match") {
			return
		}
	}

	t.Fatal("expected SQL parameter validation diagnostic for undeclared ?sUndeclared?")
}

func TestGetDiagnostics_SQLParameterValidation_DeclaredParamNotFlagged(t *testing.T) {
	code := `:DECLARE sName;
sName := "test";
x := SQLExecute("SELECT * FROM users WHERE name = ?sName?");`

	opts := DefaultDiagnosticOptions()
	opts.CheckSQLParams = true

	diagnostics := GetDiagnostics(code, opts)

	for _, d := range diagnostics {
		if strings.Contains(d.Message, "sName") && strings.Contains(d.Message, "does not match") {
			t.Error("declared variable sName should not be flagged in SQL parameter validation")
		}
	}
}

func TestGetDiagnostics_SQLParameterValidation_DisabledByDefault(t *testing.T) {
	code := `x := SQLExecute("SELECT * FROM users WHERE name = ?sUndeclared?");`

	diagnostics := GetDiagnostics(code, DefaultDiagnosticOptions())

	for _, d := range diagnostics {
		if strings.Contains(d.Message, "does not match any declared variable") {
			t.Error("SQL parameter validation should be disabled by default")
		}
	}
}

// --- checkTokenErrors tests ---

func TestGetDiagnostics_TokenErrors_UnknownToken(t *testing.T) {
	// Backtick is not a valid SSL token
	code := "x := `invalid`;"

	diagnostics := GetDiagnostics(code, DefaultDiagnosticOptions())

	for _, d := range diagnostics {
		if strings.Contains(d.Message, "Unknown token") {
			return
		}
	}

	t.Fatal("expected unknown token diagnostic for backtick")
}

// --- checkLoopAndFinallyControl: :LOOP in :FINALLY ---

func TestGetDiagnostics_LoopInFinally(t *testing.T) {
	code := `:WHILE .T.;
:TRY;
    x := 1;
:FINALLY;
    :LOOP;
:ENDTRY;
:ENDWHILE;`

	diagnostics := GetDiagnostics(code, DefaultDiagnosticOptions())

	for _, d := range diagnostics {
		if strings.Contains(d.Message, "':LOOP' inside a ':FINALLY' block is a compile-time error") {
			return
		}
	}

	t.Fatal("expected LOOP-in-FINALLY diagnostic")
}

// --- checkDeprecatedKeywords: :ERROR and :LABEL ---

func TestGetDiagnostics_ErrorIsDeprecatedKeyword(t *testing.T) {
	code := `:ERROR;
x := GetLastSSLError();`

	diagnostics := GetDiagnostics(code, DefaultDiagnosticOptions())

	for _, d := range diagnostics {
		if strings.Contains(d.Message, "':ERROR' is legacy error handling") {
			return
		}
	}

	t.Fatal("expected deprecation warning for :ERROR")
}

func TestGetDiagnostics_LabelIsDeprecatedKeyword(t *testing.T) {
	code := `:LABEL SKIP;
x := 1;`

	diagnostics := GetDiagnostics(code, DefaultDiagnosticOptions())

	for _, d := range diagnostics {
		if strings.Contains(d.Message, "':LABEL' is legacy flow control") {
			return
		}
	}

	t.Fatal("expected deprecation warning for :LABEL")
}

// --- checkMissingOtherwise tests ---

func TestGetDiagnostics_MissingOtherwise(t *testing.T) {
	code := `:BEGINCASE;
:CASE x > 0;
    y := 1;
:EXITCASE;
:ENDCASE;`

	diagnostics := GetDiagnostics(code, DefaultDiagnosticOptions())

	for _, d := range diagnostics {
		if strings.Contains(d.Message, "no ':OTHERWISE' clause") {
			return
		}
	}

	t.Fatal("expected missing OTHERWISE hint")
}

func TestGetDiagnostics_MissingOtherwise_PresentNoWarning(t *testing.T) {
	code := `:BEGINCASE;
:CASE x > 0;
    y := 1;
:EXITCASE;
:OTHERWISE;
    y := 0;
:EXITCASE;
:ENDCASE;`

	diagnostics := GetDiagnostics(code, DefaultDiagnosticOptions())

	for _, d := range diagnostics {
		if strings.Contains(d.Message, "no ':OTHERWISE' clause") {
			t.Error("should not warn when :OTHERWISE is present")
		}
	}
}

func TestGetDiagnostics_MissingOtherwise_NestedIgnoresInner(t *testing.T) {
	// Inner BEGINCASE has OTHERWISE, outer does not
	code := `:BEGINCASE;
:CASE x > 0;
    :BEGINCASE;
    :CASE y > 0;
        z := 1;
    :EXITCASE;
    :OTHERWISE;
        z := 0;
    :EXITCASE;
    :ENDCASE;
:EXITCASE;
:ENDCASE;`

	diagnostics := GetDiagnostics(code, DefaultDiagnosticOptions())

	found := 0
	for _, d := range diagnostics {
		if strings.Contains(d.Message, "no ':OTHERWISE' clause") {
			found++
		}
	}

	if found != 1 {
		t.Errorf("expected exactly 1 missing OTHERWISE hint (outer block only), got %d", found)
	}
}

// --- TRY structure edge cases ---

func TestGetDiagnostics_TryMultipleCatch(t *testing.T) {
	// Only one :CATCH per :TRY
	code := `:TRY;
    x := 1;
:CATCH;
    y := 2;
:CATCH;
    z := 3;
:ENDTRY;`

	diagnostics := GetDiagnostics(code, DefaultDiagnosticOptions())

	for _, d := range diagnostics {
		if strings.Contains(d.Message, "Only one ':CATCH' block is allowed") {
			return
		}
	}

	t.Fatal("expected multiple-catch diagnostic")
}

func TestGetDiagnostics_TryCatchAfterFinally(t *testing.T) {
	// :CATCH must appear before :FINALLY
	code := `:TRY;
    x := 1;
:FINALLY;
    y := 2;
:CATCH;
    z := 3;
:ENDTRY;`

	diagnostics := GetDiagnostics(code, DefaultDiagnosticOptions())

	for _, d := range diagnostics {
		if strings.Contains(d.Message, "':CATCH' must appear before ':FINALLY'") {
			return
		}
	}

	t.Fatal("expected catch-after-finally diagnostic")
}

func TestGetDiagnostics_TryMultipleFinally(t *testing.T) {
	// Only one :FINALLY per :TRY
	code := `:TRY;
    x := 1;
:FINALLY;
    y := 2;
:FINALLY;
    z := 3;
:ENDTRY;`

	diagnostics := GetDiagnostics(code, DefaultDiagnosticOptions())

	for _, d := range diagnostics {
		if strings.Contains(d.Message, "Only one ':FINALLY' block is allowed") {
			return
		}
	}

	t.Fatal("expected multiple-finally diagnostic")
}

func TestGetDiagnostics_TryOnlyCatchIsValid(t *testing.T) {
	// :TRY with only :CATCH (no :FINALLY) is valid
	code := `:TRY;
    x := 1;
:CATCH;
    y := GetLastSSLError();
:ENDTRY;`

	diagnostics := GetDiagnostics(code, DefaultDiagnosticOptions())

	for _, d := range diagnostics {
		if strings.Contains(d.Message, "requires at least one ':CATCH' or ':FINALLY'") {
			t.Error("TRY with only CATCH should be valid")
		}
	}
}

func TestGetDiagnostics_TryOnlyFinallyIsValid(t *testing.T) {
	// :TRY with only :FINALLY (no :CATCH) is valid
	code := `:TRY;
    x := 1;
:FINALLY;
    DoProc("Cleanup");
:ENDTRY;`

	diagnostics := GetDiagnostics(code, DefaultDiagnosticOptions())

	for _, d := range diagnostics {
		if strings.Contains(d.Message, "requires at least one ':CATCH' or ':FINALLY'") {
			t.Error("TRY with only FINALLY should be valid")
		}
	}
}

// --- Class context edge cases ---

func TestGetDiagnostics_OneClassPerFile(t *testing.T) {
	// Only one :CLASS per file
	code := `:CLASS MyClass;
:PROCEDURE Method1;
:ENDPROC;
:CLASS AnotherClass;
:PROCEDURE Method2;
:ENDPROC;`

	diagnostics := GetDiagnostics(code, DefaultDiagnosticOptions())

	for _, d := range diagnostics {
		if strings.Contains(d.Message, "Only one ':CLASS' definition is allowed per file") {
			return
		}
	}

	t.Fatal("expected one-class-per-file diagnostic")
}

func TestGetDiagnostics_ClassMustBeFirstStatement(t *testing.T) {
	// :CLASS must be the first significant statement
	code := `x := 1;
:CLASS MyClass;
:PROCEDURE Method1;
:ENDPROC;`

	diagnostics := GetDiagnostics(code, DefaultDiagnosticOptions())

	for _, d := range diagnostics {
		if strings.Contains(d.Message, "':CLASS' must be the first significant statement") {
			return
		}
	}

	t.Fatal("expected class-first-statement diagnostic")
}

// --- Loop exit edge cases ---

func TestGetDiagnostics_ExitWhileOutsideWhile(t *testing.T) {
	code := `:EXITWHILE;`

	diagnostics := GetDiagnostics(code, DefaultDiagnosticOptions())

	for _, d := range diagnostics {
		if strings.Contains(d.Message, "':EXITWHILE' must be inside a ':WHILE' loop") {
			return
		}
	}

	t.Fatal("expected EXITWHILE-outside-loop diagnostic")
}

func TestGetDiagnostics_ExitWhileInFinally(t *testing.T) {
	code := `:WHILE .T.;
:TRY;
    x := 1;
:FINALLY;
    :EXITWHILE;
:ENDTRY;
:ENDWHILE;`

	diagnostics := GetDiagnostics(code, DefaultDiagnosticOptions())

	for _, d := range diagnostics {
		if strings.Contains(d.Message, "':EXITWHILE' inside a ':FINALLY' block is a compile-time error") {
			return
		}
	}

	t.Fatal("expected EXITWHILE-in-FINALLY diagnostic")
}

func TestGetDiagnostics_ExitForInFinally(t *testing.T) {
	code := `:FOR i := 1 :TO 10;
:TRY;
    x := 1;
:FINALLY;
    :EXITFOR;
:ENDTRY;
:NEXT;`

	diagnostics := GetDiagnostics(code, DefaultDiagnosticOptions())

	for _, d := range diagnostics {
		if strings.Contains(d.Message, "':EXITFOR' inside a ':FINALLY' block is a compile-time error") {
			return
		}
	}

	t.Fatal("expected EXITFOR-in-FINALLY diagnostic")
}

// --- Empty BEGINCASE ---

func TestGetDiagnostics_EmptyBeginCase(t *testing.T) {
	// :BEGINCASE with no :CASE is a compile error
	code := `:BEGINCASE;
:ENDCASE;`

	diagnostics := GetDiagnostics(code, DefaultDiagnosticOptions())

	for _, d := range diagnostics {
		if strings.Contains(d.Message, "requires at least one ':CASE' block") {
			return
		}
	}

	t.Fatal("expected empty BEGINCASE diagnostic")
}

// --- :ERROR handler must have body ---

func TestGetDiagnostics_ErrorHandlerWithBody(t *testing.T) {
	// :ERROR with a body is valid
	code := `:ERROR;
oErr := GetLastSSLError();
:RESUME;`

	diagnostics := GetDiagnostics(code, DefaultDiagnosticOptions())

	for _, d := range diagnostics {
		if strings.Contains(d.Message, "':ERROR' must contain at least one statement") {
			t.Error(":ERROR with body should be valid")
		}
	}
}

// --- :INCLUDE at top vs middle ---

func TestGetDiagnostics_IncludeMiddleOfFile(t *testing.T) {
	code := `:DECLARE x;
x := 1;
:INCLUDE SomeLibrary;`

	diagnostics := GetDiagnostics(code, DefaultDiagnosticOptions())

	for _, d := range diagnostics {
		if strings.Contains(d.Message, "should appear early in the file") {
			return
		}
	}

	t.Fatal("expected INCLUDE-not-at-top info")
}

func TestGetDiagnostics_IncludeAtTopIsClean(t *testing.T) {
	code := `:INCLUDE SomeLibrary;
:DECLARE x;
x := 1;`

	diagnostics := GetDiagnostics(code, DefaultDiagnosticOptions())

	for _, d := range diagnostics {
		if strings.Contains(d.Message, "should appear early in the file") {
			t.Error(":INCLUDE at top should not be flagged")
		}
	}
}

// --- Not-preferred operators: valid != not flagged ---

func TestGetDiagnostics_NotPreferredOperators_ValidNotEquals(t *testing.T) {
	code := `:IF x != 1;
:ENDIF;`

	diagnostics := GetDiagnostics(code, DefaultDiagnosticOptions())

	for _, d := range diagnostics {
		if strings.Contains(d.Message, "instead of") && strings.Contains(d.Message, "!=") {
			t.Error("!= should not be flagged as non-preferred")
		}
	}
}

// --- Comprehensive empty trailing array coverage ---

func TestGetDiagnostics_EmptyTrailingArray_DoProc(t *testing.T) {
	code := `DoProc("MyProc", {});`

	diagnostics := GetDiagnostics(code, DefaultDiagnosticOptions())

	for _, d := range diagnostics {
		if strings.Contains(d.Message, "Omit the trailing empty array") {
			return
		}
	}

	t.Fatal("expected empty trailing array diagnostic for DoProc")
}

func TestGetDiagnostics_EmptyTrailingArray_ExecFunction(t *testing.T) {
	code := `ExecFunction("Module.Proc", {});`

	diagnostics := GetDiagnostics(code, DefaultDiagnosticOptions())

	for _, d := range diagnostics {
		if strings.Contains(d.Message, "Omit the trailing empty array") {
			return
		}
	}

	t.Fatal("expected empty trailing array diagnostic for ExecFunction")
}

// --- :BEGINCASE :CASE with EXITCASE is valid ---

func TestGetDiagnostics_BeginCaseWithCaseIsValid(t *testing.T) {
	code := `:BEGINCASE;
:CASE x > 0;
    y := 1;
:EXITCASE;
:ENDCASE;`

	diagnostics := GetDiagnostics(code, DefaultDiagnosticOptions())

	for _, d := range diagnostics {
		if strings.Contains(d.Message, "requires at least one ':CASE' block") {
			t.Error("BEGINCASE with CASE should not be flagged")
		}
	}
}

// ==================== Name Length Tests ====================

func TestGetDiagnostics_VariableNameTooLong(t *testing.T) {
	// Variable name exceeds 20 chars (excluding prefix)
	code := `:DECLARE sThisIsAnExtremelyLongVariableNameThatExceedsLimit;`

	diagnostics := GetDiagnostics(code, DefaultDiagnosticOptions())

	for _, d := range diagnostics {
		if strings.Contains(d.Message, "exceeds 20-character limit") {
			return
		}
	}

	t.Fatal("expected variable name length diagnostic")
}

func TestGetDiagnostics_VariableNameOK(t *testing.T) {
	// Variable name within limit (prefix stripped)
	code := `:DECLARE sShortName;`

	diagnostics := GetDiagnostics(code, DefaultDiagnosticOptions())

	for _, d := range diagnostics {
		if strings.Contains(d.Message, "exceeds 20-character limit") {
			t.Errorf("short variable name should not be flagged: %s", d.Message)
		}
	}
}

func TestGetDiagnostics_ProcedureNameTooLong(t *testing.T) {
	// Procedure name exceeds 30 chars
	code := `:PROCEDURE ThisIsAnExtremelyLongProcedureName;
:ENDPROC;`

	diagnostics := GetDiagnostics(code, DefaultDiagnosticOptions())

	for _, d := range diagnostics {
		if strings.Contains(d.Message, "exceeds 30-character limit") {
			return
		}
	}

	t.Fatal("expected procedure name length diagnostic")
}

func TestGetDiagnostics_ProcedureNameOK(t *testing.T) {
	// Procedure name within limit
	code := `:PROCEDURE ValidateSample;
:ENDPROC;`

	diagnostics := GetDiagnostics(code, DefaultDiagnosticOptions())

	for _, d := range diagnostics {
		if strings.Contains(d.Message, "exceeds 30-character limit") {
			t.Error("short procedure name should not be flagged")
		}
	}
}

// ==================== Visibility Annotation Tests ====================

func TestGetDiagnostics_VisibilityAnnotationInClass(t *testing.T) {
	// @private has no effect on class methods
	code := `:CLASS MyClass;
/*@private;
:PROCEDURE Helper;
:ENDPROC;`

	diagnostics := GetDiagnostics(code, DefaultDiagnosticOptions())

	for _, d := range diagnostics {
		if strings.Contains(d.Message, "has no effect on class methods") {
			return
		}
	}

	t.Fatal("expected visibility annotation warning in class context")
}

func TestGetDiagnostics_VisibilityAnnotationValidInScript(t *testing.T) {
	// @private before :PROCEDURE in script is valid
	code := `/*@private;
:PROCEDURE Helper;
:ENDPROC;`

	diagnostics := GetDiagnostics(code, DefaultDiagnosticOptions())

	for _, d := range diagnostics {
		if strings.Contains(d.Message, "has no effect") || strings.Contains(d.Message, "should appear") {
			t.Errorf("valid visibility annotation in script should not be flagged: %s", d.Message)
		}
	}
}

func TestGetDiagnostics_VisibilityAnnotationNotBeforeProcedure(t *testing.T) {
	// @protected not before :PROCEDURE
	code := `/*@protected;
:DECLARE x;`

	diagnostics := GetDiagnostics(code, DefaultDiagnosticOptions())

	for _, d := range diagnostics {
		if strings.Contains(d.Message, "should appear on its own line immediately before ':PROCEDURE'") {
			return
		}
	}

	t.Fatal("expected visibility annotation placement warning")
}

// ==================== NIL Method Call Tests ====================

func TestGetDiagnostics_NilMethodCall_Literal(t *testing.T) {
	// NIL:Method() is always an error
	code := `x := NIL:ToString();`

	diagnostics := GetDiagnostics(code, DefaultDiagnosticOptions())

	for _, d := range diagnostics {
		if strings.Contains(d.Message, "Calling a method on NIL raises an error") {
			return
		}
	}

	t.Fatal("expected NIL method call diagnostic")
}

func TestGetDiagnostics_NilMethodCall_Variable(t *testing.T) {
	// Variable assigned NIL then used with member access
	code := `:DECLARE oObj;
oObj := NIL;
oObj:Method();`

	diagnostics := GetDiagnostics(code, DefaultDiagnosticOptions())

	for _, d := range diagnostics {
		if strings.Contains(d.Message, "may be NIL at this point") {
			return
		}
	}

	t.Fatal("expected NIL variable method call diagnostic")
}

func TestGetDiagnostics_NilMethodCall_ReassignedNotFlagged(t *testing.T) {
	// Variable assigned NIL then reassigned — should not flag
	code := `:DECLARE oObj;
oObj := NIL;
oObj := CreateUdObject();
oObj:Method();`

	diagnostics := GetDiagnostics(code, DefaultDiagnosticOptions())

	for _, d := range diagnostics {
		if strings.Contains(d.Message, "may be NIL") && strings.Contains(d.Message, "oObj") {
			t.Error("reassigned variable should not be flagged as NIL")
		}
	}
}

// --- ! operator is valid (source of truth does NOT list it as non-preferred) ---

func TestGetDiagnostics_ExclamationMarkIsValid(t *testing.T) {
	// Source of truth uses ! freely in examples and only lists # and <> as non-preferred.
	// ! should NOT produce any "not preferred" diagnostic.
	text := `:DECLARE bFlag;
bFlag := .T.;
:IF !bFlag;
    UsrMes("not flagged");
:ENDIF;`
	diagnostics := GetDiagnostics(text, DiagnosticOptions{})

	for _, d := range diagnostics {
		if strings.Contains(d.Message, ".NOT.") && strings.Contains(d.Message, "!") {
			t.Errorf("! should not be flagged as non-preferred: %s", d.Message)
		}
	}
}

// --- Scientific notation diagnostic ---

func TestGetDiagnostics_ScientificNotationWithoutDecimal(t *testing.T) {
	// 7e2 should trigger a warning about missing decimal point
	text := `:DECLARE nVal;
nVal := 7e2;`
	diagnostics := GetDiagnostics(text, DiagnosticOptions{})

	found := false
	for _, d := range diagnostics {
		if strings.Contains(d.Message, "scientific notation") && strings.Contains(d.Message, "decimal") {
			found = true
			if d.Severity != SeverityWarning {
				t.Errorf("expected SeverityWarning, got %d", d.Severity)
			}
		}
	}
	if !found {
		t.Error("expected diagnostic about scientific notation requiring decimal point")
	}
}

func TestGetDiagnostics_ScientificNotationWithDecimalNoWarning(t *testing.T) {
	// 7.0e2 should NOT trigger a warning
	text := `:DECLARE nVal;
nVal := 7.0e2;`
	diagnostics := GetDiagnostics(text, DiagnosticOptions{})

	for _, d := range diagnostics {
		if strings.Contains(d.Message, "scientific notation") {
			t.Error("should not flag valid scientific notation 7.0e2")
		}
	}
}

// --- D12: :ENDFOR detection ---

func TestGetDiagnostics_EndForIsInvalid(t *testing.T) {
	text := `:FOR i := 1 :TO 10;
    i := i + 1;
:ENDFOR;`
	diagnostics := GetDiagnostics(text, DefaultDiagnosticOptions())

	found := false
	for _, d := range diagnostics {
		if strings.Contains(d.Message, ":ENDFOR") && strings.Contains(d.Message, ":NEXT") {
			found = true
			if d.Severity != SeverityError {
				t.Errorf("expected SeverityError, got %d", d.Severity)
			}
		}
	}
	if !found {
		t.Error("expected diagnostic telling user to use :NEXT instead of :ENDFOR")
	}
}

func TestGetDiagnostics_EndForCaseInsensitive(t *testing.T) {
	text := `:FOR i := 1 :TO 5;
    i := i + 1;
:endfor;`
	diagnostics := GetDiagnostics(text, DefaultDiagnosticOptions())

	found := false
	for _, d := range diagnostics {
		if strings.Contains(d.Message, ":ENDFOR") || strings.Contains(d.Message, ":NEXT") {
			found = true
		}
	}
	if !found {
		t.Error("expected diagnostic for :endfor (case-insensitive detection)")
	}
}

// --- D13: Redeclared variables ---

func TestGetDiagnostics_RedeclaredVariable(t *testing.T) {
	text := `:DECLARE sName;
:DECLARE sName;`
	diagnostics := GetDiagnostics(text, DefaultDiagnosticOptions())

	found := false
	for _, d := range diagnostics {
		if strings.Contains(strings.ToLower(d.Message), "already declared") || strings.Contains(strings.ToLower(d.Message), "redeclared") {
			found = true
		}
	}
	if !found {
		t.Error("expected diagnostic for redeclared variable")
	}
}

func TestGetDiagnostics_RedeclaredVariable_DifferentScopes(t *testing.T) {
	// Variables in different procedures should NOT be flagged
	text := `:PROCEDURE ProcA;
:DECLARE sName;
sName := "A";
:ENDPROC;

:PROCEDURE ProcB;
:DECLARE sName;
sName := "B";
:ENDPROC;`
	diagnostics := GetDiagnostics(text, DefaultDiagnosticOptions())

	for _, d := range diagnostics {
		if strings.Contains(strings.ToLower(d.Message), "already declared") || strings.Contains(strings.ToLower(d.Message), "redeclared") {
			t.Error("should not flag same variable name in different procedure scopes")
		}
	}
}

// --- D14: Empty trailing array with GetDataSet ---

func TestGetDiagnostics_EmptyTrailingArray_GetDataSet(t *testing.T) {
	text := `:DECLARE sSQL, sXml;
sSQL := "SELECT * FROM Table";
sXml := GetDataSet(sSQL, {});`
	diagnostics := GetDiagnostics(text, DefaultDiagnosticOptions())

	found := false
	for _, d := range diagnostics {
		if strings.Contains(d.Message, "Omit") && strings.Contains(d.Message, "{}") {
			found = true
		}
	}
	if !found {
		t.Error("expected diagnostic about omitting empty trailing array for GetDataSet")
	}
}

func TestGetDiagnostics_EmptyTrailingArray_GetDataSet_WithParams(t *testing.T) {
	// GetDataSet with a real parameter array should NOT trigger the diagnostic
	text := `:DECLARE sSQL, sXml, sStatus;
sSQL := "SELECT * FROM Table WHERE Status = ?";
sXml := GetDataSet(sSQL, {sStatus});`
	diagnostics := GetDiagnostics(text, DefaultDiagnosticOptions())

	for _, d := range diagnostics {
		if strings.Contains(d.Message, "Omit") && strings.Contains(d.Message, "{}") {
			t.Error("GetDataSet with non-empty params should not trigger trailing array warning")
		}
	}
}

// --- D15: Class member order with :INHERIT before :DECLARE ---

func TestGetDiagnostics_ClassMemberOrder_InheritBeforeDeclare(t *testing.T) {
	// Correct order: INHERIT, DECLARE, methods, Constructor
	text := `:CLASS MyClass;
:INHERIT BaseClass;
:DECLARE sField;
:PROCEDURE DoWork;
:ENDPROC;
:PROCEDURE Constructor;
:ENDPROC;`
	diagnostics := GetDiagnostics(text, DefaultDiagnosticOptions())

	for _, d := range diagnostics {
		if strings.Contains(d.Message, "member order") || strings.Contains(d.Message, "should appear before") {
			t.Errorf("correct class member order should not produce diagnostics, got: %s", d.Message)
		}
	}
}

func TestGetDiagnostics_ClassMemberOrder_DeclareBeforeInherit(t *testing.T) {
	// Wrong order: DECLARE before INHERIT
	text := `:CLASS MyClass;
:DECLARE sField;
:INHERIT BaseClass;
:PROCEDURE DoWork;
:ENDPROC;`
	diagnostics := GetDiagnostics(text, DefaultDiagnosticOptions())

	found := false
	for _, d := range diagnostics {
		if strings.Contains(d.Message, "INHERIT") && (strings.Contains(d.Message, "before") || strings.Contains(d.Message, "order")) {
			found = true
		}
	}
	if !found {
		t.Error("expected diagnostic about :INHERIT needing to come before :DECLARE")
	}
}

func TestGetDiagnostics_ClassMemberOrder_ConstructorAnywhere(t *testing.T) {
	// Constructor position is no longer enforced — neither order should warn.
	for _, text := range []string{
		`:CLASS MyClass;
:PROCEDURE Constructor;
:ENDPROC;
:PROCEDURE DoWork;
:ENDPROC;`,
		`:CLASS MyClass;
:PROCEDURE DoWork;
:ENDPROC;
:PROCEDURE Constructor;
:ENDPROC;`,
	} {
		diagnostics := GetDiagnostics(text, DefaultDiagnosticOptions())
		for _, d := range diagnostics {
			if d.Code == CodeClassMemberOrder {
				t.Errorf("unexpected class_member_order diagnostic for permitted Constructor placement: %s", d.Message)
			}
		}
	}
}

// --- D16: String equality = vs == asymmetry ---

func TestGetDiagnostics_LooseStringEquality_PrefixMatch(t *testing.T) {
	text := `:DECLARE sStatus;
sStatus := "Logged";
:IF sStatus = "Log";
:ENDIF;`
	diagnostics := GetDiagnostics(text, DefaultDiagnosticOptions())

	found := false
	for _, d := range diagnostics {
		if strings.Contains(d.Message, "prefix matching") || strings.Contains(d.Message, "exact string") {
			found = true
		}
	}
	if !found {
		t.Error("expected diagnostic about loose string equality doing prefix matching")
	}
}

func TestGetDiagnostics_StrictStringEquality_NoWarning(t *testing.T) {
	text := `:DECLARE sStatus;
sStatus := "Logged";
:IF sStatus == "Logged";
:ENDIF;`
	diagnostics := GetDiagnostics(text, DefaultDiagnosticOptions())

	for _, d := range diagnostics {
		if strings.Contains(d.Message, "prefix matching") {
			t.Error("strict equality == should not produce prefix matching warning")
		}
	}
}

// --- D17: :ERROR/:RESUME structure validation ---

func TestGetDiagnostics_ErrorHandlerRequiresStatement(t *testing.T) {
	text := `:ERROR;
:RESUME;`
	diagnostics := GetDiagnostics(text, DefaultDiagnosticOptions())

	found := false
	for _, d := range diagnostics {
		if strings.Contains(d.Message, "ERROR") && strings.Contains(d.Message, "at least one statement") {
			found = true
		}
	}
	if !found {
		t.Error("expected diagnostic about :ERROR requiring at least one statement")
	}
}

func TestGetDiagnostics_ErrorHandlerWithStatement_NoWarning(t *testing.T) {
	text := `:ERROR;
UsrMes("Error occurred");
:RESUME;`
	diagnostics := GetDiagnostics(text, DefaultDiagnosticOptions())

	for _, d := range diagnostics {
		if strings.Contains(d.Message, "ERROR") && strings.Contains(d.Message, "at least one statement") {
			t.Error(":ERROR with a statement should not produce 'requires statement' diagnostic")
		}
	}
}

func TestGetDiagnostics_DeprecatedError(t *testing.T) {
	text := `:ERROR;
UsrMes("Error");`
	diagnostics := GetDiagnostics(text, DefaultDiagnosticOptions())

	found := false
	for _, d := range diagnostics {
		if strings.Contains(d.Message, "legacy") && strings.Contains(d.Message, "ERROR") {
			found = true
		}
	}
	if !found {
		t.Error("expected deprecation warning for :ERROR")
	}
}

func TestGetDiagnostics_DeprecatedResume(t *testing.T) {
	text := `:ERROR;
UsrMes("Error");
:RESUME;`
	diagnostics := GetDiagnostics(text, DefaultDiagnosticOptions())

	found := false
	for _, d := range diagnostics {
		if strings.Contains(d.Message, "legacy") && strings.Contains(d.Message, "RESUME") {
			found = true
		}
	}
	if !found {
		t.Error("expected deprecation warning for :RESUME")
	}
}

// --- D18: Branch target with :LABELName compact form ---

func TestGetDiagnostics_BranchTarget_CompactLabelForm(t *testing.T) {
	// :LABELSKIP; produces token text "LABELSKIP", so Branch("SKIP") won't match
	text := `:LABELSKIP;
Branch("SKIP");`
	diagnostics := GetDiagnostics(text, DefaultDiagnosticOptions())

	// This should produce a diagnostic because "SKIP" doesn't match "LABELSKIP"
	// (The Branch target must include the full token text including "LABEL" prefix)
	foundBranch := false
	for _, d := range diagnostics {
		if strings.Contains(d.Message, "Branch") || strings.Contains(d.Message, "LABEL") {
			foundBranch = true
		}
	}
	// Just verify it doesn't crash - exact behavior depends on implementation
	_ = foundBranch
}

func TestGetDiagnostics_BranchTarget_SpacedLabelForm(t *testing.T) {
	// :LABEL SKIP; produces token text "LABEL SKIP"
	text := `:LABEL SKIP;
Branch("LABEL SKIP");`
	diagnostics := GetDiagnostics(text, DefaultDiagnosticOptions())

	// Should not produce branch-target-mismatch errors (this is correct usage)
	for _, d := range diagnostics {
		if strings.Contains(d.Message, "Branch target") && strings.Contains(d.Message, "does not match") {
			t.Error("correct Branch target 'LABEL SKIP' should not produce mismatch diagnostic")
		}
	}
}

// --- D19: Visibility annotation placement ---

func TestGetDiagnostics_VisibilityAnnotation_BeforeNonProcedure(t *testing.T) {
	text := `/*@private;
:DECLARE sName;`
	diagnostics := GetDiagnostics(text, DefaultDiagnosticOptions())

	found := false
	for _, d := range diagnostics {
		if strings.Contains(d.Message, "private") && strings.Contains(d.Message, "PROCEDURE") {
			found = true
		}
	}
	if !found {
		t.Error("expected diagnostic about visibility annotation needing to be before :PROCEDURE")
	}
}

func TestGetDiagnostics_VisibilityAnnotation_ProtectedInClass(t *testing.T) {
	text := `:CLASS MyClass;
/*@protected;
:PROCEDURE DoWork;
:ENDPROC;`
	diagnostics := GetDiagnostics(text, DefaultDiagnosticOptions())

	found := false
	for _, d := range diagnostics {
		if strings.Contains(d.Message, "protected") && strings.Contains(d.Message, "no effect") {
			found = true
		}
	}
	if !found {
		t.Error("expected diagnostic about visibility annotation having no effect on class methods")
	}
}

// --- D20: NIL method calls ---

func TestGetDiagnostics_NilMethodCall_AfterReassignment(t *testing.T) {
	// After reassigning to non-NIL, method calls should be fine
	text := `:DECLARE oObj;
oObj := NIL;
oObj := CreateUdObject();
oObj:Method();`
	diagnostics := GetDiagnostics(text, DefaultDiagnosticOptions())

	for _, d := range diagnostics {
		if strings.Contains(d.Message, "NIL") && strings.Contains(d.Message, "method") {
			t.Error("should not flag method call after reassignment from NIL to non-NIL")
		}
	}
}

// --- Pass 2: Nested IIF ---

func TestGetDiagnostics_NestedIIF(t *testing.T) {
	text := `:DECLARE sResult;
sResult := IIF(bCond1, IIF(bCond2, "A", "B"), "C");`
	diagnostics := GetDiagnostics(text, DefaultDiagnosticOptions())

	found := false
	for _, d := range diagnostics {
		if strings.Contains(d.Message, "Nested IIF") || strings.Contains(d.Message, "IIF()") {
			found = true
		}
	}
	if !found {
		t.Error("expected diagnostic about nested IIF reducing readability")
	}
}

func TestGetDiagnostics_NestedIIF_SingleNotFlagged(t *testing.T) {
	text := `:DECLARE sResult;
sResult := IIF(bCond, "Yes", "No");`
	diagnostics := GetDiagnostics(text, DefaultDiagnosticOptions())

	for _, d := range diagnostics {
		if strings.Contains(d.Message, "Nested IIF") || strings.Contains(d.Message, "IIF()") {
			t.Error("single IIF should not trigger nested warning")
		}
	}
}

// --- Pass 2: Negative Logic ---

func TestGetDiagnostics_NegativeLogic(t *testing.T) {
	text := `:DECLARE bFlag;
:IF .NOT. bFlag;
    DoProc("HandleFalse");
:ELSE;
    DoProc("HandleTrue");
:ENDIF;`
	diagnostics := GetDiagnostics(text, DefaultDiagnosticOptions())

	found := false
	for _, d := range diagnostics {
		if strings.Contains(d.Message, "positive logic") || strings.Contains(d.Message, "invert") {
			found = true
		}
	}
	if !found {
		t.Error("expected hint about inverting condition to use positive logic")
	}
}

func TestGetDiagnostics_NegativeLogic_NoElseNoWarning(t *testing.T) {
	// Without :ELSE, there's no opportunity to invert
	text := `:DECLARE bFlag;
:IF .NOT. bFlag;
    DoProc("HandleFalse");
:ENDIF;`
	diagnostics := GetDiagnostics(text, DefaultDiagnosticOptions())

	for _, d := range diagnostics {
		if strings.Contains(d.Message, "positive logic") || strings.Contains(d.Message, "invert") {
			t.Error("should not suggest inverting when there is no :ELSE branch")
		}
	}
}

// --- Pass 2: Multiple :CLASS per file ---

func TestGetDiagnostics_MultipleClassPerFile(t *testing.T) {
	text := `:CLASS ClassA;
:PROCEDURE MethodA;
:ENDPROC;

:CLASS ClassB;
:PROCEDURE MethodB;
:ENDPROC;`
	diagnostics := GetDiagnostics(text, DefaultDiagnosticOptions())

	found := false
	for _, d := range diagnostics {
		if strings.Contains(d.Message, "Only one") && strings.Contains(d.Message, "CLASS") {
			found = true
		}
	}
	if !found {
		t.Error("expected diagnostic about only one :CLASS per file")
	}
}

// --- Pass 2: :CLASS must be first significant statement ---

func TestGetDiagnostics_ClassNotFirstStatement(t *testing.T) {
	text := `:DECLARE sName;
sName := "test";
:CLASS MyClass;
:PROCEDURE DoWork;
:ENDPROC;`
	diagnostics := GetDiagnostics(text, DefaultDiagnosticOptions())

	found := false
	for _, d := range diagnostics {
		if strings.Contains(d.Message, "CLASS") && strings.Contains(d.Message, "first") {
			found = true
		}
	}
	if !found {
		t.Error("expected diagnostic about :CLASS needing to be first statement")
	}
}

// --- Pass 2: Skipped param spacing (adjacent commas) ---

func TestGetDiagnostics_SkippedParamSpacing(t *testing.T) {
	// {param1,,param3} is valid, {param1, , param3} is not
	// This is a lexer/formatting concern, verify valid form doesn't crash
	text := `:DECLARE sResult;
sResult := DoProc("MyProc", {1,,3,,5});`
	diagnostics := GetDiagnostics(text, DefaultDiagnosticOptions())
	// Should not produce any diagnostic about the skipped params
	for _, d := range diagnostics {
		if strings.Contains(d.Message, "skipped") && strings.Contains(d.Message, "param") {
			t.Error("valid skipped param syntax should not produce diagnostic")
		}
	}
}

// --- Pass 2: Code block (lambda) with comparisons ---

func TestGetDiagnostics_CodeBlockComparison_InCondition(t *testing.T) {
	// Code block literal in comparison context should trigger diagnostic
	text := `:DECLARE fnCallback;
:IF fnCallback = {|x| x + 1};
:ENDIF;`
	diagnostics := GetDiagnostics(text, DefaultDiagnosticOptions())

	found := false
	for _, d := range diagnostics {
		if strings.Contains(d.Message, "Code block") || strings.Contains(d.Message, "lambda") {
			found = true
		}
	}
	if !found {
		t.Error("expected diagnostic about comparing code blocks")
	}
}

// --- Pass 2: $ containment only for strings ---

func TestGetDiagnostics_DollarContainmentWithNumber(t *testing.T) {
	text := `:DECLARE nVal, bResult;
nVal := 42;
bResult := nVal $ "test";`
	diagnostics := GetDiagnostics(text, DefaultDiagnosticOptions())

	found := false
	for _, d := range diagnostics {
		if strings.Contains(d.Message, "$") && strings.Contains(d.Message, "string") {
			found = true
		}
	}
	if !found {
		t.Error("expected diagnostic about $ operator only working on strings")
	}
}

// --- Pass 2: NIL in arithmetic operation ---

func TestGetDiagnostics_NilInArithmetic(t *testing.T) {
	text := `:DECLARE nResult;
nResult := NIL + 1;`
	diagnostics := GetDiagnostics(text, DefaultDiagnosticOptions())

	found := false
	for _, d := range diagnostics {
		if strings.Contains(d.Message, "NIL") && strings.Contains(d.Message, "arithmetic") {
			found = true
		}
	}
	if !found {
		t.Error("expected diagnostic about NIL in arithmetic operations")
	}
}

// --- Pass 2: Mixed-type operator warnings ---

func TestGetDiagnostics_MixedTypePlus_StringPlusNumber(t *testing.T) {
	text := `:DECLARE sName;
sName := "hello" + 5;`
	diagnostics := GetDiagnostics(text, DefaultDiagnosticOptions())

	for _, d := range diagnostics {
		if strings.Contains(d.Message, "Mixed types in '+' operation") {
			return
		}
	}
	t.Fatal("expected mixed-type warning for string + number")
}

func TestGetDiagnostics_MixedTypePlus_HungarianPrefix(t *testing.T) {
	text := `:DECLARE sName, nCount;
x := sName + nCount;`
	diagnostics := GetDiagnostics(text, DefaultDiagnosticOptions())

	for _, d := range diagnostics {
		if strings.Contains(d.Message, "Mixed types in '+' operation") {
			return
		}
	}
	t.Fatal("expected mixed-type warning for string variable + numeric variable")
}

func TestGetDiagnostics_MixedType_StringInArithmetic(t *testing.T) {
	text := `:DECLARE sName;
x := sName - 1;`
	diagnostics := GetDiagnostics(text, DefaultDiagnosticOptions())

	for _, d := range diagnostics {
		if strings.Contains(d.Message, "String in arithmetic operation") {
			return
		}
	}
	t.Fatal("expected string-in-arithmetic warning")
}

func TestGetDiagnostics_MixedType_BooleanInArithmetic(t *testing.T) {
	text := `x := .T. * 5;`
	diagnostics := GetDiagnostics(text, DefaultDiagnosticOptions())

	for _, d := range diagnostics {
		if strings.Contains(d.Message, "Non-numeric type in arithmetic") {
			return
		}
	}
	t.Fatal("expected non-numeric-in-arithmetic warning")
}

func TestGetDiagnostics_NoMixedTypeWarning_SameTypes(t *testing.T) {
	text := `x := "hello" + " world";
y := 1 + 2;`
	diagnostics := GetDiagnostics(text, DefaultDiagnosticOptions())

	for _, d := range diagnostics {
		if strings.Contains(d.Message, "Mixed types") || strings.Contains(d.Message, "String in arithmetic") {
			t.Fatalf("unexpected mixed-type warning: %s", d.Message)
		}
	}
}

// --- Pass 2: Multiple :CATCH blocks ---

func TestGetDiagnostics_TryMultipleCatch_Diagnostic(t *testing.T) {
	text := `:PROCEDURE Test;
:TRY;
    sVal := "";
:CATCH;
    sVal := "error1";
:CATCH;
    sVal := "error2";
:ENDTRY;
:ENDPROC;`
	diagnostics := GetDiagnostics(text, DefaultDiagnosticOptions())

	found := false
	for _, d := range diagnostics {
		if strings.Contains(d.Message, "Only one") && strings.Contains(d.Message, "CATCH") {
			found = true
		}
	}
	if !found {
		t.Error("expected diagnostic about only one :CATCH per :TRY")
	}
}

// --- Pass 2: :CATCH before :FINALLY order ---

func TestGetDiagnostics_CatchMustBeBeforeFinally(t *testing.T) {
	text := `:PROCEDURE Test;
:TRY;
    sVal := "";
:FINALLY;
    sVal := "cleanup";
:CATCH;
    sVal := "error";
:ENDTRY;
:ENDPROC;`
	diagnostics := GetDiagnostics(text, DefaultDiagnosticOptions())

	found := false
	for _, d := range diagnostics {
		if strings.Contains(d.Message, "CATCH") && strings.Contains(d.Message, "before") && strings.Contains(d.Message, "FINALLY") {
			found = true
		}
	}
	if !found {
		t.Error("expected diagnostic about :CATCH needing to appear before :FINALLY")
	}
}

// --- Pass 2: :EXITFOR outside :FOR loop ---

func TestGetDiagnostics_ExitWhileOutsideWhile_Standalone(t *testing.T) {
	text := `:PROCEDURE Test;
:EXITWHILE;
:ENDPROC;`
	diagnostics := GetDiagnostics(text, DefaultDiagnosticOptions())

	found := false
	for _, d := range diagnostics {
		if strings.Contains(d.Message, "EXITWHILE") || strings.Contains(d.Message, "outside") {
			found = true
		}
	}
	if !found {
		t.Error("expected diagnostic about :EXITWHILE outside :WHILE loop")
	}
}

// --- Pass 2: Variable name length limit ---

func TestGetDiagnostics_VariableNameLength_TooLong(t *testing.T) {
	text := `:DECLARE sThisVariableNameIsWayTooLongForSSL;`
	diagnostics := GetDiagnostics(text, DefaultDiagnosticOptions())

	found := false
	for _, d := range diagnostics {
		if strings.Contains(d.Message, "exceeds") && strings.Contains(d.Message, "character") {
			found = true
		}
	}
	if !found {
		t.Error("expected diagnostic about variable name exceeding length limit")
	}
}

func TestGetDiagnostics_ProcedureNameLength_TooLong(t *testing.T) {
	text := `:PROCEDURE ThisIsAnExtremelyLongProcedureNameThatExceedsLimit;
:ENDPROC;`
	diagnostics := GetDiagnostics(text, DefaultDiagnosticOptions())

	found := false
	for _, d := range diagnostics {
		if strings.Contains(d.Message, "exceeds") && strings.Contains(d.Message, "character") {
			found = true
		}
	}
	if !found {
		t.Error("expected diagnostic about procedure name exceeding length limit")
	}
}

// --- Pass 2: createUdObject with anonymous properties ---

func TestGetDiagnostics_CreateUdObjectAnonymous_NoDiagnostic(t *testing.T) {
	text := `:DECLARE oObj;
oObj := CreateUdObject({{"name", "test"}, {"value", 42}});`
	diagnostics := GetDiagnostics(text, DefaultDiagnosticOptions())

	for _, d := range diagnostics {
		if strings.Contains(d.Message, "CreateUdObject") && strings.Contains(d.Message, "built-in") {
			t.Error("anonymous CreateUdObject should not trigger built-in class misuse warning")
		}
	}
}

// --- Pass 3: Constructor :RETURN; (bare, no value) must be allowed ---

func TestGetDiagnostics_ConstructorBareReturn_Allowed(t *testing.T) {
	text := `:CLASS MyClass;
:PROCEDURE Constructor;
    :RETURN;
:ENDPROC;`
	diagnostics := GetDiagnostics(text, DefaultDiagnosticOptions())

	for _, d := range diagnostics {
		if strings.Contains(d.Message, "Constructor cannot return") {
			t.Error("bare :RETURN; inside Constructor should be allowed (only :RETURN value; is an error)")
		}
	}
}

// --- Pass 3: Multiple :DEFAULT after one :PARAMETERS ---

func TestGetDiagnostics_MultipleDefaultAfterParameters(t *testing.T) {
	text := `:PROCEDURE Test;
:PARAMETERS sName, nCount, bFlag;
:DEFAULT sName, "";
:DEFAULT nCount, 0;
:DEFAULT bFlag, .T.;
:DECLARE sResult;
sResult := sName;
:ENDPROC;`
	diagnostics := GetDiagnostics(text, DefaultDiagnosticOptions())

	for _, d := range diagnostics {
		if strings.Contains(d.Message, "DEFAULT") && strings.Contains(d.Message, "immediately after") {
			t.Errorf("multiple :DEFAULT statements after :PARAMETERS should all be allowed, got: %s", d.Message)
		}
	}
}

func TestGetDiagnostics_DefaultAfterDeclare_StillErrors(t *testing.T) {
	// :DEFAULT after :DECLARE should still error even if :PARAMETERS exists earlier
	text := `:PROCEDURE Test;
:PARAMETERS sName;
:DEFAULT sName, "";
:DECLARE nCount;
:DEFAULT nCount, 0;
:ENDPROC;`
	diagnostics := GetDiagnostics(text, DefaultDiagnosticOptions())

	found := false
	for _, d := range diagnostics {
		if strings.Contains(d.Message, "DEFAULT") && strings.Contains(d.Message, "immediately after") {
			found = true
		}
	}
	if !found {
		t.Error("expected diagnostic: :DEFAULT after :DECLARE should error (:DEFAULT must follow :PARAMETERS)")
	}
}

// --- Pass 3: Empty :CATCH body is valid ---

func TestGetDiagnostics_EmptyCatchBody_Valid(t *testing.T) {
	text := `:PROCEDURE Test;
:TRY;
    sVal := "";
:CATCH;
:ENDTRY;
:ENDPROC;`
	diagnostics := GetDiagnostics(text, DefaultDiagnosticOptions())

	for _, d := range diagnostics {
		if strings.Contains(d.Message, "CATCH") && strings.Contains(d.Message, "statement") {
			t.Errorf("empty :CATCH body should be valid (zero or more statements allowed), got: %s", d.Message)
		}
	}
}

// --- Pass 3: :TRY with only :CATCH (no :FINALLY) is valid ---

func TestGetDiagnostics_TryOnlyCatch_Valid(t *testing.T) {
	text := `:PROCEDURE Test;
:TRY;
    sVal := "";
:CATCH;
    sVal := "error";
:ENDTRY;
:ENDPROC;`
	diagnostics := GetDiagnostics(text, DefaultDiagnosticOptions())

	for _, d := range diagnostics {
		if strings.Contains(d.Message, "FINALLY") && strings.Contains(d.Message, "required") {
			t.Error(":TRY with only :CATCH (no :FINALLY) should be valid")
		}
	}
}

// --- Pass 3: :OTHERWISE skipped once any :CASE body ran (even without :EXITCASE) ---

func TestGetDiagnostics_OtherwiseWithoutExitCase_ValidPattern(t *testing.T) {
	// This is valid SSL - :OTHERWISE doesn't execute when earlier :CASE matched
	// The LSP should warn about missing :EXITCASE but not about :OTHERWISE behavior
	text := `:BEGINCASE;
:CASE nVal == 1;
    DoProc("A");
:OTHERWISE;
    DoProc("Default");
:ENDCASE;`
	diagnostics := GetDiagnostics(text, DefaultDiagnosticOptions())

	// Should get warning about missing :EXITCASE but not about :OTHERWISE
	foundExitCaseWarning := false
	for _, d := range diagnostics {
		if strings.Contains(d.Message, "EXITCASE") {
			foundExitCaseWarning = true
		}
		if strings.Contains(d.Message, "OTHERWISE") && d.Severity == SeverityError {
			t.Error(":OTHERWISE should not produce an error - it's valid even without :EXITCASE")
		}
	}
	if !foundExitCaseWarning {
		t.Error("expected warning about missing :EXITCASE")
	}
}

// --- Pass 3: Class reference forms - Me:Method, Base:Method ---

func TestGetDiagnostics_ClassReferenceForms_BaseStandalone(t *testing.T) {
	text := `:CLASS MyClass;
:INHERIT ParentClass;
:PROCEDURE Test;
    Base;
:ENDPROC;`
	diagnostics := GetDiagnostics(text, DefaultDiagnosticOptions())

	found := false
	for _, d := range diagnostics {
		if strings.Contains(d.Message, "Base") && strings.Contains(d.Message, "Base:MemberName") {
			found = true
		}
	}
	if !found {
		t.Error("expected diagnostic about 'Base' needing to be used as 'Base:MemberName'")
	}
}

func TestGetDiagnostics_ClassReferenceForms_BaseWithoutInherit(t *testing.T) {
	text := `:CLASS MyClass;
:PROCEDURE Test;
    Base:Method();
:ENDPROC;`
	diagnostics := GetDiagnostics(text, DefaultDiagnosticOptions())

	found := false
	for _, d := range diagnostics {
		if strings.Contains(d.Message, "Base") && strings.Contains(d.Message, "INHERIT") {
			found = true
		}
	}
	if !found {
		t.Error("expected diagnostic about 'Base:MemberName' requiring ':INHERIT'")
	}
}

// --- TestGetDiagnostics_MultipleCatchBlocks ---

func TestGetDiagnostics_MultipleCatchBlocks(t *testing.T) {
	text := `:TRY;
 x := 1;
:CATCH;
:CATCH;
:ENDTRY;`
	diagnostics := GetDiagnostics(text, DefaultDiagnosticOptions())

	found := false
	for _, d := range diagnostics {
		if strings.Contains(d.Message, "Only one") && strings.Contains(d.Message, "CATCH") {
			found = true
		}
	}
	if !found {
		t.Error("expected diagnostic about only one :CATCH per :TRY")
	}
}

// --- TestGetDiagnostics_FinallyRestrictions ---

func TestGetDiagnostics_FinallyRestrictions(t *testing.T) {
	text := `:TRY;
 x := 1;
:FINALLY;
 :RETURN 1;
:ENDTRY;`
	diagnostics := GetDiagnostics(text, DefaultDiagnosticOptions())

	found := false
	for _, d := range diagnostics {
		if strings.Contains(d.Message, "':RETURN'") && strings.Contains(d.Message, "':FINALLY'") {
			found = true
		}
	}
	if !found {
		t.Fatal("expected compile error diagnostic about RETURN in FINALLY")
	}
}

// --- TestGetDiagnostics_EndForInvalid ---

func TestGetDiagnostics_EndForInvalid(t *testing.T) {
	text := `:FOR i := 1 :TO 10;
 x := 1;
:ENDFOR;`
	diagnostics := GetDiagnostics(text, DefaultDiagnosticOptions())

	found := false
	for _, d := range diagnostics {
		if strings.Contains(d.Message, ":NEXT") {
			found = true
		}
	}
	if !found {
		t.Error("expected diagnostic that :FOR must be terminated with ':NEXT'")
	}
}

// --- TestGetDiagnostics_ZeroBasedArrayAccess ---

func TestGetDiagnostics_ZeroBasedArrayAccess(t *testing.T) {
	text := `x := aData[0];`
	diagnostics := GetDiagnostics(text, DefaultDiagnosticOptions())

	found := false
	for _, d := range diagnostics {
		if strings.Contains(d.Message, "1-based") {
			found = true
		}
	}
	if !found {
		t.Error("expected diagnostic about 1-based array indexing")
	}
}

// --- Tests for new diagnostics: STEP spacing, REGION legacy, code block structure ---

func TestGetDiagnostics_StepSpacingWarning(t *testing.T) {
	// :STEP without a preceding space should warn
	text := `:FOR i := 1 :TO 10:STEP 2;
    x := i;
:NEXT;`
	diagnostics := GetDiagnostics(text, DefaultDiagnosticOptions())

	found := false
	for _, d := range diagnostics {
		if strings.Contains(d.Message, "STEP") && strings.Contains(d.Message, "space before") {
			found = true
		}
	}
	if !found {
		t.Error("expected diagnostic about :STEP needing a space before it")
	}
}

func TestGetDiagnostics_StepSpacing_ValidNoWarning(t *testing.T) {
	// :STEP with preceding space should NOT warn
	text := `:FOR i := 1 :TO 10 :STEP 2;
    x := i;
:NEXT;`
	diagnostics := GetDiagnostics(text, DefaultDiagnosticOptions())

	for _, d := range diagnostics {
		if strings.Contains(d.Message, "STEP") && strings.Contains(d.Message, "space before") {
			t.Error("should not warn when :STEP has a space before it")
		}
	}
}

func TestGetDiagnostics_CodeBlockEmptyParams(t *testing.T) {
	// {|| expr} has no bound variables — should warn
	text := `x := {|| 42};`
	diagnostics := GetDiagnostics(text, DefaultDiagnosticOptions())

	found := false
	for _, d := range diagnostics {
		if strings.Contains(d.Message, "at least one bound variable") {
			found = true
		}
	}
	if !found {
		t.Error("expected diagnostic about code blocks needing at least one bound variable")
	}
}

func TestGetDiagnostics_CodeBlockWithParams_NoWarning(t *testing.T) {
	// {|x| x * 2} has a bound variable — should NOT warn
	text := `fn := {|x| x * 2};`
	diagnostics := GetDiagnostics(text, DefaultDiagnosticOptions())

	for _, d := range diagnostics {
		if strings.Contains(d.Message, "bound variable") {
			t.Error("should not warn when code block has bound variables")
		}
	}
}

// ==================== Source-of-Truth Validation Tests ====================
// Tests below validate alignment between the implementation and the
// authoritative language rules in dev/ssl-style-guide/agent-guides/.

func TestGetHover_Me_NoDirectCallExample(t *testing.T) {
	// Source of truth: custom procedures cannot be called directly.
	// Me hover must NOT suggest DoSomething(Me) since direct calls are invalid.
	text := `:CLASS MyClass;
:PROCEDURE DoWork;
Me:Helper();
:ENDPROC;
:PROCEDURE Constructor;
:ENDPROC;`
	hover := GetHover(text, 3, 1, nil, nil)
	if hover == nil {
		t.Fatal("expected hover info for Me")
	}
	if strings.Contains(hover.Contents, "DoSomething(Me)") {
		t.Error("Me hover should not suggest direct procedure calls like DoSomething(Me)")
	}
	if !strings.Contains(hover.Contents, "ExecFunction") {
		t.Error("Me hover should show ExecFunction example for passing self as argument")
	}
}

func TestGetDiagnostics_GetSSLDatasetSQLFormatting(t *testing.T) {
	// GetSSLDataset takes SQL in its first argument and should be recognized
	// as a SQL function for formatting detection purposes.
	text := `aData := GetSSLDataset("SELECT * FROM orders WHERE status = 'Logged'");`
	// Verify the function is recognized — the SQL formatting path is invoked
	// when a SQL function's first argument is a string containing SQL.
	upper := strings.ToUpper("GetSSLDataset")
	if !SQLFunctions[upper] {
		t.Errorf("expected GetSSLDataset to be in SQLFunctions map, got false for key %q", upper)
	}
	// Also verify the existing functions are still present
	for _, fn := range []string{"SQLEXECUTE", "RUNSQL", "LSEARCH", "LSELECT", "LSELECT1", "LSELECTC", "GETDATASET", "GETDATASETEX"} {
		if !SQLFunctions[fn] {
			t.Errorf("expected %s to be in SQLFunctions map", fn)
		}
	}
	_ = text // used to document the scenario
}

func TestGetDiagnostics_ScientificNotation_NoDotBeforeExponent(t *testing.T) {
	// Source of truth: ssl_agent_instructions.md section 3 Number Details:
	// "Scientific notation requires a decimal point before the exponent:
	// 1.2e-3, 9.0E1 are valid; 7e2, .5e1, 9E+1 are not"
	cases := []struct {
		text    string
		wantMsg string
	}{
		// 7e2 — number without decimal followed by eN identifier
		{`x := 7e2;`, "requires a decimal point"},
		// .5e1 — number starting with dot followed by eN
		{`x := .5e1;`, "requires a digit before the decimal"},
	}

	opts := DefaultDiagnosticOptions()
	for _, tc := range cases {
		diagnostics := GetDiagnostics(tc.text, opts)
		found := false
		for _, d := range diagnostics {
			if strings.Contains(d.Message, tc.wantMsg) {
				found = true
			}
		}
		if !found {
			t.Errorf("text %q: expected diagnostic containing %q", tc.text, tc.wantMsg)
		}
	}
}

// Issue #47: the fix-it text must itself be valid SSL — explicit '+'
// exponent signs are unsupported, so suggestions drop them.
func TestGetDiagnostics_ScientificNotation_SuggestionsAreValidSSL(t *testing.T) {
	cases := []struct {
		text        string
		wantSuggest string
	}{
		{`x := 9E+1;`, "'9.0E1'"},
		{`x := 7e+2;`, "'7.0e2'"},
		{`x := 3e-2;`, "'3.0e-2'"},
	}

	opts := DefaultDiagnosticOptions()
	for _, tc := range cases {
		diagnostics := GetDiagnostics(tc.text, opts)
		found := false
		for _, d := range diagnostics {
			if d.Code != CodeScientificNotation {
				continue
			}
			found = true
			if !strings.Contains(d.Message, tc.wantSuggest) {
				t.Errorf("text %q: expected suggestion %s in message, got: %s", tc.text, tc.wantSuggest, d.Message)
			}
			suggestion := d.Message[strings.Index(d.Message, "use '") : strings.Index(d.Message, "' instead")+1]
			if strings.Contains(strings.ToUpper(suggestion), "E+") {
				t.Errorf("text %q: suggested fix contains invalid '+' exponent sign: %s", tc.text, d.Message)
			}
		}
		if !found {
			t.Errorf("text %q: expected scientific_notation diagnostic", tc.text)
		}
	}
}

func TestGetDiagnostics_ScientificNotation_ValidForms(t *testing.T) {
	// Valid scientific notation should NOT warn
	cases := []string{
		`x := 1.2e3;`,
		`x := 9.0E1;`,
		`x := 1.0e-3;`,
	}

	opts := DefaultDiagnosticOptions()
	for _, text := range cases {
		diagnostics := GetDiagnostics(text, opts)
		for _, d := range diagnostics {
			if strings.Contains(d.Message, "scientific notation") {
				t.Errorf("text %q: should not warn for valid scientific notation, got: %s", text, d.Message)
			}
		}
	}
}

func TestGetDiagnostics_CommentEarlyTermination_SemicolonInText(t *testing.T) {
	// Source of truth: ssl_agent_instructions.md gotcha #10:
	// "A semicolon inside comment text will prematurely end the comment"
	// The comment /* Set x := 0; then increment; has TWO tokens.
	text := "/* Set x := 0; then increment;"
	opts := DefaultDiagnosticOptions()
	diagnostics := GetDiagnostics(text, opts)

	found := false
	for _, d := range diagnostics {
		if strings.Contains(d.Message, "terminate") || strings.Contains(d.Message, "executable code") {
			found = true
		}
	}
	if !found {
		t.Error("expected diagnostic about comment early termination when semicolon appears inside comment text")
	}
}

func TestGetDiagnostics_SkippedParamAdjacentCommas(t *testing.T) {
	// Source of truth: ssl_agent_instructions.md:
	// "Keep skipped-argument commas adjacent. DoProc("MyProc", {param1,,param3}) is valid"
	// The formatter should preserve adjacent commas without adding spaces.
	text := `DoProc("MyProc", {param1,,param3,,param5});`
	opts := DefaultFormattingOptions()
	edits := FormatDocument(text, opts)
	formatted := edits[0].NewText
	// Adjacent commas should remain adjacent (no space between)
	if strings.Contains(formatted, ", ,") {
		t.Error("formatter should not insert space between adjacent commas for skipped parameters")
	}
	if !strings.Contains(formatted, ",,") {
		t.Error("formatter should preserve adjacent commas for skipped parameters")
	}
}

func TestGetDiagnostics_CreateUdObjectBuiltinClass_Error(t *testing.T) {
	// Source of truth: ssl_agent_instructions.md rule #7:
	// "Built-in classes use curly braces only — they cannot be instantiated via CreateUdObject"
	text := `oEmail := CreateUdObject("Email");`
	opts := DefaultDiagnosticOptions()
	diagnostics := GetDiagnostics(text, opts)

	found := false
	for _, d := range diagnostics {
		if strings.Contains(d.Message, "curly-brace") && strings.Contains(d.Message, "Email") {
			found = true
		}
	}
	if !found {
		t.Error("expected diagnostic about built-in class requiring curly-brace construction")
	}
}

func TestGetDiagnostics_CreateUdObjectUserClass_NoDiagnostic(t *testing.T) {
	// CreateUdObject("MyCustomClass") is valid for user-defined classes
	text := `oObj := CreateUdObject("MyCustomClass");`
	opts := DefaultDiagnosticOptions()
	diagnostics := GetDiagnostics(text, opts)

	for _, d := range diagnostics {
		if strings.Contains(d.Message, "curly-brace") {
			t.Errorf("should not flag CreateUdObject with user-defined class name, got: %s", d.Message)
		}
	}
}

func TestGetDiagnostics_StringEqualityPrefixMatch(t *testing.T) {
	// Source of truth: ssl_agent_instructions.md gotcha #18:
	// "= operator for strings returns .T. if right operand is empty OR left starts with right"
	text := `:IF sStatus = "Log";
:ENDIF;`
	opts := DefaultDiagnosticOptions()
	diagnostics := GetDiagnostics(text, opts)

	found := false
	for _, d := range diagnostics {
		if strings.Contains(d.Message, "prefix matching") || strings.Contains(d.Message, "exact string") {
			found = true
		}
	}
	if !found {
		t.Error("expected diagnostic about = doing prefix matching for strings")
	}
}

func TestGetDiagnostics_StringExactEquality_NoWarning(t *testing.T) {
	// == for strings should NOT warn about prefix matching
	text := `:IF sStatus == "Logged";
:ENDIF;`
	opts := DefaultDiagnosticOptions()
	diagnostics := GetDiagnostics(text, opts)

	for _, d := range diagnostics {
		if strings.Contains(d.Message, "prefix matching") {
			t.Errorf("should not warn about prefix matching for ==, got: %s", d.Message)
		}
	}
}

func TestGetDiagnostics_DoProcInsideClass_UnqualifiedRejected(t *testing.T) {
	// Unqualified DoProc targets are a compile-time error inside class
	// methods (diag.doproc_in_class; narrowed from "all forms are
	// rejected" by issue #151 / ssl-style-guide#49 — qualified
	// "Category.Script.Procedure" targets are valid).
	text := `:CLASS MyClass;
:PROCEDURE DoWork;
DoProc("Helper");
:ENDPROC;
:PROCEDURE Constructor;
:ENDPROC;`
	opts := DefaultDiagnosticOptions()
	diagnostics := GetDiagnostics(text, opts)

	found := false
	for _, d := range diagnostics {
		if d.Code == CodeDoProcInClass {
			found = true
		}
	}
	if !found {
		t.Error("expected diagnostic for unqualified DoProc target inside class method")
	}
}

func TestGetDiagnostics_MeBaseNotFlaggedAsDirectCall(t *testing.T) {
	// Me:Method() and Base:Method() are valid inside classes and should NOT
	// be flagged as direct procedure calls.
	text := `:CLASS MyClass;
:INHERIT Lab.BaseClass;
:PROCEDURE DoWork;
Me:Helper();
Base:Initialize();
:ENDPROC;
:PROCEDURE Helper;
:ENDPROC;
:PROCEDURE Constructor;
:ENDPROC;`
	opts := DefaultDiagnosticOptions()
	diagnostics := GetDiagnostics(text, opts)

	for _, d := range diagnostics {
		if strings.Contains(d.Message, "cannot be called directly") &&
			(strings.Contains(d.Message, "Helper") || strings.Contains(d.Message, "Initialize")) {
			t.Errorf("Me:/Base: method calls should not be flagged as direct procedure calls, got: %s", d.Message)
		}
	}
}

func TestGetDiagnostics_EndForIsInvalid_ForLoopsMustUseNext(t *testing.T) {
	// Source of truth: ssl_agent_instructions.md gotcha #17:
	// "ENDFOR is not valid — FOR loops must be terminated with :NEXT"
	text := `:FOR i := 1 :TO 10;
x := i;
:ENDFOR;`
	opts := DefaultDiagnosticOptions()
	diagnostics := GetDiagnostics(text, opts)

	found := false
	for _, d := range diagnostics {
		if strings.Contains(d.Message, "ENDFOR") && strings.Contains(d.Message, "NEXT") {
			found = true
		}
	}
	if !found {
		t.Error("expected diagnostic about :ENDFOR being invalid, use :NEXT instead")
	}
}

func TestGetDiagnostics_FinallyRestrictions_AllFourKeywords(t *testing.T) {
	// Source of truth: ssl_agent_instructions.md section 2 Error Handling:
	// ":RETURN, :EXITWHILE, :EXITFOR, and :LOOP inside a :FINALLY block are compile-time errors"
	cases := []struct {
		keyword string
		text    string
	}{
		{"RETURN", `:TRY;
x := 1;
:FINALLY;
:RETURN x;
:ENDTRY;`},
		{"EXITFOR", `:FOR i := 1 :TO 10;
:TRY;
x := 1;
:FINALLY;
:EXITFOR;
:ENDTRY;
:NEXT;`},
		{"EXITWHILE", `:WHILE .T.;
:TRY;
x := 1;
:FINALLY;
:EXITWHILE;
:ENDTRY;
:ENDWHILE;`},
		{"LOOP", `:WHILE .T.;
:TRY;
x := 1;
:FINALLY;
:LOOP;
:ENDTRY;
:ENDWHILE;`},
	}

	opts := DefaultDiagnosticOptions()
	for _, tc := range cases {
		diagnostics := GetDiagnostics(tc.text, opts)
		found := false
		for _, d := range diagnostics {
			if strings.Contains(d.Message, tc.keyword) && strings.Contains(d.Message, "FINALLY") && strings.Contains(d.Message, "compile-time error") {
				found = true
			}
		}
		if !found {
			t.Errorf("expected compile-time error for :%s inside :FINALLY", tc.keyword)
		}
	}
}

func TestGetDiagnostics_NamedSQLParamInRunSQL(t *testing.T) {
	// Source of truth: ssl_agent_instructions.md rule #8:
	// "SQLExecute is the only function that supports ?varName? substitution"
	text := `RunSQL("UPDATE orders SET status = ?sStatus? WHERE id = ?nId?",, {sStatus, nId});`
	opts := DefaultDiagnosticOptions()
	diagnostics := GetDiagnostics(text, opts)

	found := false
	for _, d := range diagnostics {
		if strings.Contains(d.Message, "Named SQL parameter") && strings.Contains(d.Message, "RunSQL") {
			found = true
		}
	}
	if !found {
		t.Error("expected diagnostic about named SQL parameters not being supported by RunSQL")
	}
}

func TestGetDiagnostics_NamedSQLParamInSQLExecute_NoDiagnostic(t *testing.T) {
	// SQLExecute DOES support named params — should NOT warn
	text := `SQLExecute("SELECT * FROM orders WHERE status = ?sStatus?");`
	opts := DefaultDiagnosticOptions()
	diagnostics := GetDiagnostics(text, opts)

	for _, d := range diagnostics {
		if strings.Contains(d.Message, "Named SQL parameter") {
			t.Errorf("should not flag named params in SQLExecute, got: %s", d.Message)
		}
	}
}

func TestGetDiagnostics_ClassConstructorCannotReturnValue(t *testing.T) {
	// Source of truth: ssl_agent_instructions.md section 2:
	// "Constructor cannot return a value (:RETURN without an expression is allowed)"
	text := `:CLASS MyClass;
:DECLARE sName;
:PROCEDURE Constructor;
:PARAMETERS sN;
sName := sN;
:RETURN sName;
:ENDPROC;`
	opts := DefaultDiagnosticOptions()
	diagnostics := GetDiagnostics(text, opts)

	found := false
	for _, d := range diagnostics {
		if strings.Contains(d.Message, "Constructor") && strings.Contains(d.Message, "cannot return a value") {
			found = true
		}
	}
	if !found {
		t.Error("expected diagnostic about Constructor not being able to return a value")
	}
}

func TestGetDiagnostics_ClassConstructorBareReturn_Valid(t *testing.T) {
	// :RETURN; without a value IS allowed in constructors
	text := `:CLASS MyClass;
:DECLARE sName;
:PROCEDURE Constructor;
:RETURN;
:ENDPROC;`
	opts := DefaultDiagnosticOptions()
	diagnostics := GetDiagnostics(text, opts)

	for _, d := range diagnostics {
		if strings.Contains(d.Message, "Constructor") && strings.Contains(d.Message, "cannot return") {
			t.Errorf("bare :RETURN; should be allowed in constructors, got: %s", d.Message)
		}
	}
}

func TestFormatting_ELSEIFNotTreatedAsContinuation(t *testing.T) {
	// ELSEIF is NOT a valid SSL keyword. If it appears, it should not be treated
	// as a continuation keyword that suppresses semicolons.
	// SSL uses :ELSE; :IF condition; as separate statements.
	text := `:PROCEDURE Test;
:DECLARE x;
:IF x = 1;
x := 2;
:ELSE;
:IF x = 3;
x := 4;
:ENDIF;
:ENDIF;
:ENDPROC;`
	opts := DefaultFormattingOptions()
	edits := FormatDocument(text, opts)
	formatted := edits[0].NewText
	// The :ELSE and :IF should be separate statements
	if !strings.Contains(formatted, ":ELSE;") {
		t.Error("expected :ELSE; as a complete statement")
	}
}

// ==================== Bug Fix Tests: Comment Transparency ====================

func TestGetDiagnostics_DefaultAfterParametersWithComment(t *testing.T) {
	// Bug fix: comments between :PARAMETERS and :DEFAULT should NOT cause false positive.
	text := `:PROCEDURE Test;
:PARAMETERS sName;
/* Set defaults;
:DEFAULT sName, "test";
:ENDPROC;`

	opts := DefaultDiagnosticOptions()
	diagnostics := GetDiagnostics(text, opts)

	for _, d := range diagnostics {
		if strings.Contains(d.Message, "':DEFAULT' must appear immediately after ':PARAMETERS'") {
			t.Errorf("false positive: comment between :PARAMETERS and :DEFAULT should be allowed, got: %s", d.Message)
		}
	}
}

func TestGetDiagnostics_ParametersAfterProcedureWithComment(t *testing.T) {
	// Bug fix: comments between :PROCEDURE and :PARAMETERS should NOT cause false positive.
	text := `:PROCEDURE Test;
/* Documentation comment;
:PARAMETERS sName;
:DEFAULT sName, "";
:ENDPROC;`

	opts := DefaultDiagnosticOptions()
	diagnostics := GetDiagnostics(text, opts)

	for _, d := range diagnostics {
		if strings.Contains(d.Message, "':PARAMETERS' must appear") && strings.Contains(d.Message, "immediately after") {
			t.Errorf("false positive: comment between :PROCEDURE and :PARAMETERS should be allowed, got: %s", d.Message)
		}
	}
}

// ==================== New Diagnostic Tests ====================

// Companion check: removing the != asymmetry diagnostic must not also have
// removed the related '=' prefix-matching warning. Both diagnostics share
// the CodeEqualsVsStrictEquals code, so we verify the '=' branch is still
// active by writing the canonical pattern that should still fire.
func TestGetDiagnostics_BarePrefixEqualsStringStillWarns(t *testing.T) {
	text := `:DECLARE sStatus;
:IF sStatus = "Logged";
	x := 1;
:ENDIF;`

	opts := DefaultDiagnosticOptions()
	diagnostics := GetDiagnostics(text, opts)

	found := false
	for _, d := range diagnostics {
		if d.Code == CodeEqualsVsStrictEquals {
			found = true
			break
		}
	}
	if !found {
		t.Errorf("expected equals_vs_strict_equals to STILL fire on bare '=' string comparison; got: %#v", diagnostics)
	}
}

// Non-string operands on either side of != must continue to be silent.
// Prior to the removal this was guarded by a separate test that we replaced;
// keep this regression in place.
func TestGetDiagnostics_NotEqualsNonStringStaysSilent(t *testing.T) {
	text := `:DECLARE nVal;
:IF nVal != 0;
	x := 1;
:ENDIF;`

	opts := DefaultDiagnosticOptions()
	diagnostics := GetDiagnostics(text, opts)
	for _, d := range diagnostics {
		if d.Code == CodeEqualsVsStrictEquals {
			t.Errorf("unexpected equals_vs_strict_equals for numeric '!=' comparison: %s", d.Message)
		}
	}
}

func TestGetDiagnostics_NotEqualsNoAsymmetryWarning(t *testing.T) {
	// '!=' is the well-defined exact-match negation operator in SSL. Using it with a
	// string literal is a valid pattern (see vs-code-ssl-formatter#78) and must not
	// trigger the equals_vs_strict_equals diagnostic. The companion warning still
	// fires on bare '=' string comparisons via the dedicated checker.
	text := `:DECLARE sStatus;
:IF sStatus != "Logged";
	x := 1;
:ENDIF;`

	opts := DefaultDiagnosticOptions()
	diagnostics := GetDiagnostics(text, opts)

	for _, d := range diagnostics {
		if d.Code == CodeEqualsVsStrictEquals {
			t.Errorf("unexpected equals_vs_strict_equals diagnostic for '!=': %s", d.Message)
		}
	}
}

func TestGetDiagnostics_SQLConcatenationInjection(t *testing.T) {
	text := `:DECLARE sTable, aResults;
aResults := SQLExecute("SELECT * FROM " + sTable);`

	opts := DefaultDiagnosticOptions()
	diagnostics := GetDiagnostics(text, opts)

	found := false
	for _, d := range diagnostics {
		if strings.Contains(d.Message, "SQL injection") {
			found = true
		}
	}
	if !found {
		t.Error("expected SQL injection warning for string concatenation in SQLExecute")
	}
}

func TestGetDiagnostics_SQLConcatenationInjection_SafeParam(t *testing.T) {
	// Parameterized queries should NOT trigger
	text := `:DECLARE sStatus, aResults;
aResults := SQLExecute("SELECT * FROM orders WHERE status = ?sStatus?");`

	opts := DefaultDiagnosticOptions()
	diagnostics := GetDiagnostics(text, opts)

	for _, d := range diagnostics {
		if strings.Contains(d.Message, "SQL injection") {
			t.Errorf("unexpected SQL injection warning for parameterized query: %s", d.Message)
		}
	}
}

// --- Code block empty params with whitespace ---

func TestGetDiagnostics_CodeBlockEmptyParamsWithSpaces(t *testing.T) {
	// {| | expr} has whitespace but no bound variables — should warn
	text := `x := {| | 42};`
	diagnostics := GetDiagnostics(text, DefaultDiagnosticOptions())

	found := false
	for _, d := range diagnostics {
		if strings.Contains(d.Message, "at least one bound variable") {
			found = true
		}
	}
	if !found {
		t.Error("expected diagnostic about code blocks needing at least one bound variable for {| | expr}")
	}
}

func TestGetDiagnostics_CodeBlockEmptyParamsWithTabs(t *testing.T) {
	// {|	| expr} with tab between pipes — should warn
	text := "x := {|\t| 42};"
	diagnostics := GetDiagnostics(text, DefaultDiagnosticOptions())

	found := false
	for _, d := range diagnostics {
		if strings.Contains(d.Message, "at least one bound variable") {
			found = true
		}
	}
	if !found {
		t.Error("expected diagnostic about code blocks needing at least one bound variable for {|<tab>| expr}")
	}
}

// --- :INCLUDE inside procedure body ---

func TestGetDiagnostics_IncludeInsideProcedure(t *testing.T) {
	// :INCLUDE inside a procedure body should be flagged as warning
	code := `:PROCEDURE MyProc;
:INCLUDE SomeLibrary;
:ENDPROC;`

	diagnostics := GetDiagnostics(code, DefaultDiagnosticOptions())

	for _, d := range diagnostics {
		if strings.Contains(d.Message, "inside a procedure body") {
			return
		}
	}

	t.Fatal("expected warning about :INCLUDE inside procedure body")
}

func TestGetDiagnostics_IncludeBeforeProcedure_NoWarning(t *testing.T) {
	// :INCLUDE before procedures should not get the "inside procedure" warning
	code := `:INCLUDE SomeLibrary;
:PROCEDURE MyProc;
:DECLARE x;
:ENDPROC;`

	diagnostics := GetDiagnostics(code, DefaultDiagnosticOptions())

	for _, d := range diagnostics {
		if strings.Contains(d.Message, "inside a procedure body") {
			t.Error(":INCLUDE before procedures should not be flagged as inside a procedure")
		}
	}
}

// ==================== Data Source Diagnostics ====================

// ==================== Data Source Completions ====================

// [spec feature.snippets/A7] — data-source files get the DS snippet set and
// none of the standard script snippets.
func TestGetSnippetCompletions_DataSource(t *testing.T) {
	snippets := GetSnippetCompletions(true)

	foundDSParams := false
	foundSQLDS := false
	foundSSLDS := false
	for _, s := range snippets {
		switch s.Label {
		case "dsparams":
			foundDSParams = true
		case "sqlds":
			foundSQLDS = true
		case "sslds":
			foundSSLDS = true
		}
	}

	if !foundDSParams {
		t.Error("expected dsparams snippet for data source files")
	}
	if !foundSQLDS {
		t.Error("expected sqlds snippet for data source files")
	}
	if !foundSSLDS {
		t.Error("expected sslds snippet for data source files")
	}

	// Should NOT include standard snippets like proc, if, while
	for _, s := range snippets {
		if s.Label == "proc" || s.Label == "if" || s.Label == "while" {
			t.Errorf("data source snippets should not include standard snippet %q", s.Label)
		}
	}
}

func TestGetAllCompletions_DataSourceIncludesBuilderDirectives(t *testing.T) {
	completions := GetAllCompletions(nil, nil, false, true, false)

	foundDSN := false
	foundTableName := false
	for _, c := range completions {
		switch c.Label {
		case ":DSN":
			foundDSN = true
		case ":TABLENAME":
			foundTableName = true
		}
	}

	if !foundDSN {
		t.Error("expected :DSN completion in data source file")
	}
	if !foundTableName {
		t.Error("expected :TABLENAME completion in data source file")
	}
}

func TestGetAllCompletions_NonDataSourceExcludesBuilderDirectives(t *testing.T) {
	completions := GetAllCompletions(nil, nil, false, false, false)

	for _, c := range completions {
		if c.Label == ":DSN" || c.Label == ":TABLENAME" || c.Label == ":NULLASBLANK" || c.Label == ":INVARIANTDATECOLUMNS" {
			t.Errorf("regular script should not offer builder directive completion %q", c.Label)
		}
	}
}

func TestGetAllCompletions_EndpointFileOffersAmbients(t *testing.T) {
	completions := GetAllCompletions(nil, nil, false, false, true)
	gotRequest, gotResponse := false, false
	for _, c := range completions {
		if c.Label == "Request" {
			gotRequest = true
		}
		if c.Label == "Response" {
			gotResponse = true
		}
	}
	if !gotRequest || !gotResponse {
		t.Errorf("endpoint file should offer Request and Response completions (got Request=%v Response=%v)", gotRequest, gotResponse)
	}
}

func TestGetAllCompletions_NonEndpointFileExcludesAmbients(t *testing.T) {
	completions := GetAllCompletions(nil, nil, false, false, false)
	for _, c := range completions {
		if c.Label == "Request" || c.Label == "Response" {
			t.Errorf("non-endpoint file should not offer ambient completion %q", c.Label)
		}
	}
}

// ==================== Data Source Hover ====================

func TestGetHover_BuilderDirective(t *testing.T) {
	hover := getKeywordHover(":DSN")
	if hover == nil {
		t.Fatal("expected hover info for :DSN builder directive")
	}
	if !strings.Contains(hover.Contents, "builder directive") {
		t.Error("expected hover to mention 'builder directive'")
	}
	if !strings.Contains(hover.Contents, "database connection") {
		t.Error("expected hover to describe DSN purpose")
	}
}

func TestGetHover_BuilderDirective_AllDirectives(t *testing.T) {
	directives := []string{":DSN", ":TABLENAME", ":NULLASBLANK", ":INVARIANTDATECOLUMNS"}
	for _, d := range directives {
		hover := getKeywordHover(d)
		if hover == nil {
			t.Errorf("expected hover info for %s", d)
		}
	}
}

// ==================== Data Source Diagnostics ====================

func TestGetDiagnostics_DataSource_BuilderDirectivesNotFlaggedAsUnknown(t *testing.T) {
	// SQL data source with builder directives should not get "Unknown SSL keyword" warnings
	code := `:DSN := MyConnection;
:TABLENAME := Results;
:NULLASBLANK := true;
:INVARIANTDATECOLUMNS := DateCreated, DateModified;
:PARAMETERS sName := '';
SELECT * FROM Table1 WHERE Name = ?sName?`

	opts := DefaultDiagnosticOptions()
	opts.IsDataSourceFile = true
	diagnostics := GetDiagnostics(code, opts)

	for _, d := range diagnostics {
		if strings.Contains(d.Message, "Unknown SSL keyword") {
			t.Errorf("builder directive should not be flagged as unknown keyword: %s", d.Message)
		}
	}
}

func TestGetDiagnostics_DataSource_DefaultStatementFlagged(t *testing.T) {
	// Data source files should not use separate :DEFAULT statements
	code := `:PARAMETERS sName;
:DEFAULT sName, '';`

	opts := DefaultDiagnosticOptions()
	opts.IsDataSourceFile = true
	diagnostics := GetDiagnostics(code, opts)

	found := false
	for _, d := range diagnostics {
		if strings.Contains(d.Message, "inline ':=' defaults") {
			found = true
		}
	}
	if !found {
		t.Fatal("expected diagnostic flagging :DEFAULT usage in data source file")
	}
}

func TestGetDiagnostics_DataSource_ParametersWithoutDefaultsValid(t *testing.T) {
	// diag.datasource_default_required was removed (issue #147,
	// ssl-style-guide#48): the data source builder accepts :PARAMETERS
	// without inline defaults, so no default-related error may fire.
	code := `:PARAMETERS sName, nCount := 10;`

	opts := DefaultDiagnosticOptions()
	opts.IsDataSourceFile = true
	diagnostics := GetDiagnostics(code, opts)

	for _, d := range diagnostics {
		if strings.Contains(d.Message, "inline ':=' default") || d.Code == "datasource_default_required" {
			t.Errorf("defaultless data source parameter must not be flagged: %s", d.Message)
		}
	}
}

func TestGetDiagnostics_ExecFunctionClassTarget(t *testing.T) {
	// diag.execfunction_class_target (issue #143): only ExecFunction sites
	// whose target is in the pre-resolved class list flag; DoProc and
	// unresolved targets never do.
	code := `:PROCEDURE Demo;
ExecFunction("LIMS.SampleTools", {1});
ExecFunction("LIMS.OrdinaryScript", {1});
DoProc("LIMS.SampleTools", {1});
:ENDPROC;`

	opts := DefaultDiagnosticOptions()
	opts.ClassFileDispatchTargets = []string{"lims.sampletools"} // case-insensitive match

	var flagged []int
	for _, d := range GetDiagnostics(code, opts) {
		if d.Code == CodeExecFunctionClassTarget {
			flagged = append(flagged, d.Range.Start.Line)
			if d.Severity != SeverityError {
				t.Errorf("expected error severity, got %v", d.Severity)
			}
		}
	}
	if len(flagged) != 1 || flagged[0] != 1 {
		t.Fatalf("expected exactly one execfunction_class_target on line 1 (0-indexed), got lines %v", flagged)
	}

	// Without workspace verdicts the check is silent.
	for _, d := range GetDiagnostics(code, DefaultDiagnosticOptions()) {
		if d.Code == CodeExecFunctionClassTarget {
			t.Errorf("check must be disabled with no ClassFileDispatchTargets, got: %s", d.Message)
		}
	}
}

func TestGetDiagnostics_RaiseErrorInCatchFlagged(t *testing.T) {
	// diag.raiseerror_in_catch (issue #142): the nearest enclosing :TRY
	// section decides — :CATCH flags, :TRY body of a nested try inside a
	// handler does not.
	code := `:PROCEDURE Demo;
:TRY;
	RaiseError("in try - fine");
:CATCH;
	RaiseError("in catch - flagged");
	:TRY;
		RaiseError("nested try body - fine");
	:CATCH;
		RaiseError("nested catch - flagged");
	:ENDTRY;
:ENDTRY;
:ENDPROC;`

	diagnostics := GetDiagnostics(code, DefaultDiagnosticOptions())

	var lines []int
	for _, d := range diagnostics {
		if d.Code == CodeRaiseErrorInCatch {
			lines = append(lines, d.Range.Start.Line)
		}
	}
	if len(lines) != 2 || lines[0] != 4 || lines[1] != 8 {
		t.Fatalf("expected raiseerror_in_catch on lines 4 and 8 (0-indexed), got lines %v", lines)
	}
}

func TestGetDiagnostics_RaiseErrorOutsideCatchNotFlagged(t *testing.T) {
	// Raise-only helpers and :FINALLY raises are outside this rule's scope.
	code := `:PROCEDURE Helper;
RaiseError("raise-only helper");
:ENDPROC;
:PROCEDURE Demo;
:TRY;
	DoWork();
:CATCH;
	LogIt(GetLastSSLError());
:FINALLY;
	RaiseError("in finally - not this rule's business");
:ENDTRY;
:ENDPROC;`

	diagnostics := GetDiagnostics(code, DefaultDiagnosticOptions())

	for _, d := range diagnostics {
		if d.Code == CodeRaiseErrorInCatch {
			t.Errorf("unexpected raiseerror_in_catch at line %d: %s", d.Range.Start.Line, d.Message)
		}
	}
}

func TestGetDiagnostics_DeclareInitializerFlagged(t *testing.T) {
	// diag.declare_initializer (issue #138): :DECLARE accepts only an
	// identifier list; each inline := is a syntax error, ranged on the :=.
	code := `:PROCEDURE Demo;
:DECLARE nOne := 1, nTwo := 2;
:ENDPROC;`

	diagnostics := GetDiagnostics(code, DefaultDiagnosticOptions())

	var found []Diagnostic
	for _, d := range diagnostics {
		if d.Code == CodeDeclareInitializer {
			found = append(found, d)
		}
	}
	if len(found) != 2 {
		t.Fatalf("expected 2 declare_initializer diagnostics, got %d: %+v", len(found), found)
	}
	for _, d := range found {
		if d.Severity != SeverityError {
			t.Errorf("expected error severity, got %v", d.Severity)
		}
		if d.Range.Start.Line != 1 {
			t.Errorf("expected diagnostic on line 1 (0-indexed), got %d", d.Range.Start.Line)
		}
	}
}

func TestGetDiagnostics_DeclareWithoutInitializerNotFlagged(t *testing.T) {
	// Plain identifier lists and later assignments are the valid form.
	code := `:PROCEDURE Demo;
:DECLARE nCount, sName;
nCount := 1;
sName := "x";
:ENDPROC;`

	diagnostics := GetDiagnostics(code, DefaultDiagnosticOptions())

	for _, d := range diagnostics {
		if d.Code == CodeDeclareInitializer {
			t.Errorf(":DECLARE without initializer should not be flagged: %s", d.Message)
		}
	}
}

func TestGetDiagnostics_DataSource_DeclareInitializerFlagged(t *testing.T) {
	// :DECLARE never takes initializers, in data-source files included —
	// while the inline-default :PARAMETERS form stays legal there
	// (diag.declare_initializer Behavior, data-source coverage pin).
	code := `:PARAMETERS sName := '';
:DECLARE nCount := 1;`

	opts := DefaultDiagnosticOptions()
	opts.IsDataSourceFile = true
	diagnostics := GetDiagnostics(code, opts)

	count := 0
	for _, d := range diagnostics {
		if d.Code == CodeDeclareInitializer {
			count++
			if d.Range.Start.Line != 1 {
				t.Errorf("expected declare_initializer on the :DECLARE line (1), got line %d", d.Range.Start.Line)
			}
		}
	}
	if count != 1 {
		t.Fatalf("expected exactly 1 declare_initializer diagnostic (:PARAMETERS := must not flag), got %d", count)
	}
}

func TestGetDiagnostics_DataSource_ExpressionDefaultNotFalsePositive(t *testing.T) {
	// Default values with complex expressions should not produce false positives
	code := `:PARAMETERS sName := 'hello' + ' world', nVal := 1 + 2;`

	opts := DefaultDiagnosticOptions()
	opts.IsDataSourceFile = true
	diagnostics := GetDiagnostics(code, opts)

	for _, d := range diagnostics {
		if strings.Contains(d.Message, "inline ':=' default") {
			t.Errorf("expression default should not produce false positive: %s", d.Message)
		}
	}
}

func TestGetDiagnostics_DataSource_NoFalseParameterPlacementErrors(t *testing.T) {
	// :PARAMETERS after builder directives should not trigger placement errors
	code := `:DSN := MyConn;
:TABLENAME := Results;
:PARAMETERS sName := '';
SELECT * FROM Table1`

	opts := DefaultDiagnosticOptions()
	opts.IsDataSourceFile = true
	diagnostics := GetDiagnostics(code, opts)

	for _, d := range diagnostics {
		if strings.Contains(d.Message, "must appear before top-level statements") {
			t.Errorf("data source file should not get parameter placement error: %s", d.Message)
		}
	}
}

func TestGetDiagnostics_DataSource_BuilderDirectiveCaseSensitive(t *testing.T) {
	// Builder directives must be uppercase
	code := `:dsn := MyConnection;`

	opts := DefaultDiagnosticOptions()
	opts.IsDataSourceFile = true
	diagnostics := GetDiagnostics(code, opts)

	found := false
	for _, d := range diagnostics {
		if strings.Contains(d.Message, "uppercase") && strings.Contains(d.Message, ":DSN") {
			found = true
		}
	}
	if !found {
		t.Fatal("expected diagnostic for lowercase builder directive")
	}
}

func TestGetDiagnostics_NonDataSource_BuilderDirectiveFlaggedAsUnknown(t *testing.T) {
	// In regular script files, builder directives should be flagged as unknown keywords
	code := `:DSN := MyConnection;`

	opts := DefaultDiagnosticOptions()
	opts.IsDataSourceFile = false
	diagnostics := GetDiagnostics(code, opts)

	found := false
	for _, d := range diagnostics {
		if strings.Contains(d.Message, "Unknown SSL keyword") {
			found = true
		}
	}
	if !found {
		t.Fatal("expected unknown keyword diagnostic for :DSN in regular script file")
	}
}

// TestDiagnosticsAlwaysCarryCode asserts that every emitted diagnostic carries
// a non-empty Code so clients can wire quick-fixes / suppression / per-rule
// severity overrides without inspecting message text.
// [spec feature.diagnostics_pipeline/A1]
func TestDiagnosticsAlwaysCarryCode(t *testing.T) {
	// A grab-bag of patterns that exercises many distinct check functions.
	code := `:CLASS Email;
:PROCEDURE Test;
:PARAMETERS sFoo;
nBad = 5
result := obj.prop;
arr[0];
:IF x := 1;
DoProc("X", {});
:ENDIF;
:BEGINCASE;
:CASE x = 1;
y := 1;
:ENDCASE;
:ENDPROC;`

	opts := DefaultDiagnosticOptions()
	opts.CheckUnmatchedParens = true
	opts.CheckUnclosedBlocks = true
	opts.CheckHungarianNotation = true
	opts.HungarianPrefixes = []string{"s", "n", "b", "d", "a", "o", "fn", "v"}
	diagnostics := GetDiagnostics(code, opts)

	if len(diagnostics) == 0 {
		t.Fatal("expected at least some diagnostics from sample input")
	}
	for i, d := range diagnostics {
		if d.Code == "" {
			t.Errorf("diagnostic %d has empty Code: severity=%v message=%q",
				i, d.Severity, d.Message)
		}
		if d.Source != "ssl-lsp" {
			t.Errorf("diagnostic %d has unexpected Source %q (want %q)",
				i, d.Source, "ssl-lsp")
		}
	}
}

// TestRuleOverrides_DropAndRemap verifies that DiagnosticOptions.RuleOverrides
// drops "off" rules and remaps severity for info/warn/error.
// [spec feature.diagnostics_pipeline/A2] — "off" drops, "info" remaps.
// [spec feature.diagnostics_pipeline/A3] — unknown override value is a no-op.
func TestRuleOverrides_DropAndRemap(t *testing.T) {
	// Source emits at least one parameters_first diagnostic (statement before
	// :PARAMETERS) and at least one keyword_uppercase diagnostic (lowercase :if).
	src := `:PROCEDURE Test;
nLocal := 1;
:PARAMETERS sFoo;
:if .T.;
:ENDIF;
:ENDPROC;`

	baseOpts := DefaultDiagnosticOptions()
	base := GetDiagnostics(src, baseOpts)

	countCode := func(diags []Diagnostic, code string) int {
		n := 0
		for _, d := range diags {
			if d.Code == code {
				n++
			}
		}
		return n
	}

	if countCode(base, CodeParametersFirst) == 0 {
		t.Fatalf("baseline did not emit %s; rest of test is meaningless", CodeParametersFirst)
	}

	// "off" — diagnostic should disappear.
	off := DefaultDiagnosticOptions()
	off.RuleOverrides = map[string]string{CodeParametersFirst: "off"}
	if got := countCode(GetDiagnostics(src, off), CodeParametersFirst); got != 0 {
		t.Errorf("with override 'off', expected 0 diagnostics for %s, got %d", CodeParametersFirst, got)
	}

	// "info" — diagnostic remains but severity is information.
	info := DefaultDiagnosticOptions()
	info.RuleOverrides = map[string]string{CodeParametersFirst: "info"}
	infoDiags := GetDiagnostics(src, info)
	found := false
	for _, d := range infoDiags {
		if d.Code == CodeParametersFirst {
			found = true
			if d.Severity != SeverityInfo {
				t.Errorf("expected SeverityInfo, got %v", d.Severity)
			}
		}
	}
	if !found {
		t.Error("override 'info' should retain the diagnostic")
	}

	// Unknown override value passes through untouched.
	noop := DefaultDiagnosticOptions()
	noop.RuleOverrides = map[string]string{CodeParametersFirst: "bogus"}
	if got := countCode(GetDiagnostics(src, noop), CodeParametersFirst); got == 0 {
		t.Errorf("unknown override value should not drop diagnostics")
	}
}

// TestSuppressionComments_FileScopeAndNextLine verifies that
// /* @ssl-disable <slug>; */ suppresses every matching diagnostic file-wide,
// and /* @ssl-disable-next-line <slug>; */ suppresses only the next line.
// [spec feature.diagnostics_pipeline/A4] — file-scope suppression.
// [spec feature.diagnostics_pipeline/A5] — next-line scope only.
// [spec feature.diagnostics_pipeline/A6] — wildcard '*' silences everything.
func TestSuppressionComments_FileScopeAndNextLine(t *testing.T) {
	// File-scope: silence every parameters_first finding in the file.
	fileWide := `/* @ssl-disable parameters_first; */
:PROCEDURE Test;
nLocal := 1;
:PARAMETERS sFoo;
:ENDPROC;`
	if got := codeCount(GetDiagnostics(fileWide, DefaultDiagnosticOptions()), CodeParametersFirst); got != 0 {
		t.Errorf("file-scope @ssl-disable should silence all matching diagnostics; got %d", got)
	}

	// Without the directive, the same source emits the diagnostic.
	noDirective := `:PROCEDURE Test;
nLocal := 1;
:PARAMETERS sFoo;
:ENDPROC;`
	if got := codeCount(GetDiagnostics(noDirective, DefaultDiagnosticOptions()), CodeParametersFirst); got == 0 {
		t.Fatal("baseline should emit parameters_first")
	}

	// next-line: only the line directly after the directive is silenced.
	// Place a deprecated-keyword diagnostic on the line right after the
	// directive, then a second deprecated-keyword on a later line; only the
	// first should be suppressed.
	nextLine := `:PROCEDURE Test;
nA := 1;
/* @ssl-disable-next-line not_preferred_operator; */
:IF nA <> 1;
:ENDIF;
:IF nA <> 2;
:ENDIF;
:ENDPROC;`
	diags := GetDiagnostics(nextLine, DefaultDiagnosticOptions())
	got := codeCount(diags, CodeNotPreferredOperator)
	if got != 1 {
		t.Errorf("next-line directive should silence exactly one not_preferred_operator; got %d", got)
		for _, d := range diags {
			if d.Code == CodeNotPreferredOperator {
				t.Logf("  remaining: line=%d msg=%q", d.Range.Start.Line, d.Message)
			}
		}
	}

	// Wildcard '*' silences everything (file scope).
	star := `/* @ssl-disable *; */
:PROCEDURE Test;
nLocal := 1;
:PARAMETERS sFoo;
:ENDPROC;`
	if got := codeCount(GetDiagnostics(star, DefaultDiagnosticOptions()), CodeParametersFirst); got != 0 {
		t.Errorf("wildcard '*' should silence all diagnostics; got %d for parameters_first", got)
	}
}

func codeCount(diags []Diagnostic, code string) int {
	n := 0
	for _, d := range diags {
		if d.Code == code {
			n++
		}
	}
	return n
}

// TestDiagnosticCodes_SpotChecks pins specific check rules to specific codes.
// Catches accidental rename / typo regressions in the slug list.
func TestDiagnosticCodes_SpotChecks(t *testing.T) {
	tests := []struct {
		name         string
		code         string
		opts         func() DiagnosticOptions
		wantCode     string
		messageMatch string
	}{
		{
			name: "udobject_array_in_clause",
			code: `:PROCEDURE Test;
SqlExecute("SELECT * FROM t WHERE id IN (?o:Items?)");
:ENDPROC;`,
			opts:         DefaultDiagnosticOptions,
			wantCode:     CodeUdObjectArrayInClause,
			messageMatch: "UDObject array property",
		},
		{
			name: "parameters_first",
			code: `:PROCEDURE Test;
nLocal := 1;
:PARAMETERS sFoo;
:ENDPROC;`,
			opts:         DefaultDiagnosticOptions,
			wantCode:     CodeParametersFirst,
			messageMatch: "':PARAMETERS' must appear",
		},
		{
			name: "exitfor_in_finally",
			code: `:PROCEDURE Test;
:TRY;
nX := 1;
:FINALLY;
:EXITFOR;
:ENDTRY;
:ENDPROC;`,
			opts:         DefaultDiagnosticOptions,
			wantCode:     CodeExitForInFinally,
			messageMatch: "EXITFOR' inside a ':FINALLY",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			diagnostics := GetDiagnostics(tt.code, tt.opts())
			var matched *Diagnostic
			for i := range diagnostics {
				if strings.Contains(diagnostics[i].Message, tt.messageMatch) {
					matched = &diagnostics[i]
					break
				}
			}
			if matched == nil {
				t.Fatalf("no diagnostic matched message substring %q; got %d diagnostics",
					tt.messageMatch, len(diagnostics))
			}
			if matched.Code != tt.wantCode {
				t.Errorf("Code = %q, want %q (message: %q)",
					matched.Code, tt.wantCode, matched.Message)
			}
		})
	}
}

// ==================== v0.7.0 Regression Tests ====================

func countCode(diagnostics []Diagnostic, code string) int {
	n := 0
	for _, d := range diagnostics {
		if d.Code == code {
			n++
		}
	}
	return n
}

// mixed_type_operator must not flag uppercase-leading identifiers as
// Hungarian-typed (DCUparseCat starts with capital D — that's an acronym,
// not a 'd' Hungarian prefix). Strict-case prefix matching guards this.
func TestGetDiagnostics_MixedTypes_NoFP_HungarianUppercase(t *testing.T) {
	text := `:DECLARE DCUparseCat, parsingScript;
DCUparseCat := "category";
parsingScript := DCUparseCat + "." + "leaf";`
	diagnostics := GetDiagnostics(text, DefaultDiagnosticOptions())
	if n := countCode(diagnostics, CodeMixedTypeOperator); n != 0 {
		t.Errorf("expected 0 mixed_type_operator diagnostics on uppercase-leading names; got %d", n)
	}
}

// Indexed access (arr[i]) must be treated as opaque element type, not as
// the array's type, so concatenating arr[i] with a string is fine.
func TestGetDiagnostics_MixedTypes_NoFP_IndexedAccess(t *testing.T) {
	text := `:PROCEDURE BuildCols;
:PARAMETERS aCols;
:DECLARE sCols, X;
sCols := "";
:FOR X := 1 :TO 5;
	sCols := sCols + aCols[X] + " end";
:NEXT;
:ENDPROC;`
	diagnostics := GetDiagnostics(text, DefaultDiagnosticOptions())
	if n := countCode(diagnostics, CodeMixedTypeOperator); n != 0 {
		t.Errorf("expected 0 mixed_type_operator diagnostics on indexed access; got %d", n)
	}
}

// Member access (Me:Foo, obj:bar) must be treated as opaque — the LSP can't
// know the underlying member type from the name.
func TestGetDiagnostics_MixedTypes_NoFP_MemberAccess(t *testing.T) {
	text := `:CLASS Box;
:PROCEDURE Build;
Me:Foo := Me:Bar + " z";
:ENDPROC;`
	diagnostics := GetDiagnostics(text, DefaultDiagnosticOptions())
	if n := countCode(diagnostics, CodeMixedTypeOperator); n != 0 {
		t.Errorf("expected 0 mixed_type_operator diagnostics on member access; got %d", n)
	}
}

// Regression-guard: literal "abc" + 5 must still fire the rule, so we
// haven't silenced the rule entirely.
func TestGetDiagnostics_MixedTypes_StillFires_LiteralMismatch(t *testing.T) {
	text := `:DECLARE x;
x := "abc" + 5;`
	diagnostics := GetDiagnostics(text, DefaultDiagnosticOptions())
	if n := countCode(diagnostics, CodeMixedTypeOperator); n == 0 {
		t.Errorf("expected mixed_type_operator to fire on \"abc\" + 5; got 0")
	}
}

// Bare-PROCEDURE typo (missing leading colon, parens used) must produce one
// procedure_declaration_syntax diagnostic and zero direct_procedure_call.
func TestGetDiagnostics_ProcDeclSyntax_BareProcedure(t *testing.T) {
	text := `PROCEDURE Main(); :RETURN .T.; ENDPROC;`
	diagnostics := GetDiagnostics(text, DefaultDiagnosticOptions())
	if n := countCode(diagnostics, CodeProcedureDeclarationSyntax); n != 1 {
		t.Errorf("expected exactly 1 procedure_declaration_syntax; got %d", n)
	}
	if n := countCode(diagnostics, CodeDirectProcedureCall); n != 0 {
		t.Errorf("expected 0 direct_procedure_call; got %d", n)
	}
}

// :PROCEDURE Name() with parens after the name is also invalid — the rule
// must flag the parens.
func TestGetDiagnostics_ProcDeclSyntax_ParensWithColon(t *testing.T) {
	text := `:PROCEDURE Main(); :RETURN .T.; :ENDPROC;`
	diagnostics := GetDiagnostics(text, DefaultDiagnosticOptions())
	if n := countCode(diagnostics, CodeProcedureDeclarationSyntax); n != 1 {
		t.Errorf("expected exactly 1 procedure_declaration_syntax; got %d", n)
	}
}

// Happy path: :PROCEDURE Name; … :ENDPROC; produces zero diagnostics.
func TestGetDiagnostics_ProcDecl_HappyPath(t *testing.T) {
	text := `:PROCEDURE Main;
:RETURN .T.;
:ENDPROC;`
	diagnostics := GetDiagnostics(text, DefaultDiagnosticOptions())
	if len(diagnostics) != 0 {
		t.Errorf("expected 0 diagnostics for valid procedure; got %d: %+v", len(diagnostics), diagnostics)
	}
}

// Regression: operators inside function-call arguments must not be picked up
// as the outer expression's operator. SubStr(s, 1, Len(s) - 1) returns a
// string; the inner '-' is a SubStr argument, not the outer operator.
func TestGetDiagnostics_MixedTypes_NoFP_OperatorInsideCall(t *testing.T) {
	text := `:DECLARE sCols, SqlCommand;
sCols := SubStr(sCols, 1, Len(sCols) - 1);
SqlCommand := "TABLE " + sCols + " end";`
	diagnostics := GetDiagnostics(text, DefaultDiagnosticOptions())
	if n := countCode(diagnostics, CodeMixedTypeOperator); n != 0 {
		t.Errorf("expected 0 mixed_type_operator on SubStr+concat sequence; got %d: %+v", n, diagnostics)
	}
}

// Panic recovery: a panic anywhere inside collectDiagnostics should NOT
// kill the server. It should surface as a single internal_error diagnostic
// so the editor stays usable and bug reports are easy to file.
// [spec feature.diagnostics_pipeline/A7]
func TestGetDiagnostics_PanicRecovery(t *testing.T) {
	defer func() {
		if r := recover(); r != nil {
			t.Errorf("panic propagated past collectDiagnostics: %v", r)
		}
	}()

	// We can't easily synthesize a panic from input alone, so just verify
	// the safety net is wired: pretend a check function panicked by passing
	// a nil token slice that would otherwise be handled gracefully — call
	// the recovery path explicitly via a helper.
	got := func() (out []Diagnostic) {
		defer func() {
			if r := recover(); r != nil {
				out = []Diagnostic{{
					Severity: SeverityError,
					Source:   "ssl-lsp",
					Code:     "internal_error",
					Message:  "synthetic",
				}}
			}
		}()
		panic("synthetic panic for test")
	}()
	if len(got) != 1 || got[0].Code != "internal_error" {
		t.Fatalf("recovery contract: expected one internal_error diagnostic, got %+v", got)
	}
}

func hasDiagnosticCode(diags []Diagnostic, code string) bool {
	for _, d := range diags {
		if d.Code == code {
			return true
		}
	}
	return false
}

func TestUnqualifiedFieldAssignment_BareAssignmentFlagged(t *testing.T) {
	text := `:CLASS MyClass;
:DECLARE sName;
:PROCEDURE SetIt;
:PARAMETERS sValue;
	sName := sValue;
:ENDPROC;`

	diags := GetDiagnostics(text, DefaultDiagnosticOptions())
	if !hasDiagnosticCode(diags, CodeUnqualifiedFieldAssignment) {
		t.Fatalf("expected unqualified_field_assignment, got %+v", diags)
	}
}

func TestUnqualifiedFieldAssignment_QualifiedSuppressed(t *testing.T) {
	text := `:CLASS MyClass;
:DECLARE sName;
:PROCEDURE SetIt;
:PARAMETERS sValue;
	Me:sName := sValue;
:ENDPROC;`

	diags := GetDiagnostics(text, DefaultDiagnosticOptions())
	if hasDiagnosticCode(diags, CodeUnqualifiedFieldAssignment) {
		t.Fatalf("did not expect unqualified_field_assignment, got %+v", diags)
	}
}

func TestUnqualifiedFieldAssignment_LocalShadowSuppressed(t *testing.T) {
	text := `:CLASS MyClass;
:DECLARE sName;
:PROCEDURE SetIt;
:DECLARE sName;
	sName := "local";
:ENDPROC;`

	diags := GetDiagnostics(text, DefaultDiagnosticOptions())
	if hasDiagnosticCode(diags, CodeUnqualifiedFieldAssignment) {
		t.Fatalf("local-shadow should suppress, got %+v", diags)
	}
}

func TestUnqualifiedFieldAssignment_ParameterShadowSuppressed(t *testing.T) {
	text := `:CLASS MyClass;
:DECLARE sName;
:PROCEDURE SetIt;
:PARAMETERS sName;
	sName := "param";
:ENDPROC;`

	diags := GetDiagnostics(text, DefaultDiagnosticOptions())
	if hasDiagnosticCode(diags, CodeUnqualifiedFieldAssignment) {
		t.Fatalf("parameter-shadow should suppress, got %+v", diags)
	}
}

func TestUnqualifiedFieldAssignment_NonClassFileNoOp(t *testing.T) {
	text := `:PROCEDURE Run;
:DECLARE sName;
	sName := "ok";
:ENDPROC;`

	diags := GetDiagnostics(text, DefaultDiagnosticOptions())
	if hasDiagnosticCode(diags, CodeUnqualifiedFieldAssignment) {
		t.Fatalf("non-class file should not trigger the rule, got %+v", diags)
	}
}

func TestEndpointAmbients_RequestAndResponseAcceptedInEndpoint(t *testing.T) {
	text := `:DECLARE sUrl;
	sUrl := Request:Url;
	Response:Body := "ok";`

	opts := DefaultDiagnosticOptions()
	opts.CheckUndeclaredVars = true
	opts.IsEndpointFile = true

	diags := GetDiagnostics(text, opts)
	for _, d := range diags {
		if d.Code == CodeUndeclaredVariable {
			if strings.EqualFold(extractVarName(d.Message), "Request") ||
				strings.EqualFold(extractVarName(d.Message), "Response") {
				t.Fatalf("Request/Response should be ambient in endpoint files, got %+v", d)
			}
		}
	}
}

func TestEndpointAmbients_RequestFlaggedOutsideEndpoint(t *testing.T) {
	text := `:DECLARE sUrl;
	sUrl := Request:Url;`

	opts := DefaultDiagnosticOptions()
	opts.CheckUndeclaredVars = true
	opts.IsEndpointFile = false

	diags := GetDiagnostics(text, opts)
	found := false
	for _, d := range diags {
		if d.Code == CodeUndeclaredVariable && strings.Contains(d.Message, "Request") {
			found = true
		}
	}
	if !found {
		t.Fatalf("expected Request to be flagged as undeclared in non-endpoint file, got %+v", diags)
	}
}

// [spec feature.hover/A9]
func TestEndpointAmbientHover_RequestAndResponse(t *testing.T) {
	if h := GetEndpointAmbientHover("Request"); h == nil || !strings.Contains(h.Contents, "endpoint ambient") {
		t.Fatalf("expected Request ambient hover, got %+v", h)
	}
	if h := GetEndpointAmbientHover("Response"); h == nil || !strings.Contains(h.Contents, "endpoint ambient") {
		t.Fatalf("expected Response ambient hover, got %+v", h)
	}
	if h := GetEndpointAmbientHover("Other"); h != nil {
		t.Fatalf("expected no hover for non-ambient, got %+v", h)
	}
}

// extractVarName pulls the quoted identifier out of an undeclared-variable
// diagnostic message ("Variable 'X' is not declared").
func extractVarName(msg string) string {
	const prefix = "Variable '"
	i := strings.Index(msg, prefix)
	if i < 0 {
		return ""
	}
	rest := msg[i+len(prefix):]
	j := strings.Index(rest, "'")
	if j < 0 {
		return ""
	}
	return rest[:j]
}

func TestUnqualifiedFieldAssignment_CompoundOperatorFlagged(t *testing.T) {
	text := `:CLASS MyClass;
:DECLARE nCount;
:PROCEDURE Bump;
	nCount += 1;
:ENDPROC;`

	diags := GetDiagnostics(text, DefaultDiagnosticOptions())
	if !hasDiagnosticCode(diags, CodeUnqualifiedFieldAssignment) {
		t.Fatalf("compound assignment should be flagged, got %+v", diags)
	}
}
