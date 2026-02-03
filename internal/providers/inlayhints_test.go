package providers

import (
	"testing"

	"starlims-lsp/internal/lexer"
	"starlims-lsp/internal/parser"
)

func TestGetInlayHints_BuiltinFunction(t *testing.T) {
	// Test built-in function with 3 parameters (above threshold)
	code := `x := Substr("Hello", 1, 5);`
	tokens := lexer.NewLexer(code).Tokenize()
	opts := DefaultInlayHintOptions()

	hints := GetInlayHints(tokens, nil, 1, 1, opts)

	// Substr has 3 parameters: source, startPos, length
	if len(hints) != 3 {
		t.Errorf("Expected 3 hints, got %d", len(hints))
		return
	}

	// Check first hint
	if hints[0].Label != "source" {
		t.Errorf("Expected first hint label 'source', got '%s'", hints[0].Label)
	}
	if hints[1].Label != "startPos" {
		t.Errorf("Expected second hint label 'startPos', got '%s'", hints[1].Label)
	}
	if hints[2].Label != "length" {
		t.Errorf("Expected third hint label 'length', got '%s'", hints[2].Label)
	}
}

func TestGetInlayHints_SingleParameter_BelowThreshold(t *testing.T) {
	// Test single parameter function - should not show hints with default threshold
	code := `x := Len(sValue);`
	tokens := lexer.NewLexer(code).Tokenize()
	opts := DefaultInlayHintOptions() // MinParameterCount = 2

	hints := GetInlayHints(tokens, nil, 1, 1, opts)

	if len(hints) != 0 {
		t.Errorf("Expected 0 hints for single param function, got %d", len(hints))
	}
}

func TestGetInlayHints_SingleParameter_ThresholdOne(t *testing.T) {
	// Test single parameter function with threshold = 1
	code := `x := Len(sValue);`
	tokens := lexer.NewLexer(code).Tokenize()
	opts := InlayHintOptions{
		Enabled:           true,
		MinParameterCount: 1,
	}

	hints := GetInlayHints(tokens, nil, 1, 1, opts)

	if len(hints) != 1 {
		t.Errorf("Expected 1 hint with threshold=1, got %d", len(hints))
		return
	}
	if hints[0].Label != "source" {
		t.Errorf("Expected hint label 'source', got '%s'", hints[0].Label)
	}
}

func TestGetInlayHints_Disabled(t *testing.T) {
	code := `x := Substr("Hello", 1, 5);`
	tokens := lexer.NewLexer(code).Tokenize()
	opts := InlayHintOptions{
		Enabled:           false,
		MinParameterCount: 2,
	}

	hints := GetInlayHints(tokens, nil, 1, 1, opts)

	if len(hints) != 0 {
		t.Errorf("Expected 0 hints when disabled, got %d", len(hints))
	}
}

func TestGetInlayHints_RangeFiltering(t *testing.T) {
	code := `x := Substr("a", 1, 2);
y := Substr("b", 3, 4);
z := Substr("c", 5, 6);`
	tokens := lexer.NewLexer(code).Tokenize()
	opts := DefaultInlayHintOptions()

	// Only request hints for line 2
	hints := GetInlayHints(tokens, nil, 2, 2, opts)

	// Should only get 3 hints for the second line
	if len(hints) != 3 {
		t.Errorf("Expected 3 hints for line 2, got %d", len(hints))
		return
	}

	// All hints should be on line 2
	for _, h := range hints {
		if h.Line != 2 {
			t.Errorf("Expected all hints on line 2, got line %d", h.Line)
		}
	}
}

func TestGetInlayHints_DoProc(t *testing.T) {
	code := `DoProc("MyProc", {100, 200});`
	tokens := lexer.NewLexer(code).Tokenize()
	opts := DefaultInlayHintOptions()

	hints := GetInlayHints(tokens, nil, 1, 1, opts)

	// DoProc has 2 parameters: sProcName, aParams
	if len(hints) != 2 {
		t.Errorf("Expected 2 hints for DoProc, got %d", len(hints))
		return
	}

	if hints[0].Label != "sProcName" {
		t.Errorf("Expected first hint 'sProcName', got '%s'", hints[0].Label)
	}
	if hints[1].Label != "aParams" {
		t.Errorf("Expected second hint 'aParams', got '%s'", hints[1].Label)
	}
}

func TestGetInlayHints_ExecFunction(t *testing.T) {
	code := `ExecFunction("MyProc", {arg1});`
	tokens := lexer.NewLexer(code).Tokenize()
	opts := DefaultInlayHintOptions()

	hints := GetInlayHints(tokens, nil, 1, 1, opts)

	// ExecFunction has same parameters as DoProc
	if len(hints) != 2 {
		t.Errorf("Expected 2 hints for ExecFunction, got %d", len(hints))
		return
	}

	if hints[0].Label != "sProcName" {
		t.Errorf("Expected first hint 'sProcName', got '%s'", hints[0].Label)
	}
}

func TestGetInlayHints_NestedCalls(t *testing.T) {
	code := `x := Upper(Substr(s, 1, 5));`
	tokens := lexer.NewLexer(code).Tokenize()
	opts := DefaultInlayHintOptions()

	hints := GetInlayHints(tokens, nil, 1, 1, opts)

	// Upper has 1 param (below threshold), Substr has 3
	// Should only get hints for Substr
	if len(hints) != 3 {
		t.Errorf("Expected 3 hints (only Substr), got %d", len(hints))
	}

	// Verify they are Substr hints
	expectedLabels := []string{"source", "startPos", "length"}
	for i, h := range hints {
		if h.Label != expectedLabels[i] {
			t.Errorf("Expected hint %d to be '%s', got '%s'", i, expectedLabels[i], h.Label)
		}
	}
}

func TestGetInlayHints_UnknownFunction(t *testing.T) {
	code := `x := UnknownFunc(a, b, c);`
	tokens := lexer.NewLexer(code).Tokenize()
	opts := DefaultInlayHintOptions()

	hints := GetInlayHints(tokens, nil, 1, 1, opts)

	if len(hints) != 0 {
		t.Errorf("Expected 0 hints for unknown function, got %d", len(hints))
	}
}

func TestGetInlayHints_EmptyTokens(t *testing.T) {
	var tokens []lexer.Token
	opts := DefaultInlayHintOptions()

	hints := GetInlayHints(tokens, nil, 1, 10, opts)

	if hints != nil {
		t.Errorf("Expected nil for empty tokens, got %v", hints)
	}
}

func TestGetInlayHints_HintPositions(t *testing.T) {
	code := `x := Substr("Hello", 1, 5);`
	tokens := lexer.NewLexer(code).Tokenize()
	opts := DefaultInlayHintOptions()

	hints := GetInlayHints(tokens, nil, 1, 1, opts)

	if len(hints) != 3 {
		t.Errorf("Expected 3 hints, got %d", len(hints))
		return
	}

	// All hints should be on line 1
	for _, h := range hints {
		if h.Line != 1 {
			t.Errorf("Expected hint on line 1, got line %d", h.Line)
		}
	}

	// Hints should have valid character positions
	for i, h := range hints {
		if h.Character < 1 {
			t.Errorf("Hint %d has invalid character position: %d", i, h.Character)
		}
	}
}

func TestGetInlayHints_HintKind(t *testing.T) {
	code := `x := Substr("Hello", 1, 5);`
	tokens := lexer.NewLexer(code).Tokenize()
	opts := DefaultInlayHintOptions()

	hints := GetInlayHints(tokens, nil, 1, 1, opts)

	for i, h := range hints {
		if h.Kind != InlayHintKindParameter {
			t.Errorf("Hint %d has wrong kind: expected %d, got %d",
				i, InlayHintKindParameter, h.Kind)
		}
	}
}

func TestGetInlayHints_TwoParameterFunction(t *testing.T) {
	// Test a function with exactly 2 parameters (at threshold)
	code := `x := Left("Hello", 3);`
	tokens := lexer.NewLexer(code).Tokenize()
	opts := DefaultInlayHintOptions()

	hints := GetInlayHints(tokens, nil, 1, 1, opts)

	// Left has 2 parameters, should show hints
	if len(hints) != 2 {
		t.Errorf("Expected 2 hints for 2-param function, got %d", len(hints))
	}
}

func TestGetInlayHints_MultipleCallsSameLine(t *testing.T) {
	code := `x := Left(s, 3); y := Right(s, 2);`
	tokens := lexer.NewLexer(code).Tokenize()
	opts := DefaultInlayHintOptions()

	hints := GetInlayHints(tokens, nil, 1, 1, opts)

	// Both Left and Right have 2 parameters
	if len(hints) != 4 {
		t.Errorf("Expected 4 hints (2 per function), got %d", len(hints))
	}
}

func TestFindFunctionCalls(t *testing.T) {
	code := `x := Substr(s, 1, 5);`
	tokens := lexer.NewLexer(code).Tokenize()

	calls := findFunctionCalls(tokens, 1, 1)

	if len(calls) != 1 {
		t.Errorf("Expected 1 function call, got %d", len(calls))
		return
	}

	if calls[0].Name != "Substr" {
		t.Errorf("Expected function name 'Substr', got '%s'", calls[0].Name)
	}

	if len(calls[0].Arguments) != 3 {
		t.Errorf("Expected 3 arguments, got %d", len(calls[0].Arguments))
	}
}

func TestFindFunctionCalls_NestedParens(t *testing.T) {
	code := `x := Outer(Inner(a, b), c);`
	tokens := lexer.NewLexer(code).Tokenize()

	calls := findFunctionCalls(tokens, 1, 1)

	if len(calls) != 2 {
		t.Errorf("Expected 2 function calls, got %d", len(calls))
		return
	}

	// Both Outer and Inner should be found
	names := make(map[string]bool)
	for _, c := range calls {
		names[c.Name] = true
	}

	if !names["Outer"] {
		t.Error("Expected to find 'Outer' function call")
	}
	if !names["Inner"] {
		t.Error("Expected to find 'Inner' function call")
	}
}

func TestParseArguments(t *testing.T) {
	code := `Func(a, b, c)`
	tokens := lexer.NewLexer(code).Tokenize()

	// Find the opening paren
	startIdx := -1
	for i, token := range tokens {
		if token.Text == "(" {
			startIdx = i + 1
			break
		}
	}

	if startIdx < 0 {
		t.Fatal("Could not find opening paren")
	}

	args := parseArguments(tokens, startIdx)

	if len(args) != 3 {
		t.Errorf("Expected 3 arguments, got %d", len(args))
	}
}

func TestParseArguments_WithNestedBraces(t *testing.T) {
	code := `DoProc("name", {a, b})`
	tokens := lexer.NewLexer(code).Tokenize()

	// Find the opening paren
	startIdx := -1
	for i, token := range tokens {
		if token.Text == "(" {
			startIdx = i + 1
			break
		}
	}

	args := parseArguments(tokens, startIdx)

	// Should be 2 arguments: "name" and {a, b}
	if len(args) != 2 {
		t.Errorf("Expected 2 arguments, got %d", len(args))
	}
}

func TestDefaultInlayHintOptions(t *testing.T) {
	opts := DefaultInlayHintOptions()

	if !opts.Enabled {
		t.Error("Expected Enabled to be true by default")
	}

	if opts.MinParameterCount != 2 {
		t.Errorf("Expected MinParameterCount to be 2, got %d", opts.MinParameterCount)
	}
}

func TestGetInlayHints_WithProcedures(t *testing.T) {
	// Test that procedures are passed through (even if not fully utilized yet)
	code := `DoProc("Calculate", {100});`
	tokens := lexer.NewLexer(code).Tokenize()
	procedures := []parser.ProcedureInfo{
		{
			Name:       "Calculate",
			Parameters: []string{"nValue"},
			StartLine:  10,
			EndLine:    15,
		},
	}
	opts := DefaultInlayHintOptions()

	hints := GetInlayHints(tokens, procedures, 1, 1, opts)

	// Should at least get DoProc hints
	if len(hints) < 2 {
		t.Errorf("Expected at least 2 hints, got %d", len(hints))
	}
}

func TestGetInlayHints_CaseInsensitiveDoProc(t *testing.T) {
	// Test that DoProc/doproc/DOPROC all work
	testCases := []string{
		`DoProc("test", {});`,
		`doproc("test", {});`,
		`DOPROC("test", {});`,
	}

	opts := DefaultInlayHintOptions()

	for _, code := range testCases {
		tokens := lexer.NewLexer(code).Tokenize()
		hints := GetInlayHints(tokens, nil, 1, 1, opts)

		if len(hints) != 2 {
			t.Errorf("Expected 2 hints for '%s', got %d", code, len(hints))
		}
	}
}
