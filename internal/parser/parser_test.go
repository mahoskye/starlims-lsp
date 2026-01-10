package parser

import (
	"strings"
	"testing"

	"starlims-lsp/internal/lexer"
)

func parseInput(t *testing.T, input string) (*Parser, *Node) {
	t.Helper()
	lex := lexer.NewLexer(input)
	tokens := lex.Tokenize()
	parser := NewParser(tokens)
	return parser, parser.Parse()
}

func collectNodesByType(node *Node, nodeType NodeType) []*Node {
	var nodes []*Node
	var walk func(*Node)
	walk = func(current *Node) {
		if current.Type == nodeType {
			nodes = append(nodes, current)
		}
		for _, child := range current.Children {
			walk(child)
		}
	}
	walk(node)
	return nodes
}

func findVariable(vars []VariableInfo, name string) *VariableInfo {
	for i := range vars {
		if vars[i].Name == name {
			return &vars[i]
		}
	}
	return nil
}

// ==================== Procedure Extraction Tests ====================

func TestParser_ExtractProcedures_Single(t *testing.T) {
	input := `:PROCEDURE Test;
:ENDPROC;`

	p, root := parseInput(t, input)
	procedures := p.ExtractProcedures(root)

	if len(procedures) != 1 {
		t.Fatalf("expected 1 procedure, got %d", len(procedures))
	}
	proc := procedures[0]
	if proc.Name != "Test" {
		t.Errorf("expected procedure name Test, got %q", proc.Name)
	}
	if proc.StartLine != 1 {
		t.Errorf("expected start line 1, got %d", proc.StartLine)
	}
	if proc.EndLine != 2 {
		t.Errorf("expected end line 2, got %d", proc.EndLine)
	}
}

func TestParser_ExtractProcedures_Multiple(t *testing.T) {
	input := `:PROCEDURE First;
:ENDPROC;

:PROCEDURE Second;
:ENDPROC;`

	p, root := parseInput(t, input)
	procedures := p.ExtractProcedures(root)

	if len(procedures) != 2 {
		t.Fatalf("expected 2 procedures, got %d", len(procedures))
	}
	if procedures[0].Name != "First" || procedures[1].Name != "Second" {
		t.Errorf("unexpected procedure names: %+v", procedures)
	}
}

func TestParser_ExtractProcedures_WithParameters(t *testing.T) {
	input := `:PROCEDURE MyProc;
:PARAMETERS param1, param2;
:ENDPROC;`

	p, root := parseInput(t, input)
	procedures := p.ExtractProcedures(root)

	if len(procedures) != 1 {
		t.Fatalf("expected 1 procedure, got %d", len(procedures))
	}
	params := procedures[0].Parameters
	if len(params) != 2 || params[0] != "param1" || params[1] != "param2" {
		t.Errorf("unexpected parameters: %v", params)
	}
}

func TestParser_ExtractProcedures_NoEndproc(t *testing.T) {
	input := `:PROCEDURE Test;
value := 1;`

	p, root := parseInput(t, input)
	procedures := p.ExtractProcedures(root)

	if len(procedures) != 1 {
		t.Fatalf("expected 1 procedure, got %d", len(procedures))
	}
	proc := procedures[0]
	if proc.EndLine != 1 {
		t.Errorf("expected end line 1 for unclosed procedure, got %d", proc.EndLine)
	}
}

func TestParser_ExtractProcedures_LineNumbers(t *testing.T) {
	input := `
:PROCEDURE Spaced;
:ENDPROC;`

	p, root := parseInput(t, input)
	procedures := p.ExtractProcedures(root)

	if len(procedures) != 1 {
		t.Fatalf("expected 1 procedure, got %d", len(procedures))
	}
	proc := procedures[0]
	if proc.StartLine != 2 {
		t.Errorf("expected start line 2, got %d", proc.StartLine)
	}
	if proc.EndLine != 3 {
		t.Errorf("expected end line 3, got %d", proc.EndLine)
	}
}

func TestFindProcedureAtLine(t *testing.T) {
	procedures := []ProcedureInfo{
		{Name: "First", StartLine: 1, EndLine: 3},
		{Name: "Second", StartLine: 5, EndLine: 7},
	}

	found := FindProcedureAtLine(procedures, 6)
	if found == nil || found.Name != "Second" {
		t.Fatalf("expected to find Second procedure, got %+v", found)
	}
	missing := FindProcedureAtLine(procedures, 10)
	if missing != nil {
		t.Errorf("expected nil for missing line, got %+v", missing)
	}
}

// ==================== Variable Extraction Tests ====================

func TestParser_ExtractVariables_Declare(t *testing.T) {
	input := `:DECLARE alpha, beta;`

	p, root := parseInput(t, input)
	variables := p.ExtractVariables(root)

	if len(variables) != 2 {
		t.Fatalf("expected 2 variables, got %d", len(variables))
	}
	alpha := findVariable(variables, "alpha")
	beta := findVariable(variables, "beta")
	if alpha == nil || beta == nil {
		t.Fatalf("expected alpha and beta variables, got %+v", variables)
	}
	if alpha.Scope != ScopeLocal || beta.Scope != ScopeLocal {
		t.Errorf("expected local scope, got alpha=%s beta=%s", alpha.Scope, beta.Scope)
	}

	lines := strings.Split(input, "\n")
	if alpha.Column != strings.Index(lines[0], "alpha")+1 {
		t.Errorf("unexpected alpha column %d", alpha.Column)
	}
	if beta.Column != strings.Index(lines[0], "beta")+1 {
		t.Errorf("unexpected beta column %d", beta.Column)
	}
}

func TestParser_ExtractVariables_PublicAndParameters(t *testing.T) {
	input := `:PUBLIC gamma;
:PARAMETERS delta;`

	p, root := parseInput(t, input)
	variables := p.ExtractVariables(root)

	gamma := findVariable(variables, "gamma")
	delta := findVariable(variables, "delta")
	if gamma == nil || delta == nil {
		t.Fatalf("expected gamma and delta variables, got %+v", variables)
	}
	if gamma.Scope != ScopePublic {
		t.Errorf("expected gamma to be public, got %s", gamma.Scope)
	}
	if delta.Scope != ScopeParameter {
		t.Errorf("expected delta to be parameter, got %s", delta.Scope)
	}
}

// ==================== Block Detection Tests ====================

func TestParser_BlockDetection_IfElse(t *testing.T) {
	input := `:IF x > 0;
value := 1;
:ELSE;
value := 2;
:ENDIF;`

	_, root := parseInput(t, input)
	blocks := collectNodesByType(root, NodeBlock)

	expected := map[int]int{1: 2, 4: 5}
	if len(blocks) != len(expected) {
		t.Fatalf("expected %d blocks, got %d", len(expected), len(blocks))
	}
	for _, block := range blocks {
		endLine, ok := expected[block.StartLine]
		if !ok {
			t.Errorf("unexpected block start line %d", block.StartLine)
			continue
		}
		if block.EndLine != endLine {
			t.Errorf("block at line %d expected end %d, got %d", block.StartLine, endLine, block.EndLine)
		}
	}
}

func TestParser_BlockDetection_TryCatchFinally(t *testing.T) {
	input := `:TRY;
value := 1;
:CATCH;
value := 2;
:FINALLY;
value := 3;
:ENDTRY;`

	_, root := parseInput(t, input)
	blocks := collectNodesByType(root, NodeBlock)

	expected := map[int]int{1: 2, 4: 4, 6: 7}
	if len(blocks) != len(expected) {
		t.Fatalf("expected %d blocks, got %d", len(expected), len(blocks))
	}
	for _, block := range blocks {
		endLine, ok := expected[block.StartLine]
		if !ok {
			t.Errorf("unexpected block start line %d", block.StartLine)
			continue
		}
		if block.EndLine != endLine {
			t.Errorf("block at line %d expected end %d, got %d", block.StartLine, endLine, block.EndLine)
		}
	}
}

func TestParser_BlockDetection_BeginCase(t *testing.T) {
	input := `:BEGINCASE;
:CASE x = 1;
result := 1;
:OTHERWISE;
result := 2;
:ENDCASE;`

	_, root := parseInput(t, input)
	blocks := collectNodesByType(root, NodeBlock)

	expected := map[int]int{1: 6, 3: 3, 5: 6}
	if len(blocks) != len(expected) {
		t.Fatalf("expected %d blocks, got %d", len(expected), len(blocks))
	}
	for _, block := range blocks {
		endLine, ok := expected[block.StartLine]
		if !ok {
			t.Errorf("unexpected block start line %d", block.StartLine)
			continue
		}
		if block.EndLine != endLine {
			t.Errorf("block at line %d expected end %d, got %d", block.StartLine, endLine, block.EndLine)
		}
	}
}

// ==================== Edge Case Tests ====================

func TestParser_EdgeCase_EmptyInput(t *testing.T) {
	_, root := parseInput(t, "")
	if len(root.Children) != 0 {
		t.Errorf("expected no children for empty input, got %d", len(root.Children))
	}
}

func TestParser_EdgeCase_MismatchedBlocks(t *testing.T) {
	input := `:IF x > 0;
value := 1;`

	_, root := parseInput(t, input)
	blocks := collectNodesByType(root, NodeBlock)

	if len(blocks) != 1 {
		t.Fatalf("expected 1 block, got %d", len(blocks))
	}
	if blocks[0].EndLine != 2 {
		t.Errorf("expected block end line 2, got %d", blocks[0].EndLine)
	}
}

func TestParser_EdgeCase_LargeFile(t *testing.T) {
	statementCount := 1000
	parts := make([]string, statementCount)
	for i := 0; i < statementCount; i++ {
		parts[i] = "value := 1;"
	}
	input := strings.Join(parts, " ")

	_, root := parseInput(t, input)
	if len(root.Children) != statementCount {
		t.Errorf("expected %d statements, got %d", statementCount, len(root.Children))
	}
}
