package providers

import (
	"testing"

	"starlims-lsp/internal/lexer"
	"starlims-lsp/internal/parser"
)

// roleFixture declares a variable, a property of the same spelling on
// another object, and a procedure of the same spelling — the three
// symbols word matching used to conflate.
const roleFixture = `:PROCEDURE Demo;
:DECLARE sName, oRec;
sName := "x";
oRec:sName := "property";
UsrMes(sName);
UsrMes(sName());
:ENDPROC;

:PROCEDURE sName;
:RETURN 1;
:ENDPROC;`

func symbolInfo(t *testing.T, text string) ([]parser.ProcedureInfo, []parser.VariableInfo) {
	t.Helper()
	p := parser.NewParser(lexer.NewLexer(text).Tokenize())
	ast := p.Parse()
	return p.ExtractProcedures(ast), p.ExtractVariables(ast)
}

func editedLines(res *RenameResult) []int {
	var lines []int
	for _, edits := range res.Changes {
		for _, e := range edits {
			lines = append(lines, e.Range.Start.Line)
		}
	}
	return lines
}

func containsLine(lines []int, want int) bool {
	for _, l := range lines {
		if l == want {
			return true
		}
	}
	return false
}

// [spec feature.rename/A17] [spec feature.rename/A18]
func TestRenameIgnoresOtherSymbolsOfTheSameName(t *testing.T) {
	procs, vars := symbolInfo(t, roleFixture)

	// Cursor on the variable use at line 5 (1-based), column 8.
	res := Rename(roleFixture, 5, 8, "sRenamed", "file:///t.ssl", procs, vars)
	if res == nil {
		t.Fatal("variable rename returned nil")
	}
	lines := editedLines(res)
	if containsLine(lines, 3) {
		t.Error("A17: the member access oRec:sName must not be edited by a variable rename")
	}
	if containsLine(lines, 8) || containsLine(lines, 5) {
		t.Error("A18: the like-named procedure header and its call must not be edited by a variable rename")
	}
	for _, want := range []int{1, 2, 4} {
		if !containsLine(lines, want) {
			t.Errorf("expected the variable's own occurrence on line %d to be edited, got %v", want, lines)
		}
	}

	// Cursor on the procedure header at line 9.
	res2 := Rename(roleFixture, 9, 12, "ProcRenamed", "file:///t.ssl", procs, vars)
	if res2 == nil {
		t.Fatal("procedure rename returned nil")
	}
	lines2 := editedLines(res2)
	if containsLine(lines2, 1) || containsLine(lines2, 2) || containsLine(lines2, 4) {
		t.Errorf("A18: a procedure rename must not edit the like-named variable, got %v", lines2)
	}
	if containsLine(lines2, 3) {
		t.Errorf("A17: a procedure rename must not edit a member access, got %v", lines2)
	}
	if !containsLine(lines2, 8) {
		t.Errorf("expected the procedure header to be edited, got %v", lines2)
	}
}

// [spec feature.references/A15] [spec feature.references/A16]
func TestReferencesIgnoreOtherSymbolsOfTheSameName(t *testing.T) {
	procs, vars := symbolInfo(t, roleFixture)

	locs := FindReferencesWithScope(roleFixture, 5, 8, "file:///t.ssl", true, procs, vars)
	for _, l := range locs {
		if l.Range.Start.Line == 3 {
			t.Error("A15: the member-access occurrence must not be returned as a reference to the variable")
		}
		if l.Range.Start.Line == 8 {
			t.Error("A16: the like-named procedure must not be returned as a reference to the variable")
		}
	}
	if len(locs) == 0 {
		t.Fatal("expected the variable's own references to be returned")
	}
}

// [spec feature.definition/A16]
func TestDefinitionOnMemberNameReturnsNothing(t *testing.T) {
	procs, vars := symbolInfo(t, roleFixture)

	if loc := FindDefinition(roleFixture, 4, 6, "file:///t.ssl", procs, vars); loc != nil {
		t.Errorf("A16: expected no definition for a member name, got line %d", loc.Range.Start.Line)
	}
	if loc := FindDefinition(roleFixture, 5, 8, "file:///t.ssl", procs, vars); loc == nil {
		t.Error("the variable use must still resolve to its declaration")
	}
}

// [spec feature.hover/A19]
func TestHoverOnMemberNameSkipsLocalVariable(t *testing.T) {
	procs, vars := symbolInfo(t, roleFixture)

	if h := GetHover(roleFixture, 4, 6, procs, vars); h != nil {
		t.Errorf("A19: expected no local-variable hover for a member name, got %q", h.Contents)
	}
	if h := GetHover(roleFixture, 5, 8, procs, vars); h == nil {
		t.Error("the variable use must still hover")
	}
}

func TestIdentifierRolesClassification(t *testing.T) {
	tokens := lexer.NewLexer(roleFixture).Tokenize()
	roles := parser.IdentifierRoles(tokens)

	seen := map[string][]parser.IdentifierRole{}
	for i, tok := range tokens {
		if tok.Type == lexer.TokenIdentifier {
			seen[tok.Text] = append(seen[tok.Text], roles[i])
		}
	}
	got := seen["sName"]
	want := []parser.IdentifierRole{
		parser.RoleDeclaredName, // :DECLARE sName
		parser.RoleVariable,     // sName := "x"
		parser.RoleMember,       // oRec:sName
		parser.RoleVariable,     // UsrMes(sName)
		parser.RoleCall,         // UsrMes(sName())
		parser.RoleProcedureName,
	}
	if len(got) != len(want) {
		t.Fatalf("expected %d sName occurrences, got %d (%v)", len(want), len(got), got)
	}
	for i := range want {
		if got[i] != want[i] {
			t.Errorf("sName occurrence %d: expected %v, got %v", i, want[i], got[i])
		}
	}
}
