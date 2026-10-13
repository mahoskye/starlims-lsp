package providers

import (
	"fmt"
	"strings"
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

// [spec feature.rename/A20] — a member name is never renameable: its
// receiver's type is unresolved, and a rename would edit every
// like-spelled member in the file, built-in .NET properties included.
func TestRenameRejectsMemberNames(t *testing.T) {
	text := `:PROCEDURE Main;
:DECLARE oA, oB;
oA := Email{};
oB := CreateUdObject();
oA:Subject := "x";
oB:Subject := "y";
:ENDPROC;`
	procs, vars := symbolInfo(t, text)

	// Cursor on `Subject` in `oA:Subject` (line 5, column 5).
	if res := PrepareRename(text, 5, 5, "file:///t.ssl", procs, vars); res != nil {
		t.Errorf("prepare-rename must reject a member name, got %+v", res)
	}
	if res := Rename(text, 5, 5, "Topic", "file:///t.ssl", procs, vars); res != nil {
		t.Errorf("rename must produce no edits for a member name, got %+v", res)
	}
	// The receiver variable itself stays renameable.
	if res := PrepareRename(text, 5, 1, "file:///t.ssl", procs, vars); res == nil {
		t.Error("the receiver variable must stay renameable")
	}
}

// [spec feature.references/A18] — a file-wide variable's references skip a
// procedure that declares its own local or parameter of the same name, and
// report character (rune) columns after non-ASCII text (issue #258).
func TestReferencesFileWideSkipsShadowingProcedure(t *testing.T) {
	text := `:DECLARE sX;
sX := "é" + sX;
:PROCEDURE Shadows;
:PARAMETERS sX;
:RETURN sX;
:ENDPROC;`
	procs, vars := symbolInfo(t, text)

	locs := FindReferencesWithScope(text, 1, 10, "file:///t.ssl", true, procs, vars)
	var got []string
	for _, l := range locs {
		got = append(got, fmt.Sprintf("%d:%d", l.Range.Start.Line, l.Range.Start.Character))
	}
	want := []string{"0:9", "1:0", "1:12"}
	if strings.Join(got, ",") != strings.Join(want, ",") {
		t.Errorf("expected references %v, got %v", want, got)
	}
}

// [spec feature.definition/A19] — a word inside a non-dispatch string or a
// comment is prose: definition returns null even when a declaration of that
// name exists (issue #258).
func TestDefinitionFromStringOrCommentReturnsNothing(t *testing.T) {
	text := `:PROCEDURE Main;
:DECLARE sName;
sName := "sName is bad";
/* sName;
:ENDPROC;`
	procs, vars := symbolInfo(t, text)
	if loc := FindDefinition(text, 3, 12, "file:///t.ssl", procs, vars); loc != nil {
		t.Errorf("definition from inside a string must be null, got %+v", loc)
	}
	if loc := FindDefinition(text, 4, 5, "file:///t.ssl", procs, vars); loc != nil {
		t.Errorf("definition from inside a comment must be null, got %+v", loc)
	}
	if loc := FindDefinition(text, 3, 2, "file:///t.ssl", procs, vars); loc == nil {
		t.Error("the code occurrence must still resolve")
	}
}
