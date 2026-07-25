package providers

import (
	"strings"
	"testing"

	"starlims-lsp/internal/lexer"
	"starlims-lsp/internal/parser"
)

// fakeResolver implements WorkspaceResolver for provider-level tests.
type fakeResolver struct {
	dispatch   map[string][]ResolvedTarget
	include    map[string][]ResolvedTarget
	dataSource map[string][]ResolvedTarget
}

func (f fakeResolver) ResolveDispatch(target string) []ResolvedTarget {
	return f.dispatch[strings.ToLower(target)]
}
func (f fakeResolver) ResolveInclude(target string) []ResolvedTarget {
	return f.include[strings.ToLower(target)]
}
func (f fakeResolver) ResolveDataSource(target string) []ResolvedTarget {
	return f.dataSource[strings.ToLower(target)]
}

func TestDispatchTargetAt(t *testing.T) {
	text := `result := ExecFunction("Cat.Script.Proc", {1});`
	// Cursor inside the target string (column of "Cat...").
	dt := DispatchTargetAt(lexer.NewLexer(text).Tokenize(), 1, 26)
	if dt == nil {
		t.Fatal("expected dispatch target")
	}
	if dt.Raw != "Cat.Script.Proc" || len(dt.Parts) != 3 || dt.IsDoProc {
		t.Errorf("unexpected target: %+v", dt)
	}

	// Cursor outside the string: nil.
	if dt := DispatchTargetAt(lexer.NewLexer(text).Tokenize(), 1, 3); dt != nil {
		t.Errorf("expected nil outside the string, got %+v", dt)
	}
}

func TestIncludeTargetAt(t *testing.T) {
	cases := []struct {
		name   string
		text   string
		column int
		want   string
	}{
		{"bare", ":INCLUDE SharedLib;", 12, "SharedLib"},
		{"dotted", ":INCLUDE File_Helpers.FileWork;", 15, "File_Helpers.FileWork"},
		{"deep dotted", ":INCLUDE A.B.C.D;", 12, "A.B.C.D"},
		{"quoted", `:INCLUDE "MyLibrary";`, 14, "MyLibrary"},
		{"cursor on keyword", ":INCLUDE SharedLib;", 3, "SharedLib"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			tokens := lexer.NewLexer(tc.text).Tokenize()
			it := IncludeTargetAt(tokens, 1, tc.column)
			if it == nil {
				t.Fatalf("expected include target for %q", tc.text)
			}
			if it.Raw != tc.want {
				t.Errorf("Raw = %q, want %q", it.Raw, tc.want)
			}
		})
	}

	// Cursor on an unrelated line: nil.
	tokens := lexer.NewLexer(":INCLUDE SharedLib;\nnCount := 1;").Tokenize()
	if it := IncludeTargetAt(tokens, 2, 3); it != nil {
		t.Errorf("expected nil off the include line, got %+v", it)
	}
}

// [spec feature.definition/A8]
func TestFindDefinitionCrossFile_ThreePartDispatch(t *testing.T) {
	text := `result := ExecFunction("Cat.Script.Proc", {});`
	tokens := lexer.NewLexer(text).Tokenize()
	resolver := fakeResolver{dispatch: map[string][]ResolvedTarget{
		"cat.script.proc": {{URI: "file:///other.ssl", Line: 7, Kind: ResolvedProcedure}},
	}}

	locs := FindDefinitionCrossFile(text, tokens, 1, 26, "file:///self.ssl", nil, nil, resolver)
	if len(locs) != 1 || locs[0].URI != "file:///other.ssl" || locs[0].Range.Start.Line != 7 {
		t.Errorf("expected cross-file procedure location, got %+v", locs)
	}
}

// [spec feature.definition/A9]
func TestFindDefinitionCrossFile_TwoPartEntryPoint(t *testing.T) {
	text := `result := ExecFunction("Cat.Script", {});`
	tokens := lexer.NewLexer(text).Tokenize()
	resolver := fakeResolver{dispatch: map[string][]ResolvedTarget{
		"cat.script": {{URI: "file:///other.ssl", Line: 1, Kind: ResolvedScriptEntry}},
	}}

	locs := FindDefinitionCrossFile(text, tokens, 1, 26, "file:///self.ssl", nil, nil, resolver)
	if len(locs) != 1 || locs[0].URI != "file:///other.ssl" || locs[0].Range.Start.Line != 1 {
		t.Errorf("expected entry-point location, got %+v", locs)
	}
}

// [spec feature.definition/A10]
func TestFindDefinitionCrossFile_IncludeTargets(t *testing.T) {
	resolver := fakeResolver{include: map[string][]ResolvedTarget{
		"sharedlib":     {{URI: "file:///lib.ssl", Line: 0, Kind: ResolvedScriptEntry}},
		"cat.sharedlib": {{URI: "file:///lib.ssl", Line: 0, Kind: ResolvedScriptEntry}},
	}}

	for _, text := range []string{
		`:INCLUDE SharedLib;`,
		`:INCLUDE Cat.SharedLib;`,
		`:INCLUDE "SharedLib";`,
	} {
		tokens := lexer.NewLexer(text).Tokenize()
		locs := FindDefinitionCrossFile(text, tokens, 1, 12, "file:///self.ssl", nil, nil, resolver)
		if len(locs) != 1 || locs[0].URI != "file:///lib.ssl" || locs[0].Range.Start.Line != 0 {
			t.Errorf("text %q: expected include location at line 0, got %+v", text, locs)
		}
	}
}

// [spec feature.definition/A11]
func TestFindDefinitionCrossFile_AmbiguousMultipleLocations(t *testing.T) {
	text := `result := DoProc("Helpers.CalculateTotal", {});`
	tokens := lexer.NewLexer(text).Tokenize()
	resolver := fakeResolver{dispatch: map[string][]ResolvedTarget{
		"helpers.calculatetotal": {
			{URI: "file:///anchored.srvscr", Line: 4, Kind: ResolvedProcedure},
			{URI: "file:///flat.ssl", Line: 4, Kind: ResolvedProcedure},
		},
	}}

	locs := FindDefinitionCrossFile(text, tokens, 1, 20, "file:///self.ssl", nil, nil, resolver)
	if len(locs) != 2 {
		t.Fatalf("expected both candidates, got %+v", locs)
	}
	if locs[0].URI != "file:///anchored.srvscr" {
		t.Errorf("expected anchored candidate first (resolver order preserved), got %+v", locs)
	}
}

// [spec feature.definition/A12]
func TestFindDefinitionCrossFile_CaseInsensitiveTarget(t *testing.T) {
	text := `result := ExecFunction("cat.script.proc", {});`
	tokens := lexer.NewLexer(text).Tokenize()
	// The fake resolver keys lowercase, mirroring the real index's
	// case-insensitive lookups.
	resolver := fakeResolver{dispatch: map[string][]ResolvedTarget{
		"cat.script.proc": {{URI: "file:///other.ssl", Line: 7, Kind: ResolvedProcedure}},
	}}

	locs := FindDefinitionCrossFile(text, tokens, 1, 26, "file:///self.ssl", nil, nil, resolver)
	if len(locs) != 1 || locs[0].URI != "file:///other.ssl" {
		t.Errorf("expected case-insensitive resolution, got %+v", locs)
	}
}

// [spec feature.definition/A6] — dotted target resolving nowhere is null;
// 1-part unknown target is null and never consults the resolver.
func TestFindDefinitionCrossFile_TruthfulNull(t *testing.T) {
	resolver := fakeResolver{dispatch: map[string][]ResolvedTarget{
		// Deliberately non-empty for a 1-part name: must NOT be consulted.
		"someproc": {{URI: "file:///other.ssl", Line: 1, Kind: ResolvedProcedure}},
	}}

	text := `result := DoProc("SomeProc", {});`
	tokens := lexer.NewLexer(text).Tokenize()
	if locs := FindDefinitionCrossFile(text, tokens, 1, 20, "file:///self.ssl", nil, nil, resolver); locs != nil {
		t.Errorf("1-part unknown target must be null, got %+v", locs)
	}

	text = `result := ExecFunction("No.Such.Target", {});`
	tokens = lexer.NewLexer(text).Tokenize()
	if locs := FindDefinitionCrossFile(text, tokens, 1, 26, "file:///self.ssl", nil, nil, resolver); locs != nil {
		t.Errorf("unresolvable dotted target must be null, got %+v", locs)
	}
}

// Nil resolver disables cross-file paths without breaking same-file flow.
func TestFindDefinitionCrossFile_NilResolver(t *testing.T) {
	text := `result := ExecFunction("Cat.Script.Proc", {});`
	tokens := lexer.NewLexer(text).Tokenize()
	if locs := FindDefinitionCrossFile(text, tokens, 1, 26, "file:///self.ssl", nil, nil, nil); locs != nil {
		t.Errorf("expected nil with nil resolver, got %+v", locs)
	}
}

func TestDataSourceTargetAt(t *testing.T) {
	text := `aRows := RunDS("QUERIES.ORDERS", {1});`
	dst := DataSourceTargetAt(lexer.NewLexer(text).Tokenize(), 1, 20)
	if dst == nil {
		t.Fatal("expected RunDS target")
	}
	if dst.Raw != "QUERIES.ORDERS" || len(dst.Parts) != 2 {
		t.Errorf("unexpected target: %+v", dst)
	}

	// Cursor outside the string: nil.
	if dst := DataSourceTargetAt(lexer.NewLexer(text).Tokenize(), 1, 3); dst != nil {
		t.Errorf("expected nil outside the string, got %+v", dst)
	}
}

// [spec feature.definition/A13]
func TestFindDefinitionCrossFile_RunDSTarget(t *testing.T) {
	resolver := fakeResolver{dataSource: map[string][]ResolvedTarget{
		"queries.orders": {{URI: "file:///Orders.ds", Line: 1, Kind: ResolvedScriptEntry}},
		"inventory":      {{URI: "file:///Inventory.ds", Line: 0, Kind: ResolvedScriptEntry}},
	}}

	text := `aRows := RunDS("QUERIES.ORDERS", {});`
	tokens := lexer.NewLexer(text).Tokenize()
	locs := FindDefinitionCrossFile(text, tokens, 1, 20, "file:///main.ssl", nil, nil, resolver)
	if len(locs) != 1 || locs[0].URI != "file:///Orders.ds" || locs[0].Range.Start.Line != 1 {
		t.Fatalf("expected Orders.ds line 1, got %+v", locs)
	}

	// 1-part RunDS targets resolve too (unlike dispatch targets).
	text = `aRows := RunDS("Inventory", {});`
	tokens = lexer.NewLexer(text).Tokenize()
	locs = FindDefinitionCrossFile(text, tokens, 1, 20, "file:///main.ssl", nil, nil, resolver)
	if len(locs) != 1 || locs[0].URI != "file:///Inventory.ds" {
		t.Fatalf("expected Inventory.ds, got %+v", locs)
	}

	// Unresolvable RunDS target: null, no fallthrough to word lookup.
	text = `aRows := RunDS("NO.SUCHDS", {});`
	tokens = lexer.NewLexer(text).Tokenize()
	if locs := FindDefinitionCrossFile(text, tokens, 1, 20, "file:///main.ssl", nil, nil, resolver); locs != nil {
		t.Fatalf("expected nil for unresolvable RunDS target, got %+v", locs)
	}
}

func TestExtractIncludeTargets(t *testing.T) {
	text := ":PARAMETERS sMode;\n:INCLUDE SharedLib;\n:INCLUDE Cat.Helpers;\n:INCLUDE \"Quoted.Lib\";\nnCount := 1;"
	tokens := lexer.NewLexer(text).Tokenize()
	got := ExtractIncludeTargets(tokens)
	want := []string{"SharedLib", "Cat.Helpers", "Quoted.Lib"}
	if len(got) != len(want) {
		t.Fatalf("ExtractIncludeTargets = %v, want %v", got, want)
	}
	for i := range want {
		if got[i] != want[i] {
			t.Errorf("target[%d] = %q, want %q", i, got[i], want[i])
		}
	}

	// No includes: nil.
	tokens = lexer.NewLexer("nCount := 1;").Tokenize()
	if got := ExtractIncludeTargets(tokens); got != nil {
		t.Errorf("expected nil for no includes, got %v", got)
	}
}

// [spec feature.definition/A14]
// [spec feature.definition/A15]
func TestFindDefinitionCrossFile_UDObjectMember(t *testing.T) {
	text := `:PROCEDURE Main;
:DECLARE oObj, Unknown;
oObj := CreateUDObject({{"Name", "x"}});
oObj:Total := 5;
nVal := oObj:Total;
sName := oObj:Name;
x := oObj:Unknown;
:ENDPROC;`
	tokens := lexer.NewLexer(text).Tokenize()
	p := parser.NewParser(tokens)
	ast := p.Parse()
	procedures := p.ExtractProcedures(ast)
	variables := p.ExtractVariables(ast)

	// Augmented property: use on line 5 -> assignment on line 4 (0-based 3).
	locs := FindDefinitionCrossFile(text, tokens, 5, 15, "file:///self.ssl", procedures, variables, nil)
	if len(locs) != 1 || locs[0].Range.Start.Line != 3 || locs[0].Range.Start.Character != 5 {
		t.Errorf("expected Total definition at line 3 char 5, got %+v", locs)
	}

	// Initializer property: use on line 6 -> literal key on line 3 (0-based 2).
	locs = FindDefinitionCrossFile(text, tokens, 6, 16, "file:///self.ssl", procedures, variables, nil)
	if len(locs) != 1 || locs[0].Range.Start.Line != 2 {
		t.Errorf("expected Name definition at line 2, got %+v", locs)
	}

	// Shaped receiver + unknown member: null, even though a variable named
	// Unknown is declared in the file (no word fallback).
	if locs := FindDefinitionCrossFile(text, tokens, 7, 12, "file:///self.ssl", procedures, variables, nil); locs != nil {
		t.Errorf("expected null for unknown member on shaped receiver, got %+v", locs)
	}
}

// --- ExtractCallSites (issue #125) ---

func extractSites(t *testing.T, src string) []CallSite {
	t.Helper()
	return ExtractCallSites(lexer.NewLexer(src).Tokenize())
}

func TestExtractCallSites_DispatchKindsAndRanges(t *testing.T) {
	src := `:PROCEDURE Run;
:RETURN ExecFunction("Cat.Script.Proc", {1});
result := DoProc('Other.Thing');
aData := RunDS("DS_CAT.Orders");
:INCLUDE SharedLib;
:ENDPROC;`
	sites := extractSites(t, src)
	if len(sites) != 4 {
		t.Fatalf("expected 4 sites, got %d: %+v", len(sites), sites)
	}

	if sites[0].Kind != CallDispatch || sites[0].Raw != "Cat.Script.Proc" || sites[0].IsDoProc {
		t.Errorf("site 0: %+v", sites[0])
	}
	if sites[0].Range.Start.Line != 1 {
		t.Errorf("site 0 line = %d, want 1", sites[0].Range.Start.Line)
	}
	wantStart := len(`:RETURN ExecFunction("`)
	if sites[0].Range.Start.Character != wantStart {
		t.Errorf("site 0 start = %d, want %d (content, quotes excluded)", sites[0].Range.Start.Character, wantStart)
	}
	if got := sites[0].Range.End.Character - sites[0].Range.Start.Character; got != len("Cat.Script.Proc") {
		t.Errorf("site 0 span = %d, want %d", got, len("Cat.Script.Proc"))
	}

	if sites[1].Kind != CallDispatch || sites[1].Raw != "Other.Thing" || !sites[1].IsDoProc {
		t.Errorf("site 1 (single quotes): %+v", sites[1])
	}
	if sites[2].Kind != CallDataSource || sites[2].Raw != "DS_CAT.Orders" {
		t.Errorf("site 2 (RunDS): %+v", sites[2])
	}
	if sites[3].Kind != CallInclude || sites[3].Raw != "SharedLib" {
		t.Errorf("site 3 (include): %+v", sites[3])
	}
}

func TestExtractCallSites_MultiLineCall(t *testing.T) {
	src := ":RETURN ExecFunction(\n    \"Cat.Script.Proc\",\n    {1});"
	sites := extractSites(t, src)
	if len(sites) != 1 || sites[0].Raw != "Cat.Script.Proc" {
		t.Fatalf("expected the multi-line call site, got %+v", sites)
	}
	if sites[0].Range.Start.Line != 1 {
		t.Errorf("site line = %d, want 1 (the string's line)", sites[0].Range.Start.Line)
	}
}

func TestExtractCallSites_CommentBetweenNameAndParen(t *testing.T) {
	src := `:RETURN DoProc /* choose the anchored target;("Real.Target");`
	sites := extractSites(t, src)
	if len(sites) != 1 || sites[0].Raw != "Real.Target" {
		t.Fatalf("expected the real target only, got %+v", sites)
	}
}

func TestExtractCallSites_NoFalsePositives(t *testing.T) {
	src := `/* DoProc("InComment.Not.A.Site") ;
sNote := "mentions ExecFunction('X.Y') in prose";
sName := "Cat.Script.Proc";
:RETURN DoProc(sName);`
	if sites := extractSites(t, src); len(sites) != 0 {
		t.Fatalf("expected no sites from comments/strings/variable args, got %+v", sites)
	}
}

// Concatenated targets are not extracted as full targets — the walk stops at
// the first string token, whose partial content never resolves (F8 pin).
func TestExtractCallSites_ConcatenatedTarget(t *testing.T) {
	src := `:RETURN DoProc("CAT." + sName);`
	sites := extractSites(t, src)
	if len(sites) != 1 || sites[0].Raw != "CAT." {
		t.Fatalf("expected the leading partial only, got %+v", sites)
	}
}

func TestExtractCallSites_BracketAndEmptyStringsSkipped(t *testing.T) {
	src := `:RETURN DoProc([Bracket.Not.Legal]);
x := ExecFunction("");`
	if sites := extractSites(t, src); len(sites) != 0 {
		t.Fatalf("expected no sites for bracket/empty strings, got %+v", sites)
	}
}

func TestDispatchTargetAt_MultiLineCall(t *testing.T) {
	src := ":RETURN ExecFunction(\n    \"Cat.Script.Proc\",\n    {1});"
	tokens := lexer.NewLexer(src).Tokenize()
	dt := DispatchTargetAt(tokens, 2, 8) // cursor inside the string on line 2
	if dt == nil || dt.Raw != "Cat.Script.Proc" {
		t.Fatalf("expected multi-line dispatch target, got %+v", dt)
	}
}
