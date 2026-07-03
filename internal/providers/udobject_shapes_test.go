package providers

import (
	"strings"
	"testing"

	"starlims-lsp/internal/lexer"
	"starlims-lsp/internal/parser"
)

func tokenize(t *testing.T, src string) []lexer.Token {
	t.Helper()
	lex := lexer.NewLexer(src)
	return lex.Tokenize()
}

func TestBuildUDObjectShapes_CreateUDObject(t *testing.T) {
	src := `:PROCEDURE Demo;
:DECLARE oTemplate;

oTemplate := CreateUDObject({
    {"tableName", ""},
    {"exists", .F.},
    {"tableSql", ""},
    {"rowCount", 0}
});

:ENDPROC;`

	shapes := BuildUDObjectShapes(tokenize(t, src))
	got, ok := shapes["otemplate"]
	if !ok {
		t.Fatalf("expected shape for oTemplate, got: %#v", shapes)
	}
	wantNames := []string{"tableName", "exists", "tableSql", "rowCount"}
	if len(got.Properties) != len(wantNames) {
		t.Fatalf("expected %d props, got %d (%#v)", len(wantNames), len(got.Properties), got.Properties)
	}
	for i, want := range wantNames {
		if got.Properties[i].Name != want {
			t.Errorf("prop[%d].Name = %q, want %q", i, got.Properties[i].Name, want)
		}
	}

	// Spot-check inferred types.
	wantTypes := map[string]string{
		"tableName": "string",
		"exists":    "boolean",
		"rowCount":  "number",
	}
	for _, p := range got.Properties {
		if want, ok := wantTypes[p.Name]; ok && p.Type != want {
			t.Errorf("prop %q type = %q, want %q", p.Name, p.Type, want)
		}
	}
}

func TestBuildUDObjectShapes_ClonePropagation(t *testing.T) {
	src := `:PROCEDURE Demo;

oTemplate := CreateUDObject({
    {"tableName", ""},
    {"exists", .F.}
});

oMetadata := oTemplate:clone();

:ENDPROC;`

	shapes := BuildUDObjectShapes(tokenize(t, src))
	if _, ok := shapes["ometadata"]; !ok {
		t.Fatalf("expected oMetadata to inherit shape from clone(), got: %#v", shapes)
	}
	if len(shapes["ometadata"].Properties) != 2 {
		t.Errorf("expected oMetadata to have 2 props, got %d", len(shapes["ometadata"].Properties))
	}
	if shapes["ometadata"].Properties[0].Name != "tableName" {
		t.Errorf("expected first prop tableName, got %q", shapes["ometadata"].Properties[0].Name)
	}
}

func TestBuildUDObjectShapes_CloneFromUntracked(t *testing.T) {
	// clone() on a variable that was never assigned a tracked shape must
	// not invent a shape — it should just be absent from the map.
	src := `:PROCEDURE Demo;
oFoo := SomethingElse();
oBar := oFoo:clone();
:ENDPROC;`

	shapes := BuildUDObjectShapes(tokenize(t, src))
	if _, ok := shapes["obar"]; ok {
		t.Errorf("expected no shape for oBar (source untracked), got: %#v", shapes["obar"])
	}
}

// [spec feature.completion/A3]
func TestGetUDObjectShapeCompletions(t *testing.T) {
	shapes := map[string]UDObjectShape{
		"ofoo": {Properties: []UDObjectProperty{
			{Name: "alpha", Type: "string"},
			{Name: "beta", Type: "boolean"},
		}},
	}

	items := GetUDObjectShapeCompletions("oFoo", shapes)
	if len(items) != 2 {
		t.Fatalf("expected 2 completions, got %d", len(items))
	}
	if items[0].Label != "alpha" || items[0].Kind != CompletionKindProperty {
		t.Errorf("unexpected first item: %#v", items[0])
	}

	// Unknown var → nil.
	if got := GetUDObjectShapeCompletions("oUnknown", shapes); got != nil {
		t.Errorf("expected nil for unknown var, got %#v", got)
	}
}

func TestBuildUDObjectShapes_Reassignment(t *testing.T) {
	// Last write wins.
	src := `:PROCEDURE Demo;
oFoo := CreateUDObject({{"a", ""}});
oFoo := CreateUDObject({{"b", 0}, {"c", .T.}});
:ENDPROC;`

	shapes := BuildUDObjectShapes(tokenize(t, src))
	got := shapes["ofoo"]
	if len(got.Properties) != 2 {
		t.Fatalf("expected last assignment to win (2 props), got %d", len(got.Properties))
	}
	if got.Properties[0].Name != "b" {
		t.Errorf("expected first prop 'b', got %q", got.Properties[0].Name)
	}
}

func TestBuildUDObjectShapes_PropertyAssignmentAugmentsShape(t *testing.T) {
	// vs-code-ssl-formatter#73 — properties added via `oVar:newProp := …`
	// should appear in the shape even though they weren't in the original
	// CreateUDObject initializer.
	src := `:PROCEDURE Demo;
oFoo := CreateUDObject({{"tableName", ""}});
oFoo:lateProp := "hello";
oFoo:numericProp := 42;
:ENDPROC;`

	shapes := BuildUDObjectShapes(tokenize(t, src))
	got, ok := shapes["ofoo"]
	if !ok {
		t.Fatalf("expected shape for oFoo")
	}
	names := map[string]string{}
	for _, p := range got.Properties {
		names[p.Name] = p.Type
	}
	if _, ok := names["tableName"]; !ok {
		t.Errorf("missing original prop tableName: %#v", got.Properties)
	}
	if got := names["lateProp"]; got != "string" {
		t.Errorf("lateProp: got %q, want string", got)
	}
	if got := names["numericProp"]; got != "number" {
		t.Errorf("numericProp: got %q, want number", got)
	}
}

func TestBuildUDObjectShapes_PropertyAssignment_NoPriorInitializer(t *testing.T) {
	// Even without a CreateUDObject anchor, repeated property assignments to
	// the same variable should build up an implicit shape (best-effort —
	// could be a strongly-typed object, could be a UDObject; either way the
	// list of assigned properties is useful for completion).
	src := `:PROCEDURE Demo;
oBar:alpha := "x";
oBar:beta := .T.;
:ENDPROC;`

	shapes := BuildUDObjectShapes(tokenize(t, src))
	got, ok := shapes["obar"]
	if !ok {
		t.Fatalf("expected implicit shape for oBar")
	}
	if len(got.Properties) != 2 {
		t.Fatalf("expected 2 props, got %d (%#v)", len(got.Properties), got.Properties)
	}
}

func TestBuildUDObjectShapes_CrossProcedurePropagation(t *testing.T) {
	// vs-code-ssl-formatter#73 — when a shaped UDObject is passed into a
	// procedure, that procedure's first parameter inherits the caller's
	// shape so completions inside the callee see the same properties.
	src := `:PROCEDURE Build;
oResult := CreateUDObject({{"alpha", ""}, {"beta", 0}});
DoProc("Use", {oResult});
:ENDPROC;

:PROCEDURE Use;
:PARAMETERS oIncoming;
:ENDPROC;`

	tokens := tokenize(t, src)
	p := parser.NewParser(tokens)
	root := p.Parse()
	procedures := p.ExtractProcedures(root)

	shapes := BuildUDObjectShapesWithProcedures(tokens, procedures)
	got, ok := shapes["oincoming"]
	if !ok {
		t.Fatalf("expected shape on callee parameter oIncoming; have keys: %v", keys(shapes))
	}
	names := map[string]bool{}
	for _, p := range got.Properties {
		names[p.Name] = true
	}
	if !names["alpha"] || !names["beta"] {
		t.Errorf("expected propagated props {alpha, beta}, got %#v", got.Properties)
	}
}

func TestBuildUDObjectShapes_MultiArgPositionalBinding(t *testing.T) {
	// `DoProc("Bar", {oFirst, oSecond})` should bind oFirst's shape to the
	// callee's first parameter and oSecond's to the second.
	src := `:PROCEDURE Build;
oFirst := CreateUDObject({{"a", ""}});
oSecond := CreateUDObject({{"b", 0}});
DoProc("Use", {oFirst, oSecond});
:ENDPROC;

:PROCEDURE Use;
:PARAMETERS oOne, oTwo;
:ENDPROC;`
	tokens := tokenize(t, src)
	p := parser.NewParser(tokens)
	procedures := p.ExtractProcedures(p.Parse())
	shapes := BuildUDObjectShapesWithProcedures(tokens, procedures)

	oneShape, ok := shapes["oone"]
	if !ok || len(oneShape.Properties) == 0 {
		t.Fatalf("expected oOne to inherit shape from oFirst; got %#v", shapes)
	}
	if oneShape.Properties[0].Name != "a" {
		t.Errorf("oOne should carry prop 'a', got %#v", oneShape.Properties)
	}

	twoShape, ok := shapes["otwo"]
	if !ok || len(twoShape.Properties) == 0 {
		t.Fatalf("expected oTwo to inherit shape from oSecond; got %#v", shapes)
	}
	if twoShape.Properties[0].Name != "b" {
		t.Errorf("oTwo should carry prop 'b', got %#v", twoShape.Properties)
	}
}

func TestBuildUDObjectShapes_StatementStartGuard(t *testing.T) {
	// `Foo(oBar:prop := 1)` is a function call with a named-argument-like
	// construct. The `oBar:prop := 1` inside parentheses must NOT be
	// mistaken for a statement-level property assignment that augments
	// oBar's shape.
	src := `:PROCEDURE Demo;
oBar := CreateUDObject({{"existing", ""}});
SomeFunc(oBar:trickProp := 1);
:ENDPROC;`
	shapes := BuildUDObjectShapes(tokenize(t, src))
	got := shapes["obar"]
	for _, p := range got.Properties {
		if strings.EqualFold(p.Name, "trickProp") {
			t.Errorf("inside-parens assignment leaked into shape: %#v", got.Properties)
		}
	}
}

func TestBuildUDObjectShapes_PropagationThroughCalleeChain(t *testing.T) {
	// Build → Use1 → Use2. When the fixpoint runs, Use2's parameter
	// should also receive the shape originally built in Build, because
	// Use1's call to Use2 happens AFTER Use1's parameter has been bound.
	src := `:PROCEDURE Build;
oResult := CreateUDObject({{"prop", ""}});
DoProc("Use1", {oResult});
:ENDPROC;

:PROCEDURE Use1;
:PARAMETERS oFromBuild;
DoProc("Use2", {oFromBuild});
:ENDPROC;

:PROCEDURE Use2;
:PARAMETERS oFinal;
:ENDPROC;`
	tokens := tokenize(t, src)
	p := parser.NewParser(tokens)
	procedures := p.ExtractProcedures(p.Parse())
	shapes := BuildUDObjectShapesWithProcedures(tokens, procedures)

	finalShape, ok := shapes["ofinal"]
	if !ok || len(finalShape.Properties) == 0 {
		t.Fatalf("expected propagation to reach oFinal; got keys %v", keys(shapes))
	}
	if finalShape.Properties[0].Name != "prop" {
		t.Errorf("oFinal: expected prop 'prop', got %#v", finalShape.Properties)
	}
}

func TestBuildUDObjectShapes_ReassignmentDoesNotEraseAugmentations(t *testing.T) {
	// After CreateUDObject + augmentation, a property assignment continues
	// to extend the shape. The "last write wins" rule applies to fresh
	// CreateUDObject assignments only.
	src := `:PROCEDURE Demo;
oFoo := CreateUDObject({{"a", ""}});
oFoo:b := 1;
oFoo:c := 2;
:ENDPROC;`
	shapes := BuildUDObjectShapes(tokenize(t, src))
	got := shapes["ofoo"]
	names := map[string]bool{}
	for _, p := range got.Properties {
		names[p.Name] = true
	}
	for _, want := range []string{"a", "b", "c"} {
		if !names[want] {
			t.Errorf("expected prop %q in augmented shape, got %#v", want, got.Properties)
		}
	}
}

func TestBuildUDObjectShapes_DoProcWithoutArgsArray(t *testing.T) {
	// `DoProc("Name")` — no comma, no args array. Propagation must
	// short-circuit gracefully.
	src := `:PROCEDURE Build;
oResult := CreateUDObject({{"prop", ""}});
DoProc("Use");
:ENDPROC;

:PROCEDURE Use;
:PARAMETERS oIncoming;
:ENDPROC;`
	tokens := tokenize(t, src)
	p := parser.NewParser(tokens)
	procedures := p.ExtractProcedures(p.Parse())
	shapes := BuildUDObjectShapesWithProcedures(tokens, procedures)
	// oIncoming receives no propagation (no args were passed).
	if got, ok := shapes["oincoming"]; ok && len(got.Properties) > 0 {
		t.Errorf("expected no shape on oIncoming when no args were passed, got %#v", got.Properties)
	}
}

func TestBuildUDObjectShapes_MoreArgsThanParameters(t *testing.T) {
	// Caller passes 3 shaped args, callee has only 2 parameters. The
	// extra arg must be ignored (no panic, no out-of-range access).
	src := `:PROCEDURE Build;
oA := CreateUDObject({{"a", ""}});
oB := CreateUDObject({{"b", ""}});
oC := CreateUDObject({{"c", ""}});
DoProc("Use", {oA, oB, oC});
:ENDPROC;

:PROCEDURE Use;
:PARAMETERS oOne, oTwo;
:ENDPROC;`
	tokens := tokenize(t, src)
	p := parser.NewParser(tokens)
	procedures := p.ExtractProcedures(p.Parse())
	shapes := BuildUDObjectShapesWithProcedures(tokens, procedures)
	if shapes["oone"].Properties[0].Name != "a" {
		t.Errorf("oOne should carry 'a', got %#v", shapes["oone"].Properties)
	}
	if shapes["otwo"].Properties[0].Name != "b" {
		t.Errorf("oTwo should carry 'b', got %#v", shapes["otwo"].Properties)
	}
	// No third parameter — extra arg simply doesn't propagate.
}

func TestBuildUDObjectShapes_CalleeHasNoParameters(t *testing.T) {
	// A target procedure with zero parameters: nothing to bind. We assert
	// both that the call doesn't panic AND that the caller's own shape
	// remains intact (no accidental clobber via the propagation loop).
	src := `:PROCEDURE Build;
oResult := CreateUDObject({{"prop", ""}});
DoProc("Use", {oResult});
:ENDPROC;

:PROCEDURE Use;
:ENDPROC;`
	tokens := tokenize(t, src)
	p := parser.NewParser(tokens)
	procedures := p.ExtractProcedures(p.Parse())
	shapes := BuildUDObjectShapesWithProcedures(tokens, procedures)
	if got := shapes["oresult"]; len(got.Properties) != 1 || got.Properties[0].Name != "prop" {
		t.Errorf("caller's shape clobbered by propagation through zero-param callee: %#v", got)
	}
}

func TestBuildUDObjectShapes_MixedArgs_NonShapedSkipped(t *testing.T) {
	// Caller passes [oShaped, "literal", nUntracked]. Only oShaped's
	// shape should propagate, to the first parameter. The literal and
	// the untracked variable must not cause off-by-one mis-binding.
	src := `:PROCEDURE Build;
oShaped := CreateUDObject({{"prop", ""}});
:DECLARE nUntracked;
DoProc("Use", {oShaped, "literal", nUntracked});
:ENDPROC;

:PROCEDURE Use;
:PARAMETERS oOne, sTwo, oThree;
:ENDPROC;`
	tokens := tokenize(t, src)
	p := parser.NewParser(tokens)
	procedures := p.ExtractProcedures(p.Parse())
	shapes := BuildUDObjectShapesWithProcedures(tokens, procedures)
	if len(shapes["oone"].Properties) == 0 || shapes["oone"].Properties[0].Name != "prop" {
		t.Errorf("oOne should receive prop from oShaped, got %#v", shapes["oone"])
	}
	// sTwo and oThree must NOT have invented shapes.
	if got, ok := shapes["stwo"]; ok && len(got.Properties) > 0 {
		t.Errorf("sTwo should have no shape (literal arg), got %#v", got)
	}
	if got, ok := shapes["othree"]; ok && len(got.Properties) > 0 {
		t.Errorf("oThree should have no shape (untracked arg), got %#v", got)
	}
}

func TestBuildUDObjectShapes_AugmentedPropertyPropagates(t *testing.T) {
	// Augmentation followed by DoProc: the late-added property must reach
	// the callee. Validates that the fixpoint loop runs augmentation +
	// propagation together rather than freezing the shape too early.
	src := `:PROCEDURE Build;
oFoo := CreateUDObject({{"initial", ""}});
oFoo:lateAdded := "x";
DoProc("Use", {oFoo});
:ENDPROC;

:PROCEDURE Use;
:PARAMETERS oRecv;
:ENDPROC;`
	tokens := tokenize(t, src)
	p := parser.NewParser(tokens)
	procedures := p.ExtractProcedures(p.Parse())
	shapes := BuildUDObjectShapesWithProcedures(tokens, procedures)
	recv := shapes["orecv"]
	names := map[string]bool{}
	for _, p := range recv.Properties {
		names[p.Name] = true
	}
	if !names["initial"] {
		t.Errorf("oRecv missing 'initial': %#v", recv.Properties)
	}
	if !names["lateAdded"] {
		t.Errorf("oRecv missing augmented 'lateAdded': %#v", recv.Properties)
	}
}

func TestBuildUDObjectShapes_DoProcWithVariableName_NoPropagation(t *testing.T) {
	// `DoProc(sName, {oFoo})` — first arg is a variable, not a string
	// literal. Without a resolvable target, no propagation can happen.
	// Must not crash and must not bind oFoo to some random procedure.
	src := `:PROCEDURE Build;
:DECLARE sName;
oFoo := CreateUDObject({{"a", ""}});
sName := "Use";
DoProc(sName, {oFoo});
:ENDPROC;

:PROCEDURE Use;
:PARAMETERS oRecv;
:ENDPROC;`
	tokens := tokenize(t, src)
	p := parser.NewParser(tokens)
	procedures := p.ExtractProcedures(p.Parse())
	shapes := BuildUDObjectShapesWithProcedures(tokens, procedures)
	if got, ok := shapes["orecv"]; ok && len(got.Properties) > 0 {
		t.Errorf("expected no propagation through variable proc name, got %#v", got.Properties)
	}
}

func keys(m map[string]UDObjectShape) []string {
	out := make([]string, 0, len(m))
	for k := range m {
		out = append(out, k)
	}
	return out
}

const memberFixture = `:PROCEDURE Main;
:DECLARE oObj, Unknown;
oObj := CreateUDObject({{"Name", "x"}});
oObj:Total := 5;
nVal := oObj:Total;
sName := oObj:Name;
x := oObj:Unknown;
:ENDPROC;`

func TestUDObjectProperty_Locations(t *testing.T) {
	shapes := BuildUDObjectShapes(tokenize(t, memberFixture))
	shape, ok := shapes["oobj"]
	if !ok {
		t.Fatalf("expected shape for oObj, got %#v", shapes)
	}

	name := FindShapeProperty(shape, "name")
	if name == nil || name.Line != 3 {
		t.Errorf("Name property should locate at the initializer key (line 3), got %+v", name)
	}
	total := FindShapeProperty(shape, "TOTAL")
	if total == nil || total.Line != 4 || total.Column != 6 {
		t.Errorf("Total property should locate at the augmenting assignment (line 4, col 6), got %+v", total)
	}
}

func TestMemberAccessAt(t *testing.T) {
	tokens := tokenize(t, memberFixture)

	// Cursor on the member of `oObj:Total` (line 5, inside "Total").
	recv, member, ok := MemberAccessAt(tokens, 5, 15)
	if !ok || recv != "oObj" || member != "Total" {
		t.Errorf("expected (oObj, Total), got (%q, %q, %v)", recv, member, ok)
	}

	// Cursor on the receiver: not a member access.
	if _, _, ok := MemberAccessAt(tokens, 5, 10); ok {
		t.Error("cursor on the receiver must not report a member access")
	}

	// Cursor on a plain identifier: not a member access.
	if _, _, ok := MemberAccessAt(tokens, 5, 2); ok {
		t.Error("plain identifier must not report a member access")
	}

	// Keyword colon forms (:DECLARE) never match.
	if _, _, ok := MemberAccessAt(tokens, 2, 5); ok {
		t.Error("keyword after ':' must not report a member access")
	}
}

// [spec feature.hover/A15]
func TestRenderUDObjectMemberHover(t *testing.T) {
	shapes := BuildUDObjectShapes(tokenize(t, memberFixture))
	shape := shapes["oobj"]

	md := RenderUDObjectMemberHover(shape, "oObj", "total")
	for _, want := range []string{"Total", "number", "oObj", "line 4"} {
		if !strings.Contains(md, want) {
			t.Errorf("member hover missing %q:\n%s", want, md)
		}
	}

	if md := RenderUDObjectMemberHover(shape, "oObj", "Missing"); md != "" {
		t.Errorf("unknown member must render empty, got %q", md)
	}
}
