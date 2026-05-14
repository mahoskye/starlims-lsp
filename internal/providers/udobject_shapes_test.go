package providers

import (
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

func keys(m map[string]UDObjectShape) []string {
	out := make([]string, 0, len(m))
	for k := range m {
		out = append(out, k)
	}
	return out
}
