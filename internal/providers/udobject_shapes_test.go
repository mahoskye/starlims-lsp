package providers

import (
	"testing"

	"starlims-lsp/internal/lexer"
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
