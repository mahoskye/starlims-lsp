package providers

import (
	"testing"

	"starlims-lsp/internal/lexer"
)

func classSites(src string) []ClassSite {
	return ExtractClassSites(lexer.NewLexer(src).Tokenize())
}

func TestExtractClassSites_KindsAndRanges(t *testing.T) {
	src := `:CLASS RushRule;
:INHERIT Rules.DueRule;
:PROCEDURE Counts;
	oRule := CreateUdObject("Rules.DueRule", {1});
	:RETURN oRule:DueDate(1) .AND. Me:Open() .AND. Base:Counts();
:ENDPROC;`
	sites := classSites(src)
	if len(sites) != 5 {
		t.Fatalf("got %d sites, want 5: %+v", len(sites), sites)
	}

	inherit := sites[0]
	if inherit.Kind != ClassInherit || inherit.Raw != "Rules.DueRule" || inherit.Range.Start.Line != 1 ||
		inherit.Range.Start.Character != len(":INHERIT ") || inherit.Range.End.Character != len(":INHERIT Rules.DueRule") {
		t.Errorf("inherit: %+v", inherit)
	}

	create := sites[1]
	wantStart := len("\toRule := CreateUdObject(\"")
	if create.Kind != ClassCreate || create.Raw != "Rules.DueRule" || create.Range.Start.Line != 3 ||
		create.Range.Start.Character != wantStart || create.Range.End.Character != wantStart+len("Rules.DueRule") {
		t.Errorf("create (content, quotes excluded): %+v", create)
	}

	variable := sites[2]
	wantStart = len("\t:RETURN ")
	if variable.Kind != ClassMethod || variable.Raw != "oRule:DueDate" || variable.Receiver != "oRule" ||
		variable.Method != "DueDate" || variable.Class != "Rules.DueRule" ||
		variable.Range.Start.Character != wantStart || variable.Range.End.Character != wantStart+len("oRule:DueDate") {
		t.Errorf("method on a variable: %+v", variable)
	}
	if me := sites[3]; me.Kind != ClassMethod || me.Raw != "Me:Open" || me.Class != "" {
		t.Errorf("Me: %+v", me)
	}
	if base := sites[4]; base.Kind != ClassMethod || base.Raw != "Base:Counts" || base.Method != "Counts" || base.Class != "" {
		t.Errorf("Base: %+v", base)
	}
}

func TestExtractClassSites_CreateForms(t *testing.T) {
	// [spec feature.call_graph/A13]
	// Only a string literal names a class: the other forms make an expando.
	for _, src := range []string{
		`o := CreateUdObject();`,
		`o := CreateUdObject({{"a", 1}});`,
		`o := CreateUdObject(sClass);`,
		`o := CreateUdObject("");`,
		`/* o := CreateUdObject("Rules.DueRule");`,
		`s := "CreateUdObject('Rules.DueRule')";`,
	} {
		if sites := classSites(src); len(sites) != 0 {
			t.Errorf("%s: got %+v, want no site", src, sites)
		}
	}
	sites := classSites("o := CreateUdObject /* which;(\n\t'Rules.DueRule');")
	if len(sites) != 1 || sites[0].Raw != "Rules.DueRule" || sites[0].Range.Start.Line != 1 {
		t.Errorf("multi-line, single quotes, comment before the paren: %+v", sites)
	}
}

func TestExtractClassSites_VariableClassMustBeSettled(t *testing.T) {
	// [spec feature.call_graph/A20]
	methods := func(src string) []ClassSite {
		var out []ClassSite
		for _, s := range classSites(src) {
			if s.Kind == ClassMethod {
				out = append(out, s)
			}
		}
		return out
	}
	create := `o := CreateUdObject("A.B");` + "\n"
	for name, src := range map[string]string{
		"never assigned":          `o:Go();`,
		"assigned something else": create + `o := DoProc("Make");` + "\n" + `o:Go();`,
		"two classes":             create + `o := CreateUdObject("A.C");` + "\n" + `o:Go();`,
		"a parameter":             ":PROCEDURE P;\n:PARAMETERS o;\n" + create + "o:Go();\n:ENDPROC;",
		"another procedure":       ":PROCEDURE P;\n" + create + ":ENDPROC;\n:PROCEDURE Q;\no:Go();\n:ENDPROC;",
		"outside the procedure":   create + ":PROCEDURE Q;\no:Go();\n:ENDPROC;",
		"a create that is used":   `o := CreateUdObject("A.B"):Value;` + "\n" + `o:Go();`,
		"a loop variable":         create + ":FOR o := 1 :TO 2;\n:NEXT;\no:Go();",
		"a member of something":   create + `x:o:Go();`,
		"an element":              create + `a[1]:Go();`,
		"a property, not a call":  create + `n := o:Count;`,
		"a space after the colon": create + `o: Go();`,
	} {
		if got := methods(src); len(got) != 0 {
			t.Errorf("%s: got %+v, want no method site", name, got)
		}
	}
	for name, src := range map[string]string{
		"created once":            create + `o:Go();`,
		"created twice, the same": create + `o := CreateUdObject("a.b", {1});` + "\n" + `o:Go();`,
		"used before it is made":  ":PROCEDURE P;\no:Go();\n" + create + ":ENDPROC;",
		"its member is assigned":  create + "o:Count := 1;\no:Go();",
		"a space before the call": create + `o:Go ();`,
	} {
		got := methods(src)
		if len(got) != 1 || got[0].Raw != "o:Go" || got[0].Class != "A.B" {
			t.Errorf("%s: got %+v, want o:Go on A.B", name, got)
		}
	}
}
