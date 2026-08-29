package parser

import (
	"testing"

	"starlims-lsp/internal/lexer"
)

func declsOf(t *testing.T, src string) []DeclarationStatement {
	t.Helper()
	return CollectDeclarations(lexer.NewLexer(src).Tokenize())
}

func namesOf(decls []DeclarationStatement) []string {
	var out []string
	for _, d := range decls {
		for _, n := range d.Names {
			out = append(out, n.Name)
		}
	}
	return out
}

func TestCollectDeclarationsLayoutIndependent(t *testing.T) {
	// The same declarations written four ways must yield the same names.
	// The bare-keyword form (`:DECLARE` alone on its line) is the one the
	// previous AST-node walk lost entirely.
	sources := []string{
		`:DECLARE sOne, sTwo, sThree;`,
		":DECLARE sOne,\n\tsTwo,\n\tsThree;",
		":DECLARE\nsOne,\nsTwo,\nsThree;",
		":DECLARE sOne, /* first;\n\tsTwo, /* second;\n\tsThree;",
	}
	for _, src := range sources {
		got := namesOf(declsOf(t, src))
		if len(got) != 3 || got[0] != "sOne" || got[1] != "sTwo" || got[2] != "sThree" {
			t.Errorf("%q: expected [sOne sTwo sThree], got %v", src, got)
		}
	}
}

func TestCollectDeclarationsScopes(t *testing.T) {
	decls := declsOf(t, `:PROCEDURE Demo;
:PARAMETERS sIn;
:DECLARE nLocal;
:PUBLIC gShared;
:ENDPROC;`)

	want := []struct {
		name  string
		scope VariableScope
	}{
		{"sIn", ScopeParameter},
		{"nLocal", ScopeLocal},
		{"gShared", ScopePublic},
	}
	if len(decls) != len(want) {
		t.Fatalf("expected %d declarations, got %d", len(want), len(decls))
	}
	for i, w := range want {
		if len(decls[i].Names) != 1 || decls[i].Names[0].Name != w.name || decls[i].Scope != w.scope {
			t.Errorf("declaration %d: expected %s/%s, got %+v", i, w.name, w.scope, decls[i])
		}
	}
}

func TestCollectDeclarationsInitializerTargetOnly(t *testing.T) {
	// `:DECLARE x := y;` is itself flagged (diag.declare_initializer); the
	// declaration binds x, and y on the right-hand side is a use.
	got := namesOf(declsOf(t, `:DECLARE nCount := nOther, sName;`))
	if len(got) != 2 || got[0] != "nCount" || got[1] != "sName" {
		t.Fatalf("expected [nCount sName], got %v", got)
	}
}

func TestDeclarationSpansCoverBindingStatements(t *testing.T) {
	src := ":DECLARE\nsOne;\n:PROCEDURE Demo;\nsOne := 1;\n:ENDPROC;"
	tokens := lexer.NewLexer(src).Tokenize()
	spans := DeclarationSpans(tokens)
	if len(spans) != len(tokens) {
		t.Fatalf("expected one span flag per token, got %d for %d tokens", len(spans), len(tokens))
	}

	var declaredName, procName, useSite int = -1, -1, -1
	seen := 0
	for i, tok := range tokens {
		if tok.Type != lexer.TokenIdentifier {
			continue
		}
		switch tok.Text {
		case "sOne":
			seen++
			if declaredName < 0 {
				declaredName = i
			} else {
				useSite = i
			}
		case "Demo":
			procName = i
		}
	}
	if seen != 2 || procName < 0 {
		t.Fatalf("test source did not produce the expected identifiers")
	}
	if !spans[declaredName] {
		t.Error("the declared name's own position should be a binding position")
	}
	if !spans[procName] {
		t.Error("a :PROCEDURE header names the procedure, not a variable")
	}
	if spans[useSite] {
		t.Error("the assignment target is a use, not a binding position")
	}
}
