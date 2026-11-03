package providers

import (
	"fmt"
	"strings"
	"testing"
	"unicode/utf8"

	"starlims-lsp/internal/lexer"
)

// describeTokens renders tokens as `text type modifier...`, one per token in
// order, reading each token's text back from the source.
func describeTokens(src string, tokens []SemanticToken) []string {
	lines := strings.Split(src, "\n")
	out := make([]string, 0, len(tokens))
	for _, token := range tokens {
		line := []rune(lines[token.Range.Start.Line])
		desc := string(line[token.Range.Start.Character:token.Range.End.Character]) + " " + SemanticTokenTypes[token.Type]
		for bit, name := range SemanticTokenModifiers {
			if token.Modifiers&(1<<bit) != 0 {
				desc += " " + name
			}
		}
		out = append(out, desc)
	}
	return out
}

func semanticDesc(src string, opts SemanticTokenOptions) []string {
	tokens := lexer.NewLexer(src).Tokenize()
	return describeTokens(src, GetSemanticTokens(src, tokens, nil, opts))
}

func wantTokens(t *testing.T, got []string, want ...string) {
	t.Helper()
	if strings.Join(got, "\n") != strings.Join(want, "\n") {
		t.Errorf("tokens:\n  %s\nwant:\n  %s", strings.Join(got, "\n  "), strings.Join(want, "\n  "))
	}
}

// wantToken asserts that the description appears among the tokens.
func wantToken(t *testing.T, got []string, want string) {
	t.Helper()
	for _, desc := range got {
		if desc == want {
			return
		}
	}
	t.Errorf("no token %q in:\n  %s", want, strings.Join(got, "\n  "))
}

func wantNoTokenFor(t *testing.T, got []string, name string) {
	t.Helper()
	for _, desc := range got {
		if desc == name || strings.HasPrefix(desc, name+" ") {
			t.Errorf("unexpected token %q", desc)
		}
	}
}

func TestSemanticTokensProcedureNames(t *testing.T) {
	// [spec feature.semantic_tokens/A3]
	wantTokens(t, semanticDesc(":PROCEDURE Calc;\n:ENDPROC;\n", SemanticTokenOptions{}),
		"Calc function declaration")
	wantToken(t, semanticDesc(":CLASS Order;\n:PROCEDURE Calc;\n:ENDPROC;\n", SemanticTokenOptions{}),
		"Calc method declaration")
}

func TestSemanticTokensParametersAndLocals(t *testing.T) {
	// [spec feature.semantic_tokens/A4]
	src := `:PROCEDURE First;
:PARAMETERS nQty;
:DEFAULT nQty, 1;
:DECLARE nTotal;
nTotal := nQty;
:ENDPROC;
:PROCEDURE Second;
:DECLARE nQty;
nQty := 1;
:ENDPROC;
`
	wantTokens(t, semanticDesc(src, SemanticTokenOptions{}),
		"First function declaration",
		"nQty parameter declaration",
		"nQty parameter",
		"nTotal variable declaration",
		"nTotal variable",
		"nQty parameter",
		"Second function declaration",
		"nQty variable declaration",
		"nQty variable",
	)
	// Top-level :PARAMETERS bind for the whole script, and a procedure
	// reading what its caller declares finds the caller's declaration.
	src = `:PARAMETERS sInput;
:PROCEDURE A;
:DECLARE sOwn;
:ENDPROC;
:PROCEDURE B;
sInput := sOwn;
:ENDPROC;
`
	got := semanticDesc(src, SemanticTokenOptions{})
	wantToken(t, got, "sInput parameter")
	wantToken(t, got, "sOwn variable")
}

func TestSemanticTokensPublic(t *testing.T) {
	// [spec feature.semantic_tokens/A5]
	src := `:PUBLIC gRate;
:PROCEDURE A;
gRate := 1;
:ENDPROC;
:PROCEDURE B;
:DECLARE gRate;
gRate := 2;
:ENDPROC;
`
	wantTokens(t, semanticDesc(src, SemanticTokenOptions{}),
		"gRate variable declaration public",
		"A function declaration",
		"gRate variable public",
		"B function declaration",
		"gRate variable declaration",
		"gRate variable",
	)
	wantTokens(t, semanticDesc("x := gConfigured;\n", SemanticTokenOptions{GlobalVariables: []string{"gConfigured"}}),
		"x variable", "gConfigured variable public")
}

func TestSemanticTokensLibraryNames(t *testing.T) {
	// [spec feature.semantic_tokens/A6]
	src := "x := MYUSERNAME;\ny := Pending;\nz := Me:sName;\nw := Base:sName;\n"
	got := semanticDesc(src, SemanticTokenOptions{})
	wantToken(t, got, "MYUSERNAME variable defaultLibrary readonly")
	wantToken(t, got, "Pending variable defaultLibrary readonly")
	wantToken(t, got, "Me variable defaultLibrary")
	wantToken(t, got, "Base variable defaultLibrary")

	wantTokens(t, semanticDesc(":DECLARE Pending;\nPending := 1;\n", SemanticTokenOptions{}),
		"Pending variable declaration", "Pending variable")
}

func TestSemanticTokensCallsAndMembers(t *testing.T) {
	// [spec feature.semantic_tokens/A7]
	src := `oMail := Email{};
oForm := FormDto{};
oMail:Subject := Len("x");
oMail:Send();
nGot := Custom();
`
	wantTokens(t, semanticDesc(src, SemanticTokenOptions{}),
		"oMail variable",
		"Email class defaultLibrary",
		"oForm variable",
		"FormDto class",
		"oMail variable",
		"Subject property",
		"Len function defaultLibrary",
		"oMail variable",
		"Send method",
		"nGot variable",
		"Custom function",
	)
}

func TestSemanticTokensUnknown(t *testing.T) {
	// [spec feature.semantic_tokens/A8]
	src := `x := nGhost + nGhost;
nGhost := 1;
y := nGhost;
nOther += 1;
`
	wantTokens(t, semanticDesc(src, SemanticTokenOptions{}),
		"x variable",
		"nGhost variable unknown",
		"nGhost variable unknown",
		"nGhost variable",
		"y variable",
		"nGhost variable",
		"nOther variable unknown",
	)
}

// TestSemanticTokensUnknownAgreesWithCheck compares the two surfaces over
// every fixture, with the check's own pipeline on the one side.
func TestSemanticTokensUnknownAgreesWithCheck(t *testing.T) {
	// [spec feature.semantic_tokens/A9]
	reportedSomewhere := false
	for name, src := range treeFixtures(t) {
		tokens := lexer.NewLexer(src).Tokenize()
		unknownAt := map[Position]string{}
		unknownNames := map[string]bool{}
		for _, token := range GetSemanticTokens(src, tokens, nil, SemanticTokenOptions{}) {
			if token.Modifiers&SemanticUnknown != 0 {
				line := []rune(strings.Split(src, "\n")[token.Range.Start.Line])
				text := strings.ToUpper(string(line[token.Range.Start.Character:token.Range.End.Character]))
				unknownAt[token.Range.Start] = text
				unknownNames[text] = true
			}
		}

		opts := DefaultDiagnosticOptions()
		opts.CheckUndeclaredVars = true
		reported := map[string]bool{}
		for _, d := range GetDiagnosticsFromTokens(tokens, nil, opts) {
			if d.Code != CodeUndeclaredVariable {
				continue
			}
			reportedSomewhere = true
			text, ok := unknownAt[d.Range.Start]
			if !ok {
				t.Errorf("%s: finding %q at %+v has no unknown token", name, d.Message, d.Range.Start)
				continue
			}
			reported[text] = true
		}
		if strings.Contains(src, "@ssl-disable") {
			continue
		}
		for text := range unknownNames {
			if !reported[text] {
				t.Errorf("%s: %s has an unknown token but no finding", name, text)
			}
		}
	}
	if !reportedSomewhere {
		t.Fatal("no fixture reported an undeclared variable; the comparison checked nothing")
	}
}

func TestSemanticTokensUnparsedStatements(t *testing.T) {
	// [spec feature.semantic_tokens/A10]
	// The statement fails on its first line, so diag.undeclared_variable
	// drops its finding for nOops, and that was the name's first use.
	src := "check the nOops;\nx := nOops;\n"
	got := semanticDesc(src, SemanticTokenOptions{})
	wantNoTokenFor(t, got, "check")
	wantNoTokenFor(t, got, "the")
	for _, desc := range got {
		if strings.Contains(desc, "unknown") {
			t.Errorf("unexpected %q", desc)
		}
	}
	wantToken(t, got, "x variable")
	wantToken(t, got, "nOops variable")

	// Prose a comment's early `;` stranded.
	src = "/* first; second part of the comment;\nx := 1;\n"
	got = semanticDesc(src, SemanticTokenOptions{})
	wantNoTokenFor(t, got, "second")
	wantNoTokenFor(t, got, "part")
	wantToken(t, got, "x variable")
}

func TestSemanticTokensIncludes(t *testing.T) {
	// [spec feature.semantic_tokens/A11]
	src := ":INCLUDE LIMS.Helpers;\nx := nShared + nGhost;\n"
	wantTokens(t, semanticDesc(src, SemanticTokenOptions{}),
		"x variable", "nShared variable", "nGhost variable")
	wantTokens(t, semanticDesc(src, SemanticTokenOptions{IncludesResolved: true, IncludeDeclaredVariables: []string{"nShared"}}),
		"x variable", "nShared variable", "nGhost variable unknown")
}

func TestSemanticTokensDataSources(t *testing.T) {
	// [spec feature.semantic_tokens/A12]
	ds := SemanticTokenOptions{IsDataSourceFile: true}
	wantTokens(t, semanticDesc("SELECT name FROM orders WHERE id = 1\n", ds))

	hybrid := ":DSN := starlims;\n:PARAMETERS sName;\nSELECT sName FROM orders WHERE name = @sName\n"
	got := semanticDesc(hybrid, ds)
	wantTokens(t, got, "sName parameter declaration")

	got = semanticDesc(":PARAMETERS sName;\n:DECLARE nCount;\nnCount := nGhost + Len(sName);\n", ds)
	wantToken(t, got, "nCount variable")
	wantToken(t, got, "sName parameter")
	wantNoTokenFor(t, got, "nGhost")
}

// fakeDispatchResolver resolves the dotted targets it holds.
type fakeDispatchResolver struct {
	WorkspaceResolver
	known map[string]bool
}

func (r fakeDispatchResolver) ResolveDispatch(target string) []ResolvedTarget {
	if r.known[target] {
		return []ResolvedTarget{{}}
	}
	return nil
}

func TestSemanticTokensDispatchTargets(t *testing.T) {
	// [spec feature.semantic_tokens/A13]
	src := `:PROCEDURE Local;
:ENDPROC;
:INCLUDE LIMS.Helpers;
x := DoProc("Local");
y := ExecFunction("LIMS.Helpers.Calc", {1});
z := ExecFunction("LIMS.Missing");
a := DoProc("Gone");
b := RunDS("LIMS.GetOrders");
c := "Local";
/* Local;
`
	opts := SemanticTokenOptions{
		IncludesResolved: true,
		Resolver:         fakeDispatchResolver{known: map[string]bool{"LIMS.Helpers.Calc": true}},
	}
	got := semanticDesc(src, opts)
	wantToken(t, got, "Local function")
	wantToken(t, got, "LIMS.Helpers.Calc function")
	wantNoTokenFor(t, got, "LIMS.Missing")
	wantNoTokenFor(t, got, "Gone")
	wantNoTokenFor(t, got, "LIMS.GetOrders")
	wantNoTokenFor(t, got, "LIMS.Helpers")
	if n := strings.Count(strings.Join(got, "\n"), "Local function"); n != 2 {
		t.Errorf("Local has %d function tokens, want the header and the DoProc:\n  %s", n, strings.Join(got, "\n  "))
	}

	// Without a resolver only one-part targets can resolve.
	got = semanticDesc(src, SemanticTokenOptions{IncludesResolved: true})
	wantNoTokenFor(t, got, "LIMS.Helpers.Calc")
}

func TestSemanticTokensLeaveTheRestToTheGrammar(t *testing.T) {
	// [spec feature.semantic_tokens/A17]
	src := `:IF .T. .AND. NIL = 1;
	x := 2 + Len;
:ENDIF;
`
	wantTokens(t, semanticDesc(src, SemanticTokenOptions{}), "x variable")
}

// checkTokenShape asserts the invariants every client relies on.
func checkTokenShape(t *testing.T, name, src string, tokens []SemanticToken) {
	t.Helper()
	lines := strings.Split(src, "\n")
	var prev Position
	for i, token := range tokens {
		r := token.Range
		if r.Start.Line != r.End.Line || r.End.Character <= r.Start.Character {
			t.Fatalf("%s: token %d not one line and non-empty: %+v", name, i, r)
		}
		if i > 0 && positionBefore(r.Start, prev) {
			t.Fatalf("%s: token %d at %+v overlaps or precedes the one before, ending at %+v", name, i, r.Start, prev)
		}
		prev = r.End
		if r.Start.Line >= len(lines) || r.End.Character > utf8.RuneCountInString(lines[r.Start.Line]) {
			t.Fatalf("%s: token %d at %+v is outside the document", name, i, r)
		}
		if token.Type < 0 || token.Type >= len(SemanticTokenTypes) || token.Modifiers >= 1<<len(SemanticTokenModifiers) {
			t.Fatalf("%s: token %d has no legend entry: %+v", name, i, token)
		}
	}
}

func TestSemanticTokensShapeOverFixtures(t *testing.T) {
	// [spec feature.semantic_tokens/A15]
	total := 0
	for name, src := range treeFixtures(t) {
		for _, ds := range []bool{false, true} {
			tokens := GetSemanticTokens(src, lexer.NewLexer(src).Tokenize(), nil, SemanticTokenOptions{IsDataSourceFile: ds})
			checkTokenShape(t, fmt.Sprintf("%s (data source: %v)", name, ds), src, tokens)
			total += len(tokens)
		}
	}
	if total == 0 {
		t.Fatal("no fixture produced a token")
	}
}

// FuzzSemanticTokens checks the same invariants on arbitrary input.
func FuzzSemanticTokens(f *testing.F) {
	addCorpusSeeds(f)
	f.Fuzz(func(t *testing.T, src string) {
		if !utf8.ValidString(src) {
			t.Skip()
		}
		tokens := GetSemanticTokens(src, lexer.NewLexer(src).Tokenize(), nil, SemanticTokenOptions{})
		checkTokenShape(t, "fuzz input", src, tokens)
	})
}
