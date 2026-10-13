package parser

import (
	"fmt"
	"reflect"
	"strings"
	"testing"

	"starlims-lsp/internal/lexer"
)

// Issue #258 parser fixes.

func procParams(t *testing.T, input, name string) []string {
	t.Helper()
	p, root := parseInput(t, input)
	for _, proc := range p.ExtractProcedures(root) {
		if proc.Name == name {
			return proc.Parameters
		}
	}
	t.Fatalf("procedure %s not found", name)
	return nil
}

// (a) Procedure parameters come from the declaration statement, so layout
// cannot hide a name.
func TestProcedureParameters_LayoutCannotHideNames(t *testing.T) {
	tests := []struct {
		name  string
		input string
		want  []string
	}{
		{"inline comment in the list", `:PROCEDURE Demo;
:PARAMETERS sA, /* the second;
 nB, /* the third;
 dC;
:ENDPROC;`, []string{"sA", "nB", "dC"}},
		{"bare keyword, names on later lines", `:PROCEDURE Demo;
:PARAMETERS
	sA,
	nB;
:ENDPROC;`, []string{"sA", "nB"}},
		{"many comment lines before :PARAMETERS", `:PROCEDURE Demo;
/* one;
/* two;
/* three;
/* four;
/* five;
/* six;
:PARAMETERS sA, nB;
:ENDPROC;`, []string{"sA", "nB"}},
		{"inline defaults are not names", `:PROCEDURE Demo;
:PARAMETERS sA := Upper(sOther), nB;
:ENDPROC;`, []string{"sA", "nB"}},
		{"header without semicolon", ":PROCEDURE Demo\n:PARAMETERS sA;\n:ENDPROC;", []string{"sA"}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			if got := procParams(t, tc.input, "Demo"); !reflect.DeepEqual(got, tc.want) {
				t.Errorf("Parameters = %v, want %v", got, tc.want)
			}
		})
	}
}

// (b) A procedure without :PARAMETERS never borrows a later statement's
// names — not the script's, and not the next procedure's.
func TestProcedureParameters_NoBorrowing(t *testing.T) {
	input := `:PROCEDURE NoArgs;
:RETURN 1;
:ENDPROC;
:PARAMETERS sScriptArg;
:PROCEDURE Next;
:PARAMETERS nNext;
:ENDPROC;`
	if got := procParams(t, input, "NoArgs"); got != nil {
		t.Errorf("NoArgs Parameters = %v, want none", got)
	}
	if got := procParams(t, input, "Next"); !reflect.DeepEqual(got, []string{"nNext"}) {
		t.Errorf("Next Parameters = %v, want [nNext]", got)
	}

	nested := `:PROCEDURE Outer;
:IF .T.;
	:PARAMETERS sNested;
:ENDIF;
:ENDPROC;`
	if got := procParams(t, nested, "Outer"); got != nil {
		t.Errorf("Outer Parameters = %v, want none (nested block)", got)
	}
}

// (c) Leading procedures are allowed before the script-level :PARAMETERS;
// the search stops at the first top-level executable statement.
func TestExtractTopLevelParameters_AfterLeadingProcedures(t *testing.T) {
	tests := []struct {
		name     string
		input    string
		want     []string
		wantLine int
	}{
		{"after leading procedures", `:PROCEDURE Helper;
:PARAMETERS sInner;
:RETURN sInner;
:ENDPROC;

/* entry;
:PARAMETERS sName, nMode;
:RETURN DoProc("Helper", {sName});`, []string{"sName", "nMode"}, 7},
		{"after include and inline code", `:INCLUDE Cat.Shared;
:BEGININLINECODE Calc;
:PARAMETERS a;
:RETURN a;
:ENDINLINECODE;
:PARAMETERS sName;`, []string{"sName"}, 6},
		{"after an executable statement is not the entry", `:PROCEDURE Helper;
:ENDPROC;
nCount := 1;
:PARAMETERS sLate;`, nil, -1},
		{"only inside a procedure", `:PROCEDURE Helper;
:PARAMETERS sInner;
:ENDPROC;`, nil, -1},
		{"multi-line list with comments", `:PARAMETERS sA, /* first;
 nB;`, []string{"sA", "nB"}, 1},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			p, root := parseInput(t, tc.input)
			got, line := p.ExtractTopLevelParameters(root)
			if !reflect.DeepEqual(got, tc.want) || line != tc.wantLine {
				t.Errorf("got (%v, %d), want (%v, %d)", got, line, tc.want, tc.wantLine)
			}
		})
	}
}

// (d) A comment mid-statement belongs to the statement: the :IF keeps its
// place in the tree and still opens its block.
func TestGroupStatements_CommentMidStatement(t *testing.T) {
	input := `:IF bA .AND. /* why;
	bB;
	nCount := 1;
:ENDIF;`
	_, root := parseInput(t, input)

	if blocks := collectNodesByType(root, NodeBlock); len(blocks) != 1 {
		t.Fatalf("want the :IF block, got %d blocks", len(blocks))
	}
	first := root.Children[0]
	if len(first.Tokens) == 0 || !strings.EqualFold(first.Tokens[0].Text, ":IF") {
		t.Fatalf("first statement should start with :IF, got %+v", first.Tokens)
	}
	if first.EndLine != 2 {
		t.Errorf(":IF statement should span lines 1-2, ends at %d", first.EndLine)
	}

	// A comment with nothing before it still stands alone.
	_, root = parseInput(t, "\t/* note;\nnCount := 1;")
	if root.Children[0].Type != NodeComment {
		t.Errorf("leading comment should be its own node, got %v", root.Children[0].Type)
	}
}

// (e) :ENDPROC closes its procedure even with a block left open inside it,
// so the procedure ends at its :ENDPROC and the next procedure is a
// sibling, not nested inside it.
func TestParse_EndprocClosesUnclosedInnerBlock(t *testing.T) {
	input := `:PROCEDURE First;
:IF .T.;
	nCount := 1;
:ENDPROC;
:PROCEDURE Second;
:RETURN 2;
:ENDPROC;`
	p, root := parseInput(t, input)
	procs := p.ExtractProcedures(root)
	if len(procs) != 2 {
		t.Fatalf("want 2 procedures, got %d", len(procs))
	}
	if procs[0].EndLine != 4 {
		t.Errorf("First ends at %d, want 4", procs[0].EndLine)
	}
	if procs[1].StartLine != 5 || procs[1].EndLine != 7 {
		t.Errorf("Second spans %d-%d, want 5-7", procs[1].StartLine, procs[1].EndLine)
	}
	// Both procedure statements sit at the top level.
	top := 0
	for _, c := range root.Children {
		if tok := firstSignificant(c); tok != nil && strings.EqualFold(tok.Text, ":PROCEDURE") {
			top++
		}
	}
	if top != 2 {
		t.Errorf("want both :PROCEDURE statements at top level, got %d", top)
	}
}

func firstSignificant(n *Node) *lexer.Token {
	for i := range n.Tokens {
		if n.Tokens[i].Type != lexer.TokenWhitespace && n.Tokens[i].Type != lexer.TokenComment {
			return &n.Tokens[i]
		}
	}
	return nil
}

// (f) A `;` ends the statement whatever the bracket depth, so one unclosed
// bracket while typing does not hide every later call site.
func TestStatementExpressions_UnclosedBracketEndsAtSemicolon(t *testing.T) {
	tokens := lexer.NewLexer("x := Len(sA;\ny := Upper(sB);\nz := Lower(sC);").Tokenize()
	stmts := ExtractStatementExpressions(tokens)
	if len(stmts) != 3 {
		t.Fatalf("want 3 statements, got %d", len(stmts))
	}
	var names []string
	for _, site := range CollectCallsIn(stmts) {
		if site.Call != nil && len(site.Call.Children) > 0 {
			names = append(names, site.Call.Children[0].Name)
		}
	}
	for _, want := range []string{"Upper", "Lower"} {
		found := false
		for _, n := range names {
			if n == want {
				found = true
			}
		}
		if !found {
			t.Errorf("call site %s hidden by the unclosed bracket; got %v", want, names)
		}
	}
}

// (g) ExtractProcedures is linear: doc and visibility lookups use an index
// built once, not a scan of every token per procedure.
func TestExtractProcedures_ManyProcedures(t *testing.T) {
	var b strings.Builder
	for i := 0; i < 2000; i++ {
		fmt.Fprintf(&b, "/* Description: proc %d;\n/*@private;\n:PROCEDURE P%d;\n:PARAMETERS sA;\n:RETURN sA;\n:ENDPROC;\n", i, i)
	}
	p, root := parseInput(t, b.String())
	procs := p.ExtractProcedures(root)
	if len(procs) != 2000 {
		t.Fatalf("want 2000 procedures, got %d", len(procs))
	}
	last := procs[len(procs)-1]
	if !last.IsPrivate || last.Doc.Description != "proc 1999" || len(last.Parameters) != 1 {
		t.Errorf("last procedure lost its doc/visibility/parameters: %+v", last)
	}
}

func BenchmarkExtractProcedures(b *testing.B) {
	doc := generateSSLDocument(500)
	tokens := lexer.NewLexer(doc).Tokenize()
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		p := NewParser(tokens)
		p.ExtractProcedures(p.Parse())
	}
}
