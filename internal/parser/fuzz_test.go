package parser

import (
	"os"
	"path/filepath"
	"strings"
	"testing"

	"starlims-lsp/internal/lexer"
)

// addCorpusSeeds seeds a fuzz target with the formatter idempotence corpus
// (internal/providers/testdata/idempotence) plus malformed shapes that have
// historically stressed statement and expression parsing.
func addCorpusSeeds(f *testing.F) {
	f.Helper()
	dir := filepath.Join("..", "providers", "testdata", "idempotence")
	entries, err := os.ReadDir(dir)
	if err != nil {
		f.Fatalf("reading seed corpus: %v", err)
	}
	for _, e := range entries {
		if e.IsDir() || !strings.HasSuffix(e.Name(), ".ssl") {
			continue
		}
		raw, err := os.ReadFile(filepath.Join(dir, e.Name()))
		if err != nil {
			f.Fatalf("reading seed %s: %v", e.Name(), err)
		}
		f.Add(string(raw))
	}
	for _, s := range []string{
		"",
		`( ( ( (`,
		`a := b:`,
		`a := b := c;`,
		`:IF;`,
		`:FOR i := :TO;`,
		`:FOR := 1 :TO 2 :STEP;`,
		`:RETURN x := .T.;`,
		`f(a,,c,);`,
		`o:m(1)[2]:p{3}++;`,
		`x := {|a| a + 1}:Eval();`,
		`:ENDPROC; :ENDIF; :NEXT; :ENDWHILE;`,
		`:PROCEDURE; :PARAMETERS; :DEFAULT;`,
		`:CLASS C; :INHERIT; :PROCEDURE M; :ENDPROC;`,
		`:BEGINCASE; :CASE; :OTHERWISE; :ENDCASE;`,
		`:TRY; :CATCH; :FINALLY; :ENDTRY;`,
		`- - - ! .NOT. ++ --;`,
		`a[;`,
		`{1, {2, {3`,
	} {
		f.Add(s)
	}
}

// checkExprBounds asserts every node of a tree is non-nil and that nodes
// covering tokens stay inside the token slice — consumers index
// tokens[Start] / tokens[End] directly to build ranges.
func checkExprBounds(t *testing.T, e *Expr, n int) {
	t.Helper()
	if e == nil {
		t.Fatal("nil expression node")
	}
	if e.End >= e.Start && (e.Start < 0 || e.End >= n) {
		t.Fatalf("expr %s span [%d,%d] outside token slice of %d", e.Kind, e.Start, e.End, n)
	}
	for _, c := range e.Children {
		checkExprBounds(t, c, n)
	}
}

// FuzzParse runs the structural parser and its extractors on arbitrary
// input. Contract: no panic (the server parses every keystroke).
func FuzzParse(f *testing.F) {
	addCorpusSeeds(f)
	f.Fuzz(func(t *testing.T, src string) {
		tokens := lexer.NewLexer(src).Tokenize()
		p := NewParser(tokens)
		root := p.Parse()
		_ = p.ExtractProcedures(root)
		_, _ = p.ExtractTopLevelParameters(root)
		_ = p.ExtractVariables()
		_ = ExtractControlFlowBlocks(tokens)
		_ = IdentifierRoles(tokens)
	})
}

// FuzzStatementExpressions runs the statement-level expression extractor
// (issue #184) and the call collector on arbitrary input. Contract: no
// panic, every tree in bounds, statement spans ordered and in bounds, and
// Complete agrees with Unexpected (Complete <=> Unexpected < 0).
func FuzzStatementExpressions(f *testing.F) {
	addCorpusSeeds(f)
	f.Fuzz(func(t *testing.T, src string) {
		tokens := lexer.NewLexer(src).Tokenize()
		n := len(tokens)
		stmts := ExtractStatementExpressions(tokens)
		prevEnd := -1
		for _, s := range stmts {
			if s.Start < 0 || s.End >= n || s.Start > s.End {
				t.Fatalf("statement span [%d,%d] invalid for %d tokens", s.Start, s.End, n)
			}
			if s.Start <= prevEnd {
				t.Fatalf("statement spans overlap: start %d <= previous end %d", s.Start, prevEnd)
			}
			prevEnd = s.End
			if s.Complete != (s.Unexpected < 0) {
				t.Fatalf("Complete=%v disagrees with Unexpected=%d", s.Complete, s.Unexpected)
			}
			if s.Unexpected >= n {
				t.Fatalf("Unexpected index %d outside %d tokens", s.Unexpected, n)
			}
			for _, e := range s.Exprs {
				checkExprBounds(t, e, n)
				_ = e.String()
			}
		}
		_ = CollectCallsIn(stmts)

		// ParseExpression from every significant position must advance or
		// report no expression, and never panic.
		for i := 0; i < n; i++ {
			e, next := ParseExpression(tokens, i)
			checkExprBounds(t, e, n)
			if next < i || next > n {
				t.Fatalf("ParseExpression(%d) returned next=%d for %d tokens", i, next, n)
			}
		}
	})
}
