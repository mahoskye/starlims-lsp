package providers

import (
	"io/fs"
	"os"
	"path/filepath"
	"regexp"
	"strings"
	"testing"

	"starlims-lsp/internal/lexer"
)

func syntaxTree(src string) []*SyntaxNode {
	return BuildSyntaxTree(src, lexer.NewLexer(src).Tokenize(), nil)
}

// treeFixtures returns every SSL source the repository keeps as a test
// input: the formatter idempotence corpus, the input of each scenario, and
// each ```ssl fence of the behavior catalog.
func treeFixtures(t *testing.T) map[string]string {
	t.Helper()
	out := map[string]string{}
	root := filepath.Join("..", "..")

	dir := filepath.Join("testdata", "idempotence")
	entries, err := os.ReadDir(dir)
	if err != nil {
		t.Fatal(err)
	}
	for _, e := range entries {
		if raw, err := os.ReadFile(filepath.Join(dir, e.Name())); err == nil && !e.IsDir() {
			out[filepath.Join(dir, e.Name())] = string(raw)
		}
	}

	walk := func(dir, suffix string, each func(path, text string)) {
		err := filepath.WalkDir(filepath.Join(root, dir), func(path string, d fs.DirEntry, err error) error {
			if err != nil || d.IsDir() || !strings.HasSuffix(path, suffix) {
				return err
			}
			raw, err := os.ReadFile(path)
			if err != nil {
				return err
			}
			each(path, string(raw))
			return nil
		})
		if err != nil {
			t.Fatal(err)
		}
	}
	// A scenario's first section is its input (scenarios/README.md).
	walk("scenarios", ".txtar", func(path, text string) {
		_, rest, ok := strings.Cut(text, "\n-- input.")
		if !ok {
			return
		}
		_, body, _ := strings.Cut(rest, "\n")
		if i := strings.Index(body, "\n-- "); i >= 0 {
			body = body[:i+1]
		}
		out[path] = body
	})
	fence := regexp.MustCompile("(?ms)^```ssl[^\n]*\n(.*?)^```")
	walk("catalog", ".md", func(path, text string) {
		for i, m := range fence.FindAllStringSubmatch(text, -1) {
			out[path+"#"+string(rune('a'+i%26))+strings.Repeat("'", i/26)] = m[1]
		}
	})
	if len(out) < 500 {
		t.Fatalf("found only %d fixtures; the fixture paths have moved", len(out))
	}
	return out
}

// checkTreeCoverage asserts the coverage contract of feature.syntax_tree
// A1 for src: the pieces of the tree (the leaves, plus each block's
// opening and closing statement) lie in document order without overlap,
// everything outside them is whitespace, every whitespace-only line is in
// a piece, and each node lies inside its parent.
func checkTreeCoverage(t *testing.T, name, src string) {
	t.Helper()
	src = strings.TrimPrefix(src, "\uFEFF") // editors strip the mark
	runes := []rune(src)
	lineStarts := []int{0}
	for i, r := range runes {
		if r == '\n' {
			lineStarts = append(lineStarts, i+1)
		}
	}
	offset := func(p Position) int {
		if p.Line < 0 || p.Line >= len(lineStarts) {
			t.Fatalf("%s: position %+v outside the document", name, p)
		}
		o := lineStarts[p.Line] + p.Character
		lineEnd := len(runes)
		if p.Line+1 < len(lineStarts) {
			lineEnd = lineStarts[p.Line+1] - 1
		}
		if p.Character < 0 || o > lineEnd {
			t.Fatalf("%s: position %+v outside its line", name, p)
		}
		return o
	}
	isSpace := func(s []rune) bool {
		return strings.TrimSpace(string(s)) == ""
	}

	inPiece := map[int]bool{} // lines some piece touches
	prev := 0
	piece := func(r Range, what string) {
		a, b := offset(r.Start), offset(r.End)
		if a < prev || b < a {
			t.Fatalf("%s: %s %+v overlaps or precedes the piece before it", name, what, r)
		}
		if !isSpace(runes[prev:a]) {
			t.Fatalf("%s: code outside every node before %s %+v: %q", name, what, r, string(runes[prev:a]))
		}
		for l := r.Start.Line; l <= r.End.Line; l++ {
			inPiece[l] = true
		}
		prev = b
	}
	inside := func(inner, outer Range) bool {
		return !after(outer.Start, inner.Start) && !after(inner.End, outer.End)
	}
	var walk func(nodes []*SyntaxNode, parent *SyntaxNode)
	walk = func(nodes []*SyntaxNode, parent *SyntaxNode) {
		for _, n := range nodes {
			if parent != nil && !inside(n.Range, parent.Range) {
				t.Fatalf("%s: %s %+v outside its parent %s %+v", name, n.Kind, n.Range, parent.Kind, parent.Range)
			}
			if n.Header != nil {
				if n.Header.Start != n.Range.Start {
					t.Fatalf("%s: %s header %+v does not start the node %+v", name, n.Kind, *n.Header, n.Range)
				}
				piece(*n.Header, n.Kind+" header")
			}
			if len(n.Children) == 0 && n.Header == nil && n.Closer == nil {
				piece(n.Range, n.Kind)
				leaf := runes[offset(n.Range.Start):offset(n.Range.End)]
				// Only a token left open (a string or comment that runs to
				// the end of the document) may end on whitespace.
				toEnd := offset(n.Range.End) == len(runes)
				if n.Kind != SyntaxBlankLines && n.Kind != SyntaxText && (len(leaf) == 0 ||
					isSpace(leaf[:1]) || (isSpace(leaf[len(leaf)-1:]) && !toEnd)) {
					t.Fatalf("%s: %s %+v does not start and end on code: %q", name, n.Kind, n.Range, string(leaf))
				}
			}
			walk(n.Children, n)
			if n.Closer != nil {
				if n.Closer.End != n.Range.End {
					t.Fatalf("%s: %s closer %+v does not end the node %+v", name, n.Kind, *n.Closer, n.Range)
				}
				piece(*n.Closer, n.Kind+" closer")
			}
		}
	}
	walk(BuildSyntaxTree(src, lexer.NewLexer(src).Tokenize(), nil), nil)
	if !isSpace(runes[prev:]) {
		t.Fatalf("%s: code after the last node: %q", name, string(runes[prev:]))
	}
	lines := strings.Split(src, "\n")
	if strings.HasSuffix(src, "\n") {
		lines = lines[:len(lines)-1]
	}
	for l, text := range lines {
		if strings.TrimSpace(text) == "" && !inPiece[l] {
			t.Fatalf("%s: blank line %d is in no node", name, l)
		}
	}
}

func TestSyntaxTreeCoversFixtures(t *testing.T) {
	// [spec feature.syntax_tree/A1]
	for name, src := range treeFixtures(t) {
		checkTreeCoverage(t, name, src)
	}
}

// FuzzSyntaxTree checks the coverage contract on arbitrary input.
func FuzzSyntaxTree(f *testing.F) {
	addCorpusSeeds(f)
	f.Fuzz(func(t *testing.T, src string) {
		checkTreeCoverage(t, "fuzz input", src)
	})
}

// shape renders a tree as one line per node, kind and keyword or name.
func shape(nodes []*SyntaxNode) string {
	var b strings.Builder
	var walk func([]*SyntaxNode, int)
	walk = func(nodes []*SyntaxNode, depth int) {
		for _, n := range nodes {
			b.WriteString(strings.Repeat("  ", depth) + n.Kind)
			if n.Keyword != "" {
				b.WriteString(" " + n.Keyword)
			}
			if n.Name != "" {
				b.WriteString(" " + n.Name)
			}
			if isBlockKind(n.Kind) && n.Closer == nil {
				b.WriteString(" (unclosed)")
			}
			b.WriteString("\n")
			walk(n.Children, depth+1)
		}
	}
	walk(nodes, 0)
	return b.String()
}

func isBlockKind(kind string) bool {
	for _, k := range blockKinds {
		if k == kind {
			return true
		}
	}
	return false
}

func assertShape(t *testing.T, src, want string) {
	t.Helper()
	nodes := syntaxTree(src)
	if got := shape(nodes); got != strings.TrimLeft(want, "\n") {
		t.Errorf("tree of\n%s\ngot:\n%s\nwant:\n%s", src, got, strings.TrimLeft(want, "\n"))
	}
	checkTreeCoverage(t, t.Name(), src)
}

func TestSyntaxTreeProcedure(t *testing.T) {
	// [spec feature.syntax_tree/A2]
	src := ":PROCEDURE Calc;\n\t:PARAMETERS nA, nB;\n\t:DEFAULT nB, 1;\n\t:RETURN nA * nB;\n:ENDPROC;\n"
	assertShape(t, src, `
procedure PROCEDURE Calc
  parameters PARAMETERS
  default DEFAULT
  statement RETURN
`)
	proc := syntaxTree(src)[0]
	want := Range{Start: Position{0, 11}, End: Position{0, 15}}
	if proc.NameRange == nil || *proc.NameRange != want {
		t.Errorf("name range = %+v, want %+v", proc.NameRange, want)
	}
	if proc.Header == nil || *proc.Header != (Range{Start: Position{0, 0}, End: Position{0, 16}}) {
		t.Errorf("header = %+v", proc.Header)
	}
	if proc.Closer == nil || *proc.Closer != (Range{Start: Position{4, 0}, End: Position{4, 9}}) {
		t.Errorf("closer = %+v", proc.Closer)
	}
	if proc.Range != (Range{Start: Position{0, 0}, End: Position{4, 9}}) {
		t.Errorf("range = %+v, want it to end after the closer's ';'", proc.Range)
	}
}

func TestSyntaxTreeBannerAndDocBlock(t *testing.T) {
	// [spec feature.syntax_tree/A3]
	assertShape(t, "/* Script: Orders;\n\n/* Totals an order;\n/*@private;\n:PROCEDURE Total;\n:ENDPROC;\n/* trailing;\n", `
banner
blankLines
docBlock
comment
procedure PROCEDURE Total
comment
`)
	// The first comment documents the procedure right below it.
	assertShape(t, "/* Totals an order;\n:PROCEDURE Total;\n:ENDPROC;\n", `
docBlock
procedure PROCEDURE Total
`)
	// A visibility annotation is never the banner.
	assertShape(t, "/*@private;\n:PROCEDURE Total;\n:ENDPROC;\n", `
comment
procedure PROCEDURE Total
`)
	// Separated by a blank line, the first comment is the banner.
	assertShape(t, "/* Script: Orders;\n\n:PROCEDURE Total;\n:ENDPROC;\n", `
banner
blankLines
procedure PROCEDURE Total
`)
}

func TestSyntaxTreeCommentRegions(t *testing.T) {
	// [spec feature.syntax_tree/A4]
	src := "/* region Outer;\n/* region Inner;\nx := 1;\n/* endregion;\n/* endregion;\n"
	assertShape(t, src, `
region Outer
  region Inner
    statement
`)
	inner := syntaxTree(src)[0].Children[0]
	if inner.NameRange == nil || *inner.NameRange != (Range{Start: Position{1, 10}, End: Position{1, 15}}) {
		t.Errorf("Inner name range = %+v", inner.NameRange)
	}
	// A region opened outside a procedure and closed inside it cannot be
	// a node: its markers stay leaves. An orphan closer is a leaf too.
	assertShape(t, "/* endregion;\n/* region Cross;\n:PROCEDURE P;\n/* endregion;\n:ENDPROC;\n", `
regionEnd
regionStart Cross
procedure PROCEDURE P
  regionEnd
`)
}

func TestSyntaxTreeControlFlow(t *testing.T) {
	// [spec feature.syntax_tree/A5]
	assertShape(t, ":IF n > 0;\n\tx := 1;\n:ELSE;\n\tx := 2;\n:ENDIF;\n", `
if IF
  branch IF
    statement
  branch ELSE
    statement
`)
	assertShape(t, ":TRY;\n\tx := 1;\n:CATCH;\n\tx := 2;\n:FINALLY;\n\tx := 3;\n:ENDTRY;\n", `
try TRY
  branch TRY
    statement
  branch CATCH
    statement
  branch FINALLY
    statement
`)
	assertShape(t, ":BEGINCASE;\n:CASE n == 1;\n\t:EXITCASE;\n:OTHERWISE;\n\t:EXITCASE;\n:ENDCASE;\n", `
case BEGINCASE
  branch CASE
    statement EXITCASE
  branch OTHERWISE
    statement EXITCASE
`)
	// The if spans from its :IF to its :ENDIF; each branch starts at its
	// own keyword.
	tree := syntaxTree(":IF n > 0;\n\tx := 1;\n:ELSE;\n\tx := 2;\n:ENDIF;\n")[0]
	if tree.Range != (Range{Start: Position{0, 0}, End: Position{4, 7}}) || tree.Closer == nil {
		t.Errorf("if range = %+v, closer %+v", tree.Range, tree.Closer)
	}
	if els := tree.Children[1]; els.Header == nil || els.Header.Start != (Position{2, 0}) {
		t.Errorf("else header = %+v", els.Header)
	}
}

func TestSyntaxTreeLoopsAndTextBlocks(t *testing.T) {
	// [spec feature.syntax_tree/A6]
	assertShape(t, ":WHILE n < 3;\n\tn += 1;\n:ENDWHILE;\n:FOR i := 1 :TO 3;\n\tn += i;\n:NEXT;\n", `
while WHILE
  statement
for FOR
  statement
`)
	assertShape(t, ":REGION Mail;\nDear {name};\n\n:ENDREGION;\n:BEGININLINECODE Code;\nx := 1;\n:ENDINLINECODE;\n", `
textRegion REGION Mail
  text
inlineCode BEGININLINECODE Code
  statement
`)
}

func TestSyntaxTreeBlankLines(t *testing.T) {
	// [spec feature.syntax_tree/A7]
	src := "x := 1;\n\n\n:IF x;\n\ty := 1;\n\n:ELSE;\n\ty := 2;\n:ENDIF;\n"
	assertShape(t, src, `
statement
blankLines
if IF
  branch IF
    statement
    blankLines
  branch ELSE
    statement
`)
	blank := syntaxTree(src)[1]
	if blank.Range != (Range{Start: Position{1, 0}, End: Position{2, 0}}) {
		t.Errorf("blank run = %+v, want lines 1-2", blank.Range)
	}
}

func TestSyntaxTreeErrors(t *testing.T) {
	// [spec feature.syntax_tree/A8]
	assertShape(t, "x := 1;\ny := ) 2;\n:ENDIF;\n:ELSE;\nz := 3;\n", `
statement
error
error ENDIF
error ELSE
  statement
`)
}

func TestSyntaxTreeUnclosedBlock(t *testing.T) {
	// [spec feature.syntax_tree/A9]
	src := ":PROCEDURE P;\n\t:IF x;\n\t\ty := 1;\n:ENDPROC;\n"
	assertShape(t, src, `
procedure PROCEDURE P
  if IF (unclosed)
    branch IF
      statement
`)
	ifNode := syntaxTree(src)[0].Children[0]
	if ifNode.Closer != nil || ifNode.Range.End != (Position{2, 9}) {
		t.Errorf("unclosed if: closer %+v, range %+v; want no closer, ending at its last statement", ifNode.Closer, ifNode.Range)
	}
}
