package lexer

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
	"unicode/utf8"
)

// addCorpusSeeds seeds a fuzz target with the formatter idempotence corpus
// (internal/providers/testdata/idempotence) plus a few shapes that have
// historically stressed the scanners.
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
		";",
		`/* unterminated comment`,
		`s := "unterminated`,
		`s := [bracket [nested] text`,
		`:REGION r;` + "\n" + `body text without end`,
		`:IF .NOT. bA .AND. .T.;`,
		`x := 7.0e2 + .5 - 1.;`,
		`fn := {|a, b| a + b};`,
		"a\r\nb\rc\n",
		"\xff\xfe:IF",
	} {
		f.Add(s)
	}
}

// FuzzTokenize checks the lexer never panics and stays lossless: the
// concatenated token text reproduces the (valid UTF-8) input exactly, and
// token offsets are strictly increasing. Downstream consumers (the
// formatter, rename, the range-based diagnostics) rely on both.
func FuzzTokenize(f *testing.F) {
	addCorpusSeeds(f)
	f.Fuzz(func(t *testing.T, src string) {
		tokens := NewLexer(src).Tokenize()
		if !utf8.ValidString(src) {
			// Invalid bytes become U+FFFD when the input is decoded to
			// runes; only the no-panic contract applies.
			return
		}
		var b strings.Builder
		prev := -1
		for _, tok := range tokens {
			if tok.Type == TokenEOF {
				continue
			}
			if tok.Offset <= prev {
				t.Fatalf("token offsets not increasing: %d after %d (%q)", tok.Offset, prev, tok.Text)
			}
			prev = tok.Offset
			b.WriteString(tok.Text)
		}
		if got := b.String(); got != src {
			t.Fatalf("token text is not lossless:\ninput: %q\ngot:   %q", src, got)
		}
	})
}
