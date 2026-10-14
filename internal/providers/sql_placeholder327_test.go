package providers

import "testing"

// A complex named placeholder is one token, so the reflow never respaces
// its interior; a positional '?' is never merged with what follows
// (issue #327, fmt.sql_in_strings).
func TestSQLLexer_ComplexPlaceholderAtomic(t *testing.T) {
	cases := []struct {
		sql  string
		want []string // placeholder tokens, in order
	}{
		{"where a = ?sPrefix + sSuffix? and b > ?nBase * 2?", []string{"?sPrefix + sSuffix?", "?nBase * 2?"}},
		{"where a = ?oRow:Id? and b = ?aIds[1]?", []string{"?oRow:Id?", "?aIds[1]?"}},
		{"where k=?AND x=?", []string{"?", "?"}},
		{"where a = ? and b = ?", []string{"?", "?"}},
	}
	for _, c := range cases {
		var got []string
		for _, tok := range NewSQLLexer(c.sql).Tokenize() {
			if tok.Type == SQLTokenPlaceholder {
				got = append(got, tok.Text)
			}
		}
		if len(got) != len(c.want) {
			t.Errorf("%q: placeholders %q, want %q", c.sql, got, c.want)
			continue
		}
		for i := range got {
			if got[i] != c.want[i] {
				t.Errorf("%q: placeholder %d = %q, want %q", c.sql, i, got[i], c.want[i])
			}
		}
	}
}
