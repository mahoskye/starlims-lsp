package providers

import (
	"strings"
	"testing"
	"unicode/utf8"
)

// Issue #258: the SQL lexer split prefixed string literals from their
// quote and broke exponent literals apart; the non-ASCII cases pin the
// UTF-8 decoding it shares with #306 (issue #256).

// TestSQLLexer_TokensCoverInputBytes: concatenating every token's Text
// reproduces the input byte for byte, whatever the input holds.
func TestSQLLexer_TokensCoverInputBytes(t *testing.T) {
	inputs := []string{
		"SELECT Gr\u00f6\u00dfe FROM t1",
		"SELECT * FROM t1 WHERE city = 'Citt\u00e0' AND c\u00e0 = 1",
		"SELECT \u2018quoted\u2019, \u201cdouble\u201d FROM t1",
		"SELECT col_a\u00a0FROM t1",
		"SELECT \U0001F600 FROM t1 -- \u00e9\n/* \u00df */ WHERE x = ?s\u00e9?",
		"SELECT N'multi   space', q'[it's]', nq'{x}' FROM t1",
		"SELECT 1.5e-3, 2E+5, 1e10, 3e FROM t1",
		"SELECT \xff\xfe bad FROM t1",
	}
	for _, in := range inputs {
		var b strings.Builder
		for _, tok := range NewSQLLexer(in).Tokenize() {
			b.WriteString(tok.Text)
		}
		if b.String() != in {
			t.Errorf("tokens do not reproduce input:\n got %q\nwant %q", b.String(), in)
		}
	}
}

// TestSQLLexer_NonASCIIIdentifiers: letters outside ASCII are identifier
// characters, and a character with no lexer rule is one unknown token
// carrying its own bytes.
func TestSQLLexer_NonASCIIIdentifiers(t *testing.T) {
	toks := filterNonWSSQL(NewSQLLexer("SELECT Gr\u00f6\u00dfe, Citt\u00e0 FROM t1").Tokenize())
	if toks[1].Type != SQLTokenIdentifier || toks[1].Text != "Gr\u00f6\u00dfe" {
		t.Errorf("want identifier Größe, got %v %q", toks[1].Type, toks[1].Text)
	}
	if toks[3].Text != "Citt\u00e0" {
		t.Errorf("want identifier Città, got %q", toks[3].Text)
	}

	toks = filterNonWSSQL(NewSQLLexer("a \u2018").Tokenize())
	last := toks[len(toks)-1]
	if last.Type != SQLTokenUnknown || last.Text != "\u2018" || !utf8.ValidString(last.Text) {
		t.Errorf("want one unknown token for the typographic quote, got %v %q", last.Type, last.Text)
	}
}

// TestSQLLexer_PrefixedStrings: N'...', q'[...]' and nq'...' are each one
// string token, prefix included; a separated prefix is not.
func TestSQLLexer_PrefixedStrings(t *testing.T) {
	tests := []struct {
		in   string
		want []string // non-whitespace token texts
	}{
		{"N'multi   space'", []string{"N'multi   space'"}},
		{"n'it''s'", []string{"n'it''s'"}},
		{"q'[it's]'", []string{"q'[it's]'"}},
		{"Q'{a ' b}'", []string{"Q'{a ' b}'"}},
		{"q'(x)'", []string{"q'(x)'"}},
		{"q'<x>'", []string{"q'<x>'"}},
		{"q'!a'b!'", []string{"q'!a'b!'"}},
		{"q'\u20aca\u20ac'", []string{"q'\u20aca\u20ac'"}},
		{"nq'[x]'", []string{"nq'[x]'"}},
		{"N 'x'", []string{"N", "'x'"}},
		{"name = 'x'", []string{"name", "=", "'x'"}},
	}
	for _, tc := range tests {
		var got []string
		for _, tok := range filterNonWSSQL(NewSQLLexer(tc.in).Tokenize()) {
			got = append(got, tok.Text)
		}
		if strings.Join(got, "|") != strings.Join(tc.want, "|") {
			t.Errorf("%q: got %q, want %q", tc.in, got, tc.want)
		}
	}
}

// TestSQLLexer_ExponentNumbers: an exponent with optional sign belongs to
// the number.
func TestSQLLexer_ExponentNumbers(t *testing.T) {
	tests := []struct {
		in   string
		want []string
	}{
		{"1.5e-3", []string{"1.5e-3"}},
		{"2E+5", []string{"2E+5"}},
		{"1e10", []string{"1e10"}},
		{"3e", []string{"3", "e"}},
		{"4e+x", []string{"4", "e", "+", "x"}},
		{"5-3", []string{"5", "-", "3"}},
	}
	for _, tc := range tests {
		var got []string
		for _, tok := range filterNonWSSQL(NewSQLLexer(tc.in).Tokenize()) {
			got = append(got, tok.Text)
		}
		if strings.Join(got, "|") != strings.Join(tc.want, "|") {
			t.Errorf("%q: got %q, want %q", tc.in, got, tc.want)
		}
	}
}

// TestSQLFormatter_PreservesNonASCIIAndLiterals formats SQL that the
// byte-walking lexer used to corrupt and requires the author's text to
// survive the reflow.
func TestSQLFormatter_PreservesNonASCIIAndLiterals(t *testing.T) {
	f := NewSQLFormatter(DefaultSQLFormattingOptions())
	tests := []struct {
		in       string
		contains []string
	}{
		{"SELECT Gr\u00f6\u00dfe, col_b FROM t1 WHERE Gr\u00f6\u00dfe > 1",
			[]string{"Gr\u00f6\u00dfe"}},
		{"SELECT col_a FROM t1 WHERE city = 'Citt\u00e0' AND Citt\u00e0_code = 1",
			[]string{"'Citt\u00e0'", "Citt\u00e0_code"}},
		{"SELECT col_a FROM t1 WHERE note = \u2018x\u2019",
			[]string{"\u2018x\u2019"}},
		{"SELECT col_a FROM t1 WHERE name = N'multi   space'",
			[]string{"N'multi   space'"}},
		{"SELECT col_a FROM t1 WHERE name = q'[it's]'",
			[]string{"q'[it's]'"}},
		{"SELECT col_a FROM t1 WHERE amount > 1.5e-3",
			[]string{"1.5e-3"}},
	}
	for _, tc := range tests {
		got := f.FormatSQL(tc.in, "")
		if !utf8.ValidString(got) {
			t.Errorf("%q: formatted output is not valid UTF-8: %q", tc.in, got)
		}
		for _, want := range tc.contains {
			if !strings.Contains(got, want) {
				t.Errorf("%q: formatted output lost %q:\n%s", tc.in, want, got)
			}
		}
		if again := f.FormatSQL(got, ""); again != got {
			t.Errorf("%q: not idempotent:\n%s\n---\n%s", tc.in, got, again)
		}
	}
}

// TestFormatDocument_LongSQLStringWithNonASCII is the reported shape: a
// SQL string long enough to be reflowed (over the 90-column default) that
// holds non-ASCII text outside SQL quotes.
func TestFormatDocument_LongSQLStringWithNonASCII(t *testing.T) {
	src := ":DECLARE aRows;\naRows := SQLExecute(\"SELECT Gr\u00f6\u00dfe, col_b, col_c, col_d FROM table_one WHERE Gr\u00f6\u00dfe > 1 AND col_b = N'a   b' AND col_c > 1.5e-3\");\n"
	edits := FormatDocument(src, DefaultFormattingOptions())
	if len(edits) == 0 {
		t.Fatal("expected the long SQL string to be reflowed")
	}
	got := edits[0].NewText
	for _, want := range []string{"Gr\u00f6\u00dfe", "N'a   b'", "1.5e-3"} {
		if !strings.Contains(got, want) {
			t.Errorf("formatted document lost %q:\n%s", want, got)
		}
	}
	if !utf8.ValidString(got) {
		t.Errorf("formatted document is not valid UTF-8:\n%q", got)
	}
}
