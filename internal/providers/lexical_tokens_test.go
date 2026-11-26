package providers

import (
	"fmt"
	"strings"
	"testing"
	"unicode/utf8"

	"starlims-lsp/internal/lexer"
)

// describeLexical renders tokens as `kind line:col-line:col text`, one per
// token in order, positions 1-based, the text read back from the source
// with its line breaks shown as `⏎`.
func describeLexical(src string, tokens []LexicalToken) []string {
	lines := strings.Split(src, "\n")
	out := make([]string, 0, len(tokens))
	for _, token := range tokens {
		r := token.Range
		var text strings.Builder
		for l := r.Start.Line; l <= r.End.Line && l < len(lines); l++ {
			runes := []rune(lines[l])
			from, to := 0, len(runes)
			if l == r.Start.Line {
				from = r.Start.Character
			}
			if l == r.End.Line {
				to = r.End.Character
			}
			if l > r.Start.Line {
				text.WriteString("⏎")
			}
			text.WriteString(string(runes[from:to]))
		}
		out = append(out, fmt.Sprintf("%s %d:%d-%d:%d %s", token.Kind,
			r.Start.Line+1, r.Start.Character+1, r.End.Line+1, r.End.Character+1, text.String()))
	}
	return out
}

func lexicalDesc(src string, isDataSource bool) []string {
	return describeLexical(src, GetLexicalTokens(src, lexer.NewLexer(src).Tokenize(), isDataSource))
}

func TestLexicalTokensCommentEndsAtItsSemicolon(t *testing.T) {
	// [spec feature.lexical_tokens/A3]
	src := "/* Reads the samples;\n" +
		"sFolder := \"exports\";\n" +
		"/* The glob docs/*/notes.md lists the rest;\n"
	wantTokens(t, lexicalDesc(src, false),
		"comment 1:1-1:22 /* Reads the samples;",
		`string 2:12-2:21 "exports"`,
		"comment 3:1-3:44 /* The glob docs/*/notes.md lists the rest;",
	)
}

func TestLexicalTokensBackslashEscapesNothing(t *testing.T) {
	// [spec feature.lexical_tokens/A4]
	src := "sDir := \"C:\\exports\\\";\n" +
		"/* Nothing after the string is part of it;\n" +
		"nCount := 0;\n"
	wantTokens(t, lexicalDesc(src, false),
		`string 1:9-1:22 "C:\exports\"`,
		"comment 2:1-2:43 /* Nothing after the string is part of it;",
	)
}

func TestLexicalTokensCommentInsideAStatement(t *testing.T) {
	// [spec feature.lexical_tokens/A5]
	src := "sText := \"first part \" +\n" +
		"/* a note in the middle of the statement;\n" +
		"\"second part\";\n"
	wantTokens(t, lexicalDesc(src, false),
		`string 1:10-1:23 "first part "`,
		"comment 2:1-2:42 /* a note in the middle of the statement;",
		`string 3:1-3:14 "second part"`,
	)
}

func TestLexicalTokensQuoteStyles(t *testing.T) {
	// [spec feature.lexical_tokens/A6]
	src := "sA := \"it's\";\n" +
		"sB := 'say \"hi\"';\n" +
		"sC := [WHERE NAME = 'x'];\n" +
		"sD := aRows[1];\n" +
		"sE := \"\";\n"
	wantTokens(t, lexicalDesc(src, false),
		`string 1:7-1:13 "it's"`,
		`string 2:7-2:17 'say "hi"'`,
		`string 3:7-3:25 [WHERE NAME = 'x']`,
		`string 5:7-5:9 ""`,
	)
}

func TestLexicalTokensKeywords(t *testing.T) {
	// [spec feature.lexical_tokens/A7]
	src := ":PROCEDURE Run;\n" +
		"\t:PARAMETERS oRule;\n" +
		"\t:for i := 1 :TO 3 :STEP 1;\n" +
		"\t\tvOut := oRule:Return;\n" +
		"\t\tvOut := oRule:Name()\n" +
		"\t\t\t:Trim();\n" +
		"\t\tvOut := NIL;\n" +
		"\t\tbDone := .T. .AND. .NOT. .F.;\n" +
		"\t:NEXT;\n" +
		":ENDPROC;\n"
	wantTokens(t, lexicalDesc(src, false),
		"keyword 1:1-1:11 :PROCEDURE",
		"keyword 2:2-2:13 :PARAMETERS",
		"keyword 3:2-3:6 :for",
		"keyword 3:14-3:17 :TO",
		"keyword 3:20-3:25 :STEP",
		"keyword 9:2-9:7 :NEXT",
		"keyword 10:1-10:9 :ENDPROC",
	)

	// A colon word in keyword position is a keyword to the lexer whether
	// or not the language has it; diag.unknown_keyword reports it.
	wantTokens(t, lexicalDesc(":DECLARE nA;\n:FROBNICATE nA;\n", false),
		"keyword 1:1-1:9 :DECLARE",
		"keyword 2:1-2:12 :FROBNICATE",
	)
}

func TestLexicalTokensMultiLineAndUnterminated(t *testing.T) {
	// [spec feature.lexical_tokens/A8]
	src := "/*\n * Reads the samples\n;\n" +
		"sSQL := \"SELECT ID\n\tFROM SAMPLES\";\n"
	wantTokens(t, lexicalDesc(src, false),
		"comment 1:1-3:2 /*⏎ * Reads the samples⏎;",
		"string 4:9-5:15 \"SELECT ID⏎\tFROM SAMPLES\"",
	)

	// A comment or a string that never ends runs to the end of the
	// document, without the final line break.
	wantTokens(t, lexicalDesc("nA := 1;\n/* never ends\nnB := 2\n", false),
		"comment 2:1-3:8 /* never ends⏎nB := 2")
	wantTokens(t, lexicalDesc("sA := \"never ends\nnB := 2;\n", false),
		"string 1:7-2:9 \"never ends⏎nB := 2;")
}

func TestLexicalTokensRegionBodyAndCodeBlock(t *testing.T) {
	// [spec feature.lexical_tokens/A9]
	src := ":REGION Query;\n" +
		"SELECT ID FROM SAMPLES /* not a comment; \"nor a string\"\n" +
		":ENDREGION;\n" +
		"fnPick := {|sId| sId == \"A\"};\n"
	wantTokens(t, lexicalDesc(src, false),
		"keyword 1:1-1:8 :REGION",
		"text 1:15-2:56 ⏎SELECT ID FROM SAMPLES /* not a comment; \"nor a string\"",
		"keyword 3:1-3:11 :ENDREGION",
		`codeBlock 4:11-4:29 {|sId| sId == "A"}`,
	)
}

func TestLexicalTokensDataSources(t *testing.T) {
	// [spec feature.lexical_tokens/A10]
	sql := "SELECT ID, 'x' AS TAG FROM SAMPLES /* all of them */\n"
	if got := lexicalDesc(sql, true); len(got) != 0 {
		t.Errorf("plain SQL data source: got %v, want none", got)
	}

	header := ":PARAMETERS sStatus := \"A\";\nSELECT ID FROM SAMPLES WHERE STATUS = @sStatus AND TAG = 'x'\n"
	wantTokens(t, lexicalDesc(header, true),
		"keyword 1:1-1:12 :PARAMETERS",
		`string 1:24-1:27 "A"`,
	)

	ssl := ":PARAMETERS sStatus;\n:DEFAULT sStatus, \"A\";\n/* reads them;\n:RETURN SQLExecute(\"SELECT ID FROM SAMPLES\");\n"
	wantTokens(t, lexicalDesc(ssl, true),
		"keyword 1:1-1:12 :PARAMETERS",
		"keyword 2:1-2:9 :DEFAULT",
		`string 2:19-2:22 "A"`,
		"comment 3:1-3:15 /* reads them;",
		"keyword 4:1-4:8 :RETURN",
		`string 4:20-4:44 "SELECT ID FROM SAMPLES"`,
	)

	// The same text as a script is SSL throughout.
	if got := lexicalDesc(sql, false); len(got) == 0 {
		t.Error("a script is lexed as SSL whatever it holds")
	}
}

// checkLexicalShape asserts the invariants a client relies on: document
// order, no overlap, inside the document, a known kind, and agreement with
// the lexer on what the text at each range is.
func checkLexicalShape(t *testing.T, name, src string, tokens []LexicalToken) {
	t.Helper()
	lines := strings.Split(src, "\n")
	kinds := map[string]bool{LexicalComment: true, LexicalString: true, LexicalKeyword: true,
		LexicalText: true, LexicalCodeBlock: true}
	var prev Position
	for i, token := range tokens {
		r := token.Range
		if !kinds[token.Kind] {
			t.Fatalf("%s: token %d has unknown kind %q", name, i, token.Kind)
		}
		if !positionBefore(r.Start, r.End) {
			t.Fatalf("%s: token %d is empty or reversed: %+v", name, i, r)
		}
		if i > 0 && positionBefore(r.Start, prev) {
			t.Fatalf("%s: token %d at %+v overlaps or precedes the one before, ending at %+v", name, i, r.Start, prev)
		}
		prev = r.End
		if r.End.Line >= len(lines) || r.End.Character > utf8.RuneCountInString(lines[r.End.Line]) ||
			r.Start.Character > utf8.RuneCountInString(lines[r.Start.Line]) {
			t.Fatalf("%s: token %d at %+v is outside the document", name, i, r)
		}
	}
}

func TestLexicalTokensShapeOverFixtures(t *testing.T) {
	// [spec feature.lexical_tokens/A11]
	total := 0
	for name, src := range treeFixtures(t) {
		lexed := lexer.NewLexer(src).Tokenize()
		tokens := GetLexicalTokens(src, lexed, false)
		checkLexicalShape(t, name, src, tokens)
		total += len(tokens)

		// Every comment and string the lexer read is reported once.
		want := 0
		for _, tk := range lexed {
			if (tk.Type == lexer.TokenComment || tk.Type == lexer.TokenString) && tk.Text != "" {
				want++
			}
		}
		got := 0
		for _, tk := range tokens {
			if tk.Kind == LexicalComment || tk.Kind == LexicalString {
				got++
			}
		}
		if got != want {
			t.Errorf("%s: %d comment and string tokens, the lexer read %d", name, got, want)
		}

		checkLexicalShape(t, name+" (data source)", src, GetLexicalTokens(src, lexed, true))
	}
	if total == 0 {
		t.Fatal("no fixture produced a token")
	}
}

func TestLexicalTokensAgreeWithSemanticTokens(t *testing.T) {
	// [spec feature.lexical_tokens/A12]
	inside := func(inner, outer Range) bool {
		return !positionBefore(inner.Start, outer.Start) && !positionBefore(outer.End, inner.End)
	}
	overlaps := func(a, b Range) bool {
		return positionBefore(a.Start, b.End) && positionBefore(b.Start, a.End)
	}
	checked := 0
	for name, src := range treeFixtures(t) {
		lexed := lexer.NewLexer(src).Tokenize()
		lexical := GetLexicalTokens(src, lexed, false)
		for _, sem := range GetSemanticTokens(src, lexed, nil, SemanticTokenOptions{}) {
			for _, lex := range lexical {
				if !overlaps(sem.Range, lex.Range) {
					continue
				}
				checked++
				// A name never lies in a comment, a keyword, a region body
				// or a code block; the one token in a string is a dispatch
				// target, which lies wholly inside it.
				if lex.Kind != LexicalString || !inside(sem.Range, lex.Range) || sem.Type != SemanticFunction {
					t.Errorf("%s: semantic token %+v (type %s) overlaps the %s at %+v",
						name, sem.Range, SemanticTokenTypes[sem.Type], lex.Kind, lex.Range)
				}
			}
		}
	}
	if checked == 0 {
		t.Fatal("no fixture has a dispatch target inside a string")
	}
}

// FuzzLexicalTokens checks the shape invariants on arbitrary input.
func FuzzLexicalTokens(f *testing.F) {
	addCorpusSeeds(f)
	f.Fuzz(func(t *testing.T, src string) {
		if !utf8.ValidString(src) {
			t.Skip()
		}
		lexed := lexer.NewLexer(src).Tokenize()
		checkLexicalShape(t, "fuzz input", src, GetLexicalTokens(src, lexed, false))
		checkLexicalShape(t, "fuzz input (data source)", src, GetLexicalTokens(src, lexed, true))
	})
}
