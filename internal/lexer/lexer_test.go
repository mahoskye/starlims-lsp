package lexer

import (
	"strings"
	"testing"
)

// ==================== Token Type Tests ====================

func TestLexer_TokenWhitespace(t *testing.T) {
	tests := []struct {
		name  string
		input string
		want  string
	}{
		{"space", " ", " "},
		{"tab", "\t", "\t"},
		{"newline", "\n", "\n"},
		{"crlf", "\r\n", "\r\n"},
		{"mixed", " \t\n  \t", " \t\n  \t"},
		{"multiple_newlines", "\n\n\n", "\n\n\n"},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			lex := NewLexer(tc.input)
			tokens := lex.Tokenize()

			if len(tokens) < 2 {
				t.Fatalf("expected at least 2 tokens (whitespace + EOF), got %d", len(tokens))
			}
			if tokens[0].Type != TokenWhitespace {
				t.Errorf("expected TokenWhitespace, got %s", tokens[0].Type)
			}
			if tokens[0].Text != tc.want {
				t.Errorf("expected %q, got %q", tc.want, tokens[0].Text)
			}
		})
	}
}

func TestLexer_TokenComment(t *testing.T) {
	tests := []struct {
		name  string
		input string
		want  string
	}{
		{"single_line", "/* comment ;", "/* comment ;"},
		{"with_newline", "/* line1\nline2 ;", "/* line1\nline2 ;"},
		{"empty", "/* ;", "/* ;"},
		// SSL comments end at the FIRST semicolon
		{"with_semicolon_inside", "/* a;", "/* a;"},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			lex := NewLexer(tc.input)
			tokens := lex.Tokenize()

			if len(tokens) < 2 {
				t.Fatalf("expected at least 2 tokens, got %d", len(tokens))
			}
			if tokens[0].Type != TokenComment {
				t.Errorf("expected TokenComment, got %s", tokens[0].Type)
			}
			if tokens[0].Text != tc.want {
				t.Errorf("expected %q, got %q", tc.want, tokens[0].Text)
			}
		})
	}
}

func TestLexer_TokenComment_Unclosed(t *testing.T) {
	// Comment without terminating semicolon should still be captured
	input := "/* unclosed comment"
	lex := NewLexer(input)
	tokens := lex.Tokenize()

	if len(tokens) < 2 {
		t.Fatalf("expected at least 2 tokens, got %d", len(tokens))
	}
	if tokens[0].Type != TokenComment {
		t.Errorf("expected TokenComment for unclosed comment, got %s", tokens[0].Type)
	}
	if tokens[0].Text != input {
		t.Errorf("expected %q, got %q", input, tokens[0].Text)
	}
}

func TestLexer_TokenString(t *testing.T) {
	tests := []struct {
		name  string
		input string
		want  string
	}{
		{"double_quote", `"hello"`, `"hello"`},
		{"single_quote", `'hello'`, `'hello'`},
		{"bracket_quote", `[hello]`, `[hello]`},
		{"bracket_nested_one_level", `[[a]b]`, `[[a]b]`},
		{"bracket_nested_content", `[[nested]]`, `[[nested]]`},
		{"empty_double", `""`, `""`},
		{"empty_single", `''`, `''`},
		{"with_spaces", `"hello world"`, `"hello world"`},
		{"with_newline", "\"hello\nworld\"", "\"hello\nworld\""},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			lex := NewLexer(tc.input)
			tokens := lex.Tokenize()

			if len(tokens) < 2 {
				t.Fatalf("expected at least 2 tokens, got %d", len(tokens))
			}
			if tokens[0].Type != TokenString {
				t.Errorf("expected TokenString, got %s", tokens[0].Type)
			}
			if tokens[0].Text != tc.want {
				t.Errorf("expected %q, got %q", tc.want, tokens[0].Text)
			}
		})
	}
}

func TestLexer_TokenString_Escaped(t *testing.T) {
	tests := []struct {
		name  string
		input string
		want  string
	}{
		{"backslash_is_literal", `"hello\\world"`, `"hello\\world"`},
		{"quote_after_backslash_still_closes", `"hello\"world"`, `"hello\"`},
		{"quote_after_multiple_backslashes_still_closes", `"a\\\"b"`, `"a\\\"`},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			lex := NewLexer(tc.input)
			tokens := lex.Tokenize()

			if len(tokens) < 2 {
				t.Fatalf("expected at least 2 tokens, got %d", len(tokens))
			}
			if tokens[0].Type != TokenString {
				t.Errorf("expected TokenString, got %s", tokens[0].Type)
			}
			if tokens[0].Text != tc.want {
				t.Errorf("expected %q, got %q", tc.want, tokens[0].Text)
			}
		})
	}
}

func TestLexer_TokenString_Unicode(t *testing.T) {
	tests := []struct {
		name  string
		input string
	}{
		{"spanish", `"Ñoño"`},
		{"japanese", `"日本語"`},
		{"emoji", `"Hello 🎉"`},
		{"mixed", `"Café, 日本, 🌍"`},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			lex := NewLexer(tc.input)
			tokens := lex.Tokenize()

			if len(tokens) < 2 {
				t.Fatalf("expected at least 2 tokens, got %d", len(tokens))
			}
			if tokens[0].Type != TokenString {
				t.Errorf("expected TokenString, got %s", tokens[0].Type)
			}
			if tokens[0].Text != tc.input {
				t.Errorf("expected %q, got %q", tc.input, tokens[0].Text)
			}
		})
	}
}

func TestLexer_TokenString_Unclosed(t *testing.T) {
	tests := []struct {
		name  string
		input string
	}{
		{"unclosed_double", `"hello`},
		{"unclosed_single", `'hello`},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			lex := NewLexer(tc.input)
			tokens := lex.Tokenize()

			// Should still get a string token even if unclosed
			if len(tokens) < 2 {
				t.Fatalf("expected at least 2 tokens, got %d", len(tokens))
			}
			if tokens[0].Type != TokenString {
				t.Errorf("expected TokenString for unclosed string, got %s", tokens[0].Type)
			}
		})
	}
}

func TestLexer_TokenNumber(t *testing.T) {
	tests := []struct {
		name  string
		input string
		want  string
	}{
		{"integer", "123", "123"},
		{"zero", "0", "0"},
		{"decimal", "123.456", "123.456"},
		{"leading_dot", ".5", ".5"},
		// Grammar: DecimalPart ::= "." Digit {Digit} — a dot with no digit
		// after it is not part of the number (issue #83).
		{"trailing_dot_not_consumed", "5.", "5"},
		{"scientific_explicit_plus_invalid", "1.5e+10", "1.5"},
		{"scientific_negative", "2.3e-5", "2.3e-5"},
		{"scientific_requires_decimal_point_lower", "1e10", "1"},
		{"scientific_requires_decimal_point_upper", "1E10", "1"},
		{"scientific_requires_leading_digit_before_decimal", ".5e1", ".5"},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			lex := NewLexer(tc.input)
			tokens := lex.Tokenize()

			if len(tokens) < 2 {
				t.Fatalf("expected at least 2 tokens, got %d", len(tokens))
			}
			if tokens[0].Type != TokenNumber {
				t.Errorf("expected TokenNumber, got %s", tokens[0].Type)
			}
			if tokens[0].Text != tc.want {
				t.Errorf("expected %q, got %q", tc.want, tokens[0].Text)
			}
		})
	}
}

func TestLexer_TokenKeyword(t *testing.T) {
	tests := []struct {
		name  string
		input string
		want  string
	}{
		{"if", ":IF", ":IF"},
		{"endif", ":ENDIF", ":ENDIF"},
		{"while", ":WHILE", ":WHILE"},
		{"procedure", ":PROCEDURE", ":PROCEDURE"},
		{"declare", ":DECLARE", ":DECLARE"},
		{"public", ":PUBLIC", ":PUBLIC"},
		{"for", ":FOR", ":FOR"},
		{"try", ":TRY", ":TRY"},
		{"resume", ":RESUME", ":RESUME"},
		{"lowercase", ":if", ":if"},
		{"mixed_case", ":If", ":If"},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			lex := NewLexer(tc.input)
			tokens := lex.Tokenize()

			if len(tokens) < 2 {
				t.Fatalf("expected at least 2 tokens, got %d", len(tokens))
			}
			if tokens[0].Type != TokenKeyword {
				t.Errorf("expected TokenKeyword, got %s", tokens[0].Type)
			}
			if tokens[0].Text != tc.want {
				t.Errorf("expected %q, got %q", tc.want, tokens[0].Text)
			}
		})
	}
}

func TestLexer_TokenKeyword_AllKeywords(t *testing.T) {
	// Test a selection of important keywords
	// Note: :ELSEIF and :EXIT do not exist in SSL
	keywords := []string{
		":PROCEDURE", ":ENDPROC", ":IF", ":ELSE", ":ENDIF",
		":WHILE", ":ENDWHILE", ":FOR", ":NEXT", ":TO", ":STEP",
		":TRY", ":CATCH", ":FINALLY", ":ENDTRY",
		":ERROR", ":RESUME",
		":DECLARE", ":PUBLIC", ":PARAMETERS", ":DEFAULT",
		":BEGINCASE", ":CASE", ":OTHERWISE", ":ENDCASE", ":EXITCASE",
		":RETURN", ":CLASS", ":INHERIT", ":REGION", ":ENDREGION",
		":BEGININLINECODE", ":ENDINLINECODE",
	}

	for _, kw := range keywords {
		t.Run(kw, func(t *testing.T) {
			lex := NewLexer(kw)
			tokens := lex.Tokenize()

			if len(tokens) < 2 {
				t.Fatalf("expected at least 2 tokens, got %d", len(tokens))
			}
			if tokens[0].Type != TokenKeyword {
				t.Errorf("expected TokenKeyword for %s, got %s", kw, tokens[0].Type)
			}
		})
	}
}

func TestLexer_TokenIdentifier(t *testing.T) {
	tests := []struct {
		name  string
		input string
		want  string
	}{
		{"simple", "myVar", "myVar"},
		{"with_underscore", "my_var", "my_var"},
		{"with_digits", "var123", "var123"},
		{"underscore_prefix", "_private", "_private"},
		{"all_caps", "CONSTANT", "CONSTANT"},
		{"mixed_case", "camelCase", "camelCase"},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			lex := NewLexer(tc.input)
			tokens := lex.Tokenize()

			if len(tokens) < 2 {
				t.Fatalf("expected at least 2 tokens, got %d", len(tokens))
			}
			if tokens[0].Type != TokenIdentifier {
				t.Errorf("expected TokenIdentifier, got %s", tokens[0].Type)
			}
			if tokens[0].Text != tc.want {
				t.Errorf("expected %q, got %q", tc.want, tokens[0].Text)
			}
		})
	}
}

func TestLexer_TokenOperator(t *testing.T) {
	tests := []struct {
		name  string
		input string
		want  string
		typ   TokenType
	}{
		{"plus", "+", "+", TokenOperator},
		{"minus", "-", "-", TokenOperator},
		{"multiply", "*", "*", TokenOperator},
		{"divide", "/", "/", TokenOperator},
		{"power", "^", "^", TokenOperator},
		{"modulo", "%", "%", TokenOperator},
		{"equal", "=", "=", TokenOperator},
		{"less_than", "<", "<", TokenOperator},
		{"greater_than", ">", ">", TokenOperator},
		{"assignment", ":=", ":=", TokenOperator},
		{"hash", "#", "#", TokenOperator},
		{"dollar", "$", "$", TokenOperator},
		// Multi-character comparison operators
		{"less_equal", "<=", "<=", TokenOperator},
		{"greater_equal", ">=", ">=", TokenOperator},
		{"equal_equal", "==", "==", TokenOperator},
		{"not_equal", "!=", "!=", TokenOperator},
		{"not_equal_legacy", "<>", "<>", TokenOperator},
		{"power_alias", "**", "**", TokenOperator},
		{"increment", "++", "++", TokenOperator},
		{"decrement", "--", "--", TokenOperator},
		{"shift_left", "<<", "<<", TokenOperator},
		{"shift_right", ">>", ">>", TokenOperator},
		// Compound assignment operators
		{"plus_equal", "+=", "+=", TokenOperator},
		{"minus_equal", "-=", "-=", TokenOperator},
		{"multiply_equal", "*=", "*=", TokenOperator},
		{"divide_equal", "/=", "/=", TokenOperator},
		{"modulo_equal", "%=", "%=", TokenOperator},
		{"power_equal", "^=", "^=", TokenOperator},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			lex := NewLexer(tc.input)
			tokens := lex.Tokenize()

			if len(tokens) < 2 {
				t.Fatalf("expected at least 2 tokens, got %d", len(tokens))
			}
			if tokens[0].Type != tc.typ {
				t.Errorf("expected %s, got %s", tc.typ, tokens[0].Type)
			}
			if tokens[0].Text != tc.want {
				t.Errorf("expected %q, got %q", tc.want, tokens[0].Text)
			}
		})
	}
}

func TestLexer_TokenOperator_Logical(t *testing.T) {
	tests := []struct {
		name  string
		input string
		want  string
	}{
		{"and", ".AND.", ".AND."},
		{"or", ".OR.", ".OR."},
		{"not", ".NOT.", ".NOT."},
		{"lowercase_and", ".and.", ".and."},
		{"lowercase_or", ".or.", ".or."},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			lex := NewLexer(tc.input)
			tokens := lex.Tokenize()

			if len(tokens) < 2 {
				t.Fatalf("expected at least 2 tokens, got %d", len(tokens))
			}
			if tokens[0].Type != TokenOperator {
				t.Errorf("expected TokenOperator, got %s", tokens[0].Type)
			}
			if tokens[0].Text != tc.want {
				t.Errorf("expected %q, got %q", tc.want, tokens[0].Text)
			}
		})
	}
}

func TestLexer_TokenOperator_Boolean(t *testing.T) {
	tests := []struct {
		name  string
		input string
	}{
		{"true_upper", ".T."},
		{"true_lower", ".t."},
		{"false_upper", ".F."},
		{"false_lower", ".f."},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			lex := NewLexer(tc.input)
			tokens := lex.Tokenize()

			if len(tokens) < 2 {
				t.Fatalf("expected at least 2 tokens, got %d", len(tokens))
			}
			// Boolean literals are treated as keywords
			if tokens[0].Type != TokenKeyword {
				t.Errorf("expected TokenKeyword for boolean literal, got %s", tokens[0].Type)
			}
		})
	}
}

func TestLexer_TokenIdentifier_TrueFalseAreNotBooleanAliases(t *testing.T) {
	tests := []string{"true", "false", "TRUE", "FALSE"}

	for _, input := range tests {
		t.Run(input, func(t *testing.T) {
			lex := NewLexer(input)
			tokens := lex.Tokenize()

			if len(tokens) < 2 {
				t.Fatalf("expected at least 2 tokens, got %d", len(tokens))
			}
			if tokens[0].Type != TokenIdentifier {
				t.Errorf("expected TokenIdentifier for %q, got %s", input, tokens[0].Type)
			}
		})
	}
}

func TestLexer_TokenPunctuation(t *testing.T) {
	tests := []struct {
		name  string
		input string
		want  string
	}{
		{"open_paren", "(", "("},
		{"close_paren", ")", ")"},
		{"open_brace", "{", "{"},
		{"close_brace", "}", "}"},
		{"semicolon", ";", ";"},
		{"comma", ",", ","},
		{"close_bracket", "]", "]"},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			lex := NewLexer(tc.input)
			tokens := lex.Tokenize()

			if len(tokens) < 2 {
				t.Fatalf("expected at least 2 tokens, got %d", len(tokens))
			}
			if tokens[0].Type != TokenPunctuation {
				t.Errorf("expected TokenPunctuation, got %s", tokens[0].Type)
			}
			if tokens[0].Text != tc.want {
				t.Errorf("expected %q, got %q", tc.want, tokens[0].Text)
			}
		})
	}
}

// ==================== Context-Sensitive Tests ====================

func TestLexer_ArrayAccess(t *testing.T) {
	tests := []struct {
		name       string
		input      string
		wantBefore TokenType
		wantText   string
	}{
		{"after_identifier", "arr[0]", TokenPunctuation, "["},
		{"after_paren", "func()[0]", TokenPunctuation, "["},
		{"after_bracket", "arr[0][1]", TokenPunctuation, "["},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			lex := NewLexer(tc.input)
			tokens := lex.Tokenize()

			// Find the [ token
			var bracketToken *Token
			for i := range tokens {
				if tokens[i].Text == "[" {
					bracketToken = &tokens[i]
					break
				}
			}

			if bracketToken == nil {
				t.Fatal("expected to find [ token")
			}
			if bracketToken.Type != tc.wantBefore {
				t.Errorf("expected %s for [, got %s", tc.wantBefore, bracketToken.Type)
			}
		})
	}
}

func TestLexer_BracketString_AtLineStart(t *testing.T) {
	// [ at line start or after operators should be a string delimiter
	input := "[hello]"
	lex := NewLexer(input)
	tokens := lex.Tokenize()

	if len(tokens) < 2 {
		t.Fatalf("expected at least 2 tokens, got %d", len(tokens))
	}
	if tokens[0].Type != TokenString {
		t.Errorf("expected TokenString for bracket string, got %s", tokens[0].Type)
	}
	if tokens[0].Text != "[hello]" {
		t.Errorf("expected [hello], got %q", tokens[0].Text)
	}
}

func TestLexer_ColonContext(t *testing.T) {
	tests := []struct {
		name     string
		input    string
		wantType TokenType
		wantText string
	}{
		{"assignment", "x := 1", TokenOperator, ":="},
		{"keyword_if", ":IF", TokenKeyword, ":IF"},
		{"keyword_procedure", ":PROCEDURE", TokenKeyword, ":PROCEDURE"},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			lex := NewLexer(tc.input)
			tokens := lex.Tokenize()

			// Find the token we're looking for
			found := false
			for _, tok := range tokens {
				if tok.Text == tc.wantText {
					found = true
					if tok.Type != tc.wantType {
						t.Errorf("expected %s for %q, got %s", tc.wantType, tc.wantText, tok.Type)
					}
					break
				}
			}
			if !found {
				t.Errorf("expected to find token %q", tc.wantText)
			}
		})
	}
}

func TestLexer_ColonAfterIdentifier(t *testing.T) {
	// x:y should tokenize as identifier, punctuation, identifier (not keyword)
	input := "obj:method"
	lex := NewLexer(input)
	tokens := lex.Tokenize()

	// Filter out EOF
	var nonEOF []Token
	for _, t := range tokens {
		if t.Type != TokenEOF {
			nonEOF = append(nonEOF, t)
		}
	}

	if len(nonEOF) != 3 {
		t.Fatalf("expected 3 tokens, got %d: %v", len(nonEOF), nonEOF)
	}
	if nonEOF[0].Type != TokenIdentifier || nonEOF[0].Text != "obj" {
		t.Errorf("expected identifier 'obj', got %s %q", nonEOF[0].Type, nonEOF[0].Text)
	}
	if nonEOF[1].Type != TokenPunctuation || nonEOF[1].Text != ":" {
		t.Errorf("expected punctuation ':', got %s %q", nonEOF[1].Type, nonEOF[1].Text)
	}
	if nonEOF[2].Type != TokenIdentifier || nonEOF[2].Text != "method" {
		t.Errorf("expected identifier 'method', got %s %q", nonEOF[2].Type, nonEOF[2].Text)
	}
}

func TestLexer_MemberAccessKeywordName(t *testing.T) {
	// oEmail:To — "To" matches keyword name TO but is just an identifier.
	input := "oEmail:To"
	lex := NewLexer(input)
	tokens := lex.Tokenize()

	var nonEOF []Token
	for _, tok := range tokens {
		if tok.Type != TokenEOF {
			nonEOF = append(nonEOF, tok)
		}
	}

	if len(nonEOF) != 3 {
		t.Fatalf("expected 3 tokens, got %d: %v", len(nonEOF), nonEOF)
	}
	if nonEOF[0].Type != TokenIdentifier || nonEOF[0].Text != "oEmail" {
		t.Errorf("expected identifier 'oEmail', got %s %q", nonEOF[0].Type, nonEOF[0].Text)
	}
	if nonEOF[1].Type != TokenPunctuation || nonEOF[1].Text != ":" {
		t.Errorf("expected punctuation ':', got %s %q", nonEOF[1].Type, nonEOF[1].Text)
	}
	if nonEOF[2].Type != TokenIdentifier || nonEOF[2].Text != "To" {
		t.Errorf("expected identifier 'To', got %s %q", nonEOF[2].Type, nonEOF[2].Text)
	}
}

// ==================== Position Tracking Tests ====================

func TestLexer_Position_LineColumn(t *testing.T) {
	input := "a\nb\nc"
	lex := NewLexer(input)
	tokens := lex.Tokenize()

	expected := []struct {
		text   string
		line   int
		column int
	}{
		{"a", 1, 1},
		{"\n", 1, 2},
		{"b", 2, 1},
		{"\n", 2, 2},
		{"c", 3, 1},
	}

	for i, exp := range expected {
		if i >= len(tokens) {
			t.Fatalf("not enough tokens")
		}
		tok := tokens[i]
		if tok.Text != exp.text {
			t.Errorf("token %d: expected text %q, got %q", i, exp.text, tok.Text)
		}
		if tok.Line != exp.line {
			t.Errorf("token %d (%q): expected line %d, got %d", i, tok.Text, exp.line, tok.Line)
		}
		if tok.Column != exp.column {
			t.Errorf("token %d (%q): expected column %d, got %d", i, tok.Text, exp.column, tok.Column)
		}
	}
}

func TestLexer_Position_AfterMultiCharToken(t *testing.T) {
	input := "abc def"
	lex := NewLexer(input)
	tokens := lex.Tokenize()

	// abc at column 1, space at column 4, def at column 5
	if tokens[0].Column != 1 {
		t.Errorf("expected 'abc' at column 1, got %d", tokens[0].Column)
	}
	if tokens[1].Column != 4 {
		t.Errorf("expected space at column 4, got %d", tokens[1].Column)
	}
	if tokens[2].Column != 5 {
		t.Errorf("expected 'def' at column 5, got %d", tokens[2].Column)
	}
}

func TestLexer_Position_Offset(t *testing.T) {
	input := "ab cd"
	lex := NewLexer(input)
	tokens := lex.Tokenize()

	if tokens[0].Offset != 0 {
		t.Errorf("expected 'ab' at offset 0, got %d", tokens[0].Offset)
	}
	if tokens[1].Offset != 2 {
		t.Errorf("expected space at offset 2, got %d", tokens[1].Offset)
	}
	if tokens[2].Offset != 3 {
		t.Errorf("expected 'cd' at offset 3, got %d", tokens[2].Offset)
	}
}

// ==================== Edge Case Tests ====================

func TestLexer_EdgeCase_EmptyInput(t *testing.T) {
	lex := NewLexer("")
	tokens := lex.Tokenize()

	if len(tokens) != 1 {
		t.Fatalf("expected 1 token (EOF), got %d", len(tokens))
	}
	if tokens[0].Type != TokenEOF {
		t.Errorf("expected TokenEOF, got %s", tokens[0].Type)
	}
}

func TestLexer_EdgeCase_OnlyWhitespace(t *testing.T) {
	lex := NewLexer("   \n\t  ")
	tokens := lex.Tokenize()

	if len(tokens) != 2 {
		t.Fatalf("expected 2 tokens (whitespace + EOF), got %d", len(tokens))
	}
	if tokens[0].Type != TokenWhitespace {
		t.Errorf("expected TokenWhitespace, got %s", tokens[0].Type)
	}
	if tokens[1].Type != TokenEOF {
		t.Errorf("expected TokenEOF, got %s", tokens[1].Type)
	}
}

func TestLexer_EdgeCase_VeryLongLine(t *testing.T) {
	// Create a very long identifier
	longIdent := strings.Repeat("a", 10000)
	lex := NewLexer(longIdent)
	tokens := lex.Tokenize()

	if len(tokens) != 2 {
		t.Fatalf("expected 2 tokens, got %d", len(tokens))
	}
	if tokens[0].Type != TokenIdentifier {
		t.Errorf("expected TokenIdentifier, got %s", tokens[0].Type)
	}
	if len(tokens[0].Text) != 10000 {
		t.Errorf("expected 10000 char identifier, got %d", len(tokens[0].Text))
	}
}

func TestLexer_EdgeCase_ManyTokens(t *testing.T) {
	// Create input with many tokens
	var parts []string
	for i := 0; i < 1000; i++ {
		parts = append(parts, "a")
	}
	input := strings.Join(parts, " ")
	lex := NewLexer(input)
	tokens := lex.Tokenize()

	// Should have 1000 identifiers + 999 whitespace + 1 EOF = 2000 tokens
	expectedTokens := 1000 + 999 + 1
	if len(tokens) != expectedTokens {
		t.Errorf("expected %d tokens, got %d", expectedTokens, len(tokens))
	}
}

func TestLexer_EdgeCase_OnlyComments(t *testing.T) {
	input := "/* comment 1 ; /* comment 2 ;"
	lex := NewLexer(input)
	tokens := lex.Tokenize()

	// Should have 2 comments + 1 EOF
	commentCount := 0
	for _, tok := range tokens {
		if tok.Type == TokenComment {
			commentCount++
		}
	}
	if commentCount != 2 {
		t.Errorf("expected 2 comments, got %d", commentCount)
	}
}

// ==================== Helper Function Tests ====================

func TestGetTokenAtPosition(t *testing.T) {
	input := "abc def ghi"
	lex := NewLexer(input)
	tokens := lex.Tokenize()

	// Token positions: abc at col 1-3, space at col 4, def at col 5-7, space at col 8, ghi at col 9-11
	// GetTokenAtPosition uses <= for tokenEnd, so boundary columns match the previous token
	// We use columns that are clearly inside each token (not at boundaries)
	tests := []struct {
		name   string
		line   int
		column int
		want   string
	}{
		{"start_of_abc", 1, 1, "abc"},
		{"middle_of_abc", 1, 2, "abc"},
		{"inside_def", 1, 6, "def"},  // column 6 is clearly inside "def"
		{"inside_ghi", 1, 10, "ghi"}, // column 10 is clearly inside "ghi"
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			tok := GetTokenAtPosition(tokens, tc.line, tc.column)
			if tok == nil {
				t.Fatalf("expected token at line %d col %d, got nil", tc.line, tc.column)
			}
			if tok.Text != tc.want {
				t.Errorf("expected %q at line %d col %d, got %q", tc.want, tc.line, tc.column, tok.Text)
			}
		})
	}
}

func TestGetTokenAtPosition_NoMatch(t *testing.T) {
	input := "abc"
	lex := NewLexer(input)
	tokens := lex.Tokenize()

	tok := GetTokenAtPosition(tokens, 2, 1) // Line 2 doesn't exist
	if tok != nil {
		t.Errorf("expected nil for non-existent line, got %v", tok)
	}
}

func TestGetWordAtPosition(t *testing.T) {
	input := "hello world test"

	tests := []struct {
		line   int
		column int
		want   string
	}{
		{1, 1, "hello"},
		{1, 3, "hello"},
		{1, 7, "world"},
		{1, 13, "test"},
	}

	for _, tc := range tests {
		t.Run(tc.want, func(t *testing.T) {
			word := GetWordAtPosition(input, tc.line, tc.column)
			if word != tc.want {
				t.Errorf("expected %q at line %d col %d, got %q", tc.want, tc.line, tc.column, word)
			}
		})
	}
}

func TestGetWordAtPosition_AtBoundary(t *testing.T) {
	input := "hello world"

	// GetWordAtPosition finds word boundaries - column 6 is after "hello" ends
	// The function looks for word chars in both directions from the position
	// Column 6 is the space, which is not a word char, so it returns ""
	// But column 5 is still part of "hello", column 7 is start of "world"
	word := GetWordAtPosition(input, 1, 5)
	if word != "hello" {
		t.Errorf("expected 'hello' at column 5, got %q", word)
	}

	word = GetWordAtPosition(input, 1, 7)
	if word != "world" {
		t.Errorf("expected 'world' at column 7, got %q", word)
	}
}

func TestGetWordAtPosition_InvalidPosition(t *testing.T) {
	input := "hello"

	// Invalid line
	word := GetWordAtPosition(input, 0, 1)
	if word != "" {
		t.Errorf("expected empty string for invalid line, got %q", word)
	}

	// Invalid column
	word = GetWordAtPosition(input, 1, 0)
	if word != "" {
		t.Errorf("expected empty string for invalid column, got %q", word)
	}
}

func TestGetWordAtPosition_MultiLine(t *testing.T) {
	input := "line1\nline2\nline3"

	tests := []struct {
		line int
		col  int
		want string
	}{
		{1, 1, "line1"},
		{2, 1, "line2"},
		{3, 1, "line3"},
	}

	for _, tc := range tests {
		word := GetWordAtPosition(input, tc.line, tc.col)
		if word != tc.want {
			t.Errorf("expected %q at line %d, got %q", tc.want, tc.line, word)
		}
	}
}

// ==================== Complete Statement Tests ====================

func TestLexer_CompleteStatement(t *testing.T) {
	input := `:PROCEDURE Test;
:DECLARE x, y;
x := 1 + 2;
:IF x > 0;
	y := x * 2;
:ENDIF;
:ENDPROC;`

	lex := NewLexer(input)
	tokens := lex.Tokenize()

	// Verify we got the expected token types in sequence
	// (simplified check - just verify we have a mix of token types)
	tokenTypes := make(map[TokenType]int)
	for _, tok := range tokens {
		tokenTypes[tok.Type]++
	}

	if tokenTypes[TokenKeyword] == 0 {
		t.Error("expected some keywords")
	}
	if tokenTypes[TokenIdentifier] == 0 {
		t.Error("expected some identifiers")
	}
	if tokenTypes[TokenOperator] == 0 {
		t.Error("expected some operators")
	}
	if tokenTypes[TokenNumber] == 0 {
		t.Error("expected some numbers")
	}
	if tokenTypes[TokenPunctuation] == 0 {
		t.Error("expected some punctuation")
	}
	if tokenTypes[TokenWhitespace] == 0 {
		t.Error("expected some whitespace")
	}
	if tokenTypes[TokenEOF] != 1 {
		t.Error("expected exactly one EOF")
	}
}

func TestLexer_SQLString(t *testing.T) {
	input := `ds := SQLExecute("SELECT * FROM users WHERE id = ?userId?");`

	lex := NewLexer(input)
	tokens := lex.Tokenize()

	// Find the string token
	var stringToken *Token
	for i := range tokens {
		if tokens[i].Type == TokenString {
			stringToken = &tokens[i]
			break
		}
	}

	if stringToken == nil {
		t.Fatal("expected to find a string token")
	}
	if !strings.Contains(stringToken.Text, "SELECT") {
		t.Errorf("expected SQL string to contain SELECT, got %q", stringToken.Text)
	}
}

// ==================== TokenType String Tests ====================

func TestTokenType_String(t *testing.T) {
	tests := []struct {
		typ  TokenType
		want string
	}{
		{TokenWhitespace, "Whitespace"},
		{TokenComment, "Comment"},
		{TokenString, "String"},
		{TokenNumber, "Number"},
		{TokenKeyword, "Keyword"},
		{TokenIdentifier, "Identifier"},
		{TokenOperator, "Operator"},
		{TokenPunctuation, "Punctuation"},
		{TokenUnknown, "Unknown"},
		{TokenEOF, "EOF"},
	}

	for _, tc := range tests {
		t.Run(tc.want, func(t *testing.T) {
			got := tc.typ.String()
			if got != tc.want {
				t.Errorf("expected %q, got %q", tc.want, got)
			}
		})
	}
}

// ==================== Benchmark Tests ====================

// generateSSLDocument creates a test SSL document with the specified number of procedures
func generateSSLDocument(procCount int) string {
	var sb strings.Builder
	for i := 0; i < procCount; i++ {
		sb.WriteString(`:PROCEDURE Proc`)
		sb.WriteString(string(rune('0' + i%10)))
		sb.WriteString(`;
:PARAMETERS param1, param2;
:DECLARE localVar, result;
:IF param1 > 0;
	result := param1 * param2;
:ELSE;
	result := 0;
:ENDIF;
:RETURN result;
:ENDPROC;

`)
	}
	return sb.String()
}

func BenchmarkLexer_Tokenize_Small(b *testing.B) {
	text := generateSSLDocument(10)
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		lex := NewLexer(text)
		_ = lex.Tokenize()
	}
}

func BenchmarkLexer_Tokenize_Medium(b *testing.B) {
	text := generateSSLDocument(100)
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		lex := NewLexer(text)
		_ = lex.Tokenize()
	}
}

func BenchmarkLexer_Tokenize_Large(b *testing.B) {
	text := generateSSLDocument(1000)
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		lex := NewLexer(text)
		_ = lex.Tokenize()
	}
}

// ==================== Context Detection Tests ====================

func TestIsInsideStringOrComment_InsideString(t *testing.T) {
	text := `x := "hello world";`
	lex := NewLexer(text)
	tokens := lex.Tokenize()

	// Position inside the string (on "hello")
	if !IsInsideStringOrComment(tokens, 1, 8) {
		t.Error("expected position inside string to return true")
	}
}

func TestIsInsideStringOrComment_OutsideString(t *testing.T) {
	text := `x := "hello world";`
	lex := NewLexer(text)
	tokens := lex.Tokenize()

	// Position on 'x' (outside string)
	if IsInsideStringOrComment(tokens, 1, 1) {
		t.Error("expected position outside string to return false")
	}
}

func TestIsInsideStringOrComment_InsideComment(t *testing.T) {
	text := `/* this is a comment;
x := 1;`
	lex := NewLexer(text)
	tokens := lex.Tokenize()

	// Position inside the comment
	if !IsInsideStringOrComment(tokens, 1, 10) {
		t.Error("expected position inside comment to return true")
	}
}

func TestIsInsideStringOrComment_OutsideComment(t *testing.T) {
	text := `/* this is a comment;
x := 1;`
	lex := NewLexer(text)
	tokens := lex.Tokenize()

	// Position on line 2 (outside comment)
	if IsInsideStringOrComment(tokens, 2, 3) {
		t.Error("expected position outside comment to return false")
	}
}

func TestGetContextAtPosition(t *testing.T) {
	text := `x := "hello"; /* comment;`
	lex := NewLexer(text)
	tokens := lex.Tokenize()

	tests := []struct {
		name     string
		line     int
		column   int
		expected ContextType
	}{
		{"identifier", 1, 1, ContextCode},
		{"inside_string", 1, 8, ContextString},
		{"after_string", 1, 14, ContextCode},
		{"inside_comment", 1, 20, ContextComment},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			ctx := GetContextAtPosition(tokens, tc.line, tc.column)
			if ctx != tc.expected {
				t.Errorf("expected %v, got %v", tc.expected, ctx)
			}
		})
	}
}

func TestLexer_NIL_Literal(t *testing.T) {
	// NIL should be tokenized as a keyword, not an identifier
	tests := []struct {
		name  string
		input string
	}{
		{"uppercase", "NIL"},
		{"lowercase", "nil"},
		{"mixed", "Nil"},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			lex := NewLexer(tc.input)
			tokens := lex.Tokenize()

			if len(tokens) < 2 {
				t.Fatalf("expected at least 2 tokens, got %d", len(tokens))
			}
			if tokens[0].Type != TokenKeyword {
				t.Errorf("expected TokenKeyword for %s, got %s", tc.input, tokens[0].Type)
			}
		})
	}
}

func TestLexer_BracketString_AfterKeyword(t *testing.T) {
	// Bracket strings after keywords should be strings, not array access
	input := `:IF [condition]`
	lex := NewLexer(input)
	tokens := lex.Tokenize()

	// Should be: :IF, whitespace, [condition], EOF
	if len(tokens) < 4 {
		t.Fatalf("expected at least 4 tokens, got %d", len(tokens))
	}

	// First token should be keyword :IF
	if tokens[0].Type != TokenKeyword {
		t.Errorf("expected TokenKeyword for :IF, got %s", tokens[0].Type)
	}

	// Second token should be whitespace
	if tokens[1].Type != TokenWhitespace {
		t.Errorf("expected TokenWhitespace, got %s", tokens[1].Type)
	}

	// Third token should be string [condition]
	if tokens[2].Type != TokenString {
		t.Errorf("expected TokenString for [condition], got %s (text: %s)", tokens[2].Type, tokens[2].Text)
	}
	if tokens[2].Text != "[condition]" {
		t.Errorf("expected [condition], got %s", tokens[2].Text)
	}
}

func TestLexer_BracketString_AfterIdentifier_IsArrayAccess(t *testing.T) {
	// Bracket after identifier should be array access (punctuation), not string
	input := `arr[0]`
	lex := NewLexer(input)
	tokens := lex.Tokenize()

	// Should be: arr, [, 0, ], EOF
	if len(tokens) < 5 {
		t.Fatalf("expected at least 5 tokens, got %d", len(tokens))
	}

	// First token should be identifier arr
	if tokens[0].Type != TokenIdentifier {
		t.Errorf("expected TokenIdentifier for arr, got %s", tokens[0].Type)
	}

	// Second token should be punctuation [
	if tokens[1].Type != TokenPunctuation || tokens[1].Text != "[" {
		t.Errorf("expected TokenPunctuation '[', got %s %s", tokens[1].Type, tokens[1].Text)
	}
}

func TestLexer_ComparisonOperators_SingleToken(t *testing.T) {
	// Verify comparison operators are tokenized as single tokens, not split
	tests := []struct {
		name   string
		input  string
		tokens int // expected number of tokens (including EOF and whitespace)
	}{
		{"less_equal", "x <= 5", 6},       // x, ws, <=, ws, 5, EOF
		{"greater_equal", "x >= 5", 6},    // x, ws, >=, ws, 5, EOF
		{"equal_equal", "x == 5", 6},      // x, ws, ==, ws, 5, EOF
		{"not_equal", "x != 5", 6},        // x, ws, !=, ws, 5, EOF
		{"not_equal_legacy", "x <> 5", 6}, // x, ws, <>, ws, 5, EOF
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			lex := NewLexer(tc.input)
			tokens := lex.Tokenize()

			if len(tokens) != tc.tokens {
				t.Errorf("expected %d tokens, got %d: %v", tc.tokens, len(tokens), tokens)
			}

			// The operator should be a single token at index 2
			if tokens[2].Type != TokenOperator {
				t.Errorf("expected TokenOperator at index 2, got %s", tokens[2].Type)
			}
		})
	}
}

func TestLexer_BracketString_NestedOneLevelYieldsContent(t *testing.T) {
	// SSL supports one level of bracket nesting: [[a]b] yields [a]b
	input := `[[a]b]`
	lex := NewLexer(input)
	tokens := lex.Tokenize()

	// Should be a single string token (plus EOF)
	if len(tokens) < 2 {
		t.Fatalf("expected at least 2 tokens, got %d", len(tokens))
	}
	if tokens[0].Type != TokenString {
		t.Errorf("expected TokenString, got %s", tokens[0].Type)
	}
	if tokens[0].Text != "[[a]b]" {
		t.Errorf("expected [[a]b], got %q", tokens[0].Text)
	}
}

func TestLexer_BracketString_OneLevelNesting(t *testing.T) {
	// Grammar specifies exactly one level of bracket nesting: [[a]b] yields [a]b
	input := `[[a]b]`
	lex := NewLexer(input)
	tokens := lex.Tokenize()

	if tokens[0].Type != TokenString {
		t.Errorf("expected TokenString, got %s", tokens[0].Type)
	}
	if tokens[0].Text != "[[a]b]" {
		t.Errorf("expected [[a]b], got %q", tokens[0].Text)
	}
}

func TestLexer_ScientificNotation_RequiresDecimal(t *testing.T) {
	// 7e2 should NOT be parsed as scientific notation (no decimal point)
	input := `7e2`
	lex := NewLexer(input)
	tokens := lex.Tokenize()

	if tokens[0].Type != TokenNumber {
		t.Fatalf("expected TokenNumber for first token, got %s", tokens[0].Type)
	}
	if tokens[0].Text != "7" {
		t.Errorf("expected '7' (lexer stops at 'e'), got %q", tokens[0].Text)
	}
	if tokens[1].Type != TokenIdentifier {
		t.Errorf("expected TokenIdentifier for 'e2', got %s", tokens[1].Type)
	}
}

func TestLexer_ScientificNotation_WithDecimalIsValid(t *testing.T) {
	// 7.0e2 should be parsed as a single number token
	input := `7.0e2`
	lex := NewLexer(input)
	tokens := lex.Tokenize()

	if tokens[0].Type != TokenNumber {
		t.Fatalf("expected TokenNumber, got %s", tokens[0].Type)
	}
	if tokens[0].Text != "7.0e2" {
		t.Errorf("expected '7.0e2', got %q", tokens[0].Text)
	}
}

func TestLexer_CodeBlockLiteral(t *testing.T) {
	input := `{|x| x * 2}`
	lex := NewLexer(input)
	tokens := lex.Tokenize()

	if tokens[0].Type != TokenCodeBlock {
		t.Errorf("expected TokenCodeBlock for code block, got %s", tokens[0].Type)
	}
	if tokens[0].Text != "{|x| x * 2}" {
		t.Errorf("expected code block text, got %q", tokens[0].Text)
	}
}

func TestLexer_CodeBlockMultipleParams(t *testing.T) {
	input := `{|a, b| a + b}`
	lex := NewLexer(input)
	tokens := lex.Tokenize()

	if tokens[0].Type != TokenCodeBlock {
		t.Errorf("expected TokenCodeBlock for code block, got %s", tokens[0].Type)
	}
	if tokens[0].Text != "{|a, b| a + b}" {
		t.Errorf("expected code block text, got %q", tokens[0].Text)
	}
}

func TestLexer_RegularBraceNotCodeBlock(t *testing.T) {
	// Regular array literal {1, 2, 3} should NOT be treated as code block
	input := `{1, 2, 3}`
	lex := NewLexer(input)
	tokens := lex.Tokenize()

	// First token should be punctuation {, not a string
	if tokens[0].Type != TokenPunctuation {
		t.Errorf("expected TokenPunctuation for {, got %s", tokens[0].Type)
	}
}

func TestLexer_CodeBlockNestedBraces(t *testing.T) {
	// EBNF: CodeBlockLiteral ::= "{|" IdentifierList "|" Expression "}"
	// Nested braces inside the expression body must be tracked by depth.
	input := `{|x| {x, x} }`
	lex := NewLexer(input)
	tokens := lex.Tokenize()

	if tokens[0].Type != TokenCodeBlock {
		t.Fatalf("expected TokenCodeBlock, got %s", tokens[0].Type)
	}
	if tokens[0].Text != `{|x| {x, x} }` {
		t.Errorf("expected full code block with nested braces, got %q", tokens[0].Text)
	}
	// Should be exactly 2 tokens: the code block and EOF
	nonWS := 0
	for _, tok := range tokens {
		if tok.Type != TokenWhitespace && tok.Type != TokenEOF {
			nonWS++
		}
	}
	if nonWS != 1 {
		t.Errorf("expected 1 non-whitespace token (the code block), got %d", nonWS)
	}
}

func TestLexer_BracketString_DeeperNesting(t *testing.T) {
	// EBNF specifies exactly one level of bracket nesting.
	// [[[x]]] should tokenize as string [[[x]] + punctuation ].
	// The second [ inside the string is at depth 1 (max), so the third [ is
	// literal content. First ] closes nesting, second ] closes the string.
	input := `[[[x]]]`
	lex := NewLexer(input)
	tokens := lex.Tokenize()

	if tokens[0].Type != TokenString {
		t.Fatalf("expected TokenString, got %s", tokens[0].Type)
	}
	if tokens[0].Text != `[[[x]]` {
		t.Errorf("expected [[[x]], got %q", tokens[0].Text)
	}
	// The trailing ] is not part of the string
	if tokens[1].Type != TokenPunctuation || tokens[1].Text != "]" {
		t.Errorf("expected trailing ] as punctuation, got %s %q", tokens[1].Type, tokens[1].Text)
	}
}

func TestLexer_ScientificNotation_IncompleteExponent(t *testing.T) {
	// 1.5e with no digits after 'e' should NOT consume the 'e'.
	// EBNF: Exponent ::= ("e" | "E") ["-"] Digit {Digit}
	// Without a following digit or minus sign, the 'e' is not part of the number.
	input := `1.5e`
	lex := NewLexer(input)
	tokens := lex.Tokenize()

	if tokens[0].Type != TokenNumber {
		t.Fatalf("expected TokenNumber, got %s", tokens[0].Type)
	}
	if tokens[0].Text != "1.5" {
		t.Errorf("expected '1.5' (stops before lone 'e'), got %q", tokens[0].Text)
	}
	if tokens[1].Type != TokenIdentifier {
		t.Errorf("expected TokenIdentifier for 'e', got %s (%q)", tokens[1].Type, tokens[1].Text)
	}
}

func TestLexer_ScientificNotation_ExponentMinusOnly(t *testing.T) {
	// 1.5e- with minus but no digits should NOT consume 'e-'.
	// The condition requires peek(1) to be '-' or digit, but then '-' must be
	// followed by at least one digit. However the lexer currently consumes the
	// minus sign eagerly. This test documents the actual behavior.
	input := `1.5e-`
	lex := NewLexer(input)
	tokens := lex.Tokenize()

	// The lexer sees 'e' followed by '-' and enters the exponent branch,
	// consuming "1.5e-" as a number. This is technically incomplete per EBNF
	// (exponent requires at least one digit after optional minus), but the
	// lexer is lenient here — the diagnostic layer can catch malformed numbers.
	if tokens[0].Type != TokenNumber {
		t.Fatalf("expected TokenNumber, got %s", tokens[0].Type)
	}
}

func TestLexer_ScientificNotation_NoPlusSign(t *testing.T) {
	// EBNF: explicit '+' in exponent is not valid (e.g., 9.0E+1 is invalid).
	// The lexer should stop before '+' and not consume the exponent.
	input := `9.0E+1`
	lex := NewLexer(input)
	tokens := lex.Tokenize()

	if tokens[0].Type != TokenNumber {
		t.Fatalf("expected TokenNumber, got %s", tokens[0].Type)
	}
	if tokens[0].Text != "9.0" {
		t.Errorf("expected '9.0' (rejects E+), got %q", tokens[0].Text)
	}
}

// Issue #83: a number followed by a glued dot-operator must not consume the
// dot (`10.AND.x` is Number(10) Operator(.AND.) Identifier(x)), and a failed
// dot-operator scan must not swallow the following character (`.nB<` used to
// eat the '<' of '<=', corrupting the rest of the line).
func TestLexer_NumberBeforeDotOperator(t *testing.T) {
	type tok struct {
		typ  TokenType
		text string
	}
	cases := []struct {
		name  string
		input string
		want  []tok
	}{
		{
			name:  "glued_upper",
			input: "nCount>=10.AND.bReady",
			want: []tok{
				{TokenIdentifier, "nCount"}, {TokenOperator, ">="}, {TokenNumber, "10"},
				{TokenOperator, ".AND."}, {TokenIdentifier, "bReady"},
			},
		},
		{
			name:  "glued_lower_with_comparison",
			input: "nA>=10.and.nB<=20",
			want: []tok{
				{TokenIdentifier, "nA"}, {TokenOperator, ">="}, {TokenNumber, "10"},
				{TokenOperator, ".and."}, {TokenIdentifier, "nB"},
				{TokenOperator, "<="}, {TokenNumber, "20"},
			},
		},
		{
			name:  "decimal_still_lexes",
			input: "nX:=10.5+1.25e-3",
			want: []tok{
				{TokenIdentifier, "nX"}, {TokenOperator, ":="}, {TokenNumber, "10.5"},
				{TokenOperator, "+"}, {TokenNumber, "1.25e-3"},
			},
		},
		{
			name:  "boolean_after_number",
			input: "bX:=7.and..T.",
			want: []tok{
				{TokenIdentifier, "bX"}, {TokenOperator, ":="}, {TokenNumber, "7"},
				{TokenOperator, ".and."}, {TokenKeyword, ".T."},
			},
		},
	}

	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			var got []tok
			for _, tk := range NewLexer(tc.input).Tokenize() {
				if tk.Type == TokenWhitespace || tk.Type == TokenEOF {
					continue
				}
				got = append(got, tok{tk.Type, tk.Text})
			}
			if len(got) != len(tc.want) {
				t.Fatalf("token count: want %d got %d: %+v", len(tc.want), len(got), got)
			}
			for i := range tc.want {
				if got[i] != tc.want[i] {
					t.Errorf("token %d: want %+v got %+v", i, tc.want[i], got[i])
				}
			}
		})
	}
}

// Issue #83: the dot-operator fallback token must end before a non-alpha
// character rather than consuming it.
func TestLexer_DotOperatorFallbackDoesNotSwallow(t *testing.T) {
	tokens := NewLexer(".nB<=20").Tokenize()
	var texts []string
	for _, tk := range tokens {
		if tk.Type == TokenWhitespace || tk.Type == TokenEOF {
			continue
		}
		texts = append(texts, tk.Text)
	}
	// ".nB" is not a valid dot operator (Unknown is fine) — but "<=" must
	// survive as a single operator token and "20" as a number.
	joined := strings.Join(texts, "|")
	if !strings.Contains(joined, "<=") || !strings.Contains(joined, "20") {
		t.Errorf("expected <= and 20 to survive after failed dot scan, got %v", texts)
	}
}

// Issue #164: :REGION bodies are opaque payload retrieved via GetRegion()
// and must lex as a single raw TokenRegionBody, not as SSL.
func TestLexer_RegionBodyRawText(t *testing.T) {
	input := ":REGION Html;\n<div onclick=\"if(a && b[0] != null) x.go();\">a.b</div>\n:ENDREGION;\n"
	tokens := NewLexer(input).Tokenize()

	var body *Token
	var sawEndRegion bool
	for i := range tokens {
		tk := &tokens[i]
		if tk.Type == TokenRegionBody {
			if body != nil {
				t.Fatalf("expected exactly one region body token, got a second: %+v", tk)
			}
			body = tk
		}
		if tk.Type == TokenKeyword && strings.EqualFold(tk.Text, ":ENDREGION") {
			sawEndRegion = true
		}
	}
	if body == nil {
		t.Fatal("expected a TokenRegionBody token")
	}
	want := "\n<div onclick=\"if(a && b[0] != null) x.go();\">a.b</div>\n"
	if body.Text != want {
		t.Errorf("region body text: want %q got %q", want, body.Text)
	}
	if !sawEndRegion {
		t.Error("expected :ENDREGION to survive as a keyword token")
	}
}

// Issue #164: a mid-line :ENDREGION is body text; only a line-leading one
// (optionally indented) terminates the region.
func TestLexer_RegionBodyMidLineEndRegionIsText(t *testing.T) {
	input := ":REGION T;\nsay :ENDREGION here\n\t:endregion;\n"
	tokens := NewLexer(input).Tokenize()

	var body *Token
	for i := range tokens {
		if tokens[i].Type == TokenRegionBody {
			body = &tokens[i]
		}
	}
	if body == nil {
		t.Fatal("expected a TokenRegionBody token")
	}
	if !strings.Contains(body.Text, "say :ENDREGION here") {
		t.Errorf("mid-line :ENDREGION should stay in the body, got %q", body.Text)
	}
	if strings.Contains(body.Text, ":endregion;") {
		t.Errorf("line-leading :endregion should terminate the body, got %q", body.Text)
	}
}

// Issue #164: an unterminated region consumes to EOF as raw text — the
// unclosed_block diagnostic (not the lexer) reports the missing closer.
func TestLexer_RegionBodyUnterminatedRunsToEOF(t *testing.T) {
	input := ":REGION T;\nraw a.b\nmore\n"
	tokens := NewLexer(input).Tokenize()

	var body *Token
	for i := range tokens {
		if tokens[i].Type == TokenRegionBody {
			body = &tokens[i]
		}
	}
	if body == nil {
		t.Fatal("expected a TokenRegionBody token")
	}
	if body.Text != "\nraw a.b\nmore\n" {
		t.Errorf("unterminated body should run to EOF, got %q", body.Text)
	}
}

// Issue #164: a same-line close (`:REGION X; :ENDREGION;`) yields no body
// token and both keywords lex normally.
func TestLexer_RegionSameLineCloseHasNoBody(t *testing.T) {
	input := ":REGION X; :ENDREGION;\n"
	tokens := NewLexer(input).Tokenize()

	var kws []string
	for _, tk := range tokens {
		if tk.Type == TokenRegionBody {
			t.Fatalf("expected no region body token, got %q", tk.Text)
		}
		if tk.Type == TokenKeyword {
			kws = append(kws, strings.ToUpper(tk.Text))
		}
	}
	joined := strings.Join(kws, "|")
	if joined != ":REGION|:ENDREGION" {
		t.Errorf("expected :REGION and :ENDREGION keywords, got %v", kws)
	}
}

func TestLexer_SpacedCodeBlock(t *testing.T) {
	// Issue #206: stock code writes code blocks with interior spacing —
	// `{ |X| ... }` — which the runtime accepts. Both forms lex as one
	// opaque TokenCodeBlock; a `{` not followed by a bar stays an array
	// literal opener.
	cases := []struct {
		code      string
		wantBlock bool
	}{
		{`AEval( aRows, {|x| x * 2} );`, true},
		{`AEval( aRows, { |X| ArrayCalc(aOut, "MERGE", BuildArray(X)) } );`, true},
		{`AEval( aRows, {  	|x| x} );`, true},
		{`aList := {1, 2, 3};`, false},
		{`aList := { "a", "b" };`, false},
	}
	for _, tc := range cases {
		found := false
		for _, tok := range NewLexer(tc.code).Tokenize() {
			if tok.Type == TokenCodeBlock {
				found = true
				if !strings.HasPrefix(tok.Text, "{") || !strings.HasSuffix(tok.Text, "}") {
					t.Errorf("%q: code block token not brace-delimited: %q", tc.code, tok.Text)
				}
			}
		}
		if found != tc.wantBlock {
			t.Errorf("%q: TokenCodeBlock presence = %v, want %v", tc.code, found, tc.wantBlock)
		}
	}
}
