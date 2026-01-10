// Package lexer provides tokenization for STARLIMS Scripting Language (SSL).
package lexer

import (
	"strings"
	"unicode"

	"starlims-lsp/internal/constants"
)

// TokenType represents the type of a token.
type TokenType int

const (
	TokenWhitespace TokenType = iota
	TokenComment
	TokenString
	TokenNumber
	TokenKeyword
	TokenIdentifier
	TokenOperator
	TokenPunctuation
	TokenUnknown
	TokenEOF
)

// String returns the string representation of a TokenType.
func (t TokenType) String() string {
	switch t {
	case TokenWhitespace:
		return "Whitespace"
	case TokenComment:
		return "Comment"
	case TokenString:
		return "String"
	case TokenNumber:
		return "Number"
	case TokenKeyword:
		return "Keyword"
	case TokenIdentifier:
		return "Identifier"
	case TokenOperator:
		return "Operator"
	case TokenPunctuation:
		return "Punctuation"
	case TokenUnknown:
		return "Unknown"
	case TokenEOF:
		return "EOF"
	default:
		return "Unknown"
	}
}

// Token represents a lexical token.
type Token struct {
	Type   TokenType
	Text   string
	Line   int
	Column int
	Offset int
}

// Lexer tokenizes SSL source code.
type Lexer struct {
	input  []rune
	pos    int
	line   int
	column int
}

// NewLexer creates a new Lexer for the given input.
func NewLexer(input string) *Lexer {
	return &Lexer{
		input:  []rune(input),
		pos:    0,
		line:   1,
		column: 1,
	}
}

// Tokenize returns all tokens from the input.
func (l *Lexer) Tokenize() []Token {
	var tokens []Token
	l.pos = 0
	l.line = 1
	l.column = 1

	for l.pos < len(l.input) {
		char := l.input[l.pos]

		switch {
		case l.isWhitespace(char):
			tokens = append(tokens, l.readWhitespace())

		case char == '/' && l.peek(1) == '*':
			tokens = append(tokens, l.readBlockComment())

		case char == ';':
			tokens = append(tokens, l.readPunctuation())

		case char == '.':
			if l.isDigit(l.peek(1)) {
				tokens = append(tokens, l.readNumber())
			} else {
				tokens = append(tokens, l.readDotOperatorOrBoolean())
			}

		case char == ':' && l.isKeywordStart(tokens):
			tokens = append(tokens, l.readKeyword())

		case char == ':' && l.peek(1) == '=':
			tokens = append(tokens, l.readAssignmentOperator())

		case char == ':':
			tokens = append(tokens, l.readPunctuation())

		case char == '"' || char == '\'':
			tokens = append(tokens, l.readString())

		case char == '[':
			if l.isArrayAccessContext(tokens) {
				tokens = append(tokens, l.readPunctuation())
			} else {
				tokens = append(tokens, l.readString())
			}

		case l.isDigit(char):
			tokens = append(tokens, l.readNumber())

		case l.isIdentifierStart(char):
			tokens = append(tokens, l.readIdentifier())

		case l.isOperatorChar(char):
			tokens = append(tokens, l.readOperator())

		case l.isPunctuation(char):
			tokens = append(tokens, l.readPunctuation())

		default:
			tokens = append(tokens, l.readUnknown())
		}
	}

	tokens = append(tokens, Token{
		Type:   TokenEOF,
		Text:   "",
		Line:   l.line,
		Column: l.column,
		Offset: l.pos,
	})

	return tokens
}

func (l *Lexer) peek(offset int) rune {
	if l.pos+offset >= len(l.input) {
		return 0
	}
	return l.input[l.pos+offset]
}

func (l *Lexer) isArrayAccessContext(tokens []Token) bool {
	for i := len(tokens) - 1; i >= 0; i-- {
		t := tokens[i]
		if t.Type == TokenWhitespace || t.Type == TokenComment {
			continue
		}
		if t.Type == TokenIdentifier || t.Type == TokenNumber || t.Type == TokenString || t.Type == TokenKeyword {
			return true
		}
		if t.Type == TokenPunctuation && (t.Text == ")" || t.Text == "]") {
			return true
		}
		return false
	}
	return false
}

func (l *Lexer) isWhitespace(char rune) bool {
	return unicode.IsSpace(char)
}

func (l *Lexer) isDigit(char rune) bool {
	return char >= '0' && char <= '9'
}

func (l *Lexer) isAlpha(char rune) bool {
	return (char >= 'a' && char <= 'z') || (char >= 'A' && char <= 'Z') || char == '_'
}

func (l *Lexer) isIdentifierStart(char rune) bool {
	return l.isAlpha(char)
}

func (l *Lexer) isIdentifierPart(char rune) bool {
	return l.isAlpha(char) || l.isDigit(char)
}

func (l *Lexer) isOperatorChar(char rune) bool {
	return strings.ContainsRune("+-*/^%=!<>#$", char)
}

func (l *Lexer) isPunctuation(char rune) bool {
	return strings.ContainsRune("(),{};]", char)
}

func (l *Lexer) readWhitespace() Token {
	start := l.pos
	line := l.line
	col := l.column
	var text strings.Builder

	for l.pos < len(l.input) && l.isWhitespace(l.input[l.pos]) {
		text.WriteRune(l.input[l.pos])
		l.advance()
	}

	return Token{Type: TokenWhitespace, Text: text.String(), Line: line, Column: col, Offset: start}
}

func (l *Lexer) readBlockComment() Token {
	// SSL comments: /* ... ;
	start := l.pos
	line := l.line
	col := l.column
	var text strings.Builder

	// Consume /*
	text.WriteRune(l.input[l.pos])
	l.advance()
	text.WriteRune(l.input[l.pos])
	l.advance()

	for l.pos < len(l.input) {
		char := l.input[l.pos]
		text.WriteRune(char)
		l.advance()

		if char == ';' {
			break
		}
	}

	return Token{Type: TokenComment, Text: text.String(), Line: line, Column: col, Offset: start}
}

func (l *Lexer) readString() Token {
	start := l.pos
	line := l.line
	col := l.column
	quote := l.input[l.pos]
	var text strings.Builder

	text.WriteRune(quote)
	l.advance()

	closeQuote := quote
	if quote == '[' {
		closeQuote = ']'
	}
	isEscaped := false

	for l.pos < len(l.input) {
		char := l.input[l.pos]
		text.WriteRune(char)
		l.advance()

		if isEscaped {
			isEscaped = false
			continue
		}

		if char == '\\' && quote != '[' {
			isEscaped = true
			continue
		}

		if char == closeQuote {
			break
		}
	}

	return Token{Type: TokenString, Text: text.String(), Line: line, Column: col, Offset: start}
}

func (l *Lexer) readNumber() Token {
	start := l.pos
	line := l.line
	col := l.column
	var text strings.Builder

	for l.pos < len(l.input) {
		char := l.input[l.pos]
		if l.isDigit(char) || char == '.' {
			text.WriteRune(char)
			l.advance()
		} else if (char == 'e' || char == 'E') && (l.peek(1) == '+' || l.peek(1) == '-' || l.isDigit(l.peek(1))) {
			text.WriteRune(char)
			l.advance()
			if l.pos < len(l.input) && (l.input[l.pos] == '+' || l.input[l.pos] == '-') {
				text.WriteRune(l.input[l.pos])
				l.advance()
			}
		} else {
			break
		}
	}

	return Token{Type: TokenNumber, Text: text.String(), Line: line, Column: col, Offset: start}
}

func (l *Lexer) isKeywordStart(tokens []Token) bool {
	if len(tokens) > 0 {
		last := tokens[len(tokens)-1]
		if last.Type == TokenIdentifier || last.Text == ")" || last.Text == "]" {
			return false
		}
	}
	return l.isAlpha(l.peek(1))
}

func (l *Lexer) readKeyword() Token {
	start := l.pos
	line := l.line
	col := l.column
	var text strings.Builder

	// Consume :
	text.WriteRune(l.input[l.pos])
	l.advance()

	for l.pos < len(l.input) && l.isIdentifierPart(l.input[l.pos]) {
		text.WriteRune(l.input[l.pos])
		l.advance()
	}

	return Token{Type: TokenKeyword, Text: text.String(), Line: line, Column: col, Offset: start}
}

func (l *Lexer) readIdentifier() Token {
	start := l.pos
	line := l.line
	col := l.column
	var text strings.Builder

	for l.pos < len(l.input) && l.isIdentifierPart(l.input[l.pos]) {
		text.WriteRune(l.input[l.pos])
		l.advance()
	}

	str := text.String()
	upper := strings.ToUpper(str)
	if constants.IsKeyword(upper) {
		return Token{Type: TokenKeyword, Text: str, Line: line, Column: col, Offset: start}
	}

	return Token{Type: TokenIdentifier, Text: str, Line: line, Column: col, Offset: start}
}

func (l *Lexer) readDotOperatorOrBoolean() Token {
	start := l.pos
	line := l.line
	col := l.column
	var text strings.Builder

	// Consume first dot
	text.WriteRune(l.input[l.pos])
	l.advance()

	for l.pos < len(l.input) {
		char := l.input[l.pos]
		text.WriteRune(char)
		l.advance()
		if char == '.' {
			break
		}
		if !l.isAlpha(char) {
			break
		}
	}

	str := text.String()
	upper := strings.ToUpper(str)
	if constants.IsSSLLiteral(upper) {
		return Token{Type: TokenKeyword, Text: str, Line: line, Column: col, Offset: start}
	}
	if constants.IsSSLOperator(upper) {
		return Token{Type: TokenOperator, Text: str, Line: line, Column: col, Offset: start}
	}

	return Token{Type: TokenUnknown, Text: str, Line: line, Column: col, Offset: start}
}

func (l *Lexer) readOperator() Token {
	start := l.pos
	line := l.line
	col := l.column
	var text strings.Builder

	char := l.input[l.pos]
	text.WriteRune(char)
	l.advance()

	if l.pos < len(l.input) {
		next := l.input[l.pos]
		twoChar := string(char) + string(next)
		if constants.IsSSLCompoundOperator(twoChar) {
			text.WriteRune(next)
			l.advance()
		}
	}

	return Token{Type: TokenOperator, Text: text.String(), Line: line, Column: col, Offset: start}
}

func (l *Lexer) readAssignmentOperator() Token {
	start := l.pos
	line := l.line
	col := l.column
	var text strings.Builder

	// Consume : and =
	text.WriteRune(l.input[l.pos])
	l.advance()
	text.WriteRune(l.input[l.pos])
	l.advance()

	return Token{Type: TokenOperator, Text: text.String(), Line: line, Column: col, Offset: start}
}

func (l *Lexer) readPunctuation() Token {
	start := l.pos
	line := l.line
	col := l.column
	text := string(l.input[l.pos])
	l.advance()
	return Token{Type: TokenPunctuation, Text: text, Line: line, Column: col, Offset: start}
}

func (l *Lexer) readUnknown() Token {
	start := l.pos
	line := l.line
	col := l.column
	text := string(l.input[l.pos])
	l.advance()
	return Token{Type: TokenUnknown, Text: text, Line: line, Column: col, Offset: start}
}

func (l *Lexer) advance() {
	if l.pos < len(l.input) && l.input[l.pos] == '\n' {
		l.line++
		l.column = 1
	} else {
		l.column++
	}
	l.pos++
}

// GetTokenAtPosition returns the token at a specific position.
func GetTokenAtPosition(tokens []Token, line, column int) *Token {
	for i := range tokens {
		token := &tokens[i]
		if token.Line == line {
			tokenEnd := token.Column + len(token.Text)
			if column >= token.Column && column <= tokenEnd {
				return token
			}
		}
	}
	return nil
}

// GetWordAtPosition returns the word at a specific position.
func GetWordAtPosition(text string, line, column int) string {
	lines := strings.Split(text, "\n")
	if line < 1 || line > len(lines) {
		return ""
	}

	lineText := lines[line-1]
	if column < 1 || column > len(lineText)+1 {
		return ""
	}

	runes := []rune(lineText)

	// Find word boundaries
	start := column - 1
	end := column - 1

	isWordChar := func(r rune) bool {
		return (r >= 'a' && r <= 'z') || (r >= 'A' && r <= 'Z') || (r >= '0' && r <= '9') || r == '_'
	}

	for start > 0 && isWordChar(runes[start-1]) {
		start--
	}
	for end < len(runes) && isWordChar(runes[end]) {
		end++
	}

	if start == end {
		return ""
	}
	return string(runes[start:end])
}
