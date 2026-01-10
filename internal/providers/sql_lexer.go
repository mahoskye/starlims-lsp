// Package providers implements LSP feature providers for SSL.
package providers

import (
	"strings"
	"unicode"
)

// SQLTokenType represents the type of a SQL token.
type SQLTokenType int

const (
	SQLTokenKeyword SQLTokenType = iota
	SQLTokenIdentifier
	SQLTokenString
	SQLTokenNumber
	SQLTokenOperator
	SQLTokenPunctuation
	SQLTokenWhitespace
	SQLTokenPlaceholder
	SQLTokenUnknown
)

// SQLToken represents a single SQL token.
type SQLToken struct {
	Type   SQLTokenType
	Text   string
	Line   int
	Column int
}

// SQLLexer tokenizes SQL strings.
type SQLLexer struct {
	input  string
	pos    int
	line   int
	column int
}

// NewSQLLexer creates a new SQL lexer.
func NewSQLLexer(input string) *SQLLexer {
	return &SQLLexer{
		input:  input,
		pos:    0,
		line:   1,
		column: 1,
	}
}

// Tokenize returns all tokens from the SQL string.
func (l *SQLLexer) Tokenize() []SQLToken {
	var tokens []SQLToken
	l.pos = 0
	l.line = 1
	l.column = 1

	for l.pos < len(l.input) {
		char := l.input[l.pos]

		switch {
		case unicode.IsSpace(rune(char)):
			tokens = append(tokens, l.readWhitespace())
		case char == '-' && l.peek() == '-':
			tokens = append(tokens, l.readLineComment())
		case char == '/' && l.peek() == '*':
			tokens = append(tokens, l.readBlockComment())
		case char == '\'' || char == '"':
			tokens = append(tokens, l.readString())
		case char == '?':
			tokens = append(tokens, l.readSSLParameter())
		case unicode.IsDigit(rune(char)):
			tokens = append(tokens, l.readNumber())
		case unicode.IsLetter(rune(char)) || char == '_':
			tokens = append(tokens, l.readIdentifierOrKeyword())
		case strings.ContainsRune("(),;.", rune(char)):
			tokens = append(tokens, l.readPunctuation())
		case strings.ContainsRune("+-*/%=!<>|", rune(char)):
			tokens = append(tokens, l.readOperator())
		default:
			tokens = append(tokens, l.readUnknown())
		}
	}

	return tokens
}

func (l *SQLLexer) peek() byte {
	if l.pos+1 >= len(l.input) {
		return 0
	}
	return l.input[l.pos+1]
}

func (l *SQLLexer) advance() {
	if l.pos < len(l.input) {
		if l.input[l.pos] == '\n' {
			l.line++
			l.column = 1
		} else {
			l.column++
		}
		l.pos++
	}
}

func (l *SQLLexer) readWhitespace() SQLToken {
	line := l.line
	col := l.column
	var text strings.Builder

	for l.pos < len(l.input) && unicode.IsSpace(rune(l.input[l.pos])) {
		text.WriteByte(l.input[l.pos])
		l.advance()
	}

	return SQLToken{Type: SQLTokenWhitespace, Text: text.String(), Line: line, Column: col}
}

func (l *SQLLexer) readLineComment() SQLToken {
	line := l.line
	col := l.column
	var text strings.Builder

	// Read --
	text.WriteByte(l.input[l.pos])
	l.advance()
	text.WriteByte(l.input[l.pos])
	l.advance()

	// Read until newline
	for l.pos < len(l.input) && l.input[l.pos] != '\n' {
		text.WriteByte(l.input[l.pos])
		l.advance()
	}

	// Treat comments as whitespace for formatting
	return SQLToken{Type: SQLTokenWhitespace, Text: text.String(), Line: line, Column: col}
}

func (l *SQLLexer) readBlockComment() SQLToken {
	line := l.line
	col := l.column
	var text strings.Builder

	// Read /*
	text.WriteByte(l.input[l.pos])
	l.advance()
	text.WriteByte(l.input[l.pos])
	l.advance()

	// Read until */
	for l.pos < len(l.input) {
		char := l.input[l.pos]
		text.WriteByte(char)
		l.advance()
		if char == '*' && l.pos < len(l.input) && l.input[l.pos] == '/' {
			text.WriteByte(l.input[l.pos])
			l.advance()
			break
		}
	}

	return SQLToken{Type: SQLTokenWhitespace, Text: text.String(), Line: line, Column: col}
}

func (l *SQLLexer) readString() SQLToken {
	line := l.line
	col := l.column
	quote := l.input[l.pos]
	var text strings.Builder

	text.WriteByte(quote)
	l.advance()

	for l.pos < len(l.input) {
		char := l.input[l.pos]
		text.WriteByte(char)
		l.advance()

		if char == quote {
			// Check for escaped quote (doubled)
			if l.pos < len(l.input) && l.input[l.pos] == quote {
				text.WriteByte(l.input[l.pos])
				l.advance()
			} else {
				break
			}
		}
	}

	return SQLToken{Type: SQLTokenString, Text: text.String(), Line: line, Column: col}
}

func (l *SQLLexer) readNumber() SQLToken {
	line := l.line
	col := l.column
	var text strings.Builder

	for l.pos < len(l.input) && (unicode.IsDigit(rune(l.input[l.pos])) || l.input[l.pos] == '.') {
		text.WriteByte(l.input[l.pos])
		l.advance()
	}

	return SQLToken{Type: SQLTokenNumber, Text: text.String(), Line: line, Column: col}
}

func (l *SQLLexer) readIdentifierOrKeyword() SQLToken {
	line := l.line
	col := l.column
	var text strings.Builder

	for l.pos < len(l.input) {
		char := l.input[l.pos]
		if unicode.IsLetter(rune(char)) || unicode.IsDigit(rune(char)) || char == '_' {
			text.WriteByte(char)
			l.advance()
		} else {
			break
		}
	}

	tokenText := text.String()
	upper := strings.ToUpper(tokenText)

	if SQLKeywords[upper] {
		return SQLToken{Type: SQLTokenKeyword, Text: tokenText, Line: line, Column: col}
	}

	return SQLToken{Type: SQLTokenIdentifier, Text: tokenText, Line: line, Column: col}
}

func (l *SQLLexer) readPunctuation() SQLToken {
	line := l.line
	col := l.column
	text := string(l.input[l.pos])
	l.advance()
	return SQLToken{Type: SQLTokenPunctuation, Text: text, Line: line, Column: col}
}

func (l *SQLLexer) readOperator() SQLToken {
	line := l.line
	col := l.column
	var text strings.Builder

	text.WriteByte(l.input[l.pos])
	first := l.input[l.pos]
	l.advance()

	// Check for multi-character operators: <=, >=, <>, !=, ||
	if l.pos < len(l.input) {
		next := l.input[l.pos]
		if (first == '<' || first == '>' || first == '!') && next == '=' {
			text.WriteByte(next)
			l.advance()
		} else if first == '<' && next == '>' {
			text.WriteByte(next)
			l.advance()
		} else if first == '|' && next == '|' {
			text.WriteByte(next)
			l.advance()
		}
	}

	return SQLToken{Type: SQLTokenOperator, Text: text.String(), Line: line, Column: col}
}

func (l *SQLLexer) readSSLParameter() SQLToken {
	line := l.line
	col := l.column
	var text strings.Builder

	text.WriteByte('?')
	l.advance()

	// Look ahead to see if this is an SSL parameter ?varName?
	tempPos := l.pos
	foundClosing := false
	parenDepth := 0

	for tempPos < len(l.input) {
		c := l.input[tempPos]

		if c == '(' {
			parenDepth++
		} else if c == ')' {
			if parenDepth > 0 {
				parenDepth--
			}
		}

		if c == '?' && parenDepth == 0 {
			foundClosing = true
			break
		}

		if parenDepth == 0 {
			if unicode.IsSpace(rune(c)) || strings.ContainsRune(",;+-*/%=!<>", rune(c)) {
				break
			}
		}
		tempPos++
	}

	if foundClosing {
		// Consume SSL parameter until closing ?
		for l.pos < len(l.input) && l.input[l.pos] != '?' {
			text.WriteByte(l.input[l.pos])
			l.advance()
		}
		if l.pos < len(l.input) && l.input[l.pos] == '?' {
			text.WriteByte('?')
			l.advance()
		}
	}

	return SQLToken{Type: SQLTokenPlaceholder, Text: text.String(), Line: line, Column: col}
}

func (l *SQLLexer) readUnknown() SQLToken {
	line := l.line
	col := l.column
	text := string(l.input[l.pos])
	l.advance()
	return SQLToken{Type: SQLTokenUnknown, Text: text, Line: line, Column: col}
}
