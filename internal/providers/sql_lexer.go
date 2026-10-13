// Package providers implements LSP feature providers for SSL.
package providers

import (
	"strings"
	"unicode"
	"unicode/utf8"
)

// SQLTokenType represents the type of a SQL token.
type SQLTokenType int

const (
	SQLTokenKeyword SQLTokenType = iota
	SQLTokenFunction
	SQLTokenIdentifier
	SQLTokenString
	SQLTokenNumber
	SQLTokenOperator
	SQLTokenPunctuation
	SQLTokenWhitespace
	SQLTokenHint    // optimizer hints /*+ ... */
	SQLTokenComment // regular SQL comments (-- and /* */)
	SQLTokenPlaceholder
	SQLTokenUnknown
)

// SQLToken represents a single SQL token.
type SQLToken struct {
	Type   SQLTokenType
	Text   string
	Line   int
	Column int
	// Unterminated is set on a string, quoted identifier or block comment
	// that ran to the end of the input without its closing delimiter.
	Unterminated bool
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
		// Classify by the whole UTF-8 sequence, not its first byte: a
		// lead or continuation byte read as a Latin-1 rune misclassified
		// non-ASCII text (U+00A0 and U+0085 are spaces, U+00C3 a letter)
		// and readUnknown re-encoded single bytes as runes, corrupting
		// every non-ASCII character in reformatted SQL (issue #256, found
		// by FuzzFormat). Invalid bytes decode to utf8.RuneError, width 1,
		// and pass through readUnknown unchanged.
		r, _ := l.current()

		switch {
		case unicode.IsSpace(r):
			tokens = append(tokens, l.readWhitespace())
		case char == '-' && l.peek() == '-':
			tokens = append(tokens, l.readLineComment())
		case char == '/' && l.peek() == '*':
			tokens = append(tokens, l.readBlockComment())
		case char == '\'' || char == '"':
			tokens = append(tokens, l.readString())
		case l.atPrefixedString():
			tokens = append(tokens, l.readPrefixedString())
		case char == '?':
			tokens = append(tokens, l.readSSLParameter())
		case unicode.IsDigit(r):
			tokens = append(tokens, l.readNumber())
		case isSQLIdentRune(r) && !unicode.IsDigit(r):
			tokens = append(tokens, l.readIdentifierOrKeyword())
		case strings.ContainsRune("(),;.{}", rune(char)):
			tokens = append(tokens, l.readPunctuation())
		case strings.ContainsRune("+-*/%=!<>|", rune(char)):
			tokens = append(tokens, l.readOperator())
		default:
			tokens = append(tokens, l.readUnknown())
		}
	}

	return tokens
}

// current decodes the rune at the cursor and its width in bytes.
func (l *SQLLexer) current() (rune, int) {
	return utf8.DecodeRuneInString(l.input[l.pos:])
}

// consumeRune appends the raw bytes of the rune at the cursor to text and
// advances past it, so invalid bytes are carried through verbatim.
func (l *SQLLexer) consumeRune(text *strings.Builder, size int) {
	text.WriteString(l.input[l.pos : l.pos+size])
	for i := 0; i < size; i++ {
		l.advance()
	}
}

// isSQLIdentRune reports whether r can continue a SQL identifier.
func isSQLIdentRune(r rune) bool {
	return r != utf8.RuneError && (unicode.IsLetter(r) || unicode.IsDigit(r) || r == '_')
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

	for l.pos < len(l.input) {
		r, size := l.current()
		if !unicode.IsSpace(r) {
			break
		}
		l.consumeRune(&text, size)
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

	return SQLToken{Type: SQLTokenComment, Text: text.String(), Line: line, Column: col}
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

	// Check for optimizer hint: /*+ ... */
	isHint := l.pos < len(l.input) && l.input[l.pos] == '+'

	// Read until */
	closed := false
	for l.pos < len(l.input) {
		char := l.input[l.pos]
		text.WriteByte(char)
		l.advance()
		if char == '*' && l.pos < len(l.input) && l.input[l.pos] == '/' {
			text.WriteByte(l.input[l.pos])
			l.advance()
			closed = true
			break
		}
	}

	if isHint {
		return SQLToken{Type: SQLTokenHint, Text: text.String(), Line: line, Column: col, Unterminated: !closed}
	}
	return SQLToken{Type: SQLTokenComment, Text: text.String(), Line: line, Column: col, Unterminated: !closed}
}

func (l *SQLLexer) readString() SQLToken {
	line := l.line
	col := l.column
	quote := l.input[l.pos]
	var text strings.Builder

	text.WriteByte(quote)
	l.advance()

	closed := false
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
				closed = true
				break
			}
		}
	}

	return SQLToken{Type: SQLTokenString, Text: text.String(), Line: line, Column: col, Unterminated: !closed}
}

func (l *SQLLexer) readNumber() SQLToken {
	line := l.line
	col := l.column
	var text strings.Builder

	for l.pos < len(l.input) {
		r, size := l.current()
		if !unicode.IsDigit(r) && r != '.' {
			break
		}
		// A '.' belongs to the number only before a digit (`1.5`) or at
		// the literal's end (`1.` before whitespace or end of input).
		// Anything else glued to it is a separate token: the formatter
		// glues both sides of a '.', so `0 .A` became `0.A`, and reading
		// that back as the number `0.` followed by `A` re-spaced it to
		// `0. A` on the next pass (issue #303, found by FuzzFormat).
		if r == '.' {
			next, nextSize := utf8.DecodeRuneInString(l.input[l.pos+size:])
			if nextSize > 0 && !unicode.IsDigit(next) && !unicode.IsSpace(next) {
				break
			}
		}
		l.consumeRune(&text, size)
	}

	// An exponent — `1.5e-3`, `2E+5`, `1e10` — belongs to the number. The
	// marker is taken only when digits follow it (after an optional sign),
	// so `3e` stays a number followed by an identifier (issue #258).
	if l.pos < len(l.input) && (l.input[l.pos] == 'e' || l.input[l.pos] == 'E') {
		j := l.pos + 1
		if j < len(l.input) && (l.input[j] == '+' || l.input[j] == '-') {
			j++
		}
		if j < len(l.input) && isASCIIDigit(l.input[j]) {
			for l.pos < j || (l.pos < len(l.input) && isASCIIDigit(l.input[l.pos])) {
				text.WriteByte(l.input[l.pos])
				l.advance()
			}
		}
	}

	return SQLToken{Type: SQLTokenNumber, Text: text.String(), Line: line, Column: col}
}

func isASCIIDigit(c byte) bool {
	return c >= '0' && c <= '9'
}

// atPrefixedString reports whether the read position starts a prefixed
// string literal: SQL Server's national-character N'...', or Oracle's
// alternative quoting q'[...]' and its national form nq'[...]' (any case).
// The prefix must touch the quote; `N 'x'` is an identifier and a string.
func (l *SQLLexer) atPrefixedString() bool {
	rest := l.input[l.pos:]
	isN := func(c byte) bool { return c == 'N' || c == 'n' }
	isQ := func(c byte) bool { return c == 'Q' || c == 'q' }
	switch {
	case len(rest) >= 2 && isN(rest[0]) && rest[1] == '\'':
		return true
	case len(rest) >= 3 && isQ(rest[0]) && rest[1] == '\'':
		return true
	case len(rest) >= 4 && isN(rest[0]) && isQ(rest[1]) && rest[2] == '\'':
		return true
	}
	return false
}

// readPrefixedString lexes a literal atPrefixedString recognized as one
// string token, prefix included, so the formatter never separates the
// prefix from its quote (issue #258). An Oracle q-quote ends at its
// closing delimiter followed by a quote — `[`, `{`, `(`, `<` close with
// their mirror, any other delimiter with itself — and may contain bare
// quotes.
func (l *SQLLexer) readPrefixedString() SQLToken {
	line := l.line
	col := l.column
	var text strings.Builder

	if c := l.input[l.pos]; c == 'N' || c == 'n' {
		text.WriteByte(c)
		l.advance()
	}
	if l.input[l.pos] == '\'' {
		str := l.readString()
		text.WriteString(str.Text)
		return SQLToken{Type: SQLTokenString, Text: text.String(), Line: line, Column: col, Unterminated: str.Unterminated}
	}

	// q'<delim> ... <close>'
	text.WriteString(l.input[l.pos : l.pos+2])
	l.advance()
	l.advance()
	closed := false
	if l.pos < len(l.input) {
		open, size := l.current()
		closer := open
		switch open {
		case '[':
			closer = ']'
		case '{':
			closer = '}'
		case '(':
			closer = ')'
		case '<':
			closer = '>'
		}
		l.consumeRune(&text, size)
		for l.pos < len(l.input) {
			r, size := l.current()
			l.consumeRune(&text, size)
			if r == closer && l.pos < len(l.input) && l.input[l.pos] == '\'' {
				text.WriteByte('\'')
				l.advance()
				closed = true
				break
			}
		}
	}
	return SQLToken{Type: SQLTokenString, Text: text.String(), Line: line, Column: col, Unterminated: !closed}
}

func (l *SQLLexer) readIdentifierOrKeyword() SQLToken {
	line := l.line
	col := l.column
	var text strings.Builder

	for l.pos < len(l.input) {
		r, size := l.current()
		if !isSQLIdentRune(r) {
			break
		}
		l.consumeRune(&text, size)
	}

	tokenText := text.String()
	upper := strings.ToUpper(tokenText)

	if SQLKeywords[upper] {
		return SQLToken{Type: SQLTokenKeyword, Text: tokenText, Line: line, Column: col}
	}

	if SQLBuiltinFunctions[upper] {
		return SQLToken{Type: SQLTokenFunction, Text: tokenText, Line: line, Column: col}
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

	// A placeholder opens only on a name, a quoted expression, or the
	// empty `??`. A lone `?` the formatter glued to a following unknown
	// character or '.' must not pair with a later `?` on the next pass:
	// `?# ?x?` formatted to `?#?x?`, which then read `?#?` as one
	// placeholder and re-spaced the rest (issue #303, found by FuzzFormat).
	if first, _ := utf8.DecodeRuneInString(l.input[l.pos:]); !isSQLIdentRune(first) && first != '\'' && first != '?' {
		tempPos = len(l.input)
	}

	for tempPos < len(l.input) {
		c := l.input[tempPos]

		// A quoted span inside the placeholder is opaque — SSL substitutes
		// expressions like ?'<<username>>'? whole, and breaking on the
		// quote's characters fragmented the placeholder so the formatter
		// respaced its interior (issue #217).
		if c == '\'' {
			tempPos++
			for tempPos < len(l.input) && l.input[tempPos] != '\'' {
				tempPos++
			}
			if tempPos < len(l.input) {
				tempPos++
			}
			continue
		}

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
			if (c < utf8.RuneSelf && unicode.IsSpace(rune(c))) || strings.ContainsRune(",;+-*/%=!<>", rune(c)) {
				break
			}
		}
		tempPos++
	}

	if foundClosing {
		// Consume SSL parameter until closing ? — quoted spans stay opaque
		// here too, or a ? inside quotes would end the placeholder early.
		for l.pos < len(l.input) && l.input[l.pos] != '?' {
			if l.input[l.pos] == '\'' {
				text.WriteByte(l.input[l.pos])
				l.advance()
				for l.pos < len(l.input) && l.input[l.pos] != '\'' {
					text.WriteByte(l.input[l.pos])
					l.advance()
				}
				if l.pos < len(l.input) {
					text.WriteByte(l.input[l.pos])
					l.advance()
				}
				continue
			}
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
	var text strings.Builder
	_, size := l.current()
	l.consumeRune(&text, size)
	return SQLToken{Type: SQLTokenUnknown, Text: text.String(), Line: line, Column: col}
}
