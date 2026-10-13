package server

import (
	"unicode/utf8"

	protocol "github.com/tliron/glsp/protocol_3_16"
)

// LSP positions count characters in UTF-16 code units (the only encoding
// an LSP 3.16 client is guaranteed to speak, and the one this server
// advertises). The lexer, parser, and providers count runes. Every
// position a client sends is converted here before it reaches a provider,
// so a character outside the Basic Multilingual Plane (an emoji: two
// UTF-16 units, one rune) cannot shift a cursor (issue #258). Incremental
// edits are converted the same way by applyChange (positionOffset).

// positionEncodingUTF16 is the LSP 3.17 PositionEncodingKind the server
// advertises.
const positionEncodingUTF16 = "utf-16"

// toInternalPosition converts an LSP position in content into the
// 1-based line and 1-based rune column the providers expect. A character
// past the end of its line clamps to the line end.
func toInternalPosition(content string, pos protocol.Position) (line, column int) {
	lineStart := positionOffset(content, int(pos.Line), 0)
	offset := positionOffset(content, int(pos.Line), int(pos.Character))
	return int(pos.Line) + 1, utf8.RuneCountInString(content[lineStart:offset]) + 1
}

// position converts an LSP position in the open document uri into the
// providers' 1-based line and rune column. A document the server does not
// hold falls back to treating the UTF-16 offset as a rune index.
func (s *SSLServer) position(uri string, pos protocol.Position) (line, column int) {
	content, ok := s.documents.GetDocument(uri)
	if !ok {
		return int(pos.Line) + 1, int(pos.Character) + 1
	}
	return toInternalPosition(content, pos)
}

// runePosition returns pos with its Character converted from a UTF-16
// offset to a 0-based rune index in the open document uri.
func (s *SSLServer) runePosition(uri string, pos protocol.Position) protocol.Position {
	_, column := s.position(uri, pos)
	return protocol.Position{Line: pos.Line, Character: protocol.UInteger(column - 1)}
}
