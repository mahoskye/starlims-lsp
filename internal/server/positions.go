package server

import (
	"os"
	"unicode/utf8"

	"starlims-lsp/internal/providers"

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

// The output side runs the other way: every position the server sends is
// a provider position (0-based line, 0-based rune column) that must become
// a UTF-16 offset in the text of the document it points into (issue
// #307). The conversion happens once, here, at the protocol boundary; the
// providers keep counting runes.

// utf16Mapper converts provider positions in one document's text into
// LSP positions. A nil *utf16Mapper is the identity, and is what
// newUTF16Mapper returns for text with no character outside the Basic
// Multilingual Plane: there every rune is one UTF-16 unit, so nothing
// needs converting.
type utf16Mapper struct {
	content    string
	lineStarts []int // byte offset of each line start, built on first use
}

// newUTF16Mapper returns the mapper for content, or nil (the identity)
// when content holds no character outside the Basic Multilingual Plane.
func newUTF16Mapper(content string) *utf16Mapper {
	if !hasAstral(content) {
		return nil
	}
	return &utf16Mapper{content: content}
}

// hasAstral reports whether s may hold a character outside the Basic
// Multilingual Plane. Those, and only those, are encoded in four UTF-8
// bytes, led by a byte of 0xF0 or more; an invalid byte in that range
// only costs the slow path, which still counts it correctly.
func hasAstral(s string) bool {
	for i := 0; i < len(s); i++ {
		if s[i] >= 0xF0 {
			return true
		}
	}
	return false
}

// line returns the text of the 0-based line, without its newline; ""
// past the end of the content.
func (m *utf16Mapper) line(line int) string {
	if m.lineStarts == nil {
		m.lineStarts = []int{0}
		for i := 0; i < len(m.content); i++ {
			if m.content[i] == '\n' {
				m.lineStarts = append(m.lineStarts, i+1)
			}
		}
	}
	if line < 0 || line >= len(m.lineStarts) {
		return ""
	}
	start := m.lineStarts[line]
	end := len(m.content)
	if line+1 < len(m.lineStarts) {
		end = m.lineStarts[line+1] - 1
	}
	return m.content[start:end]
}

// character converts a 0-based rune column on the 0-based line into a
// UTF-16 offset. A line with no astral character passes through. A column
// past the end of the line keeps its overshoot, one unit per rune.
func (m *utf16Mapper) character(line, column int) int {
	if m == nil || column <= 0 {
		return column
	}
	text := m.line(line)
	if !hasAstral(text) {
		return column
	}
	units, runes := 0, 0
	for _, r := range text {
		if runes == column {
			return units
		}
		if r >= 0x10000 {
			units += 2
		} else {
			units++
		}
		runes++
	}
	return units + column - runes
}

// position converts a provider position into a protocol position.
func (m *utf16Mapper) position(pos providers.Position) protocol.Position {
	return protocol.Position{
		Line:      protocol.UInteger(pos.Line),
		Character: protocol.UInteger(m.character(pos.Line, pos.Character)),
	}
}

// rng converts a provider range into a protocol range.
func (m *utf16Mapper) rng(r providers.Range) protocol.Range {
	return protocol.Range{Start: m.position(r.Start), End: m.position(r.End)}
}

// textEdits converts provider text edits into protocol text edits.
func (m *utf16Mapper) textEdits(edits []providers.TextEdit) []protocol.TextEdit {
	out := make([]protocol.TextEdit, 0, len(edits))
	for _, edit := range edits {
		out = append(out, protocol.TextEdit{Range: m.rng(edit.Range), NewText: edit.NewText})
	}
	return out
}

// outputPositions converts provider positions that may point into any
// document (cross-file definitions, references and rename edits), with
// one mapper per URI for the life of a request. A document's text is the
// open buffer when the client holds it open, the file on disk otherwise.
type outputPositions struct {
	s       *SSLServer
	mappers map[string]*utf16Mapper
}

// outputPositions returns a converter for one request's results.
func (s *SSLServer) outputPositions() *outputPositions {
	return &outputPositions{s: s, mappers: make(map[string]*utf16Mapper)}
}

// mapper returns the mapper for uri's current text, loading it once.
func (o *outputPositions) mapper(uri string) *utf16Mapper {
	if m, ok := o.mappers[uri]; ok {
		return m
	}
	m := newUTF16Mapper(o.s.documentText(uri))
	o.mappers[uri] = m
	return m
}

// rng converts a range in uri. A range whose columns are both 0 (a
// line-start location, such as a cross-file procedure target or a
// workspace symbol) reads the same in every encoding, so its document is
// never loaded.
func (o *outputPositions) rng(uri string, r providers.Range) protocol.Range {
	if r.Start.Character == 0 && r.End.Character == 0 {
		return (*utf16Mapper)(nil).rng(r)
	}
	return o.mapper(uri).rng(r)
}

// location converts a provider location.
func (o *outputPositions) location(loc providers.Location) protocol.Location {
	return protocol.Location{URI: loc.URI, Range: o.rng(loc.URI, loc.Range)}
}

// locations converts provider locations, mapping nil to nil.
func (o *outputPositions) locations(locations []providers.Location) []protocol.Location {
	if locations == nil {
		return nil
	}
	out := make([]protocol.Location, 0, len(locations))
	for _, loc := range locations {
		out = append(out, o.location(loc))
	}
	return out
}

// workspaceEdit converts per-document provider edits into a WorkspaceEdit,
// each document's edits against that document's own text.
func (o *outputPositions) workspaceEdit(providerChanges map[string][]providers.TextEdit) *protocol.WorkspaceEdit {
	changes := make(map[protocol.DocumentUri][]protocol.TextEdit, len(providerChanges))
	for uri, edits := range providerChanges {
		out := make([]protocol.TextEdit, 0, len(edits))
		for _, edit := range edits {
			out = append(out, protocol.TextEdit{Range: o.rng(uri, edit.Range), NewText: edit.NewText})
		}
		changes[uri] = out
	}
	return &protocol.WorkspaceEdit{Changes: changes}
}

// documentText returns uri's current text: the open buffer, else the file
// on disk ("" when it cannot be read, which converts as the identity).
func (s *SSLServer) documentText(uri string) string {
	if content, ok := s.documents.GetDocument(uri); ok {
		return content
	}
	path := uriToPath(uri)
	if path == "" {
		return ""
	}
	data, err := os.ReadFile(path)
	if err != nil {
		return ""
	}
	return string(data)
}
