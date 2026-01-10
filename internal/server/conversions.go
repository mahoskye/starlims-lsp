package server

import (
	"starlims-lsp/internal/providers"

	protocol "github.com/tliron/glsp/protocol_3_16"
)

func toProtocolPosition(pos providers.Position) protocol.Position {
	return protocol.Position{
		Line:      protocol.UInteger(pos.Line),
		Character: protocol.UInteger(pos.Character),
	}
}

func toProtocolRange(r providers.Range) protocol.Range {
	return protocol.Range{
		Start: toProtocolPosition(r.Start),
		End:   toProtocolPosition(r.End),
	}
}

func toProtocolLocation(loc providers.Location) protocol.Location {
	return protocol.Location{
		URI:   loc.URI,
		Range: toProtocolRange(loc.Range),
	}
}

func toProtocolTextEdit(edit providers.TextEdit) protocol.TextEdit {
	return protocol.TextEdit{
		Range:   toProtocolRange(edit.Range),
		NewText: edit.NewText,
	}
}
