// Package server provides LSP server implementation.
// This file defines LSP 3.17 inlay hint types not available in glsp v0.2.2.
package server

// InlayHintKind defines the kind of an inlay hint.
type InlayHintKind uint32

const (
	// InlayHintKindType is for type annotations.
	InlayHintKindType InlayHintKind = 1
	// InlayHintKindParameter is for parameter hints.
	InlayHintKindParameter InlayHintKind = 2
)

// InlayHintParams represents the parameters for a textDocument/inlayHint request.
type InlayHintParams struct {
	// TextDocument identifies the document for which inlay hints are requested.
	TextDocument TextDocumentIdentifier `json:"textDocument"`
	// Range is the visible range for which inlay hints should be computed.
	Range InlayHintRange `json:"range"`
}

// TextDocumentIdentifier identifies a text document.
type TextDocumentIdentifier struct {
	// URI is the text document's URI.
	URI string `json:"uri"`
}

// InlayHintRange represents a range in a document.
type InlayHintRange struct {
	// Start is the range's start position.
	Start InlayHintPosition `json:"start"`
	// End is the range's end position.
	End InlayHintPosition `json:"end"`
}

// InlayHintPosition represents a position in a document.
type InlayHintPosition struct {
	// Line is the zero-based line value.
	Line uint32 `json:"line"`
	// Character is the zero-based character value.
	Character uint32 `json:"character"`
}

// InlayHint represents an inlay hint to display.
type InlayHint struct {
	// Position is the position where the hint should be displayed.
	Position InlayHintPosition `json:"position"`
	// Label is the hint's label.
	Label string `json:"label"`
	// Kind is the kind of this hint (optional).
	Kind *InlayHintKind `json:"kind,omitempty"`
	// PaddingLeft adds padding before the hint.
	PaddingLeft bool `json:"paddingLeft,omitempty"`
	// PaddingRight adds padding after the hint.
	PaddingRight bool `json:"paddingRight,omitempty"`
}

// MethodTextDocumentInlayHint is the LSP method name for inlay hints.
const MethodTextDocumentInlayHint = "textDocument/inlayHint"
