// Package server provides LSP server implementation.
// This file implements a wrapper handler to support LSP 3.17 methods.
package server

import (
	"encoding/json"

	"github.com/tliron/glsp"
	protocol "github.com/tliron/glsp/protocol_3_16"
)

// WrapperHandler wraps the standard protocol.Handler to add LSP 3.17 support.
// It intercepts methods not supported by glsp and handles them directly.
type WrapperHandler struct {
	inner  *protocol.Handler
	server *SSLServer
}

// NewWrapperHandler creates a new wrapper handler.
func NewWrapperHandler(inner *protocol.Handler, server *SSLServer) *WrapperHandler {
	return &WrapperHandler{
		inner:  inner,
		server: server,
	}
}

// Handle implements the glsp.Handler interface.
// It intercepts LSP 3.17 methods and delegates others to the wrapped handler.
func (w *WrapperHandler) Handle(context *glsp.Context) (r any, validMethod bool, validParams bool, err error) {
	// Intercept inlayHint requests (LSP 3.17)
	if context.Method == MethodTextDocumentInlayHint {
		var params InlayHintParams
		if err = json.Unmarshal(context.Params, &params); err != nil {
			return nil, true, false, err
		}
		result, err := w.server.handleInlayHint(context, &params)
		return result, true, true, err
	}

	// Delegate to standard handler for all other methods
	return w.inner.Handle(context)
}
