// Package server provides LSP server implementation.
// This file implements a wrapper handler to support LSP 3.17 methods.
package server

import (
	"encoding/json"
	"fmt"
	"os"
	"runtime/debug"

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
//
// Every request and notification passes through here, so this is the one
// place a panic in any handler or provider is contained: glsp does not
// recover, and an unrecovered panic would end the server process, taking
// every open document's features down with it (issue #258). The panic is
// logged with its stack to stderr (the editor's LSP output channel) and
// the request fails with an error response instead.
func (w *WrapperHandler) Handle(context *glsp.Context) (r any, validMethod bool, validParams bool, err error) {
	defer func() {
		if rec := recover(); rec != nil {
			fmt.Fprintf(os.Stderr, "ssl-lsp: panic handling %s: %v\n%s\n", context.Method, rec, debug.Stack())
			r, validMethod, validParams = nil, true, true
			err = fmt.Errorf("ssl-lsp internal error handling %s: %v", context.Method, rec)
		}
	}()

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
