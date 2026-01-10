package server

import (
	"encoding/json"

	"starlims-lsp/internal/providers"

	"github.com/tliron/glsp"
	protocol "github.com/tliron/glsp/protocol_3_16"
	"github.com/tliron/glsp/server"
)

// ClientSettings represents the settings sent from the client.
type ClientSettings struct {
	SSL *SSLSettings `json:"ssl"`
}

// SSLSettings represents SSL-specific settings from the client.
type SSLSettings struct {
	Format *FormatSettings `json:"format"`
}

// FormatSettings represents formatting settings from the client.
type FormatSettings struct {
	IndentStyle            *string            `json:"indentStyle"`
	IndentSize             *int               `json:"indentSize"`
	MaxLineLength          *int               `json:"maxLineLength"`
	OperatorSpacing        *bool              `json:"operatorSpacing"`
	CommaSpacing           *bool              `json:"commaSpacing"`
	SemicolonEnforcement   *bool              `json:"semicolonEnforcement"`
	BlankLinesBetweenProcs *int               `json:"blankLinesBetweenProcs"`
	SQL                    *SQLFormatSettings `json:"sql"`
}

// SQLFormatSettings represents SQL formatting settings from the client.
type SQLFormatSettings struct {
	Enabled       *bool   `json:"enabled"`
	Style         *string `json:"style"`
	KeywordCase   *string `json:"keywordCase"`
	IndentSize    *int    `json:"indentSize"`
	MaxLineLength *int    `json:"maxLineLength"`
}

const serverName = "starlims-lsp"

var version = "0.1.0"

// SSLServer is the SSL language server.
type SSLServer struct {
	documents       *DocumentManager
	handler         protocol.Handler
	settings        Settings
	documentVersion map[string]int
}

// Settings contains server settings.
type Settings struct {
	MaxNumberOfProblems int
	Diagnostics         providers.DiagnosticOptions
	Formatting          providers.FormattingOptions
}

// DefaultSettings returns default settings.
func DefaultSettings() Settings {
	return Settings{
		MaxNumberOfProblems: 100,
		Diagnostics:         providers.DefaultDiagnosticOptions(),
		Formatting:          providers.DefaultFormattingOptions(),
	}
}

// NewSSLServer creates a new SSL language server.
func NewSSLServer() *SSLServer {
	s := &SSLServer{
		documents:       NewDocumentManager(),
		settings:        DefaultSettings(),
		documentVersion: make(map[string]int),
	}

	s.handler = protocol.Handler{
		Initialize:                      s.handleInitialize,
		Initialized:                     s.handleInitialized,
		Shutdown:                        s.handleShutdown,
		TextDocumentDidOpen:             s.handleDidOpen,
		TextDocumentDidChange:           s.handleDidChange,
		TextDocumentDidClose:            s.handleDidClose,
		TextDocumentCompletion:          s.handleCompletion,
		TextDocumentHover:               s.handleHover,
		TextDocumentDefinition:          s.handleDefinition,
		TextDocumentReferences:          s.handleReferences,
		TextDocumentDocumentSymbol:      s.handleDocumentSymbol,
		TextDocumentFoldingRange:        s.handleFoldingRange,
		TextDocumentSignatureHelp:       s.handleSignatureHelp,
		TextDocumentDidSave:             s.handleDidSave,
		TextDocumentFormatting:          s.handleFormatting,
		TextDocumentRangeFormatting:     s.handleRangeFormatting,
		WorkspaceDidChangeConfiguration: s.handleDidChangeConfiguration,
	}

	return s
}

// Run starts the language server.
func (s *SSLServer) Run() error {
	srv := server.NewServer(&s.handler, serverName, false)
	return srv.RunStdio()
}

// handleInitialize handles the initialize request.
func (s *SSLServer) handleInitialize(context *glsp.Context, params *protocol.InitializeParams) (any, error) {
	capabilities := s.handler.CreateServerCapabilities()

	capabilities.TextDocumentSync = protocol.TextDocumentSyncKindIncremental
	capabilities.CompletionProvider = &protocol.CompletionOptions{
		TriggerCharacters: []string{":", ".", "(", ","},
	}
	capabilities.HoverProvider = true
	capabilities.DefinitionProvider = true
	capabilities.ReferencesProvider = true
	capabilities.DocumentSymbolProvider = true
	capabilities.FoldingRangeProvider = true
	capabilities.SignatureHelpProvider = &protocol.SignatureHelpOptions{
		TriggerCharacters:   []string{"(", ","},
		RetriggerCharacters: []string{","},
	}
	capabilities.DocumentFormattingProvider = true
	capabilities.DocumentRangeFormattingProvider = true

	return protocol.InitializeResult{
		Capabilities: capabilities,
		ServerInfo: &protocol.InitializeResultServerInfo{
			Name:    serverName,
			Version: &version,
		},
	}, nil
}

// handleInitialized handles the initialized notification.
func (s *SSLServer) handleInitialized(context *glsp.Context, params *protocol.InitializedParams) error {
	return nil
}

// handleShutdown handles the shutdown request.
func (s *SSLServer) handleShutdown(context *glsp.Context) error {
	return nil
}

// handleDidOpen handles document open.
func (s *SSLServer) handleDidOpen(context *glsp.Context, params *protocol.DidOpenTextDocumentParams) error {
	uri := params.TextDocument.URI
	version := int(params.TextDocument.Version)
	content := params.TextDocument.Text

	s.documents.SetDocument(uri, content, version)
	s.documentVersion[uri] = version

	// Validate
	s.validateDocument(context, uri)

	return nil
}

// handleDidChange handles document changes.
func (s *SSLServer) handleDidChange(context *glsp.Context, params *protocol.DidChangeTextDocumentParams) error {
	uri := params.TextDocument.URI
	version := int(params.TextDocument.Version)

	// Get current content
	content, ok := s.documents.GetDocument(uri)
	if !ok {
		return nil
	}

	// Apply changes
	for _, change := range params.ContentChanges {
		if changeEvent, ok := change.(protocol.TextDocumentContentChangeEvent); ok {
			content = applyChange(content, changeEvent)
		} else if changeEventWhole, ok := change.(protocol.TextDocumentContentChangeEventWhole); ok {
			content = changeEventWhole.Text
		}
	}

	s.documents.SetDocument(uri, content, version)
	s.documentVersion[uri] = version

	// Validate
	s.validateDocument(context, uri)

	return nil
}

// handleDidClose handles document close.
func (s *SSLServer) handleDidClose(context *glsp.Context, params *protocol.DidCloseTextDocumentParams) error {
	uri := params.TextDocument.URI
	s.documents.RemoveDocument(uri)
	delete(s.documentVersion, uri)
	return nil
}

// handleDidSave handles document save.
func (s *SSLServer) handleDidSave(context *glsp.Context, params *protocol.DidSaveTextDocumentParams) error {
	// Re-validate on save
	s.validateDocument(context, params.TextDocument.URI)
	return nil
}

// handleDidChangeConfiguration handles configuration changes.
func (s *SSLServer) handleDidChangeConfiguration(context *glsp.Context, params *protocol.DidChangeConfigurationParams) error {
	// Parse settings from client
	if params.Settings != nil {
		s.applySettings(params.Settings)
	}

	// Re-validate all documents
	for _, uri := range s.documents.AllDocuments() {
		s.validateDocument(context, uri)
	}
	return nil
}

// applySettings applies client settings to server configuration.
func (s *SSLServer) applySettings(settings interface{}) {
	// Convert settings to JSON and back to parse
	data, err := json.Marshal(settings)
	if err != nil {
		return
	}

	var clientSettings ClientSettings
	if err := json.Unmarshal(data, &clientSettings); err != nil {
		return
	}

	if clientSettings.SSL == nil || clientSettings.SSL.Format == nil {
		return
	}

	fmt := clientSettings.SSL.Format

	// Apply formatting settings
	applyOptional(&s.settings.Formatting.IndentStyle, fmt.IndentStyle)
	applyOptional(&s.settings.Formatting.IndentSize, fmt.IndentSize)
	applyOptional(&s.settings.Formatting.MaxLineLength, fmt.MaxLineLength)
	applyOptional(&s.settings.Formatting.OperatorSpacing, fmt.OperatorSpacing)
	applyOptional(&s.settings.Formatting.CommaSpacing, fmt.CommaSpacing)
	applyOptional(&s.settings.Formatting.SemicolonEnforcement, fmt.SemicolonEnforcement)
	applyOptional(&s.settings.Formatting.BlankLinesBetweenProcs, fmt.BlankLinesBetweenProcs)

	// Apply SQL formatting settings
	if fmt.SQL != nil {
		sql := fmt.SQL
		applyOptional(&s.settings.Formatting.SQL.Enabled, sql.Enabled)
		applyOptional(&s.settings.Formatting.SQL.Style, sql.Style)
		applyOptional(&s.settings.Formatting.SQL.KeywordCase, sql.KeywordCase)
		applyOptional(&s.settings.Formatting.SQL.IndentSize, sql.IndentSize)
		applyOptional(&s.settings.Formatting.SQL.MaxLineLength, sql.MaxLineLength)
	}
}

func applyOptional[T any](target *T, value *T) {
	if value != nil {
		*target = *value
	}
}

// validateDocument validates a document and sends diagnostics.
func (s *SSLServer) validateDocument(context *glsp.Context, uri string) {
	content, ok := s.documents.GetDocument(uri)
	if !ok {
		return
	}

	diagnostics := providers.GetDiagnostics(content, s.settings.Diagnostics)

	// Convert to protocol diagnostics
	protocolDiags := make([]protocol.Diagnostic, 0, len(diagnostics))
	for i, d := range diagnostics {
		if i >= s.settings.MaxNumberOfProblems {
			break
		}
		protocolDiags = append(protocolDiags, protocol.Diagnostic{
			Range:    toProtocolRange(d.Range),
			Severity: ptrTo(protocol.DiagnosticSeverity(d.Severity)),
			Source:   &d.Source,
			Message:  d.Message,
		})
	}

	context.Notify(protocol.ServerTextDocumentPublishDiagnostics, protocol.PublishDiagnosticsParams{
		URI:         uri,
		Diagnostics: protocolDiags,
	})
}

// applyChange applies an incremental change to content.
func applyChange(content string, change protocol.TextDocumentContentChangeEvent) string {
	if change.Range == nil {
		return change.Text
	}

	runes := []rune(content)
	lines := splitLines(string(runes))

	startLine := int(change.Range.Start.Line)
	startChar := int(change.Range.Start.Character)
	endLine := int(change.Range.End.Line)
	endChar := int(change.Range.End.Character)

	// Calculate byte offsets
	startOffset := 0
	for i := 0; i < startLine && i < len(lines); i++ {
		startOffset += len(lines[i]) + 1 // +1 for newline
	}
	if startLine < len(lines) {
		lineRunes := []rune(lines[startLine])
		if startChar <= len(lineRunes) {
			startOffset += len(string(lineRunes[:startChar]))
		}
	}

	endOffset := 0
	for i := 0; i < endLine && i < len(lines); i++ {
		endOffset += len(lines[i]) + 1
	}
	if endLine < len(lines) {
		lineRunes := []rune(lines[endLine])
		if endChar <= len(lineRunes) {
			endOffset += len(string(lineRunes[:endChar]))
		}
	}

	// Apply change
	if startOffset > len(content) {
		startOffset = len(content)
	}
	if endOffset > len(content) {
		endOffset = len(content)
	}

	return content[:startOffset] + change.Text + content[endOffset:]
}

// splitLines splits content into lines.
func splitLines(content string) []string {
	var lines []string
	start := 0
	for i, r := range content {
		if r == '\n' {
			lines = append(lines, content[start:i])
			start = i + 1
		}
	}
	if start <= len(content) {
		lines = append(lines, content[start:])
	}
	return lines
}

// ptrTo returns a pointer to the value.
func ptrTo[T any](v T) *T {
	return &v
}
