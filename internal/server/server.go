package server

import (
	"encoding/json"
	"strings"

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
	Format      *FormatSettings      `json:"format"`
	Diagnostics *DiagnosticsSettings `json:"diagnostics"`
	InlayHints  *InlayHintsSettings  `json:"inlayHints"`
}

// DiagnosticsSettings represents diagnostics settings from the client.
type DiagnosticsSettings struct {
	HungarianNotation *bool     `json:"hungarianNotation"`
	HungarianPrefixes *[]string `json:"hungarianPrefixes"`
	Globals           *[]string `json:"globals"`
	MaxBlockDepth     *int      `json:"maxBlockDepth"`
}

// InlayHintsSettings represents inlay hint settings from the client.
type InlayHintsSettings struct {
	Enabled           *bool `json:"enabled"`
	MinParameterCount *int  `json:"minParameterCount"`
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
	Enabled          *bool   `json:"enabled"`
	Style            *string `json:"style"`
	KeywordCase      *string `json:"keywordCase"`
	IndentSize       *int    `json:"indentSize"`
	MaxLineLength    *int    `json:"maxLineLength"`
	DetectSQLStrings *bool   `json:"detectSQLStrings"`
}

const serverName = "starlims-lsp"

// Version is the server version reported to LSP clients in
// `InitializeResult.serverInfo.version`. It is overridden at startup by
// the cmd/starlims-lsp main package via SetVersion so the wire-reported
// version matches the binary's built-in version (which is in turn supplied
// at build time by `-X main.version=...`).
var version = "dev"

// SetVersion overrides the version reported to LSP clients. Call from main
// before NewSSLServer.
func SetVersion(v string) {
	if v != "" {
		version = v
	}
}

// ExtendedServerCapabilities embeds the standard capabilities and adds LSP 3.17 features.
type ExtendedServerCapabilities struct {
	protocol.ServerCapabilities
	// InlayHintProvider indicates inlay hint support (LSP 3.17).
	InlayHintProvider bool `json:"inlayHintProvider,omitempty"`
}

// ExtendedInitializeResult is the initialize result with extended capabilities.
type ExtendedInitializeResult struct {
	Capabilities ExtendedServerCapabilities           `json:"capabilities"`
	ServerInfo   *protocol.InitializeResultServerInfo `json:"serverInfo,omitempty"`
}

// SSLServer is the SSL language server.
type SSLServer struct {
	documents       *DocumentManager
	handler         protocol.Handler
	settings        Settings
	documentVersion map[string]int
	workspaceIndex  *WorkspaceIndex
	rootURIs        []string
}

// Settings contains server settings.
type Settings struct {
	MaxNumberOfProblems int
	Diagnostics         providers.DiagnosticOptions
	Formatting          providers.FormattingOptions
	InlayHints          providers.InlayHintOptions
}

// DefaultSettings returns default settings.
func DefaultSettings() Settings {
	return Settings{
		MaxNumberOfProblems: 100,
		Diagnostics:         providers.DefaultDiagnosticOptions(),
		Formatting:          providers.DefaultFormattingOptions(),
		InlayHints:          providers.DefaultInlayHintOptions(),
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
		TextDocumentRename:              s.handleRename,
		TextDocumentPrepareRename:       s.handlePrepareRename,
		WorkspaceSymbol:                 s.handleWorkspaceSymbol,
		WorkspaceDidChangeConfiguration: s.handleDidChangeConfiguration,
		WorkspaceDidChangeWatchedFiles:  s.handleDidChangeWatchedFiles,
	}

	return s
}

// Run starts the language server.
func (s *SSLServer) Run() error {
	wrapper := NewWrapperHandler(&s.handler, s)
	srv := server.NewServer(wrapper, serverName, false)
	return srv.RunStdio()
}

// handleInitialize handles the initialize request.
func (s *SSLServer) handleInitialize(context *glsp.Context, params *protocol.InitializeParams) (any, error) {
	// Capture workspace roots for indexing
	if len(params.WorkspaceFolders) > 0 {
		for _, folder := range params.WorkspaceFolders {
			s.rootURIs = append(s.rootURIs, folder.URI)
		}
	} else if params.RootURI != nil {
		s.rootURIs = []string{*params.RootURI}
	} else if params.RootPath != nil {
		s.rootURIs = []string{pathToURI(*params.RootPath)}
	}

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
	capabilities.WorkspaceSymbolProvider = true
	capabilities.RenameProvider = &protocol.RenameOptions{
		PrepareProvider: ptrTo(true),
	}

	// Return extended capabilities with inlay hint support
	return ExtendedInitializeResult{
		Capabilities: ExtendedServerCapabilities{
			ServerCapabilities: capabilities,
			InlayHintProvider:  true,
		},
		ServerInfo: &protocol.InitializeResultServerInfo{
			Name:    serverName,
			Version: &version,
		},
	}, nil
}

// handleInitialized handles the initialized notification.
func (s *SSLServer) handleInitialized(context *glsp.Context, params *protocol.InitializedParams) error {
	// Start workspace indexing if we have roots
	if len(s.rootURIs) > 0 {
		s.workspaceIndex = NewWorkspaceIndex(s.rootURIs)
		s.workspaceIndex.StartBackgroundIndex()

		// Register file watchers for SSL file types
		context.Call(string(protocol.ServerClientRegisterCapability),
			protocol.RegistrationParams{
				Registrations: []protocol.Registration{{
					ID:     "ssl-file-watcher",
					Method: string(protocol.MethodWorkspaceDidChangeWatchedFiles),
					RegisterOptions: protocol.DidChangeWatchedFilesRegistrationOptions{
						Watchers: []protocol.FileSystemWatcher{
							{GlobPattern: "**/*.srvscr"},
							{GlobPattern: "**/*.ssl"},
							{GlobPattern: "**/*.ssl.txt"},
							{GlobPattern: "**/*.ds"},
							{GlobPattern: "**/*.ds.txt"},
						},
					},
				}},
			}, nil)
	}

	return nil
}

// handleShutdown handles the shutdown request.
func (s *SSLServer) handleShutdown(context *glsp.Context) error {
	if s.workspaceIndex != nil {
		s.workspaceIndex.Stop()
	}
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

	// Re-index from disk so the workspace index has the latest saved content
	if s.workspaceIndex != nil {
		go s.workspaceIndex.IndexFile(uri)
	}

	return nil
}

// handleDidSave handles document save.
func (s *SSLServer) handleDidSave(context *glsp.Context, params *protocol.DidSaveTextDocumentParams) error {
	// Re-validate on save
	s.validateDocument(context, params.TextDocument.URI)
	return nil
}

// handleDidChangeWatchedFiles handles file system change events.
func (s *SSLServer) handleDidChangeWatchedFiles(context *glsp.Context, params *protocol.DidChangeWatchedFilesParams) error {
	if s.workspaceIndex == nil {
		return nil
	}
	for _, event := range params.Changes {
		uri := event.URI
		switch event.Type {
		case protocol.FileChangeTypeCreated, protocol.FileChangeTypeChanged:
			// Skip if file is currently open (open document is more up-to-date)
			if _, open := s.documents.GetDocument(uri); !open {
				s.workspaceIndex.IndexFile(uri)
			}
		case protocol.FileChangeTypeDeleted:
			s.workspaceIndex.RemoveFile(uri)
		}
	}
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
	if settings == nil {
		return
	}

	// Convert settings to JSON and back to parse
	data, err := json.Marshal(settings)
	if err != nil {
		// Log the error but continue with existing settings
		// This is not fatal - we just can't apply the new settings
		return
	}

	var clientSettings ClientSettings
	if err := json.Unmarshal(data, &clientSettings); err != nil {
		// Log the error but continue with existing settings
		return
	}

	if clientSettings.SSL == nil {
		return
	}

	if clientSettings.SSL.Format != nil {
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
			applyOptional(&s.settings.Formatting.SQL.DetectSQLStrings, sql.DetectSQLStrings)
		}
	}

	// Apply diagnostics settings
	if clientSettings.SSL.Diagnostics != nil {
		diagnostics := clientSettings.SSL.Diagnostics
		applyOptional(&s.settings.Diagnostics.CheckHungarianNotation, diagnostics.HungarianNotation)
		applyOptional(&s.settings.Diagnostics.HungarianPrefixes, diagnostics.HungarianPrefixes)
		applyOptional(&s.settings.Diagnostics.GlobalVariables, diagnostics.Globals)
		applyOptional(&s.settings.Diagnostics.MaxBlockDepth, diagnostics.MaxBlockDepth)
	}

	// Apply inlay hints settings
	if clientSettings.SSL.InlayHints != nil {
		inlayHints := clientSettings.SSL.InlayHints
		applyOptional(&s.settings.InlayHints.Enabled, inlayHints.Enabled)
		applyOptional(&s.settings.InlayHints.MinParameterCount, inlayHints.MinParameterCount)
	}
}

func applyOptional[T any](target *T, value *T) {
	if value != nil {
		*target = *value
	}
}

// isDataSourceURI checks if a document URI refers to a data source file (.ds or .ds.txt).
func isDataSourceURI(uri string) bool {
	lower := strings.ToLower(uri)
	return strings.HasSuffix(lower, ".ds") || strings.HasSuffix(lower, ".ds.txt")
}

// validateDocument validates a document and sends diagnostics.
func (s *SSLServer) validateDocument(context *glsp.Context, uri string) {
	if _, ok := s.documents.GetDocument(uri); !ok {
		return
	}

	version := s.documentVersion[uri]
	cache := s.documents.ParseDocument(uri, version)
	opts := s.settings.Diagnostics
	opts.IsDataSourceFile = isDataSourceURI(uri)
	diagnostics := providers.GetDiagnosticsFromTokens(cache.Tokens, cache.AST, opts)

	// Convert to protocol diagnostics
	protocolDiags := make([]protocol.Diagnostic, 0, len(diagnostics))
	for i, d := range diagnostics {
		if i >= s.settings.MaxNumberOfProblems {
			break
		}
		pd := protocol.Diagnostic{
			Range:    toProtocolRange(d.Range),
			Severity: ptrTo(protocol.DiagnosticSeverity(d.Severity)),
			Source:   &d.Source,
			Message:  d.Message,
		}
		if d.Code != "" {
			pd.Code = &protocol.IntegerOrString{Value: d.Code}
		}
		protocolDiags = append(protocolDiags, pd)
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
