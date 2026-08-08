package server

import (
	"encoding/json"
	"regexp"
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
	Format       *FormatSettings       `json:"format"`
	Diagnostics  *DiagnosticsSettings  `json:"diagnostics"`
	InlayHints   *InlayHintsSettings   `json:"inlayHints"`
	IntelliSense *IntelliSenseSettings `json:"intellisense"`
}

// IntelliSenseSettings groups completion/signature-help client settings.
type IntelliSenseSettings struct {
	SignatureHelp *SignatureHelpSettings `json:"signatureHelp"`
}

// SignatureHelpSettings configures signature help behavior.
type SignatureHelpSettings struct {
	// AutoTrigger controls whether the server advertises trigger characters
	// for signature help. When false (default), signature help still works
	// on explicit invocation (Ctrl+Shift+Space) and hover, but does not
	// auto-pop while typing. See issue #9.
	AutoTrigger *bool `json:"autoTrigger"`
}

// DiagnosticsSettings represents diagnostics settings from the client.
type DiagnosticsSettings struct {
	HungarianNotation *bool              `json:"hungarianNotation"`
	UnusedVariables   *bool              `json:"unusedVariables"`
	HungarianPrefixes *[]string          `json:"hungarianPrefixes"`
	Globals           *[]string          `json:"globals"`
	MaxBlockDepth     *int               `json:"maxBlockDepth"`
	Rules             *map[string]string `json:"rules"`
	// EndpointPatterns is a list of substrings/suffixes; any URI whose
	// lowercased path contains one of these is treated as an endpoint
	// script (Request/Response are then pre-injected ambients). In
	// addition to this setting, a leading-docblock `Endpoint:` marker
	// in the first ~30 lines also activates endpoint mode.
	EndpointPatterns *[]string `json:"endpointPatterns"`
}

// InlayHintsSettings represents inlay hint settings from the client.
type InlayHintsSettings struct {
	Enabled           *bool `json:"enabled"`
	MinParameterCount *int  `json:"minParameterCount"`
}

// FormatSettings represents formatting settings from the client.
type FormatSettings struct {
	IndentStyle              *string            `json:"indentStyle"`
	IndentSize               *int               `json:"indentSize"`
	MaxLineLength            *int               `json:"maxLineLength"`
	OperatorSpacing          *bool              `json:"operatorSpacing"`
	CommaSpacing             *bool              `json:"commaSpacing"`
	SemicolonEnforcement     *bool              `json:"semicolonEnforcement"`
	BlankLinesBetweenProcs   *int               `json:"blankLinesBetweenProcs"`
	BlankLineBetweenBlocks   *bool              `json:"blankLineBetweenBlocks"`
	TrimTrailingWhitespace   *bool              `json:"trimTrailingWhitespace"`
	MaxConsecutiveBlankLines *int               `json:"maxConsecutiveBlankLines"`
	BuiltinFunctionCase      *string            `json:"builtinFunctionCase"`
	SQL                      *SQLFormatSettings `json:"sql"`
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
	MaxNumberOfProblems      int
	Diagnostics              providers.DiagnosticOptions
	Formatting               providers.FormattingOptions
	InlayHints               providers.InlayHintOptions
	SignatureHelpAutoTrigger bool
	// EndpointPatterns is a list of case-insensitive path substrings;
	// any document URI whose lowercased path contains one of these
	// patterns is treated as an SSL endpoint script.
	EndpointPatterns []string
}

// DefaultSettings returns default settings.
func DefaultSettings() Settings {
	return Settings{
		MaxNumberOfProblems:      100,
		Diagnostics:              providers.DefaultDiagnosticOptions(),
		Formatting:               providers.DefaultFormattingOptions(),
		InlayHints:               providers.DefaultInlayHintOptions(),
		SignatureHelpAutoTrigger: false,
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

	// Issue #55: apply settings sent via initializationOptions. The VS Code
	// extension sends its rebuilt settings tree (ssl.diagnostics.globals,
	// etc.) here at startup. Without this, configured globals — and every
	// other client setting — were silently dropped until the user happened
	// to change a setting later.
	if params.InitializationOptions != nil {
		s.applySettings(params.InitializationOptions)
	}

	capabilities := s.handler.CreateServerCapabilities()

	capabilities.TextDocumentSync = protocol.TextDocumentSyncKindIncremental
	capabilities.CompletionProvider = &protocol.CompletionOptions{
		// Only ':' is advertised. '.' and ',' fire too aggressively during
		// list/decimal/expression entry. '(' was removed because the popup
		// it produced after typing an open-paren competed with signature
		// help and the full inventory it dumped was noisy. ':' is kept
		// because it is the SSL keyword prefix (`:DECLARE`) and the
		// member-access operator (`obj:prop`) — both are high-signal
		// completion moments. When ':' fires and no context-aware match is
		// found (see contextAwareCompletions), the handler returns only
		// keyword completions. The full inventory is reserved for explicit
		// Ctrl+Space invocation. See issue #8.
		TriggerCharacters: []string{":"},
	}
	capabilities.HoverProvider = true
	capabilities.DefinitionProvider = true
	capabilities.ReferencesProvider = true
	capabilities.DocumentSymbolProvider = true
	capabilities.FoldingRangeProvider = true
	// Signature help is always supported via explicit invocation
	// (Ctrl+Shift+Space). Auto-trigger characters are only advertised when
	// the user opts in via ssl.intellisense.signatureHelp.autoTrigger,
	// because the popup obscures the line being typed otherwise. See #9.
	sigHelpOpts := &protocol.SignatureHelpOptions{}
	if s.settings.SignatureHelpAutoTrigger {
		sigHelpOpts.TriggerCharacters = []string{"(", ","}
		sigHelpOpts.RetriggerCharacters = []string{","}
	}
	capabilities.SignatureHelpProvider = sigHelpOpts
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
func (s *SSLServer) handleInitialized(ctx *glsp.Context, params *protocol.InitializedParams) error {
	// Start workspace indexing if we have roots
	if len(s.rootURIs) > 0 {
		s.workspaceIndex = NewWorkspaceIndex(s.rootURIs)
		s.workspaceIndex.StartBackgroundIndex()

		// Register file watchers for SSL file types.
		//
		// IMPORTANT: ctx.Call is a synchronous client→server round-trip and
		// `initialized` is dispatched on the same goroutine that reads
		// incoming messages. Calling it inline deadlocks: the reader is
		// blocked waiting for the client's registerCapability response,
		// which can only arrive via the reader. Run the registration in a
		// background goroutine so the handler returns and the reader keeps
		// pumping messages.
		go ctx.Call(string(protocol.ServerClientRegisterCapability),
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
		applyOptional(&s.settings.Formatting.BlankLineBetweenBlocks, fmt.BlankLineBetweenBlocks)
		applyOptional(&s.settings.Formatting.TrimTrailingWhitespace, fmt.TrimTrailingWhitespace)
		applyOptional(&s.settings.Formatting.MaxConsecutiveBlankLines, fmt.MaxConsecutiveBlankLines)
		applyOptional(&s.settings.Formatting.BuiltinFunctionCase, fmt.BuiltinFunctionCase)

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
		applyOptional(&s.settings.Diagnostics.CheckUnusedVars, diagnostics.UnusedVariables)
		applyOptional(&s.settings.Diagnostics.HungarianPrefixes, diagnostics.HungarianPrefixes)
		applyOptional(&s.settings.Diagnostics.GlobalVariables, diagnostics.Globals)
		applyOptional(&s.settings.Diagnostics.MaxBlockDepth, diagnostics.MaxBlockDepth)
		applyOptional(&s.settings.Diagnostics.RuleOverrides, diagnostics.Rules)
		applyOptional(&s.settings.EndpointPatterns, diagnostics.EndpointPatterns)
	}

	// Apply inlay hints settings
	if clientSettings.SSL.InlayHints != nil {
		inlayHints := clientSettings.SSL.InlayHints
		applyOptional(&s.settings.InlayHints.Enabled, inlayHints.Enabled)
		applyOptional(&s.settings.InlayHints.MinParameterCount, inlayHints.MinParameterCount)
	}

	// Apply intellisense settings
	if clientSettings.SSL.IntelliSense != nil && clientSettings.SSL.IntelliSense.SignatureHelp != nil {
		applyOptional(&s.settings.SignatureHelpAutoTrigger, clientSettings.SSL.IntelliSense.SignatureHelp.AutoTrigger)
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

// isEndpointFile decides whether a document should be treated as an SSL
// endpoint script (where `Request` and `Response` are pre-injected runtime
// ambients). It uses two signals:
//
//  1. Configured `EndpointPatterns` — any pattern whose lowercased value
//     appears as a substring of the lowercased URI activates endpoint mode.
//  2. A leading-docblock `Endpoint:` marker scanned from the first ~30
//     lines of the file (the same convention used by the SSL agent guides).
//
// The default-empty pattern list means there are zero false positives
// out of the box: users opt in either via the marker in their files or
// the workspace setting.
func isEndpointFile(uri string, content string, patterns []string) bool {
	lowerURI := strings.ToLower(uri)
	for _, p := range patterns {
		if p == "" {
			continue
		}
		if strings.Contains(lowerURI, strings.ToLower(p)) {
			return true
		}
	}

	// Scan only the leading docblock region — keep this cheap and
	// resistant to false positives from a stray "Endpoint:" appearing
	// deeper in the file (e.g. inside a string literal or comment block
	// that documents something unrelated).
	const maxLines = 30
	lines := 0
	for i := 0; i < len(content) && lines < maxLines; i++ {
		if content[i] == '\n' {
			lines++
		}
	}
	head := content
	if lines >= maxLines {
		// Truncate to the first `maxLines` lines.
		count := 0
		for i := 0; i < len(content); i++ {
			if content[i] == '\n' {
				count++
				if count >= maxLines {
					head = content[:i]
					break
				}
			}
		}
	}
	// Match `Endpoint:` on a docblock line (preceded by `*` or at line start).
	// Case-insensitive.
	return endpointMarkerRegexp.MatchString(head)
}

var endpointMarkerRegexp = regexp.MustCompile(`(?im)^[\s*]*Endpoint\s*:`)

// validateDocument validates a document and sends diagnostics.
func (s *SSLServer) validateDocument(context *glsp.Context, uri string) {
	if _, ok := s.documents.GetDocument(uri); !ok {
		return
	}

	version := s.documentVersion[uri]
	cache := s.documents.ParseDocument(uri, version)
	content, _ := s.documents.GetDocument(uri)
	opts := s.settings.Diagnostics
	opts.IsDataSourceFile = isDataSourceURI(uri)
	opts.IsEndpointFile = isEndpointFile(uri, content, s.settings.EndpointPatterns)
	opts.IncludeDeclaredVariables = (liveResolver{s}).includeDeclaredVariables(cache.Tokens, uri)
	if !opts.IsDataSourceFile {
		opts.ClassFileDispatchTargets = (liveResolver{s}).classFileDispatchTargets(cache.Tokens)
	}

	// Data-source documents route through the text path, which owns the
	// SQL-mode handling: plain SQL gets no SSL diagnostics at all
	// (feature.diagnostics_pipeline A10, issue #77), and the hybrid
	// directives-then-SQL shape keeps diagnostics on its header only
	// (issue #104). Publishing continues either way so previously
	// published diagnostics are cleared.
	var diagnostics []providers.Diagnostic
	if opts.IsDataSourceFile {
		diagnostics = providers.GetDiagnostics(content, opts)
	} else {
		diagnostics = providers.GetDiagnosticsFromTokens(cache.Tokens, cache.AST, opts)
	}

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
