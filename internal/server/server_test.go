package server

import (
	"encoding/json"
	"reflect"
	"strings"
	"testing"

	"starlims-lsp/internal/providers"

	"github.com/tliron/glsp"
	protocol "github.com/tliron/glsp/protocol_3_16"
)

// [spec feature.diagnostics_pipeline/A9] — configuration changes re-publish
// diagnostics for every open document.
func TestHandleDidChangeConfiguration_RevalidatesDocuments(t *testing.T) {
	s := NewSSLServer()
	uriA := "file:///alpha.ssl"
	uriB := "file:///beta.ssl"

	s.documents.SetDocument(uriA, ":PROCEDURE Alpha;:ENDPROC;", 1)
	s.documents.SetDocument(uriB, ":PROCEDURE Beta;:ENDPROC;", 1)
	s.documentVersion[uriA] = 1
	s.documentVersion[uriB] = 1

	diagnostics := make([]protocol.PublishDiagnosticsParams, 0)
	ctx := &glsp.Context{
		Notify: func(method string, params any) {
			if method != protocol.ServerTextDocumentPublishDiagnostics {
				return
			}
			if diagParams, ok := params.(protocol.PublishDiagnosticsParams); ok {
				diagnostics = append(diagnostics, diagParams)
			}
		},
	}

	settings := map[string]interface{}{
		"ssl": map[string]interface{}{
			"format": map[string]interface{}{
				"indentStyle": "space",
			},
		},
	}

	err := s.handleDidChangeConfiguration(ctx, &protocol.DidChangeConfigurationParams{Settings: settings})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	if s.settings.Formatting.IndentStyle != "space" {
		t.Errorf("expected indent style 'space', got %q", s.settings.Formatting.IndentStyle)
	}

	if len(diagnostics) != 2 {
		t.Fatalf("expected diagnostics for 2 documents, got %d", len(diagnostics))
	}

	seen := map[string]bool{}
	for _, diag := range diagnostics {
		seen[diag.URI] = true
	}

	if !seen[uriA] || !seen[uriB] {
		t.Errorf("expected diagnostics for %s and %s", uriA, uriB)
	}
}

func TestNewSSLServer(t *testing.T) {
	s := NewSSLServer()

	if s == nil {
		t.Fatal("expected non-nil server")
	}

	if s.documents == nil {
		t.Error("expected non-nil document manager")
	}

	if s.documentVersion == nil {
		t.Error("expected non-nil document version map")
	}
}

func TestDefaultSettings(t *testing.T) {
	settings := DefaultSettings()

	if settings.MaxNumberOfProblems != 100 {
		t.Errorf("expected max problems 100, got %d", settings.MaxNumberOfProblems)
	}

	// Verify formatting defaults
	if settings.Formatting.IndentStyle != "tab" {
		t.Errorf("expected indent style 'tab', got %q", settings.Formatting.IndentStyle)
	}

	if settings.Formatting.IndentSize != 4 {
		t.Errorf("expected indent size 4, got %d", settings.Formatting.IndentSize)
	}
}

func TestApplySettings_FullSettings(t *testing.T) {
	s := NewSSLServer()

	// Create settings that mirror what the client would send
	settings := map[string]interface{}{
		"ssl": map[string]interface{}{
			"format": map[string]interface{}{
				"indentStyle":            "space",
				"indentSize":             2,
				"maxLineLength":          120,
				"operatorSpacing":        false,
				"commaSpacing":           false,
				"semicolonEnforcement":   false,
				"blankLinesBetweenProcs": 3,
				"sql": map[string]interface{}{
					"enabled":       false,
					"style":         "canonicalCompact",
					"keywordCase":   "lower",
					"indentSize":    2,
					"maxLineLength": 80,
				},
			},
			"diagnostics": map[string]interface{}{
				"hungarianNotation": true,
				"hungarianPrefixes": []string{"a", "b"},
				"maxBlockDepth":     6,
			},
		},
	}

	s.applySettings(settings)

	// Verify all settings were applied
	if s.settings.Formatting.IndentStyle != "space" {
		t.Errorf("expected indent style 'space', got %q", s.settings.Formatting.IndentStyle)
	}
	if s.settings.Formatting.IndentSize != 2 {
		t.Errorf("expected indent size 2, got %d", s.settings.Formatting.IndentSize)
	}
	if s.settings.Formatting.MaxLineLength != 120 {
		t.Errorf("expected max line length 120, got %d", s.settings.Formatting.MaxLineLength)
	}
	if s.settings.Formatting.OperatorSpacing != false {
		t.Error("expected operator spacing false")
	}
	if s.settings.Formatting.CommaSpacing != false {
		t.Error("expected comma spacing false")
	}
	if s.settings.Formatting.SemicolonEnforcement != false {
		t.Error("expected semicolon enforcement false")
	}
	if s.settings.Formatting.BlankLinesBetweenProcs != 3 {
		t.Errorf("expected blank lines between procs 3, got %d", s.settings.Formatting.BlankLinesBetweenProcs)
	}

	// Verify SQL settings
	if s.settings.Formatting.SQL.Enabled != false {
		t.Error("expected SQL enabled false")
	}
	if s.settings.Formatting.SQL.Style != "canonicalCompact" {
		t.Errorf("expected SQL style 'canonicalCompact', got %q", s.settings.Formatting.SQL.Style)
	}
	if s.settings.Formatting.SQL.KeywordCase != "lower" {
		t.Errorf("expected SQL keyword case 'lower', got %q", s.settings.Formatting.SQL.KeywordCase)
	}
	if s.settings.Formatting.SQL.IndentSize != 2 {
		t.Errorf("expected SQL indent size 2, got %d", s.settings.Formatting.SQL.IndentSize)
	}
	if s.settings.Formatting.SQL.MaxLineLength != 80 {
		t.Errorf("expected SQL max line length 80, got %d", s.settings.Formatting.SQL.MaxLineLength)
	}

	if !s.settings.Diagnostics.CheckHungarianNotation {
		t.Error("expected hungarian notation diagnostics to be enabled")
	}
	if !reflect.DeepEqual(s.settings.Diagnostics.HungarianPrefixes, []string{"a", "b"}) {
		t.Errorf("expected Hungarian prefixes [a b], got %v", s.settings.Diagnostics.HungarianPrefixes)
	}
	if s.settings.Diagnostics.MaxBlockDepth != 6 {
		t.Errorf("expected max block depth 6, got %d", s.settings.Diagnostics.MaxBlockDepth)
	}
}

func TestApplySettings_Globals(t *testing.T) {
	s := NewSSLServer()

	settings := map[string]interface{}{
		"ssl": map[string]interface{}{
			"diagnostics": map[string]interface{}{
				"globals": []string{"gCurrentUser", "gAppName", "gDebugMode"},
			},
		},
	}

	s.applySettings(settings)

	expectedGlobals := []string{"gCurrentUser", "gAppName", "gDebugMode"}
	if !reflect.DeepEqual(s.settings.Diagnostics.GlobalVariables, expectedGlobals) {
		t.Errorf("expected globals %v, got %v", expectedGlobals, s.settings.Diagnostics.GlobalVariables)
	}
}

// Issue #48: ssl.diagnostics.unusedVariables wires the previously
// unreachable CheckUnusedVars opt-in.
func TestApplySettings_UnusedVariables(t *testing.T) {
	s := NewSSLServer()

	if s.settings.Diagnostics.CheckUnusedVars {
		t.Fatal("CheckUnusedVars must default to false (DECISIONS.md D5)")
	}

	settings := map[string]interface{}{
		"ssl": map[string]interface{}{
			"diagnostics": map[string]interface{}{
				"unusedVariables": true,
			},
		},
	}

	s.applySettings(settings)

	if !s.settings.Diagnostics.CheckUnusedVars {
		t.Error("expected ssl.diagnostics.unusedVariables to enable CheckUnusedVars")
	}
}

// Issue #55: settings sent via initializationOptions during the LSP
// `initialize` request must be applied. Previously they were dropped, which
// meant `ssl.globals` configured by the user never reached the diagnostic
// engine until the user happened to change a setting later (and even then,
// only if the change went through the rebuilt-payload path).
func TestHandleInitialize_AppliesInitializationOptions(t *testing.T) {
	s := NewSSLServer()

	initOpts := map[string]interface{}{
		"ssl": map[string]interface{}{
			"diagnostics": map[string]interface{}{
				"globals": []string{"gCurrentUser", "gAppName"},
			},
		},
	}

	params := &protocol.InitializeParams{
		InitializationOptions: initOpts,
	}
	if _, err := s.handleInitialize(nil, params); err != nil {
		t.Fatalf("handleInitialize returned error: %v", err)
	}

	expected := []string{"gCurrentUser", "gAppName"}
	if !reflect.DeepEqual(s.settings.Diagnostics.GlobalVariables, expected) {
		t.Errorf("expected globals from initializationOptions to be applied: want %v, got %v",
			expected, s.settings.Diagnostics.GlobalVariables)
	}
}

func TestApplySettings_PartialSettings(t *testing.T) {
	s := NewSSLServer()

	// Apply only some settings - others should remain at defaults
	settings := map[string]interface{}{
		"ssl": map[string]interface{}{
			"format": map[string]interface{}{
				"indentStyle": "space",
				"indentSize":  2,
			},
		},
	}

	s.applySettings(settings)

	// Verify applied settings
	if s.settings.Formatting.IndentStyle != "space" {
		t.Errorf("expected indent style 'space', got %q", s.settings.Formatting.IndentStyle)
	}
	if s.settings.Formatting.IndentSize != 2 {
		t.Errorf("expected indent size 2, got %d", s.settings.Formatting.IndentSize)
	}

	// Verify defaults are preserved for unapplied settings
	if s.settings.Formatting.MaxLineLength != 90 {
		t.Errorf("expected max line length 90 (default), got %d", s.settings.Formatting.MaxLineLength)
	}
	if !s.settings.Formatting.OperatorSpacing {
		t.Error("expected operator spacing true (default)")
	}
}

func TestApplySettings_EmptySettings(t *testing.T) {
	s := NewSSLServer()
	originalSettings := s.settings

	// Apply empty/nil settings - should not change anything
	s.applySettings(nil)

	if !reflect.DeepEqual(s.settings, originalSettings) {
		t.Error("settings should not change when nil is applied")
	}

	s.applySettings(map[string]interface{}{})

	if !reflect.DeepEqual(s.settings, originalSettings) {
		t.Error("settings should not change when empty map is applied")
	}
}

func TestApplySettings_InvalidJSON(t *testing.T) {
	s := NewSSLServer()
	originalSettings := s.settings

	// Apply something that can't be properly unmarshaled
	s.applySettings("not a valid settings object")

	if s.settings.Formatting.IndentStyle != originalSettings.Formatting.IndentStyle {
		t.Error("settings should not change when invalid data is applied")
	}
}

func TestClientSettings_JSONParsing(t *testing.T) {
	jsonData := `{
		"ssl": {
			"format": {
				"indentStyle": "space",
				"indentSize": 4,
				"maxLineLength": 100,
				"operatorSpacing": true,
				"commaSpacing": true,
				"semicolonEnforcement": true,
				"blankLinesBetweenProcs": 1,
				"sql": {
					"enabled": true,
					"style": "standard",
					"keywordCase": "upper",
					"indentSize": 4,
					"maxLineLength": 90
				}
			}
		}
	}`

	var settings ClientSettings
	err := json.Unmarshal([]byte(jsonData), &settings)
	if err != nil {
		t.Fatalf("failed to unmarshal settings: %v", err)
	}

	if settings.SSL == nil {
		t.Fatal("expected SSL settings to be non-nil")
	}
	if settings.SSL.Format == nil {
		t.Fatal("expected Format settings to be non-nil")
	}
	if *settings.SSL.Format.IndentStyle != "space" {
		t.Errorf("expected indent style 'space', got %q", *settings.SSL.Format.IndentStyle)
	}
	if *settings.SSL.Format.IndentSize != 4 {
		t.Errorf("expected indent size 4, got %d", *settings.SSL.Format.IndentSize)
	}
	if settings.SSL.Format.SQL == nil {
		t.Fatal("expected SQL settings to be non-nil")
	}
	if *settings.SSL.Format.SQL.Enabled != true {
		t.Error("expected SQL enabled to be true")
	}
}

// applyChange is the server's half of incremental text sync: every edit the
// editor sends is replayed onto the stored copy, so an offset error
// silently desynchronizes the document until it is reopened. (The previous
// test here never called applyChange; issue #256.)
func TestApplyChange(t *testing.T) {
	rng := func(sl, sc, el, ec uint32) *protocol.Range {
		return &protocol.Range{
			Start: protocol.Position{Line: sl, Character: sc},
			End:   protocol.Position{Line: el, Character: ec},
		}
	}
	cases := []struct {
		name    string
		content string
		change  protocol.TextDocumentContentChangeEvent
		want    string
	}{
		{"full replace", "line1\nline2", protocol.TextDocumentContentChangeEvent{Text: "new"}, "new"},
		{"insert mid-line", "x := 1;", protocol.TextDocumentContentChangeEvent{Range: rng(0, 5, 0, 5), Text: "2"}, "x := 21;"},
		{"insert at end of document", "a\nb", protocol.TextDocumentContentChangeEvent{Range: rng(1, 1, 1, 1), Text: ";"}, "a\nb;"},
		{"replace across lines", "one\ntwo\nthree", protocol.TextDocumentContentChangeEvent{Range: rng(0, 1, 2, 2), Text: "X"}, "oXree"},
		{"delete a newline", "a;\nb;", protocol.TextDocumentContentChangeEvent{Range: rng(0, 2, 1, 0), Text: ""}, "a;b;"},
		{"after BMP non-ASCII", "s := \"é\";", protocol.TextDocumentContentChangeEvent{Range: rng(0, 7, 0, 8), Text: ";"}, "s := \"é;;"},
		// U+1F600 is one rune but two UTF-16 code units: column 8 is the
		// closing quote, not the character after it.
		{"after astral character", "s := \"\U0001F600\";", protocol.TextDocumentContentChangeEvent{Range: rng(0, 8, 0, 9), Text: "'"}, "s := \"\U0001F600';"},
		{"CRLF line", "a := 1;\r\nb := 2;", protocol.TextDocumentContentChangeEvent{Range: rng(1, 5, 1, 6), Text: "3"}, "a := 1;\r\nb := 3;"},
		{"column past line end clamps to line end", "ab\ncd", protocol.TextDocumentContentChangeEvent{Range: rng(0, 99, 0, 99), Text: "!"}, "ab!\ncd"},
		{"line past end clamps to document end", "ab", protocol.TextDocumentContentChangeEvent{Range: rng(5, 0, 5, 0), Text: "!"}, "ab!"},
		{"inverted range inserts at start", "abc", protocol.TextDocumentContentChangeEvent{Range: rng(0, 2, 0, 1), Text: "!"}, "ab!c"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			if got := applyChange(tc.content, tc.change); got != tc.want {
				t.Errorf("got %q, want %q", got, tc.want)
			}
		})
	}
}

// The document lifecycle handlers keep the stored text and version in step
// with the editor and publish diagnostics for each new state.
func TestDocumentLifecycleHandlers(t *testing.T) {
	s := NewSSLServer()
	uri := "file:///lifecycle.ssl"
	var published []protocol.PublishDiagnosticsParams
	ctx := &glsp.Context{Notify: func(method string, params any) {
		if p, ok := params.(protocol.PublishDiagnosticsParams); ok && method == protocol.ServerTextDocumentPublishDiagnostics {
			published = append(published, p)
		}
	}}

	if err := s.handleDidOpen(ctx, &protocol.DidOpenTextDocumentParams{TextDocument: protocol.TextDocumentItem{
		URI: uri, Version: 1, Text: ":IF .T.;\n:ENDIF;\n",
	}}); err != nil {
		t.Fatal(err)
	}
	if len(published) != 1 || len(published[0].Diagnostics) != 0 {
		t.Fatalf("open: expected one clean publish, got %+v", published)
	}

	// Delete ":ENDIF;" incrementally: the block is now unclosed.
	if err := s.handleDidChange(ctx, &protocol.DidChangeTextDocumentParams{
		TextDocument: protocol.VersionedTextDocumentIdentifier{
			TextDocumentIdentifier: protocol.TextDocumentIdentifier{URI: uri}, Version: 2,
		},
		ContentChanges: []any{protocol.TextDocumentContentChangeEvent{
			Range: &protocol.Range{Start: protocol.Position{Line: 1}, End: protocol.Position{Line: 1, Character: 7}},
		}},
	}); err != nil {
		t.Fatal(err)
	}
	if got, _ := s.documents.GetDocument(uri); got != ":IF .T.;\n\n" {
		t.Errorf("change: stored text %q", got)
	}
	if s.documentVersion[uri] != 2 {
		t.Errorf("change: version %d, want 2", s.documentVersion[uri])
	}
	if len(published) != 2 || len(published[1].Diagnostics) == 0 {
		t.Fatalf("change: expected a publish with the unclosed-block error, got %+v", published)
	}

	// A whole-document change event replaces the text.
	if err := s.handleDidChange(ctx, &protocol.DidChangeTextDocumentParams{
		TextDocument: protocol.VersionedTextDocumentIdentifier{
			TextDocumentIdentifier: protocol.TextDocumentIdentifier{URI: uri}, Version: 3,
		},
		ContentChanges: []any{protocol.TextDocumentContentChangeEventWhole{Text: "x := 1;\n"}},
	}); err != nil {
		t.Fatal(err)
	}
	if got, _ := s.documents.GetDocument(uri); got != "x := 1;\n" {
		t.Errorf("whole change: stored text %q", got)
	}

	if err := s.handleDidClose(ctx, &protocol.DidCloseTextDocumentParams{
		TextDocument: protocol.TextDocumentIdentifier{URI: uri},
	}); err != nil {
		t.Fatal(err)
	}
	if _, ok := s.documents.GetDocument(uri); ok {
		t.Error("close: document still stored")
	}
	if _, ok := s.documentVersion[uri]; ok {
		t.Error("close: version still tracked")
	}

	// A change for a document that is not open is ignored.
	if err := s.handleDidChange(ctx, &protocol.DidChangeTextDocumentParams{
		TextDocument: protocol.VersionedTextDocumentIdentifier{
			TextDocumentIdentifier: protocol.TextDocumentIdentifier{URI: uri}, Version: 4,
		},
		ContentChanges: []any{protocol.TextDocumentContentChangeEventWhole{Text: "y"}},
	}); err != nil {
		t.Fatal(err)
	}
	if _, ok := s.documents.GetDocument(uri); ok {
		t.Error("change after close re-created the document")
	}
}

func TestDocumentManagerCacheLifecycle(t *testing.T) {
	dm := NewDocumentManager()
	uri := "file:///test.cache.ssl"
	content := ":PROCEDURE Test;\n:DECLARE x;\n:ENDPROC;"

	dm.SetDocument(uri, content, 1)
	cache := dm.ParseDocument(uri, 1)
	if cache.Version != 1 {
		t.Fatalf("expected cache version 1, got %d", cache.Version)
	}
	if len(cache.Procedures) != 1 {
		t.Fatalf("expected 1 procedure, got %d", len(cache.Procedures))
	}
	if len(cache.Variables) != 1 {
		t.Fatalf("expected 1 variable, got %d", len(cache.Variables))
	}

	cached, ok := dm.GetCache(uri, 1)
	if !ok {
		t.Fatal("expected cache to be stored")
	}
	if cached != cache {
		t.Fatal("expected cached instance to match parse result")
	}

	dm.SetDocument(uri, content+"\nvalue := 1;", 2)
	if _, ok := dm.GetCache(uri, 1); ok {
		t.Fatal("expected cache invalidation after version change")
	}
}

func TestDocumentManagerParseMissingDocument(t *testing.T) {
	dm := NewDocumentManager()
	uri := "file:///missing.ssl"
	cache := dm.ParseDocument(uri, 1)
	if cache.Version != 1 {
		t.Fatalf("expected cache version 1, got %d", cache.Version)
	}
	if len(cache.Procedures) != 0 || len(cache.Variables) != 0 {
		t.Fatalf("expected empty cache, got %+v", cache)
	}
}

func TestDocumentManager(t *testing.T) {
	dm := NewDocumentManager()

	// Test SetDocument and GetDocument
	uri := "file:///test.ssl"
	content := ":PROCEDURE Test;\n:ENDPROC;"
	version := 1

	dm.SetDocument(uri, content, version)

	retrieved, ok := dm.GetDocument(uri)
	if !ok {
		t.Error("expected document to be found")
	}
	if retrieved != content {
		t.Errorf("expected %q, got %q", content, retrieved)
	}

	// Test AllDocuments
	docs := dm.AllDocuments()
	found := false
	for _, doc := range docs {
		if doc == uri {
			found = true
			break
		}
	}
	if !found {
		t.Error("expected URI to be in AllDocuments")
	}

	// Test RemoveDocument
	dm.RemoveDocument(uri)
	_, ok = dm.GetDocument(uri)
	if ok {
		t.Error("expected document to be removed")
	}
}

func TestDocumentManager_ParseDocument(t *testing.T) {
	dm := NewDocumentManager()

	uri := "file:///test.ssl"
	content := `:PROCEDURE Test;
:DECLARE x, y;
x := 1;
:ENDPROC;`
	version := 1

	dm.SetDocument(uri, content, version)

	cache := dm.ParseDocument(uri, version)

	if cache == nil {
		t.Fatal("expected non-nil cache")
	}

	// Should have parsed the procedure
	if len(cache.Procedures) == 0 {
		t.Error("expected at least one procedure")
	}

	// Should have parsed the variables
	if len(cache.Variables) == 0 {
		t.Error("expected at least one variable")
	}

	// Test cache hit - same version should return same result
	cache2 := dm.ParseDocument(uri, version)
	if cache2 != cache {
		t.Error("expected cache hit for same version")
	}

	// Test cache miss - different version should trigger reparse
	dm.SetDocument(uri, content+"// modified", version+1)
	cache3 := dm.ParseDocument(uri, version+1)
	if cache3 == cache {
		t.Error("expected cache miss for different version")
	}
}

func TestPtrTo(t *testing.T) {
	intVal := 42
	ptr := ptrTo(intVal)

	if ptr == nil {
		t.Fatal("expected non-nil pointer")
	}
	if *ptr != intVal {
		t.Errorf("expected %d, got %d", intVal, *ptr)
	}

	strVal := "test"
	strPtr := ptrTo(strVal)
	if *strPtr != strVal {
		t.Errorf("expected %q, got %q", strVal, *strPtr)
	}
}

// Test that formatting settings actually affect output
// [spec feature.formatting/A8] — configured ssl.format.* options change the
// output accordingly.
func TestSettingsAffectFormatting(t *testing.T) {
	input := `:PROCEDURE Test;:DECLARE x;x:=1;:ENDPROC;`

	// Test with tab indentation
	tabOpts := providers.DefaultFormattingOptions()
	tabOpts.IndentStyle = "tab"
	tabEdits := providers.FormatDocument(input, tabOpts)
	tabFormatted := tabEdits[0].NewText

	// Test with space indentation
	spaceOpts := providers.DefaultFormattingOptions()
	spaceOpts.IndentStyle = "space"
	spaceOpts.IndentSize = 4
	spaceEdits := providers.FormatDocument(input, spaceOpts)
	spaceFormatted := spaceEdits[0].NewText

	// They should be different
	if tabFormatted == spaceFormatted {
		t.Error("expected different formatting with different settings")
	}

	// Tab version should contain tabs
	if len(tabFormatted) > 0 {
		hasTab := false
		for _, c := range tabFormatted {
			if c == '\t' {
				hasTab = true
				break
			}
		}
		if !hasTab {
			t.Error("expected tab indentation in tab-formatted output")
		}
	}
}

func TestIsDataSourceURI(t *testing.T) {
	tests := []struct {
		uri  string
		want bool
	}{
		{"file:///project/query.ds", true},
		{"file:///project/query.ds.txt", true},
		{"file:///project/query.DS", true},
		{"file:///project/query.DS.TXT", true},
		{"file:///project/script.ssl", false},
		{"file:///project/script.ssl.txt", false},
		{"file:///project/script.srvscr", false},
		{"file:///project/code.go", false},
	}

	for _, tt := range tests {
		if got := isDataSourceURI(tt.uri); got != tt.want {
			t.Errorf("isDataSourceURI(%q) = %v, want %v", tt.uri, got, tt.want)
		}
	}
}

// [spec feature.diagnostics_pipeline/A8] — URI pattern activates endpoint
// mode; non-matching URIs stay non-endpoint.
func TestIsEndpointFile_PatternMatch(t *testing.T) {
	if !isEndpointFile("file:///work/Endpoints/Foo.ssl", "", []string{"/endpoints/"}) {
		t.Error("expected pattern /endpoints/ to match")
	}
	if isEndpointFile("file:///work/lib/Bar.ssl", "", []string{"/endpoints/"}) {
		t.Error("did not expect lib/Bar.ssl to match")
	}
}

// [spec feature.diagnostics_pipeline/A8] — leading-docblock Endpoint: marker.
func TestIsEndpointFile_DocblockMarker(t *testing.T) {
	content := `/*
 * Endpoint: GetUser
 * Description: returns the user
;
:DECLARE sUser;`
	if !isEndpointFile("file:///x.ssl", content, nil) {
		t.Error("expected docblock marker to activate endpoint mode")
	}
}

// Non-endpoint files must not get endpoint-ambient treatment.
// [spec feature.hover/A9]
func TestIsEndpointFile_NoSignal(t *testing.T) {
	content := `:DECLARE sValue;
	sValue := "ok";`
	if isEndpointFile("file:///x.ssl", content, nil) {
		t.Error("plain file should not be flagged as endpoint")
	}
}

// [spec feature.diagnostics_pipeline/A8] — a marker past the leading region
// must not activate endpoint mode.
func TestIsEndpointFile_MarkerOnlyInLeadingRegion(t *testing.T) {
	var b strings.Builder
	for range 50 {
		b.WriteString(":DECLARE x;\n")
	}
	b.WriteString("/* Endpoint: late ;\n")
	if isEndpointFile("file:///x.ssl", b.String(), nil) {
		t.Error("marker past the leading region should not activate endpoint mode")
	}
}

// Issue #9: signature-help auto-trigger is opt-in via
// ssl.intellisense.signatureHelp.autoTrigger. Default is off.
// [spec feature.signature_help/A6]
func TestSignatureHelpAutoTriggerSetting(t *testing.T) {
	s := NewSSLServer()
	if s.settings.SignatureHelpAutoTrigger {
		t.Fatal("expected default SignatureHelpAutoTrigger=false")
	}

	s.applySettings(map[string]interface{}{
		"ssl": map[string]interface{}{
			"intellisense": map[string]interface{}{
				"signatureHelp": map[string]interface{}{
					"autoTrigger": true,
				},
			},
		},
	})
	if !s.settings.SignatureHelpAutoTrigger {
		t.Error("expected SignatureHelpAutoTrigger=true after opt-in")
	}
}
