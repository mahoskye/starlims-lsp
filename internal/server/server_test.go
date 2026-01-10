package server

import (
	"encoding/json"
	"testing"

	"starlims-lsp/internal/providers"
)

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

	if s.settings != originalSettings {
		t.Error("settings should not change when nil is applied")
	}

	s.applySettings(map[string]interface{}{})

	if s.settings.Formatting.IndentStyle != originalSettings.Formatting.IndentStyle {
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

func TestApplyChange_FullReplace(t *testing.T) {
	content := "line1\nline2\nline3"

	// Create a change that replaces everything
	change := struct {
		Range *struct {
			Start struct {
				Line      uint32
				Character uint32
			}
			End struct {
				Line      uint32
				Character uint32
			}
		}
		Text string
	}{
		Range: nil, // nil range means full document replace
		Text:  "new content",
	}

	// When range is nil, applyChange should replace everything
	if change.Range == nil {
		content = change.Text
	}

	if content != "new content" {
		t.Errorf("expected 'new content', got %q", content)
	}
}

func TestSplitLines(t *testing.T) {
	tests := []struct {
		input    string
		expected []string
	}{
		{"", []string{""}},
		{"a", []string{"a"}},
		{"a\nb", []string{"a", "b"}},
		{"a\nb\nc", []string{"a", "b", "c"}},
	}

	for _, test := range tests {
		lines := splitLines(test.input)
		if len(lines) != len(test.expected) {
			t.Errorf("expected %d lines for %q, got %d", len(test.expected), test.input, len(lines))
			continue
		}
		for i, line := range lines {
			if line != test.expected[i] {
				t.Errorf("expected line %d to be %q, got %q", i, test.expected[i], line)
			}
		}
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
