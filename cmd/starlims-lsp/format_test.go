package main

import (
	"testing"

	"starlims-lsp/internal/providers"
)

// [spec feature.formatting/A11] — the CLI's SQL-mode passthrough applies
// only to data sources (.ds/.ds.txt paths, or --ds). An ordinary script
// with no colon keyword and no `:=` is formatted as SSL, not mistaken for
// SQL by the data-source classifier (issue #255).
func TestFormatContent_SQLModeGateOnlyForDataSources(t *testing.T) {
	opts := providers.DefaultFormattingOptions()

	script := "usrmes( \"hi\" ) ;\n"
	if got := formatContent(script, opts, false); got != "UsrMes(\"hi\");\n" {
		t.Errorf("a marker-free script must be formatted as SSL, got %q", got)
	}

	sql := "SELECT a FROM t WHERE x = :bind\n"
	if got := formatContent(sql, opts, true); got != sql {
		t.Errorf("SQL-mode data-source content must pass through unchanged, got %q", got)
	}

	if !isDataSourcePath("Queries/Orders.DS.txt") || isDataSourcePath("lib/Helpers.ssl") {
		t.Error("data-source path detection must key on .ds/.ds.txt, case-insensitively")
	}
}
