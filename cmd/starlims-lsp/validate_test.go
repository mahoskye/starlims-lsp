package main

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// [spec feature.diagnostics_pipeline/A14] --validate on a .ds file applies
// the same data-source SQL-mode classification as the editor path: no SSL
// diagnostic fires on SQL content (issue #141).
func TestValidateFilePath_DataSourceSQLMode(t *testing.T) {
	dir := t.TempDir()

	// Hybrid shape: inline-defaults :PARAMETERS header, then raw SQL with a
	// dot-qualified column — the issue #141 repro.
	hybrid := ":PARAMETERS sMode := \"Post\";\nselect IO.OBJECT_NAME as name, @sMode\n"
	hybridPath := filepath.Join(dir, "hybrid.ds")
	if err := os.WriteFile(hybridPath, []byte(hybrid), 0o644); err != nil {
		t.Fatal(err)
	}
	result := validateFilePath(hybridPath, false)
	if !result.Valid {
		t.Errorf("hybrid .ds file should be valid, got diagnostics: %+v", result.Diagnostics)
	}
	for _, d := range result.Diagnostics {
		if strings.Contains(d.Message, "property access") {
			t.Errorf("dot_property_access fired on SQL body of .ds file: %s", d.Message)
		}
	}

	// Plain SQL document.
	plainPath := filepath.Join(dir, "plain.ds")
	if err := os.WriteFile(plainPath, []byte("SELECT T.COL1, T.COL2 FROM MYTABLE T WHERE T.ID = 1\n"), 0o644); err != nil {
		t.Fatal(err)
	}
	result = validateFilePath(plainPath, false)
	if len(result.Diagnostics) != 0 {
		t.Errorf("plain SQL .ds file should produce no diagnostics, got: %+v", result.Diagnostics)
	}

	// SSL-content .ds file keeps the full data-source diagnostic set:
	// a :DEFAULT statement is rejected in data sources.
	sslPath := filepath.Join(dir, "sslcontent.ds")
	if err := os.WriteFile(sslPath, []byte(":PARAMETERS sName;\n:DEFAULT sName, '';\n"), 0o644); err != nil {
		t.Fatal(err)
	}
	result = validateFilePath(sslPath, false)
	if result.Valid {
		t.Error("SSL-content .ds file with :DEFAULT statement should produce data-source diagnostics")
	}
}

// [spec feature.diagnostics_pipeline/A15] --ds classifies extensionless
// content (the stdin case) as a data-source document; without it the same
// content is an ordinary SSL document.
func TestValidateContent_DsFlag(t *testing.T) {
	content := ":PARAMETERS sMode := \"Post\";\nselect IO.OBJECT_NAME as name, @sMode\n"

	asDataSource := validateContent("stdin", content, true)
	if len(asDataSource.Diagnostics) != 0 {
		t.Errorf("with --ds, SQL-mode content should produce no diagnostics, got: %+v", asDataSource.Diagnostics)
	}

	asSSL := validateContent("stdin", content, false)
	if asSSL.Valid {
		t.Error("without --ds, the SQL body should produce SSL diagnostics (dot property access on IO.OBJECT_NAME)")
	}
}
