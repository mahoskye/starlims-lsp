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
	result := validateFilePath(hybridPath, validateFlags{})
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
	result = validateFilePath(plainPath, validateFlags{})
	if len(result.Diagnostics) != 0 {
		t.Errorf("plain SQL .ds file should produce no diagnostics, got: %+v", result.Diagnostics)
	}

	// SSL-content .ds file keeps the full data-source diagnostic set:
	// a :DEFAULT statement is rejected in data sources.
	sslPath := filepath.Join(dir, "sslcontent.ds")
	if err := os.WriteFile(sslPath, []byte(":PARAMETERS sName;\n:DEFAULT sName, '';\n"), 0o644); err != nil {
		t.Fatal(err)
	}
	result = validateFilePath(sslPath, validateFlags{})
	if result.Valid {
		t.Error("SSL-content .ds file with :DEFAULT statement should produce data-source diagnostics")
	}
}

// [spec feature.diagnostics_pipeline/A15] --ds classifies extensionless
// content (the stdin case) as a data-source document; without it the same
// content is an ordinary SSL document.
func TestValidateContent_DsFlag(t *testing.T) {
	content := ":PARAMETERS sMode := \"Post\";\nselect IO.OBJECT_NAME as name, @sMode\n"

	asDataSource := validateContent("stdin", content, validateFlags{dataSource: true})
	if len(asDataSource.Diagnostics) != 0 {
		t.Errorf("with --ds, SQL-mode content should produce no diagnostics, got: %+v", asDataSource.Diagnostics)
	}

	asSSL := validateContent("stdin", content, validateFlags{})
	if asSSL.Valid {
		t.Error("without --ds, the SQL body should produce SSL diagnostics (dot property access on IO.OBJECT_NAME)")
	}
}

// [spec feature.diagnostics_pipeline/A28] The opt-in diagnostic options
// --validate exposes are off unless their flag is passed.
func TestValidateOptInFlags(t *testing.T) {
	// `#` is a not-preferred operator (info severity); nCode := SubStr(...)
	// promises a number and produces a string (hungarian_type_mismatch),
	// and `notes` carries no recognized prefix (hungarian_notation).
	content := `:PROCEDURE Demo;
:DECLARE nCode, sText, notes;
nCode := SubStr(sText, 1, 4);
:IF nCode # 2;
:ENDIF;
:ENDPROC;
`
	codes := func(flags validateFlags) map[string]bool {
		out := map[string]bool{}
		for _, d := range validateContent("stdin", content, flags).Diagnostics {
			out[d.Code] = true
		}
		return out
	}

	base := codes(validateFlags{})
	for _, code := range []string{"not_preferred_operator", "hungarian_type_mismatch", "hungarian_notation"} {
		if base[code] {
			t.Errorf("%s fired without its flag — CLI defaults must match the editor defaults", code)
		}
	}

	if !codes(validateFlags{includeInfo: true})["not_preferred_operator"] {
		t.Error("--info did not deliver info-severity diagnostics")
	}

	withHungarian := codes(validateFlags{hungarian: true})
	for _, code := range []string{"hungarian_type_mismatch", "hungarian_notation"} {
		if !withHungarian[code] {
			t.Errorf("--hungarian did not enable %s", code)
		}
	}
	if withHungarian["not_preferred_operator"] {
		t.Error("--hungarian must not imply --info")
	}

	// --hungarian-types takes the correctness check without the
	// convention audit, which is what a consumer reviewing code that does
	// not use the convention wants.
	typesOnly := codes(validateFlags{hungarianTypes: true})
	if !typesOnly["hungarian_type_mismatch"] {
		t.Error("--hungarian-types did not enable hungarian_type_mismatch")
	}
	if typesOnly["hungarian_notation"] {
		t.Error("--hungarian-types must not enable hungarian_notation")
	}
}
