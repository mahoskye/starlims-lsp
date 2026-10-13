package main

import (
	"strings"
	"testing"

	"starlims-lsp/internal/constants"
)

// TestVersionTextNamesReferenceData pins the drift signal from issue #254:
// --version names the upstream commit the bundled SSL reference data came
// from, so a stale snapshot is visible to anyone who runs it.
func TestVersionTextNamesReferenceData(t *testing.T) {
	out := versionText()
	if !strings.HasPrefix(out, "starlims-lsp version ") {
		t.Errorf("first line must keep the established version format, got:\n%s", out)
	}
	ref := constants.DataProvenance.Reference
	want := "SSL reference data: " + ref.Repo + "@" + ref.ShortCommit() + " (" + ref.Date + ")"
	if !strings.Contains(out, want) {
		t.Errorf("expected --version to contain %q, got:\n%s", want, out)
	}
}
