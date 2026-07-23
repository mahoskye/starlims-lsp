package providers

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// knownNonIdempotent lists corpus files that currently fail the format-twice
// contract (feature.formatting A6), keyed by filename with the open issue
// that tracks the fix. The harness ratchets: a listed file that becomes
// idempotent fails the build until its entry is removed — the same
// force-promotion mechanic as the catalog's expect=fail fences (issue #103).
var knownNonIdempotent = map[string]string{
	// (empty — every fixture is currently idempotent; add entries here with
	// their tracking issue when a change legitimately introduces a known
	// non-idempotent case)
}

// TestFormatIdempotenceCorpus formats every corpus fixture twice under the
// default options and requires byte-identical output — the contract that
// makes format-on-save safe (feature.formatting A6, issue #103).
// [spec feature.formatting/A6]
func TestFormatIdempotenceCorpus(t *testing.T) {
	dir := filepath.Join("testdata", "idempotence")
	entries, err := os.ReadDir(dir)
	if err != nil {
		t.Fatalf("reading corpus dir: %v", err)
	}
	if len(entries) == 0 {
		t.Fatal("idempotence corpus is empty")
	}

	opts := DefaultFormattingOptions()
	for _, e := range entries {
		if e.IsDir() || !strings.HasSuffix(e.Name(), ".ssl") {
			continue
		}
		t.Run(e.Name(), func(t *testing.T) {
			raw, err := os.ReadFile(filepath.Join(dir, e.Name()))
			if err != nil {
				t.Fatalf("reading fixture: %v", err)
			}

			once := formatAll(string(raw), opts)
			twice := formatAll(once, opts)
			issue, expectedFailure := knownNonIdempotent[e.Name()]

			if once == twice {
				if expectedFailure {
					t.Errorf("fixture is now idempotent — %s must be resolved: remove it from knownNonIdempotent", issue)
				}
				return
			}
			if expectedFailure {
				t.Skipf("known non-idempotent, tracked by %s", issue)
			}
			t.Errorf("format twice differs.\n--- first ---\n%s\n--- second ---\n%s", once, twice)
		})
	}
}

// formatAll runs the full-document formatter and returns the resulting text
// (input unchanged when the formatter returns no edits, e.g. SQL-mode
// data-source content).
func formatAll(text string, opts FormattingOptions) string {
	edits := FormatDocument(text, opts)
	if len(edits) == 0 {
		return text
	}
	return edits[0].NewText
}
