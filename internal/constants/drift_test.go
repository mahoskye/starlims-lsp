package constants

import (
	"bytes"
	"encoding/json"
	"os"
	"path/filepath"
	"regexp"
	"runtime"
	"testing"
	"time"
)

// Drift guards for the vendored element data (issue #123). The 446→460
// staleness went unnoticed because nothing compared the vendored JSONs to
// their canonical source or to each other. Three layers now do:
//
//   - TestVendoredDataMatchesSibling compares each file byte-for-byte
//     against the copy published by its producer in a sibling checkout:
//     the reference JSON against ssl-style-guide (whose generator writes
//     it), the meta JSON against starlims-ssl-reference (checked out as
//     ssl-docs, whose extractor writes it). The refresh is a plain cp, so
//     byte equality is the contract — mirroring ssl-style-guide's
//     check-reference-drift.mjs. Dev-machine guard only: each comparison
//     skips when its sibling checkout is absent, as in CI.
//
//   - TestVendoredDataInternallyConsistent is hermetic and always runs:
//     the vendored files must agree with their own totals blocks and with
//     the generated InventoryTotals (catches an edited JSON without
//     `go generate`, complementing CI's regen-freshness step).
//
//   - TestDataProvenance (issue #254) is hermetic and always runs: it
//     checks data/provenance.json is well-formed and logs which upstream
//     commit the data came from and how old it is, so staleness shows up
//     in `go test -v` output and not only as wrong hover text.

// repoRoot resolves the repository root from this source file's location.
func repoRoot(t *testing.T) string {
	t.Helper()
	_, file, _, ok := runtime.Caller(0)
	if !ok {
		t.Fatal("runtime.Caller failed")
	}
	return filepath.Dir(filepath.Dir(filepath.Dir(file)))
}

// vendoredSources maps each vendored file to the sibling checkout that
// publishes it: an env override, and a default path relative to this
// repository's root.
var vendoredSources = []struct {
	name       string
	envVar     string
	defaultDir []string
}{
	// Note the doubled directory: the ssl-style-guide repo holds a
	// same-named subdirectory containing the JSON.
	{"ssl-element-reference.json", "SSL_STYLE_GUIDE_DIR", []string{"..", "ssl-style-guide", "ssl-style-guide"}},
	{"ssl-element-meta.json", "SSL_DOCS_DIR", []string{"..", "ssl-docs", "content", "data"}},
}

func TestVendoredDataMatchesSibling(t *testing.T) {
	root := repoRoot(t)
	for _, src := range vendoredSources {
		canonicalDir := os.Getenv(src.envVar)
		if canonicalDir == "" {
			canonicalDir = filepath.Join(append([]string{root}, src.defaultDir...)...)
		}
		if _, err := os.Stat(canonicalDir); os.IsNotExist(err) {
			t.Logf("skipping %s: sibling checkout not found at %s (set %s to override)", src.name, canonicalDir, src.envVar)
			continue
		}
		canonical, err := os.ReadFile(filepath.Join(canonicalDir, src.name))
		if err != nil {
			t.Errorf("reading canonical %s: %v", src.name, err)
			continue
		}
		vendored, err := os.ReadFile(filepath.Join(root, "internal", "constants", "data", src.name))
		if err != nil {
			t.Errorf("reading vendored %s: %v", src.name, err)
			continue
		}
		if !bytes.Equal(canonical, vendored) {
			t.Errorf("vendored %s has drifted from the canonical copy (vendored from %s).\nRefresh per internal/constants/data/README.md:\n"+
				"  cp %s internal/constants/data/%s\n"+
				"  update internal/constants/data/provenance.json\n"+
				"  go generate ./internal/constants/...",
				src.name, DataProvenance.Reference, filepath.Join(canonicalDir, src.name), src.name)
		}
	}
}

func TestDataProvenance(t *testing.T) {
	commit := regexp.MustCompile(`^[0-9a-f]{40}$`)
	for _, entry := range []struct {
		label string
		u     UpstreamCommit
	}{
		{"reference", DataProvenance.Reference},
		{"generator", DataProvenance.Generator},
	} {
		if entry.u.Repo == "" {
			t.Errorf("provenance %s: repo is empty (is data/provenance.json well-formed?)", entry.label)
		}
		if !commit.MatchString(entry.u.Commit) {
			t.Errorf("provenance %s: commit %q is not a full 40-character hash", entry.label, entry.u.Commit)
		}
		date, err := time.Parse("2006-01-02", entry.u.Date)
		if err != nil {
			t.Errorf("provenance %s: date %q is not YYYY-MM-DD: %v", entry.label, entry.u.Date, err)
			continue
		}
		t.Logf("vendored element data %s: %s, %d days old", entry.label, entry.u, int(time.Since(date).Hours()/24))
	}
}

func TestVendoredDataInternallyConsistent(t *testing.T) {
	root := repoRoot(t)
	dataDir := filepath.Join(root, "internal", "constants", "data")

	// Reference file: every totals entry must match its category's key
	// count, and totals.all must be the category sum.
	raw, err := os.ReadFile(filepath.Join(dataDir, "ssl-element-reference.json"))
	if err != nil {
		t.Fatalf("reading vendored reference: %v", err)
	}
	var ref struct {
		Totals map[string]int `json:"totals"`
	}
	if err := json.Unmarshal(raw, &ref); err != nil {
		t.Fatalf("parsing vendored reference: %v", err)
	}
	var categories map[string]json.RawMessage
	if err := json.Unmarshal(raw, &categories); err != nil {
		t.Fatalf("parsing vendored reference categories: %v", err)
	}
	sum := 0
	for key, want := range ref.Totals {
		if key == "all" {
			continue
		}
		sum += want
		body, ok := categories[key]
		if !ok {
			t.Errorf("totals key %q has no matching top-level category", key)
			continue
		}
		var entries map[string]json.RawMessage
		if err := json.Unmarshal(body, &entries); err != nil {
			t.Errorf("category %q is not an object: %v", key, err)
			continue
		}
		if len(entries) != want {
			t.Errorf("category %q: totals say %d, file holds %d", key, want, len(entries))
		}
	}
	if all := ref.Totals["all"]; all != sum {
		t.Errorf("reference totals.all is %d but categories sum to %d", all, sum)
	}

	// Generated InventoryTotals must match the vendored totals (catches an
	// edited JSON without `go generate`).
	generated := map[string]int{
		"keywords":      InventoryTotals.Keywords,
		"operators":     InventoryTotals.Operators,
		"literals":      InventoryTotals.Literals,
		"types":         InventoryTotals.Types,
		"classes":       InventoryTotals.Classes,
		"special_forms": InventoryTotals.SpecialForms,
		"returns":       InventoryTotals.Returns,
		"functions":     InventoryTotals.Functions,
		"all":           InventoryTotals.All,
	}
	for key, got := range generated {
		if want, ok := ref.Totals[key]; ok && got != want {
			t.Errorf("InventoryTotals[%s]=%d but vendored JSON says %d — run go generate ./internal/constants/...", key, got, want)
		}
	}

	// Meta file: totals must match the elements array, and by_type must
	// tally the per-element types.
	rawMeta, err := os.ReadFile(filepath.Join(dataDir, "ssl-element-meta.json"))
	if err != nil {
		t.Fatalf("reading vendored meta: %v", err)
	}
	var meta struct {
		Totals struct {
			Elements int            `json:"elements"`
			ByType   map[string]int `json:"by_type"`
		} `json:"totals"`
		Elements []struct {
			ElementType string `json:"element_type"`
		} `json:"elements"`
	}
	if err := json.Unmarshal(rawMeta, &meta); err != nil {
		t.Fatalf("parsing vendored meta: %v", err)
	}
	if meta.Totals.Elements != len(meta.Elements) {
		t.Errorf("meta totals.elements is %d but elements array holds %d", meta.Totals.Elements, len(meta.Elements))
	}
	byType := map[string]int{}
	for _, e := range meta.Elements {
		byType[e.ElementType]++
	}
	for typ, want := range meta.Totals.ByType {
		if got := byType[typ]; got != want {
			t.Errorf("meta by_type[%s] says %d, elements tally %d", typ, want, got)
		}
	}
	if meta.Totals.Elements != InventoryTotals.All {
		t.Errorf("meta holds %d elements but reference inventory totals %d — the two vendored files are out of step", meta.Totals.Elements, InventoryTotals.All)
	}
}
