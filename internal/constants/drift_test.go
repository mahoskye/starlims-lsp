package constants

import (
	"bytes"
	"encoding/json"
	"os"
	"path/filepath"
	"runtime"
	"testing"
)

// Drift guards for the vendored element data (issue #123). The 446→460
// staleness went unnoticed because nothing compared the vendored JSONs to
// their canonical source or to each other. Two layers now do:
//
//   - TestVendoredDataMatchesSibling compares byte-for-byte against the
//     canonical copies in a sibling ssl-style-guide checkout (the documented
//     refresh is a plain cp, so byte equality is the contract — mirroring
//     ssl-style-guide's check-reference-drift.mjs). Dev-machine guard only:
//     it skips when the sibling checkout is absent, as in CI.
//
//   - TestVendoredDataInternallyConsistent is hermetic and always runs:
//     the vendored files must agree with their own totals blocks and with
//     the generated InventoryTotals (catches an edited JSON without
//     `go generate`, complementing CI's regen-freshness step).

var vendoredFiles = []string{"ssl-element-reference.json", "ssl-element-meta.json"}

// repoRoot resolves the repository root from this source file's location.
func repoRoot(t *testing.T) string {
	t.Helper()
	_, file, _, ok := runtime.Caller(0)
	if !ok {
		t.Fatal("runtime.Caller failed")
	}
	return filepath.Dir(filepath.Dir(filepath.Dir(file)))
}

func TestVendoredDataMatchesSibling(t *testing.T) {
	root := repoRoot(t)
	canonicalDir := os.Getenv("SSL_STYLE_GUIDE_DIR")
	if canonicalDir == "" {
		// Note the doubled directory: the ssl-style-guide repo holds a
		// same-named subdirectory containing the JSONs.
		canonicalDir = filepath.Join(root, "..", "ssl-style-guide", "ssl-style-guide")
	}
	if _, err := os.Stat(canonicalDir); os.IsNotExist(err) {
		t.Skipf("sibling ssl-style-guide checkout not found at %s (set SSL_STYLE_GUIDE_DIR to override)", canonicalDir)
	}

	for _, name := range vendoredFiles {
		canonical, err := os.ReadFile(filepath.Join(canonicalDir, name))
		if err != nil {
			t.Errorf("reading canonical %s: %v", name, err)
			continue
		}
		vendored, err := os.ReadFile(filepath.Join(root, "internal", "constants", "data", name))
		if err != nil {
			t.Errorf("reading vendored %s: %v", name, err)
			continue
		}
		if !bytes.Equal(canonical, vendored) {
			t.Errorf("vendored %s has drifted from the canonical copy.\nRefresh with:\n"+
				"  cp %s internal/constants/data/%s\n"+
				"  go generate ./internal/constants/...",
				name, filepath.Join(canonicalDir, name), name)
		}
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
