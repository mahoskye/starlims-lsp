package catalog

import (
	"fmt"
	"go/ast"
	"go/parser"
	"go/token"
	"io/fs"
	"os"
	"path/filepath"
	"regexp"
	"strings"
	"testing"
)

const catalogRoot = "../../catalog"
const repoRoot = "../.."
const diagnosticCodesFile = "../providers/diagnostic_codes.go"
const configurationDoc = "../../docs/configuration/CONFIGURATION.md"

func loadEntries(t *testing.T) []Entry {
	t.Helper()
	entries, err := Load(catalogRoot)
	if err != nil {
		t.Fatal(err)
	}
	if len(entries) == 0 {
		t.Fatal("catalog is empty — expected entries under catalog/")
	}
	return entries
}

// diagnosticCodeSlugs extracts every Code* constant value from
// diagnostic_codes.go via the AST, so renames and additions are caught
// without regex fragility.
func diagnosticCodeSlugs(t *testing.T) map[string]string {
	t.Helper()
	fset := token.NewFileSet()
	f, err := parser.ParseFile(fset, diagnosticCodesFile, nil, 0)
	if err != nil {
		t.Fatalf("parsing %s: %v", diagnosticCodesFile, err)
	}
	slugs := map[string]string{} // slug -> const name
	for _, decl := range f.Decls {
		gd, ok := decl.(*ast.GenDecl)
		if !ok || gd.Tok != token.CONST {
			continue
		}
		for _, spec := range gd.Specs {
			vs, ok := spec.(*ast.ValueSpec)
			if !ok {
				continue
			}
			for i, name := range vs.Names {
				if !strings.HasPrefix(name.Name, "Code") || i >= len(vs.Values) {
					continue
				}
				lit, ok := vs.Values[i].(*ast.BasicLit)
				if !ok || lit.Kind != token.STRING {
					continue
				}
				slug := strings.Trim(lit.Value, `"`)
				if prev, dup := slugs[slug]; dup {
					t.Errorf("duplicate diagnostic code %q (%s and %s)", slug, prev, name.Name)
				}
				slugs[slug] = name.Name
			}
		}
	}
	if len(slugs) == 0 {
		t.Fatal("no Code* constants found in diagnostic_codes.go")
	}
	return slugs
}

// TestSlugBijection: every diagnostic code constant has a catalog entry, and
// every diagnostic entry corresponds to a code constant — except that
// removed/planned entries must NOT exist in code (they record deliberate
// removals and not-yet-implemented rules).
func TestSlugBijection(t *testing.T) {
	entries := loadEntries(t)
	slugs := diagnosticCodeSlugs(t)

	byslug := map[string]Entry{}
	for _, e := range entries {
		if e.Kind == KindDiagnostic {
			byslug[e.Slug()] = e
		}
	}

	for slug, constName := range slugs {
		e, ok := byslug[slug]
		if !ok {
			t.Errorf("diagnostic code %q (%s) has no catalog entry (want catalog/diagnostics/%s.md)", slug, constName, slug)
			continue
		}
		if e.Status == StatusRemoved || e.Status == StatusPlanned {
			t.Errorf("%s: status is %q but code %s still exists in diagnostic_codes.go", e.Path, e.Status, constName)
		}
	}
	for slug, e := range byslug {
		if _, ok := slugs[slug]; !ok && e.Status != StatusRemoved && e.Status != StatusPlanned {
			t.Errorf("%s: no diagnostic code %q in diagnostic_codes.go (mark the entry removed/planned or add the code)", e.Path, slug)
		}
	}
}

// TestEntryLints enforces status-dependent prose requirements. Structural
// validity (frontmatter, enums, ID/path agreement) is enforced by Load.
func TestEntryLints(t *testing.T) {
	entries := loadEntries(t)

	for _, e := range entries {
		// The review ratchet reached zero on 2026-07-02: every entry has
		// been human-reviewed, so drafts are no longer permitted at all.
		if e.Status == StatusDraft {
			t.Errorf("%s: draft entries are no longer allowed — every entry is reviewed; use planned/active/removed", e.Path)
			continue
		}
		if e.Status == StatusRemoved || e.Status == StatusPlanned {
			// Removed/planned entries must at least explain themselves.
			if len(e.History) == 0 {
				t.Errorf("%s: %s entries require history explaining the decision", e.Path, e.Status)
			}
			continue
		}

		// Active entries: full requirements.
		if !e.HasH2("Behavior") {
			t.Errorf("%s: active entry missing '## Behavior'", e.Path)
		}
		if !e.HasH2("Rationale") {
			t.Errorf("%s: active entry missing '## Rationale'", e.Path)
		}
		if len(e.History) == 0 {
			t.Errorf("%s: active entry requires non-empty history", e.Path)
		}
		switch e.Kind {
		case KindDiagnostic:
			if !e.HasH2("Examples") {
				t.Errorf("%s: active diagnostic missing '## Examples'", e.Path)
			}
			if countExecutable(e.FencesIn("Flags")) == 0 {
				t.Errorf("%s: active diagnostic needs at least one non-expect=fail '### Flags' fence", e.Path)
			}
			if countExecutable(e.FencesIn("Does not flag")) == 0 {
				t.Errorf("%s: active diagnostic needs at least one non-expect=fail '### Does not flag' fence", e.Path)
			}
		case KindFormatter:
			if !e.HasH2("Examples") {
				t.Errorf("%s: active formatter entry missing '## Examples'", e.Path)
			}
			before, after := e.FencesIn("Before"), e.FencesIn("After")
			if len(before) == 0 || len(before) != len(after) {
				t.Errorf("%s: active formatter entry needs matched '### Before'/'### After' fence pairs (got %d/%d)", e.Path, len(before), len(after))
			}
		case KindFeature:
			if !e.HasH2("Acceptance") || len(e.Criteria) == 0 {
				t.Errorf("%s: active feature needs '## Acceptance' with at least one criterion (- A1: ...)", e.Path)
			}
			seen := map[int]bool{}
			for _, c := range e.Criteria {
				if seen[c.Num] {
					t.Errorf("%s:%d: duplicate acceptance criterion A%d", e.Path, c.Line, c.Num)
				}
				seen[c.Num] = true
			}
		}
	}

}

// countExecutable counts the fences the spec-runner will actually assert on
// as plain expectations: ssl-language, not expect=fail.
func countExecutable(fences []Fence) int {
	n := 0
	for _, f := range fences {
		if f.Lang() == "ssl" && !f.ExpectFail {
			n++
		}
	}
	return n
}

// TestConfigKeysDocumented: every user-facing setting a catalog entry claims
// to honor must appear in CONFIGURATION.md. This permanently prevents the
// implemented-but-undocumented settings drift.
func TestConfigKeysDocumented(t *testing.T) {
	entries := loadEntries(t)
	doc, err := os.ReadFile(configurationDoc)
	if err != nil {
		t.Fatal(err)
	}
	text := string(doc)
	for _, e := range entries {
		if e.Status == StatusDraft {
			continue
		}
		for _, key := range e.Config {
			if !strings.Contains(text, key) {
				t.Errorf("%s: config key %q is not documented in docs/configuration/CONFIGURATION.md", e.Path, key)
			}
		}
	}
}

// TestGeneratedDocsCurrent: docs/reference/DIAGNOSTICS.md and docs/STATUS.md
// are generated from the catalog by cmd/gen-catalog-docs; regenerate and
// compare so they can never drift. Run `go generate ./internal/catalog` to
// refresh them.
func TestGeneratedDocsCurrent(t *testing.T) {
	entries := loadEntries(t)
	for relPath, rendered := range GeneratedDocs(entries) {
		path := filepath.Join(repoRoot, relPath)
		onDisk, err := os.ReadFile(path)
		if err != nil {
			t.Errorf("%s: %v (run `go generate ./internal/catalog`)", path, err)
			continue
		}
		if string(onDisk) != rendered {
			t.Errorf("%s is stale — run `go generate ./internal/catalog`", path)
		}
	}
}

var specTagRe = regexp.MustCompile(`\[spec ((?:feature|diag|fmt)\.[a-z0-9_]+)/A(\d+)\]`)

// TestCriterionTraceability: every acceptance criterion on an active feature
// entry must be cited by a `[spec <id>/A<n>]` tag somewhere in a _test.go
// file, and every citation must resolve to a real criterion. Criteria marked
// (planned) are exempt until implemented.
func TestCriterionTraceability(t *testing.T) {
	entries := loadEntries(t)

	// Collect all [spec ...] tags in the repo's Go tests.
	cited := map[string]bool{} // "feature.folding/A2" -> true
	err := filepath.WalkDir(repoRoot, func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if d.IsDir() {
			name := d.Name()
			if name == ".git" || name == "bin" || name == "node_modules" || name == "dev" || name == "snapshots" {
				return filepath.SkipDir
			}
			return nil
		}
		if !strings.HasSuffix(path, "_test.go") {
			return nil
		}
		data, err := os.ReadFile(path)
		if err != nil {
			return err
		}
		for _, m := range specTagRe.FindAllStringSubmatch(string(data), -1) {
			cited[fmt.Sprintf("%s/A%s", m[1], m[2])] = true
		}
		return nil
	})
	if err != nil {
		t.Fatal(err)
	}

	valid := map[string]bool{}
	for _, e := range entries {
		for _, c := range e.Criteria {
			key := fmt.Sprintf("%s/A%d", e.ID, c.Num)
			valid[key] = true
			if e.Status != StatusActive || c.Planned {
				continue
			}
			if !cited[key] {
				t.Errorf("%s:%d: criterion A%d is not cited by any test (add a `[spec %s]` tag)", e.Path, c.Line, c.Num, key)
			}
		}
	}
	for key := range cited {
		if !valid[key] {
			t.Errorf("test tag [spec %s] does not match any catalog criterion", key)
		}
	}
}
