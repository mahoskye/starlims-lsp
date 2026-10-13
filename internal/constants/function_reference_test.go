package constants

import (
	"encoding/json"
	"os"
	"path/filepath"
	"regexp"
	"strings"
	"testing"
)

// TestFunctionSignaturesMatchReference enforces issue #252: every built-in
// function's parameter names, types, required-ness, and defaults are the
// vendored reference's, read straight from the JSON (not through the
// generator), so neither hand edits nor a generator bug can drift them.
// The only departures allowed are the documented overrides in
// function_overrides.go, and those may only add a parameter the published
// signature already names.
func TestFunctionSignaturesMatchReference(t *testing.T) {
	raw, err := os.ReadFile(filepath.Join(repoRoot(t), "internal", "constants", "data", "ssl-element-reference.json"))
	if err != nil {
		t.Fatal(err)
	}
	var ref struct {
		Functions map[string]struct {
			Signature string `json:"signature"`
			Returns   *struct {
				Type string `json:"type"`
			} `json:"returns"`
			Parameters []struct {
				Name     string `json:"name"`
				Type     string `json:"type"`
				Required string `json:"required"`
				Default  string `json:"default"`
			} `json:"parameters"`
		} `json:"functions"`
	}
	if err := json.Unmarshal(raw, &ref); err != nil {
		t.Fatal(err)
	}
	if len(ref.Functions) != len(SSLFunctionSignatures) {
		t.Errorf("reference has %d functions, SSLFunctionSignatures has %d", len(ref.Functions), len(SSLFunctionSignatures))
	}

	for name, fn := range ref.Functions {
		lower := strings.ToLower(name)
		sig, ok := SSLFunctionSignatures[lower]
		if !ok {
			t.Errorf("%s: missing from SSLFunctionSignatures", name)
			continue
		}
		if sig.Name != name {
			t.Errorf("%s: signature name is %q", name, sig.Name)
		}
		if fn.Returns != nil && sig.ReturnType != strings.Trim(fn.Returns.Type, "`") {
			t.Errorf("%s: return type %q, reference says %q", name, sig.ReturnType, fn.Returns.Type)
		}

		override := functionOverrides[lower]
		if len(sig.Parameters) != len(fn.Parameters)+len(override.appendParameters) {
			t.Errorf("%s: %d parameters, reference rows %d (+%d documented override)",
				name, len(sig.Parameters), len(fn.Parameters), len(override.appendParameters))
			continue
		}
		for i, row := range fn.Parameters {
			got := sig.Parameters[i]
			if got.Name != row.Name {
				t.Errorf("%s: parameter %d is %q, reference names it %q", name, i+1, got.Name, row.Name)
			}
			if got.Type != row.Type {
				t.Errorf("%s(%s): type %q, reference says %q", name, row.Name, got.Type, row.Type)
			}
			if got.Required != (row.Required == "yes") || got.Conditional != (row.Required == "conditional") {
				t.Errorf("%s(%s): required=%t conditional=%t, reference says %q", name, row.Name, got.Required, got.Conditional, row.Required)
			}
			wantDefault := row.Default
			if wantDefault == "—" || wantDefault == "omitted" {
				wantDefault = ""
			}
			if got.Default != wantDefault {
				t.Errorf("%s(%s): default %q, reference says %q", name, row.Name, got.Default, row.Default)
			}
		}
		for _, p := range override.appendParameters {
			if !regexp.MustCompile(`\b` + regexp.QuoteMeta(p.Name) + `\b`).MatchString(fn.Signature) {
				t.Errorf("%s: override appends %q, which the published signature %q does not name", name, p.Name, fn.Signature)
			}
		}
	}

	for lower, o := range functionOverrides {
		if strings.TrimSpace(o.why) == "" {
			t.Errorf("override %s must document why it exists", lower)
		}
		sig, ok := SSLFunctionSignatures[lower]
		if !ok {
			t.Errorf("override %s names no reference function", lower)
			continue
		}
		known := map[string]bool{}
		for _, p := range sig.Parameters {
			known[p.Name] = true
		}
		for _, f := range o.forms {
			for _, n := range f.Parameters {
				if !known[n] {
					t.Errorf("override %s: call form names %q, which is not a reference parameter", lower, n)
				}
			}
		}
	}
}
