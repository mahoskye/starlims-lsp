package catalog

import (
	"reflect"
	"strings"
	"testing"

	"starlims-lsp/internal/providers"
)

// The spec-runner executes the catalog's examples against the real
// implementation. Only ```ssl fences run; other languages are illustration.
//
// Active diagnostics: every '### Flags' fence must produce the entry's
// code, every '### Does not flag' fence must not. Active formatter entries:
// each '### Before' fence must format equal to its paired '### After'
// fence; '### Idempotent' fences must format to themselves.
//
// Fences marked `expect=fail` (on either fence of a Before/After pair) run
// as expected failures — an unexpected pass breaks the build so a landed
// fix must promote its spec in the same PR. Planned entries run with every
// Flags/Before expectation treated as expect=fail: their behavior is
// specified but unimplemented, and the moment it starts passing the entry
// must be promoted. Draft and removed entries are skipped.

func TestDiagnosticSpecs(t *testing.T) {
	for _, e := range loadEntries(t) {
		if e.Kind != KindDiagnostic || (e.Status != StatusActive && e.Status != StatusPlanned) {
			continue
		}
		e := e
		t.Run(e.ID, func(t *testing.T) {
			slug := e.Slug()
			opts := providers.DefaultDiagnosticOptions()
			applySpecOptions(t, &e, &opts)
			planned := e.Status == StatusPlanned

			for _, f := range sslFences(e.FencesIn("Flags")) {
				fires := diagnosticFires(f.Code, opts, slug)
				expectFail := f.ExpectFail || planned
				reportSpec(t, &e, f, fires, expectFail,
					"expected code "+quote(slug)+" to fire",
					"code "+quote(slug)+" fired")
			}
			for _, f := range sslFences(e.FencesIn("Does not flag")) {
				fires := diagnosticFires(f.Code, opts, slug)
				// "Does not flag" holds trivially while the rule is
				// unimplemented, so planned status adds no xfail here.
				reportSpec(t, &e, f, !fires, f.ExpectFail,
					"expected code "+quote(slug)+" NOT to fire",
					"code "+quote(slug)+" did not fire")
			}
		})
	}
}

func TestFormatterSpecs(t *testing.T) {
	for _, e := range loadEntries(t) {
		if e.Kind != KindFormatter || (e.Status != StatusActive && e.Status != StatusPlanned) {
			continue
		}
		e := e
		t.Run(e.ID, func(t *testing.T) {
			opts := providers.DefaultFormattingOptions()
			applySpecOptions(t, &e, &opts)
			planned := e.Status == StatusPlanned

			before, after := sslFences(e.FencesIn("Before")), sslFences(e.FencesIn("After"))
			if len(before) != len(after) {
				t.Fatalf("%s: unmatched ssl Before/After fences (%d/%d)", e.Path, len(before), len(after))
			}
			for i, bf := range before {
				got, want := format(bf.Code, opts), after[i].Code
				expectFail := bf.ExpectFail || after[i].ExpectFail || planned
				reportSpec(t, &e, bf, got == want, expectFail,
					"Before/After mismatch\n--- got ---\n"+got+"\n--- want ---\n"+want,
					"Before/After pair matches")
				// Every After fence is also an idempotence fixture: formatted
				// output must be stable under a second pass (issue #103,
				// feature.formatting A6).
				stable := format(want, opts)
				reportSpec(t, &e, after[i], stable == want, expectFail,
					"After fence not idempotent\n--- reformatted ---\n"+stable+"\n--- want ---\n"+want,
					"After fence is stable")
			}
			for _, f := range sslFences(e.FencesIn("Idempotent")) {
				got := format(f.Code, opts)
				reportSpec(t, &e, f, got == f.Code, f.ExpectFail || planned,
					"Idempotent fence changed under formatting\n--- got ---\n"+got+"\n--- want ---\n"+f.Code,
					"Idempotent fence is stable")
			}
		})
	}
}

// reportSpec applies the shared pass/xfail logic: a plain expectation must
// hold; an expect=fail expectation must NOT hold yet, and starts failing
// the build the moment it passes.
func reportSpec(t *testing.T, e *Entry, f Fence, ok, expectFail bool, failMsg, xpassMsg string) {
	t.Helper()
	switch {
	case !ok && !expectFail:
		t.Errorf("%s:%d: %s\n%s", e.Path, f.Line, failMsg, indent(f.Code))
	case ok && expectFail:
		t.Errorf("%s:%d: expect=fail spec unexpectedly passes (%s) — the fix landed; promote the entry and drop expect=fail", e.Path, f.Line, xpassMsg)
	}
}

func sslFences(fences []Fence) []Fence {
	var out []Fence
	for _, f := range fences {
		if f.Lang() == "ssl" {
			out = append(out, f)
		}
	}
	return out
}

func diagnosticFires(code string, opts providers.DiagnosticOptions, slug string) bool {
	for _, d := range providers.GetDiagnostics(code, opts) {
		if d.Code == slug {
			return true
		}
	}
	return false
}

func format(text string, opts providers.FormattingOptions) string {
	edits := providers.FormatDocument(text, opts)
	if len(edits) == 0 {
		return text
	}
	// FormatDocument returns a single full-document replacement. Trim
	// exactly one trailing newline to mirror how fence content is captured
	// (the fence parser strips the newline before the closing ```), so
	// intentional trailing blank lines still compare.
	return strings.TrimSuffix(edits[0].NewText, "\n")
}

func quote(s string) string {
	return `"` + s + `"`
}

func indent(code string) string {
	return "    " + strings.ReplaceAll(code, "\n", "\n    ")
}

// applySpecOptions sets the entry's spec_options onto a DiagnosticOptions or
// FormattingOptions struct by (case-insensitive) snake_case field matching,
// so entries can enable opt-in checks like check_undeclared_vars or
// is_endpoint_file.
func applySpecOptions(t *testing.T, e *Entry, target any) {
	t.Helper()
	if len(e.SpecOptions) == 0 {
		return
	}
	v := reflect.ValueOf(target).Elem()
	vt := v.Type()
	for key, raw := range e.SpecOptions {
		camel := strings.ReplaceAll(key, "_", "")
		var field reflect.Value
		for i := 0; i < vt.NumField(); i++ {
			if strings.EqualFold(vt.Field(i).Name, camel) {
				field = v.Field(i)
				break
			}
		}
		if !field.IsValid() {
			t.Fatalf("%s: spec_options key %q matches no field on %s", e.Path, key, vt.Name())
		}
		switch field.Kind() {
		case reflect.Bool:
			b, ok := raw.(bool)
			if !ok {
				t.Fatalf("%s: spec_options %q wants bool", e.Path, key)
			}
			field.SetBool(b)
		case reflect.Int:
			n, ok := raw.(int)
			if !ok {
				t.Fatalf("%s: spec_options %q wants int", e.Path, key)
			}
			field.SetInt(int64(n))
		case reflect.String:
			s, ok := raw.(string)
			if !ok {
				t.Fatalf("%s: spec_options %q wants string", e.Path, key)
			}
			field.SetString(s)
		case reflect.Slice:
			if field.Type().Elem().Kind() != reflect.String {
				t.Fatalf("%s: spec_options %q: unsupported slice type", e.Path, key)
			}
			items, ok := raw.([]any)
			if !ok {
				t.Fatalf("%s: spec_options %q wants a string list", e.Path, key)
			}
			out := make([]string, len(items))
			for i, it := range items {
				s, ok := it.(string)
				if !ok {
					t.Fatalf("%s: spec_options %q wants a string list", e.Path, key)
				}
				out[i] = s
			}
			field.Set(reflect.ValueOf(out))
		default:
			t.Fatalf("%s: spec_options %q: unsupported field kind %s", e.Path, key, field.Kind())
		}
	}
}
