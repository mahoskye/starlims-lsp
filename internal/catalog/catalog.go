// Package catalog loads and validates the behavior catalog under catalog/.
// The catalog is the normative source of truth for tool behavior; see
// catalog/README.md for the format specification. This package is consumed
// by the conformance and spec-runner tests, and by cmd/gen-catalog-docs.
//
//go:generate go run ../../cmd/gen-catalog-docs -root ../..
package catalog

import (
	"fmt"
	"os"
	"path/filepath"
	"regexp"
	"slices"
	"sort"
	"strings"

	"gopkg.in/yaml.v3"
)

// Kind values.
const (
	KindDiagnostic = "diagnostic"
	KindFeature    = "feature"
	KindFormatter  = "formatter"
)

// Status values.
const (
	StatusDraft   = "draft"
	StatusActive  = "active"
	StatusRemoved = "removed"
	StatusPlanned = "planned"
)

// Authority values.
const (
	AuthorityAuthoritative = "authoritative"
	AuthorityStyleOnly     = "style_only"
	AuthorityAdvisory      = "advisory"
	AuthorityTool          = "tool"
)

// HistoryEvent is one dated decision record on an entry.
type HistoryEvent struct {
	Date string `yaml:"date"`
	Ref  string `yaml:"ref"`
	Note string `yaml:"note"`
}

// Fence is one fenced code block from an entry body, tagged with the
// headings it appeared under and whether it is an expected failure.
type Fence struct {
	Section    string // nearest ### heading, e.g. "Flags", "Before"
	H2         string // nearest ## heading, e.g. "Examples", "Known gaps"
	Info       string // full fence info string, e.g. "ssl expect=fail"
	Code       string
	ExpectFail bool
	Line       int // 1-based line of the opening fence in the file
}

// Lang returns the fence's language token (the first word of the info
// string). Only "ssl" fences are executable spec; anything else is
// illustration.
func (f Fence) Lang() string {
	lang, _, _ := strings.Cut(f.Info, " ")
	return lang
}

// Criterion is one acceptance criterion from a feature entry.
type Criterion struct {
	Num     int    // the N in A<N>
	Text    string // full criterion text
	Planned bool   // ends with (planned); exempt from citation requirement
	Line    int
}

// Entry is one catalog entry: strict frontmatter plus parsed body structure.
type Entry struct {
	ID                  string         `yaml:"id"`
	Title               string         `yaml:"title"`
	Kind                string         `yaml:"kind"`
	Status              string         `yaml:"status"`
	Authority           string         `yaml:"authority"`
	SchemaRef           *string        `yaml:"schema_ref"`
	DefaultSeverity     string         `yaml:"default_severity"`
	Config              []string       `yaml:"config"`
	SeverityOverridable *bool          `yaml:"severity_overridable"`
	Suppressible        *bool          `yaml:"suppressible"`
	SpecOptions         map[string]any `yaml:"spec_options"`
	Tests               []string       `yaml:"tests"`
	History             []HistoryEvent `yaml:"history"`
	Issues              []string       `yaml:"issues"`

	// Derived from the body, not frontmatter.
	Path      string      `yaml:"-"`
	H2s       []string    `yaml:"-"`
	Fences    []Fence     `yaml:"-"`
	Criteria  []Criterion `yaml:"-"`
	BodyLines []string    `yaml:"-"`
}

// Slug returns the ID without its kind prefix (diag./feature./fmt.).
func (e *Entry) Slug() string {
	if _, after, found := strings.Cut(e.ID, "."); found {
		return after
	}
	return e.ID
}

// HasH2 reports whether the body contains the given ## heading.
func (e *Entry) HasH2(name string) bool {
	return slices.Contains(e.H2s, name)
}

// FencesIn returns the fences under the given ### section heading.
func (e *Entry) FencesIn(section string) []Fence {
	var out []Fence
	for _, f := range e.Fences {
		if f.Section == section {
			out = append(out, f)
		}
	}
	return out
}

// kindDirs maps entry kind to its directory and ID prefix.
var kindDirs = map[string]struct{ dir, prefix string }{
	KindDiagnostic: {"diagnostics", "diag."},
	KindFeature:    {"features", "feature."},
	KindFormatter:  {"formatting", "fmt."},
}

var validStatus = map[string]bool{StatusDraft: true, StatusActive: true, StatusRemoved: true, StatusPlanned: true}
var validAuthority = map[string]bool{AuthorityAuthoritative: true, AuthorityStyleOnly: true, AuthorityAdvisory: true, AuthorityTool: true}
var validSeverity = map[string]bool{"error": true, "warning": true, "info": true, "hint": true}

var criterionRe = regexp.MustCompile(`^-\s*A(\d+)\s*[:.]\s*(.*)$`)

// Load reads every entry under root (the catalog/ directory) and validates
// structural invariants: strict frontmatter, valid enums, ID/path/kind
// agreement. Prose-level requirements (headings, examples, history) are the
// conformance test's job because they depend on status.
func Load(root string) ([]Entry, error) {
	var entries []Entry
	var errs []string

	for kind, kd := range kindDirs {
		dir := filepath.Join(root, kd.dir)
		files, err := filepath.Glob(filepath.Join(dir, "*.md"))
		if err != nil {
			return nil, err
		}
		sort.Strings(files)
		for _, path := range files {
			e, err := parseFile(path)
			if err != nil {
				errs = append(errs, fmt.Sprintf("%s: %v", path, err))
				continue
			}
			if e.Kind != kind {
				errs = append(errs, fmt.Sprintf("%s: kind %q does not belong in %s/", path, e.Kind, kd.dir))
			}
			wantID := kd.prefix + strings.TrimSuffix(filepath.Base(path), ".md")
			if e.ID != wantID {
				errs = append(errs, fmt.Sprintf("%s: id %q does not match filename (want %q)", path, e.ID, wantID))
			}
			entries = append(entries, *e)
		}
	}

	sort.Slice(entries, func(i, j int) bool { return entries[i].ID < entries[j].ID })
	if len(errs) > 0 {
		return entries, fmt.Errorf("catalog validation failed:\n  %s", strings.Join(errs, "\n  "))
	}
	return entries, nil
}

func parseFile(path string) (*Entry, error) {
	raw, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	text := strings.ReplaceAll(string(raw), "\r\n", "\n")

	front, body, bodyStart, err := splitFrontmatter(text)
	if err != nil {
		return nil, err
	}

	var e Entry
	dec := yaml.NewDecoder(strings.NewReader(front))
	dec.KnownFields(true)
	if err := dec.Decode(&e); err != nil {
		return nil, fmt.Errorf("frontmatter: %v", err)
	}

	if e.ID == "" || e.Title == "" || e.Kind == "" || e.Status == "" || e.Authority == "" {
		return nil, fmt.Errorf("frontmatter: id, title, kind, status, and authority are required")
	}
	if _, ok := kindDirs[e.Kind]; !ok {
		return nil, fmt.Errorf("frontmatter: invalid kind %q", e.Kind)
	}
	if !validStatus[e.Status] {
		return nil, fmt.Errorf("frontmatter: invalid status %q", e.Status)
	}
	if !validAuthority[e.Authority] {
		return nil, fmt.Errorf("frontmatter: invalid authority %q", e.Authority)
	}
	if e.Kind == KindDiagnostic {
		if e.DefaultSeverity == "" || !validSeverity[e.DefaultSeverity] {
			return nil, fmt.Errorf("frontmatter: diagnostics require default_severity of error|warning|info|hint (got %q)", e.DefaultSeverity)
		}
	} else if e.DefaultSeverity != "" {
		return nil, fmt.Errorf("frontmatter: default_severity is only valid on diagnostics")
	}

	e.Path = path
	if err := parseBody(&e, body, bodyStart); err != nil {
		return nil, err
	}
	return &e, nil
}

func splitFrontmatter(text string) (front, body string, bodyStartLine int, err error) {
	if !strings.HasPrefix(text, "---\n") {
		return "", "", 0, fmt.Errorf("missing frontmatter (file must start with ---)")
	}
	rest := text[len("---\n"):]
	end := strings.Index(rest, "\n---\n")
	if end < 0 {
		return "", "", 0, fmt.Errorf("unterminated frontmatter")
	}
	front = rest[:end]
	body = rest[end+len("\n---\n"):]
	bodyStartLine = strings.Count(front, "\n") + 4 // opening ---, front lines, closing ---
	return front, body, bodyStartLine, nil
}

func parseBody(e *Entry, body string, startLine int) error {
	lines := strings.Split(body, "\n")
	e.BodyLines = lines

	var h2, h3 string
	inFence := false
	var fence *Fence

	for i, line := range lines {
		fileLine := startLine + i
		if inFence {
			if strings.HasPrefix(line, "```") {
				fence.Code = strings.TrimSuffix(fence.Code, "\n")
				e.Fences = append(e.Fences, *fence)
				inFence = false
				fence = nil
			} else {
				fence.Code += line + "\n"
			}
			continue
		}
		switch {
		case strings.HasPrefix(line, "## ") && !strings.HasPrefix(line, "###"):
			h2 = strings.TrimSpace(strings.TrimPrefix(line, "## "))
			h3 = ""
			e.H2s = append(e.H2s, h2)
		case strings.HasPrefix(line, "### "):
			h3 = strings.TrimSpace(strings.TrimPrefix(line, "### "))
		case strings.HasPrefix(line, "```"):
			info := strings.TrimSpace(strings.TrimPrefix(line, "```"))
			inFence = true
			fence = &Fence{
				Section:    h3,
				H2:         h2,
				Info:       info,
				ExpectFail: strings.Contains(info, "expect=fail"),
				Line:       fileLine,
			}
		case h2 == "Acceptance":
			if m := criterionRe.FindStringSubmatch(strings.TrimSpace(line)); m != nil {
				num := 0
				fmt.Sscanf(m[1], "%d", &num)
				text := strings.TrimSpace(m[2])
				e.Criteria = append(e.Criteria, Criterion{
					Num:     num,
					Text:    text,
					Planned: strings.HasSuffix(text, "(planned)"),
					Line:    fileLine,
				})
			}
		}
	}
	if inFence {
		// Without this, the pending fence and everything after it would be
		// silently swallowed — including executable spec fences.
		return fmt.Errorf("unterminated code fence opened at line %d", fence.Line)
	}
	return nil
}
