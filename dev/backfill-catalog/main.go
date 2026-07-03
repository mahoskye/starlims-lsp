// backfill-catalog is a one-shot generator that seeds catalog/diagnostics/
// with draft entries — one per Code* constant in diagnostic_codes.go. It
// infers authority + schema_ref from the schema's lints categories, detects
// default severity from emit sites, and prefills test-file references.
// Existing entry files are never overwritten. Run from the repo root:
//
//	go run ./dev/backfill-catalog
package main

import (
	"fmt"
	"go/ast"
	"go/parser"
	"go/token"
	"os"
	"path/filepath"
	"regexp"
	"sort"
	"strings"
)

const (
	codesFile  = "internal/providers/diagnostic_codes.go"
	schemaFile = "dev/ssl-style-guide/ssl-style-guide/ssl-style-guide.schema.yaml"
	outDir     = "catalog/diagnostics"
)

type code struct {
	constName string
	slug      string
}

func main() {
	codes := parseCodes()
	schemaCat, schemaLevel := parseSchemaLints()
	severities := detectSeverities(codes)
	testRefs := detectTests(codes)

	if err := os.MkdirAll(outDir, 0o755); err != nil {
		fatal(err)
	}

	created, skipped := 0, 0
	for _, c := range codes {
		path := filepath.Join(outDir, c.slug+".md")
		if _, err := os.Stat(path); err == nil {
			skipped++
			continue
		}

		authority := "tool"
		schemaRef := "null"
		if cat, ok := schemaCat[c.slug]; ok {
			schemaRef = fmt.Sprintf("lints.%s.%s", cat, c.slug)
			if lvl, ok := schemaLevel[cat]; ok {
				authority = lvl
			} else {
				authority = "advisory"
			}
		}
		severity := severities[c.slug]
		if severity == "" {
			severity = "warning"
		}

		tests := testRefs[c.slug]
		testsYAML := "tests: []"
		if len(tests) > 0 {
			var b strings.Builder
			b.WriteString("tests:")
			for _, tf := range tests {
				b.WriteString("\n  - " + tf)
			}
			testsYAML = b.String()
		}

		content := fmt.Sprintf(`---
id: diag.%s
title: %s
kind: diagnostic
status: draft
authority: %s
schema_ref: %s
default_severity: %s
severity_overridable: true
suppressible: true
%s
history: []
issues: []
---

## Behavior

TODO: normative statement — what this rule flags, and the boundaries of what
it must not flag.

## Examples

### Flags

`+"```ssl"+`
/* TODO: minimal SSL that must produce %s; */
`+"```"+`

### Does not flag

`+"```ssl"+`
/* TODO: nearby-but-valid SSL that must NOT produce %s; */
`+"```"+`

## Rationale

TODO: why this behavior and this severity; cite history refs.
`, c.slug, humanize(c.slug), authority, schemaRef, severity, testsYAML, c.slug, c.slug)

		if err := os.WriteFile(path, []byte(content), 0o644); err != nil {
			fatal(err)
		}
		created++
	}
	fmt.Printf("codes: %d, created: %d, skipped (already exist): %d\n", len(codes), created, skipped)
}

func parseCodes() []code {
	fset := token.NewFileSet()
	f, err := parser.ParseFile(fset, codesFile, nil, 0)
	if err != nil {
		fatal(err)
	}
	var codes []code
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
				codes = append(codes, code{name.Name, strings.Trim(lit.Value, `"`)})
			}
		}
	}
	return codes
}

// parseSchemaLints scans the schema's lints section for `- rule: <slug>`
// entries, returning slug->category and category->level maps.
func parseSchemaLints() (map[string]string, map[string]string) {
	data, err := os.ReadFile(schemaFile)
	if err != nil {
		fmt.Fprintf(os.Stderr, "warning: cannot read schema (%v); all entries get authority: tool\n", err)
		return map[string]string{}, map[string]string{}
	}
	slugCat := map[string]string{}
	catLevel := map[string]string{}
	catRe := regexp.MustCompile(`^    (\w+):\s*$`)
	levelRe := regexp.MustCompile(`^      level:\s*(\w+)`)
	ruleRe := regexp.MustCompile(`^      - rule:\s*(\w+)`)

	inLints := false
	cat := ""
	for _, line := range strings.Split(string(data), "\n") {
		switch {
		case strings.HasPrefix(line, "  lints:"):
			inLints = true
		case len(line) > 2 && line[0] != ' ' || regexp.MustCompile(`^  \w`).MatchString(line):
			if inLints && !strings.HasPrefix(line, "  lints:") {
				inLints = false
			}
		}
		if !inLints {
			continue
		}
		if m := catRe.FindStringSubmatch(line); m != nil {
			cat = m[1]
		} else if m := levelRe.FindStringSubmatch(line); m != nil && cat != "" {
			catLevel[cat] = m[1]
		} else if m := ruleRe.FindStringSubmatch(line); m != nil && cat != "" {
			slugCat[m[1]] = cat
		}
	}
	return slugCat, catLevel
}

// detectSeverities finds `Code: <Const>` emit sites in providers/*.go and
// takes the nearest preceding `Severity: Severity<X>` within the same
// composite literal. When emit sites disagree, the most common wins.
func detectSeverities(codes []code) map[string]string {
	files, _ := filepath.Glob("internal/providers/*.go")
	sevRe := regexp.MustCompile(`Severity:\s+Severity(\w+)`)

	counts := map[string]map[string]int{}
	for _, path := range files {
		if strings.HasSuffix(path, "_test.go") {
			continue
		}
		data, err := os.ReadFile(path)
		if err != nil {
			continue
		}
		lines := strings.Split(string(data), "\n")
		for _, c := range codes {
			codeRe := regexp.MustCompile(`Code:\s+` + c.constName + `\b`)
			for i, line := range lines {
				if !codeRe.MatchString(line) {
					continue
				}
				// Look up to 10 lines back for the Severity field.
				for j := i; j >= 0 && j > i-10; j-- {
					if m := sevRe.FindStringSubmatch(lines[j]); m != nil {
						sev := strings.ToLower(m[1])
						if sev == "information" {
							sev = "info"
						}
						if counts[c.slug] == nil {
							counts[c.slug] = map[string]int{}
						}
						counts[c.slug][sev]++
						break
					}
				}
			}
		}
	}

	out := map[string]string{}
	for slug, byName := range counts {
		best, bestN := "", 0
		for sev, n := range byName {
			if n > bestN {
				best, bestN = sev, n
			}
		}
		out[slug] = best
	}
	return out
}

// detectTests lists provider test files that mention each code's const name
// or slug literal.
func detectTests(codes []code) map[string][]string {
	files, _ := filepath.Glob("internal/providers/*_test.go")
	out := map[string][]string{}
	for _, path := range files {
		data, err := os.ReadFile(path)
		if err != nil {
			continue
		}
		text := string(data)
		for _, c := range codes {
			if strings.Contains(text, c.constName) || strings.Contains(text, `"`+c.slug+`"`) {
				out[c.slug] = append(out[c.slug], path)
			}
		}
	}
	for slug := range out {
		sort.Strings(out[slug])
	}
	return out
}

func humanize(slug string) string {
	s := strings.ReplaceAll(slug, "_", " ")
	if len(s) > 0 {
		s = strings.ToUpper(s[:1]) + s[1:]
	}
	return s
}

func fatal(err error) {
	fmt.Fprintln(os.Stderr, err)
	os.Exit(1)
}
