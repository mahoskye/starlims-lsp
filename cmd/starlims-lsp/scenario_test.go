package main

// The scenario suite (scenarios/README.md): each scenarios/**/*.txtar file
// holds one SSL input and the outcomes the CLI produces for it — formatted
// text and diagnostics under one or more flag profiles. The suite runs
// through the same functions --format and --validate use, so a scenario
// records exactly what a consumer sees.
//
//	go test ./cmd/starlims-lsp -run TestScenarios            # check
//	go test ./cmd/starlims-lsp -run TestScenarios -update    # regenerate
//
// -update rewrites every expected section from the current behavior and
// adds the default sections a scenario lacks; review the diff like code.
// Every scenario is also held to invariants that need no expected text
// (checkInvariants).

import (
	"bytes"
	"flag"
	"fmt"
	"os"
	"path/filepath"
	"regexp"
	"sort"
	"strings"
	"testing"
	"unicode"

	"starlims-lsp/internal/catalog"
	"starlims-lsp/internal/providers"
)

var updateScenarios = flag.Bool("update", false, "rewrite scenario expected sections from current behavior")

const scenarioRoot = "../../scenarios"

// defaultSections are the expected sections every scenario carries; -update
// adds any that are missing.
var defaultSections = []string{"format", "diagnostics", "diagnostics [--strict --info]"}

// txtarFile is one "-- name --" section of a scenario archive.
type txtarFile struct {
	name string
	data string
}

// scenario is a parsed .txtar archive: a description header, the input
// file, and the expected sections in file order.
type scenario struct {
	header string
	files  []txtarFile
}

// parseTxtar reads the txtar format (golang.org/x/tools/txtar): free text,
// then sections each opened by a line "-- name --".
func parseTxtar(data string) scenario {
	var sc scenario
	var cur *txtarFile
	var buf strings.Builder
	flush := func() {
		if cur == nil {
			sc.header = buf.String()
		} else {
			cur.data = buf.String()
			sc.files = append(sc.files, *cur)
		}
		buf.Reset()
	}
	for _, line := range strings.SplitAfter(data, "\n") {
		if name, ok := txtarMarker(line); ok {
			flush()
			cur = &txtarFile{name: name}
			continue
		}
		buf.WriteString(line)
	}
	flush()
	return sc
}

func txtarMarker(line string) (string, bool) {
	l := strings.TrimRight(line, "\r\n")
	if !strings.HasPrefix(l, "-- ") || !strings.HasSuffix(l, " --") || len(l) < 7 {
		return "", false
	}
	return strings.TrimSpace(l[3 : len(l)-3]), true
}

func (sc scenario) format() string {
	var b strings.Builder
	b.WriteString(sc.header)
	for _, f := range sc.files {
		fmt.Fprintf(&b, "-- %s --\n", f.name)
		b.WriteString(f.data)
		if f.data != "" && !strings.HasSuffix(f.data, "\n") {
			b.WriteString("\n")
		}
	}
	return b.String()
}

// sectionFlags splits a section name "kind [--flag value ...]" into its
// kind and CLI flags.
func sectionFlags(name string) (kind string, flags []string, err error) {
	kind = name
	if i := strings.Index(name, " ["); i >= 0 {
		if !strings.HasSuffix(name, "]") {
			return "", nil, fmt.Errorf("section %q: unclosed flag list", name)
		}
		kind = name[:i]
		flags = strings.Fields(name[i+2 : len(name)-1])
	}
	return kind, flags, nil
}

// isInputName reports whether a section is the scenario's input; its
// extension decides data-source classification, as a CLI path would.
func isInputName(name string) bool {
	return strings.HasPrefix(name, "input.")
}

// scenarioFormat runs --format on content with the given flags.
func scenarioFormat(content, inputName string, flags []string) (string, error) {
	opts, err := parseFormatArgs(append(append([]string{}, flags...), "--stdin"))
	if err != nil {
		return "", err
	}
	return formatContent(content, opts.fmt, opts.ds || isDataSourcePath(inputName)), nil
}

// scenarioDiagnostics runs --validate on content with the given flags and
// renders one line per diagnostic, sorted by position:
//
//	startLine:startCol-endLine:endCol severity code: message
//
// Positions are 1-based like the CLI's JSON.
func scenarioDiagnostics(content, inputName string, flags []string) (string, []providers.Diagnostic, error) {
	args, err := parseValidateArgs(append(append([]string{}, flags...), "--stdin"))
	if err != nil {
		return "", nil, err
	}
	vf := args.flags.withDataSource(args.flags.dataSource || isDataSourcePath(inputName))
	diags := providers.GetDiagnostics(content, diagnosticOptions(vf))
	sort.SliceStable(diags, func(i, j int) bool {
		a, b := diags[i], diags[j]
		if a.Range.Start.Line != b.Range.Start.Line {
			return a.Range.Start.Line < b.Range.Start.Line
		}
		if a.Range.Start.Character != b.Range.Start.Character {
			return a.Range.Start.Character < b.Range.Start.Character
		}
		if a.Code != b.Code {
			return a.Code < b.Code
		}
		return a.Message < b.Message
	})
	var b strings.Builder
	for _, d := range diags {
		fmt.Fprintf(&b, "%d:%d-%d:%d %s %s: %s\n",
			d.Range.Start.Line+1, d.Range.Start.Character+1,
			d.Range.End.Line+1, d.Range.End.Character+1,
			severityToString(d.Severity), d.Code, strings.ReplaceAll(d.Message, "\n", `\n`))
	}
	return b.String(), diags, nil
}

func TestScenarios(t *testing.T) {
	var paths []string
	err := filepath.WalkDir(scenarioRoot, func(p string, d os.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if !d.IsDir() && strings.HasSuffix(p, ".txtar") {
			paths = append(paths, p)
		}
		return nil
	})
	if err != nil {
		t.Fatalf("walking %s: %v", scenarioRoot, err)
	}
	if len(paths) == 0 {
		t.Fatal("no scenarios found")
	}
	for _, p := range paths {
		rel, _ := filepath.Rel(scenarioRoot, p)
		t.Run(filepath.ToSlash(strings.TrimSuffix(rel, ".txtar")), func(t *testing.T) {
			runScenario(t, p)
		})
	}
}

func runScenario(t *testing.T, path string) {
	raw, err := os.ReadFile(path)
	if err != nil {
		t.Fatal(err)
	}
	sc := parseTxtar(string(raw))
	if strings.TrimSpace(sc.header) == "" {
		t.Error("scenario has no description header: say what it covers and why")
	}
	if len(sc.files) == 0 || !isInputName(sc.files[0].name) {
		t.Fatal("first section must be the input (-- input.ssl --, -- input.ds --, ...)")
	}
	inputName, input := sc.files[0].name, sc.files[0].data

	if *updateScenarios {
		have := map[string]bool{}
		for _, f := range sc.files {
			have[f.name] = true
		}
		for _, name := range defaultSections {
			if !have[name] {
				sc.files = append(sc.files, txtarFile{name: name})
			}
		}
	}

	formatFlags := [][]string{nil}
	for i := 1; i < len(sc.files); i++ {
		f := &sc.files[i]
		kind, flags, err := sectionFlags(f.name)
		if err != nil {
			t.Fatal(err)
		}
		var got string
		switch kind {
		case "format":
			got, err = scenarioFormat(input, inputName, flags)
			if flags != nil {
				formatFlags = append(formatFlags, flags)
			}
		case "diagnostics":
			got, _, err = scenarioDiagnostics(input, inputName, flags)
		default:
			t.Fatalf("unknown section %q (want format or diagnostics, optionally with [flags])", f.name)
		}
		if err != nil {
			t.Fatalf("section %q: %v", f.name, err)
		}
		if *updateScenarios {
			f.data = got
			continue
		}
		if got != f.data {
			t.Errorf("section %q differs from expected (rerun with -update to accept)\n%s", f.name, lineDiff(f.data, got))
		}
	}

	if *updateScenarios {
		if out := sc.format(); out != string(raw) {
			if err := os.WriteFile(path, []byte(out), 0o644); err != nil {
				t.Fatal(err)
			}
		}
	}

	known := knownFailures(sc.header)
	failed := map[string]bool{}
	for _, flags := range formatFlags {
		for _, f := range checkInvariants(input, inputName, flags) {
			failed[f.invariant] = true
			if _, ok := known[f.invariant]; !ok {
				t.Error(f.message)
			}
		}
	}
	for inv, why := range known {
		if !invariantNames[inv] {
			t.Errorf("Known failure names unknown invariant %q (want one of %s)", inv, strings.Join(sortedKeys(invariantNames), ", "))
		} else if !failed[inv] {
			t.Errorf("invariant %q passes now (%s): remove its Known failure line", inv, why)
		}
	}
}

// invariantNames are the invariants a scenario header may mark as a
// known failure.
var invariantNames = map[string]bool{
	"idempotent": true, "whitespace-only": true, "no-gained-errors": true,
	"range-stable": true, "no-panic": true, "placeholders-verbatim": true,
}

// knownFailures reads the header's "Known failure: <invariant> — <why>"
// lines. A known failure is tolerated until it stops failing, when the
// runner demands the line's removal — the same ratchet as the catalog's
// expect=fail fences. <why> names the tracking issue.
func knownFailures(header string) map[string]string {
	out := map[string]string{}
	for _, line := range strings.Split(header, "\n") {
		rest, ok := strings.CutPrefix(strings.TrimSpace(line), "Known failure:")
		if !ok {
			continue
		}
		name, why, _ := strings.Cut(strings.TrimSpace(rest), " ")
		out[name] = strings.TrimLeft(strings.TrimSpace(why), "—- ")
	}
	return out
}

func sortedKeys(m map[string]bool) []string {
	var out []string
	for k := range m {
		out = append(out, k)
	}
	sort.Strings(out)
	return out
}

// invariantFailure is one broken invariant.
type invariantFailure struct {
	invariant string
	message   string
}

// checkInvariants holds every scenario to the contracts that need no
// expected text, under one set of format flags:
//   - nothing panics (a recovered panic surfaces as internal_error);
//   - formatting is idempotent (feature.formatting A6);
//   - formatting changes only whitespace, letter case and semicolons;
//   - named SQL placeholders (?name?) survive byte for byte;
//   - formatting introduces no error a validator did not already report;
//   - every line of the formatted document is a fixed point of range
//     formatting (feature.formatting A2).
func checkInvariants(input, inputName string, flags []string) []invariantFailure {
	var fails []invariantFailure
	fail := func(inv, format string, args ...any) {
		fails = append(fails, invariantFailure{inv, fmt.Sprintf(format, args...)})
	}
	label := "format"
	if flags != nil {
		label = "format [" + strings.Join(flags, " ") + "]"
	}
	once, err := scenarioFormat(input, inputName, flags)
	if err != nil {
		return []invariantFailure{{"no-panic", fmt.Sprintf("%s: %v", label, err)}}
	}
	twice, _ := scenarioFormat(once, inputName, flags)
	if twice != once {
		fail("idempotent", "%s: not idempotent\n%s", label, lineDiff(once, twice))
	}
	for _, ph := range namedPlaceholderRe.FindAllString(input, -1) {
		if !strings.Contains(once, ph) {
			fail("placeholders-verbatim", "%s: SQL placeholder %q was not kept verbatim", label, ph)
		}
	}
	if a, b := semanticSkeleton(input), semanticSkeleton(once); a != b {
		fail("whitespace-only", "%s: changed more than whitespace, case and semicolons\ninput skeleton:  %s\noutput skeleton: %s", label, a, b)
	}

	for _, profile := range [][]string{nil, {"--strict", "--info"}} {
		_, before, _ := scenarioDiagnostics(input, inputName, profile)
		_, after, _ := scenarioDiagnostics(once, inputName, profile)
		for _, d := range append(before, after...) {
			if d.Code == providers.CodeInternalError {
				fail("no-panic", "internal error (recovered panic): %s", d.Message)
			}
		}
		if gained := gainedErrors(before, after); len(gained) > 0 {
			fail("no-gained-errors", "%s: formatting introduced errors (profile %v): %s", label, profile, strings.Join(gained, ", "))
		}
	}

	opts, _ := parseFormatArgs(append(append([]string{}, flags...), "--stdin"))
	if (opts.ds || isDataSourcePath(inputName)) && providers.IsSQLModeDataSource(input) {
		return fails // SQL-mode data sources are never formatted
	}
	for i, line := range strings.Split(once, "\n") {
		if strings.TrimSpace(line) == "" {
			continue
		}
		edits := providers.FormatDocumentRange(once, i, 0, i, len([]rune(line)), opts.fmt)
		if len(edits) == 0 {
			continue
		}
		if got := applyEdit(once, edits[0]); got != once {
			fail("range-stable", "%s: range-formatting line %d of the formatted output changes it\n%s", label, i+1, lineDiff(once, got))
		}
	}
	return fails
}

// applyEdit applies a provider TextEdit (rune-based positions, like the
// providers package) to text.
func applyEdit(text string, e providers.TextEdit) string {
	offset := func(p providers.Position) int {
		lines := strings.SplitAfter(text, "\n")
		off := 0
		for l := 0; l < p.Line && l < len(lines); l++ {
			off += len(lines[l])
		}
		if p.Line < len(lines) {
			runes := []rune(strings.TrimSuffix(lines[p.Line], "\n"))
			c := min(p.Character, len(runes))
			off += len(string(runes[:c]))
		}
		return off
	}
	start, end := offset(e.Range.Start), offset(e.Range.End)
	if end < start {
		end = start
	}
	return text[:start] + e.NewText + text[end:]
}

// namedPlaceholderRe matches a SQLExecute named placeholder such as
// ?sName? or ?oRow:Id?: its interior reaches the database's parameter
// binding, so formatting must not touch a byte of it (fmt.sql_in_strings).
var namedPlaceholderRe = regexp.MustCompile(`\?[A-Za-z_][^?\n"']*\?`)

// semanticSkeleton reduces source to what formatting must preserve:
// everything but whitespace and semicolons, case-folded.
func semanticSkeleton(s string) string {
	return strings.Map(func(r rune) rune {
		if unicode.IsSpace(r) || r == ';' || r == '\uFEFF' {
			return -1
		}
		return r
	}, strings.ToUpper(s))
}

// gainedErrors lists error codes that occur more often after formatting.
func gainedErrors(before, after []providers.Diagnostic) []string {
	count := func(ds []providers.Diagnostic) map[string]int {
		m := map[string]int{}
		for _, d := range ds {
			if d.Severity == providers.SeverityError {
				m[d.Code]++
			}
		}
		return m
	}
	b, a := count(before), count(after)
	var out []string
	for code, n := range a {
		if n > b[code] {
			out = append(out, fmt.Sprintf("%s (%d -> %d)", code, b[code], n))
		}
	}
	sort.Strings(out)
	return out
}

// lineDiff renders a minimal line-oriented diff of want vs got.
func lineDiff(want, got string) string {
	w, g := strings.Split(want, "\n"), strings.Split(got, "\n")
	// Longest common subsequence table.
	lcs := make([][]int, len(w)+1)
	for i := range lcs {
		lcs[i] = make([]int, len(g)+1)
	}
	for i := len(w) - 1; i >= 0; i-- {
		for j := len(g) - 1; j >= 0; j-- {
			if w[i] == g[j] {
				lcs[i][j] = lcs[i+1][j+1] + 1
			} else if lcs[i+1][j] >= lcs[i][j+1] {
				lcs[i][j] = lcs[i+1][j]
			} else {
				lcs[i][j] = lcs[i][j+1]
			}
		}
	}
	var b bytes.Buffer
	i, j := 0, 0
	for i < len(w) || j < len(g) {
		switch {
		case i < len(w) && j < len(g) && w[i] == g[j]:
			i, j = i+1, j+1
		case j < len(g) && (i == len(w) || lcs[i][j+1] >= lcs[i+1][j]):
			fmt.Fprintf(&b, "+ %s\n", visible(g[j]))
			j++
		default:
			fmt.Fprintf(&b, "- %s\n", visible(w[i]))
			i++
		}
	}
	return b.String()
}

// visible shows tabs as → so indentation differences are readable.
func visible(s string) string { return strings.ReplaceAll(s, "\t", "→") }

const uncoveredCodesFile = "../../scenarios/uncovered_codes.txt"

// TestScenarioCoverage ratchets the suite toward every active diagnostic
// code. scenarios/uncovered_codes.txt lists the codes no scenario's
// expected diagnostics contain yet. A listed code that a scenario now
// covers must be removed, and a new code must either get a scenario or be
// listed; -update rewrites the file.
func TestScenarioCoverage(t *testing.T) {
	entries, err := catalog.Load(catalogDir)
	if err != nil {
		t.Fatal(err)
	}
	active := map[string]bool{}
	for _, e := range entries {
		if e.Kind == catalog.KindDiagnostic && e.Status == catalog.StatusActive {
			active[e.Slug()] = true
		}
	}

	covered := map[string]bool{}
	err = filepath.WalkDir(scenarioRoot, func(p string, d os.DirEntry, err error) error {
		if err != nil || d.IsDir() || !strings.HasSuffix(p, ".txtar") {
			return err
		}
		raw, err := os.ReadFile(p)
		if err != nil {
			return err
		}
		for _, f := range parseTxtar(string(raw)).files {
			if kind, _, _ := sectionFlags(f.name); kind != "diagnostics" {
				continue
			}
			for _, line := range strings.Split(f.data, "\n") {
				// "L:C-L:C severity code: message"
				if fields := strings.Fields(line); len(fields) >= 3 {
					covered[strings.TrimSuffix(fields[2], ":")] = true
				}
			}
		}
		return nil
	})
	if err != nil {
		t.Fatal(err)
	}

	var uncovered []string
	for code := range active {
		if !covered[code] {
			uncovered = append(uncovered, code)
		}
	}
	sort.Strings(uncovered)
	t.Logf("scenario coverage: %d of %d active diagnostic codes", len(active)-len(uncovered), len(active))

	if *updateScenarios {
		var b strings.Builder
		b.WriteString("# Active diagnostic codes that no scenario's expected diagnostics contain.\n")
		b.WriteString("# TestScenarioCoverage keeps this list exact; add a scenario, then rerun\n")
		b.WriteString("# go test ./cmd/starlims-lsp -run TestScenario -update to shrink it.\n")
		for _, c := range uncovered {
			b.WriteString(c + "\n")
		}
		if err := os.WriteFile(uncoveredCodesFile, []byte(b.String()), 0o644); err != nil {
			t.Fatal(err)
		}
		return
	}

	raw, err := os.ReadFile(uncoveredCodesFile)
	if err != nil {
		t.Fatalf("%v (create it with -update)", err)
	}
	listed := map[string]bool{}
	for _, line := range strings.Split(string(raw), "\n") {
		if line = strings.TrimSpace(line); line != "" && !strings.HasPrefix(line, "#") {
			listed[line] = true
		}
	}
	for _, c := range uncovered {
		if !listed[c] {
			t.Errorf("diagnostic code %s has no scenario: add one, or list it in %s", c, filepath.Base(uncoveredCodesFile))
		}
	}
	for c := range listed {
		if covered[c] {
			t.Errorf("%s is covered by a scenario now: remove it from %s", c, filepath.Base(uncoveredCodesFile))
		} else if !active[c] {
			t.Errorf("%s is not an active diagnostic code: remove it from %s", c, filepath.Base(uncoveredCodesFile))
		}
	}
}
