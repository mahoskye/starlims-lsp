package main

import (
	"bufio"
	"bytes"
	"encoding/json"
	"os"
	"path/filepath"
	"reflect"
	"regexp"
	"slices"
	"sort"
	"strings"
	"testing"

	"starlims-lsp/internal/catalog"
	"starlims-lsp/internal/providers"
)

// The tests in this file enforce feature.cli_contract — the contract
// between the --validate/--format command line and the programs that
// gate or score SSL with it (issue #262).

const (
	contractEntry = "../../catalog/features/cli_contract.md"
	catalogDir    = "../../catalog"
	errorCodePin  = "testdata/error_codes.txt"
)

// cliRun captures one in-process invocation of a CLI mode.
type cliRun struct {
	stdout, stderr string
	code           int
}

func runValidateCLI(t *testing.T, stdin string, args ...string) cliRun {
	t.Helper()
	var out, errOut bytes.Buffer
	code := validateMain(args, strings.NewReader(stdin), &out, &errOut)
	return cliRun{out.String(), errOut.String(), code}
}

func runFormatCLI(t *testing.T, stdin string, args ...string) cliRun {
	t.Helper()
	var out, errOut bytes.Buffer
	code := formatMain(args, strings.NewReader(stdin), &out, &errOut)
	return cliRun{out.String(), errOut.String(), code}
}

// decodeResults parses --validate stdout generically, so the test sees
// the JSON a consumer sees rather than the Go structs.
func decodeResults(t *testing.T, stdout string) []map[string]any {
	t.Helper()
	var results []map[string]any
	if err := json.Unmarshal([]byte(stdout), &results); err != nil {
		t.Fatalf("stdout is not a JSON array of objects: %v\n%s", err, stdout)
	}
	return results
}

func writeFile(t *testing.T, dir, name, content string) string {
	t.Helper()
	path := filepath.Join(dir, name)
	if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(path, []byte(content), 0o644); err != nil {
		t.Fatal(err)
	}
	return path
}

const (
	cleanSSL = ":PROCEDURE Demo;\n:DECLARE sName;\nsName := \"x\";\n:RETURN sName;\n:ENDPROC;\n"
	// An unclosed :IF is an error; dot property access is an error too.
	invalidSSL = ":PROCEDURE Demo;\n:DECLARE oItem;\n:IF oItem.Name == \"x\";\n:ENDPROC;\n"
)

// [spec feature.cli_contract/A1] One array, one object per input, stdin
// first, then files in argument order, with the four documented fields.
func TestCLIContract_OutputShape(t *testing.T) {
	dir := t.TempDir()
	a := writeFile(t, dir, "a.ssl", cleanSSL)
	b := writeFile(t, dir, "sub/b.ssl", invalidSSL)

	// --stdin given last still reports first.
	run := runValidateCLI(t, cleanSSL, a, b, "--stdin")
	results := decodeResults(t, run.stdout)
	if len(results) != 3 {
		t.Fatalf("want 3 results, got %d", len(results))
	}
	for i, want := range []string{"stdin", "a.ssl", "b.ssl"} {
		if got := results[i]["file"]; got != want {
			t.Errorf("result %d: file = %v, want %q (stdin first, then argument order; base name only)", i, got, want)
		}
	}
	for i, r := range results {
		if _, ok := r["version"].(string); !ok {
			t.Errorf("result %d: version missing or not a string: %v", i, r["version"])
		}
		if _, ok := r["valid"].(bool); !ok {
			t.Errorf("result %d: valid missing or not a boolean: %v", i, r["valid"])
		}
		if _, ok := r["diagnostics"].([]any); !ok {
			t.Errorf("result %d: diagnostics missing, null, or not an array: %v", i, r["diagnostics"])
		}
	}
	if !strings.Contains(run.stdout, `"diagnostics": []`) {
		t.Errorf("a clean input must serialize diagnostics as [], not null:\n%s", run.stdout)
	}
}

// [spec feature.cli_contract/A2] Diagnostic objects: 1-based line and
// code-point column, a known severity, and a code on every ssl-lsp
// diagnostic.
func TestCLIContract_DiagnosticShape(t *testing.T) {
	// Two two-byte runes before the offending `x.y`: a byte-counted
	// column would be 16, a code-point column is 14.
	run := runValidateCLI(t, ":DECLARE sA;\nsA := \"éé\"; x.y := 1;\n", "--stdin", "--info", "--strict", "--hungarian")
	results := decodeResults(t, run.stdout)
	diags := results[0]["diagnostics"].([]any)
	if len(diags) == 0 {
		t.Fatal("expected diagnostics")
	}
	severities := map[string]bool{"error": true, "warning": true, "info": true, "hint": true}
	foundColumn := false
	for _, raw := range diags {
		d := raw.(map[string]any)
		line, lok := d["line"].(float64)
		col, cok := d["column"].(float64)
		if !lok || !cok || line < 1 || col < 1 || line != float64(int(line)) || col != float64(int(col)) {
			t.Errorf("line/column must be positive integers: %v", d)
		}
		if !severities[d["severity"].(string)] {
			t.Errorf("unknown severity %v", d["severity"])
		}
		if _, ok := d["message"].(string); !ok {
			t.Errorf("message missing: %v", d)
		}
		if d["source"] != "ssl-lsp" {
			t.Errorf("pipeline diagnostic source = %v, want ssl-lsp", d["source"])
		}
		if code, _ := d["code"].(string); code == "" {
			t.Errorf("ssl-lsp diagnostic without a code: %v", d)
		}
		if d["code"] == "dot_property_access" && line == 2 && col == 14 {
			foundColumn = true
		}
	}
	if !foundColumn {
		t.Errorf("expected dot_property_access at 2:14 (code-point column), got %v", diags)
	}
}

// [spec feature.cli_contract/A3] valid is exactly "no error-severity
// diagnostic", under every profile combination.
func TestCLIContract_ValidSemantics(t *testing.T) {
	inputs := []string{
		cleanSSL,
		invalidSSL,
		// Warnings and strict findings only: undeclared, unused, `#`.
		":PROCEDURE Demo;\n:DECLARE sName, sUnused, notes;\nsName := \"x\" + sTypo;\n:IF sName # \"\";\n:ENDIF;\n:RETURN sName;\n:ENDPROC;\n",
	}
	profiles := [][]string{
		nil, {"--info"}, {"--hungarian"}, {"--hungarian-types"}, {"--strict"},
		{"--info", "--hungarian", "--strict"},
	}
	for i, in := range inputs {
		var baseValid *bool
		for _, p := range profiles {
			run := runValidateCLI(t, in, append([]string{"--stdin"}, p...)...)
			r := decodeResults(t, run.stdout)[0]
			hasError := false
			for _, raw := range r["diagnostics"].([]any) {
				if raw.(map[string]any)["severity"] == "error" {
					hasError = true
				}
			}
			valid := r["valid"].(bool)
			if valid == hasError {
				t.Errorf("input %d %v: valid=%v but hasError=%v", i, p, valid, hasError)
			}
			if baseValid == nil {
				baseValid = &valid
			} else if valid != *baseValid {
				t.Errorf("input %d: profile %v flipped valid from %v", i, p, *baseValid)
			}
		}
	}
}

// [spec feature.cli_contract/A4] Exit 0 when every input is valid, 1
// when any is not; the JSON is written either way.
func TestCLIContract_ExitCodes(t *testing.T) {
	dir := t.TempDir()
	good := writeFile(t, dir, "good.ssl", cleanSSL)
	bad := writeFile(t, dir, "bad.ssl", invalidSSL)

	run := runValidateCLI(t, "", good)
	if run.code != exitOK {
		t.Errorf("all valid: exit %d, want 0", run.code)
	}
	decodeResults(t, run.stdout)

	run = runValidateCLI(t, "", good, bad)
	if run.code != exitFinding {
		t.Errorf("one invalid: exit %d, want 1", run.code)
	}
	if results := decodeResults(t, run.stdout); len(results) != 2 || results[1]["valid"] != false {
		t.Errorf("invalid run must still write every result: %v", results)
	}
}

// [spec feature.cli_contract/A5] An unreadable input is an ssl-validate
// error without a code, and the rest of the batch still runs.
func TestCLIContract_UnreadableInput(t *testing.T) {
	dir := t.TempDir()
	good := writeFile(t, dir, "good.ssl", cleanSSL)
	missing := filepath.Join(dir, "missing.ssl")

	run := runValidateCLI(t, "", missing, good)
	if run.code != exitFinding {
		t.Errorf("exit %d, want 1", run.code)
	}
	results := decodeResults(t, run.stdout)
	if len(results) != 2 {
		t.Fatalf("want 2 results, got %d", len(results))
	}
	r := results[0]
	diags := r["diagnostics"].([]any)
	if r["valid"] != false || len(diags) != 1 {
		t.Fatalf("unreadable input: want valid=false and one diagnostic, got %v", r)
	}
	d := diags[0].(map[string]any)
	if d["severity"] != "error" || d["source"] != "ssl-validate" {
		t.Errorf("read failure must be an ssl-validate error, got %v", d)
	}
	if _, has := d["code"]; has {
		t.Errorf("read failure must carry no code, got %v", d["code"])
	}
	if results[1]["valid"] != true {
		t.Errorf("the readable input must still be validated: %v", results[1])
	}
}

// [spec feature.cli_contract/A6] Unknown flags are usage errors, never
// file paths.
func TestCLIContract_UnknownFlagRejected(t *testing.T) {
	dir := t.TempDir()
	good := writeFile(t, dir, "good.ssl", cleanSSL)
	for _, args := range [][]string{
		{"--future-flag", good},
		{good, "--future-flag"},
		{"--stdin", "-x"},
		{"--strict", "--hungarian-type", good}, // a typo is unknown too
	} {
		run := runValidateCLI(t, cleanSSL, args...)
		bad := ""
		for _, a := range args {
			if isFlagLike(a) && a != "--stdin" && a != "--strict" {
				bad = a
			}
		}
		if run.code != exitUsage {
			t.Errorf("%v: exit %d, want 2", args, run.code)
		}
		if run.stdout != "" {
			t.Errorf("%v: usage error must write nothing to stdout, got %q", args, run.stdout)
		}
		if !strings.Contains(run.stderr, bad) || !strings.Contains(run.stderr, "unknown flag") {
			t.Errorf("%v: stderr must name the unknown flag %q, got %q", args, bad, run.stderr)
		}
	}
}

// [spec feature.cli_contract/A7] `--` ends flag parsing; a lone `-` is a
// path.
func TestCLIContract_EndOfFlags(t *testing.T) {
	dir := t.TempDir()
	dashed := writeFile(t, dir, "-dashed.ssl", cleanSSL)

	run := runValidateCLI(t, "", "--strict", "--", dashed)
	if run.code != exitOK {
		t.Fatalf("exit %d, stderr %q", run.code, run.stderr)
	}
	results := decodeResults(t, run.stdout)
	if len(results) != 1 || results[0]["file"] != "-dashed.ssl" || results[0]["valid"] != true {
		t.Errorf("a dash-prefixed path after -- must be validated as a file: %v", results)
	}

	// Parser-level: after --, even a known flag's spelling is a path.
	if parsed, err := parseValidateArgs([]string{"--", "--strict"}); err != nil || parsed.flags.strict || !slices.Equal(parsed.files, []string{"--strict"}) {
		t.Errorf("after --, --strict must be a file path: %+v %v", parsed, err)
	}
	if parsed, err := parseValidateArgs([]string{"-"}); err != nil || !slices.Equal(parsed.files, []string{"-"}) {
		t.Errorf("a lone - must be a file path, not a flag: %+v %v", parsed, err)
	}
	if _, err := parseValidateArgs([]string{"-dashed.ssl"}); err == nil {
		t.Error("a dash-prefixed argument before -- must be rejected as an unknown flag")
	}
}

// [spec feature.cli_contract/A8] No inputs is a usage error.
func TestCLIContract_NoInputs(t *testing.T) {
	for _, args := range [][]string{nil, {"--strict", "--info"}} {
		run := runValidateCLI(t, "", args...)
		if run.code != exitUsage || run.stdout != "" || !strings.Contains(run.stderr, "no input files") {
			t.Errorf("%v: want exit 2, empty stdout, usage error on stderr; got %d %q %q", args, run.code, run.stdout, run.stderr)
		}
	}
}

// [spec feature.cli_contract/A9] Every result carries the version
// --version reports.
func TestCLIContract_VersionField(t *testing.T) {
	saved := version
	version = "v9.9.9-contract"
	t.Cleanup(func() { version = saved })

	if !strings.Contains(versionLine(), " "+version+" ") {
		t.Fatalf("--version line %q does not report %q", versionLine(), version)
	}
	dir := t.TempDir()
	good := writeFile(t, dir, "good.ssl", cleanSSL)
	run := runValidateCLI(t, cleanSSL, "--stdin", good, filepath.Join(dir, "missing.ssl"))
	for i, r := range decodeResults(t, run.stdout) {
		if r["version"] != version {
			t.Errorf("result %d (%v): version = %v, want %q", i, r["file"], r["version"], version)
		}
	}
}

// [spec feature.cli_contract/A10] Flag position and repetition do not
// matter, and flags compose by union.
func TestCLIContract_FlagPositionAndComposition(t *testing.T) {
	dir := t.TempDir()
	content := ":PROCEDURE Demo;\n:DECLARE sName, sUnused;\nsName := \"x\" + sTypo;\n:IF sName # \"\";\n:ENDIF;\n:RETURN sName;\n:ENDPROC;\n"
	a := writeFile(t, dir, "a.ssl", content)
	b := writeFile(t, dir, "b.ssl", content)

	want := runValidateCLI(t, "", "--strict", "--info", a, b).stdout
	for _, args := range [][]string{
		{a, "--strict", b, "--info"},
		{a, b, "--info", "--strict"},
		{"--strict", a, "--strict", "--info", b, "--info"},
	} {
		if got := runValidateCLI(t, "", args...).stdout; got != want {
			t.Errorf("%v: output differs from flags-up-front run", args)
		}
	}

	codes := map[string]bool{}
	for _, raw := range decodeResults(t, want)[0]["diagnostics"].([]any) {
		codes[raw.(map[string]any)["code"].(string)] = true
	}
	for _, c := range []string{"undeclared_variable", "unused_variable", "not_preferred_operator"} {
		if !codes[c] {
			t.Errorf("--strict --info must deliver both profiles; %s missing (got %v)", c, codes)
		}
	}
}

// [spec feature.cli_contract/A11] Each profile flag switches on exactly
// its table row.
func TestCLIContract_ProfileTable(t *testing.T) {
	type toggles struct {
		dataSource, info, notation, types, undeclared, unused, sqlParams bool
	}
	read := func(o providers.DiagnosticOptions) toggles {
		return toggles{o.IsDataSourceFile, o.IncludeInfoDiagnostics, o.CheckHungarianNotation,
			o.CheckHungarianTypes, o.CheckUndeclaredVars, o.CheckUnusedVars, o.CheckSQLParams}
	}
	cases := map[string]toggles{
		"":                  {},
		"--ds":              {dataSource: true},
		"--info":            {info: true},
		"--hungarian":       {notation: true, types: true},
		"--hungarian-types": {types: true},
		"--strict":          {undeclared: true, unused: true, sqlParams: true},
	}
	defaults := providers.DefaultDiagnosticOptions()
	if read(defaults) != (toggles{}) {
		t.Fatalf("editor defaults changed under the profile table: %+v", read(defaults))
	}
	for flag, want := range cases {
		args := []string{"--stdin"}
		if flag != "" {
			args = append(args, flag)
		}
		parsed, err := parseValidateArgs(args)
		if err != nil {
			t.Fatal(err)
		}
		opts := diagnosticOptions(parsed.flags)
		if got := read(opts); got != want {
			t.Errorf("%q: toggles %+v, want %+v", flag, got, want)
		}
		// Everything outside the table stays at the editor default.
		opts.IsDataSourceFile, opts.IncludeInfoDiagnostics = defaults.IsDataSourceFile, defaults.IncludeInfoDiagnostics
		opts.CheckHungarianNotation, opts.CheckHungarianTypes = defaults.CheckHungarianNotation, defaults.CheckHungarianTypes
		opts.CheckUndeclaredVars, opts.CheckUnusedVars, opts.CheckSQLParams = defaults.CheckUndeclaredVars, defaults.CheckUnusedVars, defaults.CheckSQLParams
		if !reflect.DeepEqual(opts, defaults) {
			t.Errorf("%q changed an option outside the profile table", flag)
		}
	}
}

// [spec feature.cli_contract/A12] --format: unknown flags, `--`, --check
// exit codes, and usage errors.
func TestCLIContract_Format(t *testing.T) {
	dir := t.TempDir()
	raw := ":procedure Demo;\n:declare sName;\nsName:=\"x\";\n:endproc;\n"
	formatted := formatContent(raw, providers.DefaultFormattingOptions())
	if formatted == raw {
		t.Fatal("fixture must need formatting")
	}
	unformattedPath := writeFile(t, dir, "-unformatted.ssl", raw)
	formattedPath := writeFile(t, dir, "formatted.ssl", formatted)

	run := runFormatCLI(t, "", "--check", "--future-flag", formattedPath)
	if run.code != exitUsage || run.stdout != "" || !strings.Contains(run.stderr, "--future-flag") {
		t.Errorf("unknown flag: got %d %q %q", run.code, run.stdout, run.stderr)
	}

	if run = runFormatCLI(t, "", "--check", formattedPath); run.code != exitOK {
		t.Errorf("--check on a formatted file: exit %d, want 0 (%s)", run.code, run.stderr)
	}
	if run = runFormatCLI(t, "", "--check", "--", unformattedPath); run.code != exitFinding {
		t.Errorf("--check on an unformatted dash-named file after --: exit %d, want 1 (%s)", run.code, run.stderr)
	}
	if run = runFormatCLI(t, "", "--", unformattedPath); run.code != exitOK || run.stdout != formatted {
		t.Errorf("formatting a dash-named file after --: exit %d (%s)", run.code, run.stderr)
	}
	for _, args := range [][]string{{"--write", "--check", formattedPath}, {}, {"--stdin", formattedPath}} {
		if run = runFormatCLI(t, "", args...); run.code != exitUsage || run.stdout != "" {
			t.Errorf("%v: want usage error exit 2 with empty stdout, got %d %q", args, run.code, run.stdout)
		}
	}
}

// [spec feature.cli_contract/A13] The codes that can make valid false
// are pinned; changing the set is a D16 breaking change.
func TestCLIContract_ErrorCodePin(t *testing.T) {
	entries, err := catalog.Load(catalogDir)
	if err != nil {
		t.Fatal(err)
	}
	var live []string
	for _, e := range entries {
		if e.Kind == catalog.KindDiagnostic && e.Status == catalog.StatusActive && e.DefaultSeverity == "error" {
			live = append(live, e.Slug())
		}
	}
	sort.Strings(live)

	f, err := os.Open(errorCodePin)
	if err != nil {
		t.Fatal(err)
	}
	defer f.Close()
	var pinned []string
	sc := bufio.NewScanner(f)
	for sc.Scan() {
		line := strings.TrimSpace(sc.Text())
		if line != "" && !strings.HasPrefix(line, "#") {
			pinned = append(pinned, line)
		}
	}
	sort.Strings(pinned)

	if !slices.Equal(live, pinned) {
		added, removed := diffSets(live, pinned), diffSets(pinned, live)
		t.Errorf("error-severity code set changed — a BREAKING change to the CLI contract (DECISIONS.md D16).\n"+
			"  new error codes: %v\n  no longer error: %v\n"+
			"Ship it in a minor release with a CHANGELOG `### Breaking` note, then update %s.", added, removed, errorCodePin)
	}
}

// diffSets returns the members of a not in b.
func diffSets(a, b []string) []string {
	var out []string
	for _, x := range a {
		if !slices.Contains(b, x) {
			out = append(out, x)
		}
	}
	return out
}

// [spec feature.cli_contract/A14] The JSON field sets match the entry's
// tables exactly.
func TestCLIContract_FieldSets(t *testing.T) {
	text, err := os.ReadFile(contractEntry)
	if err != nil {
		t.Fatal(err)
	}
	for _, tc := range []struct {
		table string
		typ   reflect.Type
	}{
		{"Result object:", reflect.TypeOf(DiagnosticOutput{})},
		{"Diagnostic object:", reflect.TypeOf(DiagnosticDetail{})},
	} {
		documented := tableFields(string(text), tc.table)
		var actual []string
		for i := 0; i < tc.typ.NumField(); i++ {
			name, _, _ := strings.Cut(tc.typ.Field(i).Tag.Get("json"), ",")
			actual = append(actual, name)
		}
		sort.Strings(documented)
		sort.Strings(actual)
		if !slices.Equal(documented, actual) {
			t.Errorf("%s fields: entry documents %v, code emits %v — adding a field means documenting it; renaming or removing one is breaking (D16)",
				tc.table, documented, actual)
		}
	}
}

// tableFields returns the backticked first-column names of the markdown
// table that follows the line equal to heading.
func tableFields(text, heading string) []string {
	row := regexp.MustCompile("^\\| `([a-z_]+)` \\|")
	var out []string
	inTable := false
	lines := strings.Split(text, "\n")
	for i, line := range lines {
		if strings.TrimSpace(line) == heading {
			for _, l := range lines[i+1:] {
				if strings.HasPrefix(l, "|") {
					inTable = true
					if m := row.FindStringSubmatch(l); m != nil {
						out = append(out, m[1])
					}
				} else if inTable {
					break
				}
			}
			break
		}
	}
	return out
}

// [spec feature.cli_contract/A15] --ds classifies file arguments too,
// not only stdin.
func TestCLIContract_DsAppliesToFiles(t *testing.T) {
	dir := t.TempDir()
	// Hybrid data-source shape: a header, then raw SQL whose
	// table.column would be dot_property_access in SSL.
	path := writeFile(t, dir, "query.sql.txt", ":PARAMETERS sMode := \"Post\";\nselect T.NAME as name, @sMode\n")

	plain := decodeResults(t, runValidateCLI(t, "", path).stdout)[0]
	if plain["valid"] != false {
		t.Fatalf("without --ds the SQL body should fail as SSL: %v", plain)
	}
	asDS := runValidateCLI(t, "", "--ds", path)
	if r := decodeResults(t, asDS.stdout)[0]; r["valid"] != true || len(r["diagnostics"].([]any)) != 0 {
		t.Errorf("--ds must classify a file argument as a data source: %v", r)
	}
}
