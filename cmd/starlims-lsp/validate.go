package main

import (
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"strings"

	"starlims-lsp/internal/providers"
)

// DiagnosticOutput represents the JSON output format for one input's
// diagnostics. The shape is a public contract (feature.cli_contract):
// fields may be added, but never renamed, retyped, or removed outside a
// breaking release (DECISIONS.md D16).
type DiagnosticOutput struct {
	File string `json:"file"`
	// Version is the build-stamped tool version — the value --version
	// prints — so a consumer's logs show which binary produced a result.
	// It repeats on every per-input object instead of living in a
	// wrapper object because the document's top level is an array that
	// existing consumers index directly; wrapping it would break them
	// (issue #262).
	Version     string             `json:"version"`
	Valid       bool               `json:"valid"`
	Diagnostics []DiagnosticDetail `json:"diagnostics"`
}

// DiagnosticDetail represents a single diagnostic in the output.
type DiagnosticDetail struct {
	Line     int    `json:"line"`
	Column   int    `json:"column"`
	Severity string `json:"severity"`
	Message  string `json:"message"`
	Source   string `json:"source"`
	Code     string `json:"code,omitempty"`
}

// validateFlags are the CLI switches that shape a validate run. The
// opt-in diagnostic options are exposed here because --validate serves
// agent skills and CI, where the caller — not a user settings file —
// decides how much detail it wants.
type validateFlags struct {
	// dataSource forces data-source classification (--ds).
	dataSource bool
	// includeInfo delivers the opt-in info advisory tier (--info).
	includeInfo bool
	// hungarian enables both Hungarian checks (--hungarian):
	// hungarian_notation and hungarian_type_mismatch.
	hungarian bool
	// hungarianTypes enables only the type cross-check
	// (--hungarian-types): hungarian_type_mismatch, without the
	// convention audit that hungarian_notation performs.
	hungarianTypes bool
	// strict enables the three provider checks the editor leaves off
	// (--strict): undeclared_variable, unused_variable, and
	// invalid_sql_param. They are off by default because an editor
	// reports them continuously while a human is mid-edit, when a name
	// is legitimately undeclared or unused for the next few keystrokes.
	// A non-interactive consumer — an agent or a CI gate — sees only
	// finished code, where each of the three is a defect rather than a
	// transient state, so it wants all three (issue #249).
	strict bool
}

// withDataSource returns a copy with dataSource set, so a file path's
// own extension can classify it without mutating the caller's flags.
func (f validateFlags) withDataSource(on bool) validateFlags {
	f.dataSource = on
	return f
}

// Exit codes shared by the --validate and --format modes
// (feature.cli_contract): 0 success, 1 a finding (an invalid or
// unreadable input, or a file --format --check would change), 2 a usage
// error (unknown flag, bad flag value, no inputs) — in which case
// nothing is written to stdout.
const (
	exitOK      = 0
	exitFinding = 1
	exitUsage   = 2
)

// errHelp is returned by an argument parser when --help/-h was passed:
// the caller prints the mode's help to stdout and exits 0.
var errHelp = errors.New("help requested")

// isFlagLike reports whether a command-line argument is shaped like a
// flag. A lone "-" is not: it is an ordinary (if unusual) path.
func isFlagLike(arg string) bool {
	return len(arg) > 1 && arg[0] == '-'
}

// validateArgs is the parsed command line of a --validate run.
type validateArgs struct {
	stdin bool
	flags validateFlags
	files []string
}

// parseValidateArgs parses --validate's flags and file arguments. Flags
// may appear anywhere among the files and compose by union. A
// dash-prefixed argument it does not know is a usage error, never a file
// path: a consumer passing a flag this binary predates must fail loudly
// rather than receive a phantom file entry beside a silently different
// result (issue #262). A bare `--` ends flag parsing, so a file whose
// name starts with a dash goes after it.
func parseValidateArgs(args []string) (validateArgs, error) {
	out := validateArgs{files: make([]string, 0, len(args))}
	for i, arg := range args {
		if arg == "--" {
			out.files = append(out.files, args[i+1:]...)
			break
		}
		switch arg {
		case "--help", "-h":
			return out, errHelp
		case "--stdin":
			out.stdin = true
		case "--ds":
			out.flags.dataSource = true
		case "--info":
			out.flags.includeInfo = true
		case "--hungarian":
			out.flags.hungarian = true
		case "--hungarian-types":
			out.flags.hungarianTypes = true
		case "--strict":
			out.flags.strict = true
		default:
			if isFlagLike(arg) {
				return out, fmt.Errorf("unknown flag %q", arg)
			}
			out.files = append(out.files, arg)
		}
	}
	if !out.stdin && len(out.files) == 0 {
		return out, errors.New("no input files specified")
	}
	return out, nil
}

// runValidate handles the --validate CLI mode.
func runValidate(args []string) {
	os.Exit(validateMain(args, os.Stdin, os.Stdout, os.Stderr))
}

// validateMain runs one --validate invocation against the given streams
// and returns the process exit code. It validates stdin content and/or
// SSL files and writes a single JSON array of per-input results.
func validateMain(args []string, stdin io.Reader, stdout, stderr io.Writer) int {
	parsed, err := parseValidateArgs(args)
	if errors.Is(err, errHelp) {
		printValidateHelp(stdout)
		return exitOK
	}
	if err != nil {
		fmt.Fprintf(stderr, "Error: %v\n", err)
		fmt.Fprintln(stderr, "Usage: starlims-lsp --validate [flags] <file.ssl> [file2.ssl ...]")
		fmt.Fprintln(stderr, "       starlims-lsp --validate [flags] --stdin")
		fmt.Fprintln(stderr, "Run 'starlims-lsp --validate --help' for more information")
		return exitUsage
	}

	results := make([]DiagnosticOutput, 0, len(parsed.files)+1)
	hasErrors := false

	if parsed.stdin {
		result := validateReader(stdin, parsed.flags)
		results = append(results, result)
		if !result.Valid {
			hasErrors = true
		}
	}

	for _, filePath := range parsed.files {
		result := validateFilePath(filePath, parsed.flags)
		results = append(results, result)
		if !result.Valid {
			hasErrors = true
		}
	}

	encoder := json.NewEncoder(stdout)
	encoder.SetIndent("", "  ")
	if err := encoder.Encode(results); err != nil {
		fmt.Fprintf(stderr, "Error encoding JSON output: %v\n", err)
		return exitFinding
	}

	if hasErrors {
		return exitFinding
	}
	return exitOK
}

func printValidateHelp(w io.Writer) {
	fmt.Fprintln(w, "starlims-lsp --validate - Validate SSL files for syntax errors")
	fmt.Fprintln(w)
	fmt.Fprintln(w, "Usage:")
	fmt.Fprintln(w, "  starlims-lsp --validate [flags] <file1.ssl> [file2.ssl ...]")
	fmt.Fprintln(w, "  starlims-lsp --validate [flags] --stdin")
	fmt.Fprintln(w, "  cat script.ssl | starlims-lsp --validate --stdin")
	fmt.Fprintln(w)
	fmt.Fprintln(w, "Description:")
	fmt.Fprintln(w, "  Validates SSL files and outputs structured JSON diagnostics.")
	fmt.Fprintln(w, "  Designed for programmatic use by agent skills and CI pipelines.")
	fmt.Fprintln(w)
	fmt.Fprintln(w, "Flags:")
	fmt.Fprintln(w, "  --stdin     Read SSL content from stdin instead of files")
	fmt.Fprintln(w, "  --ds        Treat input as a data source (.ds) document; needed for")
	fmt.Fprintln(w, "              stdin content, where there is no file extension to detect.")
	fmt.Fprintln(w, "              Data source SQL content is exempt from SSL checks.")
	fmt.Fprintln(w, "  --info      Include info-severity diagnostics (the opt-in advisory")
	fmt.Fprintln(w, "              tier; dropped by default to keep output actionable).")
	fmt.Fprintln(w, "  --hungarian Enable both Hungarian checks, off by default:")
	fmt.Fprintln(w, "              hungarian_notation (a declared name carries no")
	fmt.Fprintln(w, "              recognized prefix) and hungarian_type_mismatch (the")
	fmt.Fprintln(w, "              type a prefix promises disagrees with the assigned")
	fmt.Fprintln(w, "              expression). hungarian_notation reports every name in")
	fmt.Fprintln(w, "              a codebase that does not use the convention, so it is")
	fmt.Fprintln(w, "              loud on legacy code.")
	fmt.Fprintln(w, "  --hungarian-types")
	fmt.Fprintln(w, "              Enable only hungarian_type_mismatch. It looks solely at")
	fmt.Fprintln(w, "              names that already carry a prefix, so it stays quiet on")
	fmt.Fprintln(w, "              code that does not use the convention while still")
	fmt.Fprintln(w, "              reporting names that contradict their own prefix.")
	fmt.Fprintln(w, "  --strict    Enable the three checks the editor leaves off:")
	fmt.Fprintln(w, "              undeclared_variable (a name used but never declared),")
	fmt.Fprintln(w, "              unused_variable (a declaration nothing reads), and")
	fmt.Fprintln(w, "              invalid_sql_param (a ?marker? with no matching name).")
	fmt.Fprintln(w, "              They are off by default because an editor reports them")
	fmt.Fprintln(w, "              while a human is mid-edit, when a name is legitimately")
	fmt.Fprintln(w, "              undeclared or unused for the next few keystrokes. On")
	fmt.Fprintln(w, "              finished code each is a defect, so agents and CI want")
	fmt.Fprintln(w, "              all three. None is error severity, so --strict never")
	fmt.Fprintln(w, "              flips the exit code on its own.")
	fmt.Fprintln(w, "  --help      Print this help message")
	fmt.Fprintln(w, "  --          End of flags: every later argument is a file path,")
	fmt.Fprintln(w, "              even one that starts with a dash")
	fmt.Fprintln(w)
	fmt.Fprintln(w, "  Flags may appear anywhere among the files and combine freely.")
	fmt.Fprintln(w, "  Any other argument starting with '-' is rejected as an unknown flag.")
	fmt.Fprintln(w)
	fmt.Fprintln(w, "Exit codes:")
	fmt.Fprintln(w, "  0 - All inputs are valid (no errors; warnings are OK)")
	fmt.Fprintln(w, "  1 - One or more inputs have errors or could not be read")
	fmt.Fprintln(w, "  2 - Usage error (unknown flag, no inputs); nothing is written to stdout")
	fmt.Fprintln(w)
	fmt.Fprintln(w, "Output format:")
	fmt.Fprintln(w, "  JSON array with one result per input (stdin first, then files in")
	fmt.Fprintln(w, "  argument order), each containing:")
	fmt.Fprintln(w, "    - file: base name of the file (\"stdin\" for piped input)")
	fmt.Fprintln(w, "    - version: the tool version, as --version reports it")
	fmt.Fprintln(w, "    - valid: true if no error-severity diagnostics")
	fmt.Fprintln(w, "    - diagnostics: array of issues with line, column, severity,")
	fmt.Fprintln(w, "      message, source, and code (the stable rule slug)")
	fmt.Fprintln(w)
	fmt.Fprintln(w, "Diagnostic checks:")
	fmt.Fprintln(w, "  - Unclosed blocks (:IF without :ENDIF, :FOR without :NEXT, etc.)")
	fmt.Fprintln(w, "  - Unmatched delimiters (parentheses, brackets, braces)")
	fmt.Fprintln(w, "  - Missing :EXITCASE in :CASE/:OTHERWISE blocks (style warning)")
	fmt.Fprintln(w, "  - Bare logical operators (AND instead of .AND.)")
	fmt.Fprintln(w, "  - Invalid keyword forms (missing colon, wrong case, unknown :keyword)")
	fmt.Fprintln(w, "  - :DEFAULT on :DECLARE line (invalid syntax)")
	fmt.Fprintln(w, "  - :DEFAULT not placed immediately after :PARAMETERS")
	fmt.Fprintln(w, "  - :BEGINCASE blocks without any :CASE")
	fmt.Fprintln(w, "  - :TRY blocks missing both :CATCH and :FINALLY")
	fmt.Fprintln(w, "  - Invalid loop control placement and :FINALLY restrictions")
	fmt.Fprintln(w, "  - Legacy :ERROR/:RESUME/:LABEL usage")
	fmt.Fprintln(w, "  - Dot property access (should use colon notation)")
	fmt.Fprintln(w, "  - Direct procedure calls (should use DoProc/ExecFunction)")
	fmt.Fprintln(w, "  - Zero-based array indexing (SSL is 1-based)")
	fmt.Fprintln(w, "  - Class instantiation with () instead of {}")
	fmt.Fprintln(w, "  - Built-in classes passed to CreateUdObject(\"ClassName\")")
	fmt.Fprintln(w, "  - Class-only constraints (DoProc in methods, Constructor return values, multiple :CLASS)")
	fmt.Fprintln(w, "  - Assignment in conditions (:= instead of = or ==)")
	fmt.Fprintln(w, "  - Block nesting depth warnings")
	fmt.Fprintln(w)
	fmt.Fprintln(w, "Examples:")
	fmt.Fprintln(w, "  starlims-lsp --validate script.ssl")
	fmt.Fprintln(w, "  starlims-lsp --validate *.ssl")
	fmt.Fprintln(w, "  echo ':PROCEDURE Test;:ENDPROC;' | starlims-lsp --validate --stdin")
}

func validateFilePath(filePath string, flags validateFlags) DiagnosticOutput {
	fileName := filepath.Base(filePath)

	content, err := os.ReadFile(filePath)
	if err != nil {
		return DiagnosticOutput{
			File:    fileName,
			Version: version,
			Valid:   false,
			Diagnostics: []DiagnosticDetail{
				{
					Line:     1,
					Column:   1,
					Severity: "error",
					Message:  fmt.Sprintf("Failed to read file: %v", err),
					Source:   "ssl-validate",
				},
			},
		}
	}

	return validateContent(fileName, string(content), flags.withDataSource(flags.dataSource || isDataSourcePath(fileName)))
}

// isDataSourcePath checks if a file path refers to a data source file (.ds or .ds.txt).
func isDataSourcePath(name string) bool {
	lower := strings.ToLower(name)
	return strings.HasSuffix(lower, ".ds") || strings.HasSuffix(lower, ".ds.txt")
}

func validateReader(stdin io.Reader, flags validateFlags) DiagnosticOutput {
	content, err := io.ReadAll(stdin)
	if err != nil {
		return DiagnosticOutput{
			File:    "stdin",
			Version: version,
			Valid:   false,
			Diagnostics: []DiagnosticDetail{
				{
					Line:     1,
					Column:   1,
					Severity: "error",
					Message:  fmt.Sprintf("Failed to read stdin: %v", err),
					Source:   "ssl-validate",
				},
			},
		}
	}

	return validateContent("stdin", string(content), flags)
}

// diagnosticOptions maps a run's flags onto the diagnostic options. It
// starts from the editor defaults, and each profile flag switches on
// exactly the options in feature.cli_contract's profile table — no flag
// implies another.
func diagnosticOptions(flags validateFlags) providers.DiagnosticOptions {
	opts := providers.DefaultDiagnosticOptions()
	opts.IsDataSourceFile = flags.dataSource
	opts.IncludeInfoDiagnostics = flags.includeInfo
	opts.CheckHungarianNotation = flags.hungarian
	opts.CheckHungarianTypes = flags.hungarian || flags.hungarianTypes
	opts.CheckUndeclaredVars = flags.strict
	opts.CheckUnusedVars = flags.strict
	opts.CheckSQLParams = flags.strict
	return opts
}

func validateContent(name string, content string, flags validateFlags) DiagnosticOutput {
	// Route through the text path, which owns data-source SQL-mode
	// classification (feature.diagnostics_pipeline A14, issue #141): plain
	// SQL .ds content gets no SSL diagnostics, and the hybrid
	// directives-then-SQL shape keeps diagnostics on its header only —
	// identical to the editor path in validateDocument.
	diagnostics := providers.GetDiagnostics(content, diagnosticOptions(flags))

	// Convert to output format
	details := make([]DiagnosticDetail, 0, len(diagnostics))
	hasErrors := false

	for _, diag := range diagnostics {
		severity := severityToString(diag.Severity)
		if severity == "error" {
			hasErrors = true
		}

		details = append(details, DiagnosticDetail{
			Line:     diag.Range.Start.Line + 1, // Convert 0-indexed to 1-indexed
			Column:   diag.Range.Start.Character + 1,
			Severity: severity,
			Message:  diag.Message,
			Source:   diag.Source,
			Code:     diag.Code,
		})
	}

	return DiagnosticOutput{
		File:        name,
		Version:     version,
		Valid:       !hasErrors,
		Diagnostics: details,
	}
}

func severityToString(severity providers.DiagnosticSeverity) string {
	switch severity {
	case providers.SeverityError:
		return "error"
	case providers.SeverityWarning:
		return "warning"
	case providers.SeverityInfo:
		return "info"
	case providers.SeverityHint:
		return "hint"
	default:
		return "unknown"
	}
}
