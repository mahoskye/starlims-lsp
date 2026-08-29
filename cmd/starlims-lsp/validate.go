package main

import (
	"encoding/json"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"strings"

	"starlims-lsp/internal/providers"
)

// DiagnosticOutput represents the JSON output format for a file's diagnostics.
type DiagnosticOutput struct {
	File        string             `json:"file"`
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
	// hungarian enables the Hungarian-notation checks (--hungarian):
	// hungarian_notation and hungarian_type_mismatch.
	hungarian bool
}

// withDataSource returns a copy with dataSource set, so a file path's
// own extension can classify it without mutating the caller's flags.
func (f validateFlags) withDataSource(on bool) validateFlags {
	f.dataSource = on
	return f
}

// runValidate handles the --validate CLI mode.
// It validates SSL files or stdin content and outputs JSON diagnostics.
func runValidate(args []string) {
	// Check for --help
	for _, arg := range args {
		if arg == "--help" || arg == "-h" {
			printValidateHelp()
			os.Exit(0)
		}
	}

	useStdin := false
	var flags validateFlags
	files := make([]string, 0, len(args))
	for _, arg := range args {
		switch arg {
		case "--stdin":
			useStdin = true
		case "--ds":
			flags.dataSource = true
		case "--info":
			flags.includeInfo = true
		case "--hungarian":
			flags.hungarian = true
		default:
			files = append(files, arg)
		}
	}

	if !useStdin && len(files) == 0 {
		fmt.Fprintln(os.Stderr, "Error: no input files specified")
		fmt.Fprintln(os.Stderr, "Usage: starlims-lsp --validate <file.ssl> [file2.ssl ...]")
		fmt.Fprintln(os.Stderr, "       starlims-lsp --validate --stdin")
		fmt.Fprintln(os.Stderr, "Run 'starlims-lsp --validate --help' for more information")
		os.Exit(1)
	}

	results := make([]DiagnosticOutput, 0)
	hasErrors := false

	if useStdin {
		result := validateStdin(flags)
		results = append(results, result)
		if !result.Valid {
			hasErrors = true
		}
	}

	for _, filePath := range files {
		result := validateFilePath(filePath, flags)
		results = append(results, result)
		if !result.Valid {
			hasErrors = true
		}
	}

	// Output JSON results
	encoder := json.NewEncoder(os.Stdout)
	encoder.SetIndent("", "  ")
	if err := encoder.Encode(results); err != nil {
		fmt.Fprintf(os.Stderr, "Error encoding JSON output: %v\n", err)
		os.Exit(1)
	}

	if hasErrors {
		os.Exit(1)
	}
}

func printValidateHelp() {
	fmt.Println("starlims-lsp --validate - Validate SSL files for syntax errors")
	fmt.Println()
	fmt.Println("Usage:")
	fmt.Println("  starlims-lsp --validate <file1.ssl> [file2.ssl ...]")
	fmt.Println("  starlims-lsp --validate --stdin")
	fmt.Println("  cat script.ssl | starlims-lsp --validate --stdin")
	fmt.Println()
	fmt.Println("Description:")
	fmt.Println("  Validates SSL files and outputs structured JSON diagnostics.")
	fmt.Println("  Designed for programmatic use by agent skills and CI pipelines.")
	fmt.Println()
	fmt.Println("Flags:")
	fmt.Println("  --stdin     Read SSL content from stdin instead of files")
	fmt.Println("  --ds        Treat input as a data source (.ds) document; needed for")
	fmt.Println("              stdin content, where there is no file extension to detect.")
	fmt.Println("              Data source SQL content is exempt from SSL checks.")
	fmt.Println("  --info      Include info-severity diagnostics (the opt-in advisory")
	fmt.Println("              tier; dropped by default to keep output actionable).")
	fmt.Println("  --hungarian Enable the Hungarian-notation checks, both off by")
	fmt.Println("              default: hungarian_notation (a declared name carries no")
	fmt.Println("              recognized prefix) and hungarian_type_mismatch (the type")
	fmt.Println("              a prefix promises disagrees with the assigned")
	fmt.Println("              expression). Noisy on legacy code that predates the")
	fmt.Println("              convention.")
	fmt.Println("  --help      Print this help message")
	fmt.Println()
	fmt.Println("Exit codes:")
	fmt.Println("  0 - All inputs are valid (no errors; warnings are OK)")
	fmt.Println("  1 - One or more inputs have errors")
	fmt.Println()
	fmt.Println("Output format:")
	fmt.Println("  JSON array of results, each containing:")
	fmt.Println("    - file: filename (\"stdin\" for piped input)")
	fmt.Println("    - valid: true if no errors")
	fmt.Println("    - diagnostics: array of issues with line, column, severity, message")
	fmt.Println()
	fmt.Println("Diagnostic checks:")
	fmt.Println("  - Unclosed blocks (:IF without :ENDIF, :FOR without :NEXT, etc.)")
	fmt.Println("  - Unmatched delimiters (parentheses, brackets, braces)")
	fmt.Println("  - Missing :EXITCASE in :CASE/:OTHERWISE blocks (style warning)")
	fmt.Println("  - Bare logical operators (AND instead of .AND.)")
	fmt.Println("  - Invalid keyword forms (missing colon, wrong case, unknown :keyword)")
	fmt.Println("  - :DEFAULT on :DECLARE line (invalid syntax)")
	fmt.Println("  - :DEFAULT not placed immediately after :PARAMETERS")
	fmt.Println("  - :BEGINCASE blocks without any :CASE")
	fmt.Println("  - :TRY blocks missing both :CATCH and :FINALLY")
	fmt.Println("  - Invalid loop control placement and :FINALLY restrictions")
	fmt.Println("  - Legacy :ERROR/:RESUME/:LABEL usage")
	fmt.Println("  - Dot property access (should use colon notation)")
	fmt.Println("  - Direct procedure calls (should use DoProc/ExecFunction)")
	fmt.Println("  - Zero-based array indexing (SSL is 1-based)")
	fmt.Println("  - Class instantiation with () instead of {}")
	fmt.Println("  - Built-in classes passed to CreateUdObject(\"ClassName\")")
	fmt.Println("  - Class-only constraints (DoProc in methods, Constructor return values, multiple :CLASS)")
	fmt.Println("  - Assignment in conditions (:= instead of = or ==)")
	fmt.Println("  - Block nesting depth warnings")
	fmt.Println()
	fmt.Println("Examples:")
	fmt.Println("  starlims-lsp --validate script.ssl")
	fmt.Println("  starlims-lsp --validate *.ssl")
	fmt.Println("  echo ':PROCEDURE Test;:ENDPROC;' | starlims-lsp --validate --stdin")
}

func validateFilePath(filePath string, flags validateFlags) DiagnosticOutput {
	fileName := filepath.Base(filePath)

	content, err := os.ReadFile(filePath)
	if err != nil {
		return DiagnosticOutput{
			File:  fileName,
			Valid: false,
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

func validateStdin(flags validateFlags) DiagnosticOutput {
	content, err := io.ReadAll(os.Stdin)
	if err != nil {
		return DiagnosticOutput{
			File:  "stdin",
			Valid: false,
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

func validateContent(name string, content string, flags validateFlags) DiagnosticOutput {
	// Route through the text path, which owns data-source SQL-mode
	// classification (feature.diagnostics_pipeline A14, issue #141): plain
	// SQL .ds content gets no SSL diagnostics, and the hybrid
	// directives-then-SQL shape keeps diagnostics on its header only —
	// identical to the editor path in validateDocument.
	opts := providers.DefaultDiagnosticOptions()
	opts.IsDataSourceFile = flags.dataSource
	opts.IncludeInfoDiagnostics = flags.includeInfo
	opts.CheckHungarianNotation = flags.hungarian
	diagnostics := providers.GetDiagnostics(content, opts)

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
