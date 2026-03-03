package main

import (
	"encoding/json"
	"fmt"
	"io"
	"os"
	"path/filepath"

	"starlims-lsp/internal/lexer"
	"starlims-lsp/internal/parser"
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

	// Check for --stdin flag
	useStdin := false
	files := make([]string, 0, len(args))
	for _, arg := range args {
		if arg == "--stdin" {
			useStdin = true
		} else {
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
		result := validateStdin()
		results = append(results, result)
		if !result.Valid {
			hasErrors = true
		}
	}

	for _, filePath := range files {
		result := validateFilePath(filePath)
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
	fmt.Println("  - Missing :EXITCASE in :CASE/:OTHERWISE blocks")
	fmt.Println("  - Bare logical operators (AND instead of .AND.)")
	fmt.Println("  - :DEFAULT on :DECLARE line (invalid syntax)")
	fmt.Println("  - Dot property access (should use colon notation)")
	fmt.Println("  - Direct procedure calls (should use DoProc/ExecFunction)")
	fmt.Println("  - Zero-based array indexing (SSL is 1-based)")
	fmt.Println("  - Class instantiation with () instead of {}")
	fmt.Println("  - Assignment in conditions (:= instead of = or ==)")
	fmt.Println("  - Block nesting depth warnings")
	fmt.Println()
	fmt.Println("Examples:")
	fmt.Println("  starlims-lsp --validate script.ssl")
	fmt.Println("  starlims-lsp --validate *.ssl")
	fmt.Println("  echo ':PROCEDURE Test;:ENDPROC;' | starlims-lsp --validate --stdin")
}

func validateFilePath(filePath string) DiagnosticOutput {
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

	return validateContent(fileName, string(content))
}

func validateStdin() DiagnosticOutput {
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

	return validateContent("stdin", string(content))
}

func validateContent(name string, content string) DiagnosticOutput {
	// Tokenize
	lex := lexer.NewLexer(content)
	tokens := lex.Tokenize()

	// Parse
	p := parser.NewParser(tokens)
	ast := p.Parse()

	// Get diagnostics with default options
	opts := providers.DefaultDiagnosticOptions()
	diagnostics := providers.GetDiagnosticsFromTokens(tokens, ast, opts)

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
