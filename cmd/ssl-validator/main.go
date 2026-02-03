// ssl-validator is a command-line tool for validating SSL syntax.
package main

import (
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"

	"starlims-lsp/internal/lexer"
	"starlims-lsp/internal/parser"
	"starlims-lsp/internal/providers"
)

var (
	version   = "dev"
	buildTime = "unknown"
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

func main() {
	// Handle version flag
	if len(os.Args) > 1 && (os.Args[1] == "--version" || os.Args[1] == "-v") {
		fmt.Printf("ssl-validator version %s (built %s)\n", version, buildTime)
		os.Exit(0)
	}

	// Handle help flag
	if len(os.Args) > 1 && (os.Args[1] == "--help" || os.Args[1] == "-h") {
		printHelp()
		os.Exit(0)
	}

	// Validate arguments
	if len(os.Args) < 2 {
		fmt.Fprintln(os.Stderr, "Error: no input files specified")
		fmt.Fprintln(os.Stderr, "Usage: ssl-validator <file1.ssl> [file2.ssl ...]")
		fmt.Fprintln(os.Stderr, "Run 'ssl-validator --help' for more information")
		os.Exit(1)
	}

	files := os.Args[1:]
	results := make([]DiagnosticOutput, 0, len(files))
	hasErrors := false

	// Process each file
	for _, filePath := range files {
		result := validateFile(filePath)
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

	// Exit with error code if any files had errors
	if hasErrors {
		os.Exit(1)
	}
}

func printHelp() {
	fmt.Println("ssl-validator - CLI tool for validating SSL syntax")
	fmt.Println()
	fmt.Println("Usage:")
	fmt.Println("  ssl-validator <file1.ssl> [file2.ssl ...]")
	fmt.Println()
	fmt.Println("Description:")
	fmt.Println("  Validates one or more SSL files for syntax errors and warnings.")
	fmt.Println("  Outputs structured JSON with diagnostics for each file.")
	fmt.Println()
	fmt.Println("Exit codes:")
	fmt.Println("  0 - All files are valid (no errors)")
	fmt.Println("  1 - One or more files have errors")
	fmt.Println()
	fmt.Println("Output format:")
	fmt.Println("  JSON array of file results with diagnostics including:")
	fmt.Println("    - file: filename")
	fmt.Println("    - valid: true if no errors (warnings are OK)")
	fmt.Println("    - diagnostics: array of issues found")
	fmt.Println()
	fmt.Println("Diagnostic checks:")
	fmt.Println("  - Unclosed blocks (:IF without :ENDIF, :FOR without :NEXT, etc.)")
	fmt.Println("  - Unmatched delimiters (parentheses, brackets, braces)")
	fmt.Println("  - Missing :EXITCASE in :CASE/:OTHERWISE blocks")
	fmt.Println("  - Bare logical operators (AND instead of .AND.)")
	fmt.Println("  - :DEFAULT on :DECLARE line (invalid syntax)")
	fmt.Println("  - Block nesting depth warnings")
	fmt.Println()
	fmt.Println("Examples:")
	fmt.Println("  ssl-validator script.ssl")
	fmt.Println("  ssl-validator file1.ssl file2.ssl file3.ssl")
	fmt.Println("  ssl-validator examples/*.ssl")
	fmt.Println()
	fmt.Println("Flags:")
	fmt.Println("  --version   Print version information")
	fmt.Println("  --help      Print this help message")
}

func validateFile(filePath string) DiagnosticOutput {
	fileName := filepath.Base(filePath)

	// Read file content
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
					Source:   "ssl-validator",
				},
			},
		}
	}

	// Tokenize
	lex := lexer.NewLexer(string(content))
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
		File:        fileName,
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
