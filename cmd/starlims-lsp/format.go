package main

import (
	"fmt"
	"io"
	"os"
	"path/filepath"

	"starlims-lsp/internal/providers"
)

// runFormat handles the --format CLI mode.
// It reads SSL content from files or stdin, formats it, and writes the result to stdout.
func runFormat(args []string) {
	for _, arg := range args {
		if arg == "--help" || arg == "-h" {
			printFormatHelp()
			os.Exit(0)
		}
	}

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
		fmt.Fprintln(os.Stderr, "Usage: starlims-lsp --format <file.ssl> [file2.ssl ...]")
		fmt.Fprintln(os.Stderr, "       starlims-lsp --format --stdin")
		fmt.Fprintln(os.Stderr, "Run 'starlims-lsp --format --help' for more information")
		os.Exit(1)
	}

	if useStdin && len(files) > 0 {
		fmt.Fprintln(os.Stderr, "Error: --stdin cannot be combined with file arguments")
		os.Exit(1)
	}

	if useStdin {
		formatFromStdin()
		return
	}

	// Multiple files: format each, separated by a header comment
	hasErrors := false
	for i, filePath := range files {
		if i > 0 {
			fmt.Println()
		}
		if len(files) > 1 {
			fmt.Fprintf(os.Stderr, "==> %s <==\n", filepath.Base(filePath))
		}
		if !formatFromFile(filePath) {
			hasErrors = true
		}
	}
	if hasErrors {
		os.Exit(1)
	}
}

func printFormatHelp() {
	fmt.Println("starlims-lsp --format - Format SSL source code")
	fmt.Println()
	fmt.Println("Usage:")
	fmt.Println("  starlims-lsp --format <file1.ssl> [file2.ssl ...]")
	fmt.Println("  starlims-lsp --format --stdin")
	fmt.Println("  cat script.ssl | starlims-lsp --format --stdin")
	fmt.Println()
	fmt.Println("Description:")
	fmt.Println("  Formats SSL source code and writes the result to stdout.")
	fmt.Println("  Uses the canonical style-guide formatting options by default.")
	fmt.Println()
	fmt.Println("Flags:")
	fmt.Println("  --stdin     Read SSL content from stdin instead of files")
	fmt.Println("  --help      Print this help message")
	fmt.Println()
	fmt.Println("Formatting rules:")
	fmt.Println("  - Tab indentation (1 tab per level)")
	fmt.Println("  - Operator spacing (spaces around operators)")
	fmt.Println("  - Comma spacing (space after commas)")
	fmt.Println("  - Semicolon enforcement (ensures statements end with semicolons)")
	fmt.Println("  - Keyword normalization (:if -> :IF, :endif -> :ENDIF)")
	fmt.Println("  - Blank line between procedures")
	fmt.Println("  - Line wrapping at 90 characters")
	fmt.Println("  - Embedded SQL formatting (canonical compact style)")
	fmt.Println()
	fmt.Println("Examples:")
	fmt.Println("  starlims-lsp --format script.ssl")
	fmt.Println("  starlims-lsp --format *.ssl")
	fmt.Println("  echo ':PROCEDURE Test;x:=1;:ENDPROC;' | starlims-lsp --format --stdin")
}

func formatFromStdin() {
	content, err := io.ReadAll(os.Stdin)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error reading stdin: %v\n", err)
		os.Exit(1)
	}

	formatted := formatContent(string(content))
	fmt.Print(formatted)
}

func formatFromFile(filePath string) bool {
	content, err := os.ReadFile(filePath)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error reading %s: %v\n", filePath, err)
		return false
	}

	formatted := formatContent(string(content))
	fmt.Print(formatted)
	return true
}

func formatContent(content string) string {
	opts := providers.DefaultFormattingOptions()
	edits := providers.FormatDocument(content, opts)

	// FormatDocument returns a single edit replacing the entire document
	if len(edits) > 0 {
		return edits[0].NewText
	}

	// No edits means content is already formatted (or empty)
	return content
}

