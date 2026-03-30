// starlims-lsp is a Language Server Protocol implementation for STARLIMS Scripting Language.
package main

import (
	"fmt"
	"os"

	"starlims-lsp/internal/server"
)

var (
	version   = "dev"
	buildTime = "unknown"
)

func main() {
	// Handle version flag
	if len(os.Args) > 1 && (os.Args[1] == "--version" || os.Args[1] == "-v") {
		fmt.Printf("starlims-lsp version %s (built %s)\n", version, buildTime)
		os.Exit(0)
	}

	// Handle validate flag
	if len(os.Args) > 1 && os.Args[1] == "--validate" {
		runValidate(os.Args[2:])
		return
	}

	// Handle format flag
	if len(os.Args) > 1 && os.Args[1] == "--format" {
		runFormat(os.Args[2:])
		return
	}

	// Handle export-signatures flag
	if len(os.Args) > 1 && os.Args[1] == "--export-signatures" {
		runExportSignatures()
		return
	}

	// Handle help flag
	if len(os.Args) > 1 && (os.Args[1] == "--help" || os.Args[1] == "-h") {
		fmt.Println("starlims-lsp - Language Server for STARLIMS Scripting Language (SSL)")
		fmt.Println()
		fmt.Println("Usage:")
		fmt.Println("  starlims-lsp [flags]")
		fmt.Println("  starlims-lsp --validate <file.ssl> [file2.ssl ...]")
		fmt.Println("  starlims-lsp --validate --stdin")
		fmt.Println("  starlims-lsp --format <file.ssl> [file2.ssl ...]")
		fmt.Println("  starlims-lsp --format --stdin")
		fmt.Println()
		fmt.Println("Flags:")
		fmt.Println("  --stdio               Use stdio for communication (default)")
		fmt.Println("  --validate            Validate SSL files and output JSON diagnostics")
		fmt.Println("  --format              Format SSL files and output to stdout")
		fmt.Println("  --export-signatures   Export function signatures, classes, and keywords as JSON")
		fmt.Println("  --version             Print version information")
		fmt.Println("  --help                Print this help message")
		fmt.Println()
		fmt.Println("The server communicates via stdin/stdout using the Language Server Protocol.")
		fmt.Println()
		fmt.Println("Validate mode:")
		fmt.Println("  Validates SSL files for syntax errors and outputs structured JSON.")
		fmt.Println("  Designed for agent skills, CI pipelines, and programmatic use.")
		fmt.Println("  Run 'starlims-lsp --validate --help' for detailed usage.")
		fmt.Println()
		fmt.Println("Format mode:")
		fmt.Println("  Formats SSL source code using canonical style-guide rules.")
		fmt.Println("  Run 'starlims-lsp --format --help' for detailed usage.")
		fmt.Println()
		fmt.Println("Editor integration:")
		fmt.Println("  VS Code:  Use the vs-code-ssl-formatter extension")
		fmt.Println("  Neovim:   Configure with nvim-lspconfig")
		fmt.Println("  Other:    Any LSP-compatible editor can use this server")
		os.Exit(0)
	}

	srv := server.NewSSLServer()
	if err := srv.Run(); err != nil {
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		os.Exit(1)
	}
}
