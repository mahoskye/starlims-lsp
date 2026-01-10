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

	// Handle help flag
	if len(os.Args) > 1 && (os.Args[1] == "--help" || os.Args[1] == "-h") {
		fmt.Println("starlims-lsp - Language Server for STARLIMS Scripting Language (SSL)")
		fmt.Println()
		fmt.Println("Usage:")
		fmt.Println("  starlims-lsp [flags]")
		fmt.Println()
		fmt.Println("Flags:")
		fmt.Println("  --stdio     Use stdio for communication (default)")
		fmt.Println("  --version   Print version information")
		fmt.Println("  --help      Print this help message")
		fmt.Println()
		fmt.Println("The server communicates via stdin/stdout using the Language Server Protocol.")
		fmt.Println()
		fmt.Println("Editor integration:")
		fmt.Println("  VS Code:  Use the ssl-vscode extension")
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
