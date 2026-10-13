// starlims-lsp is a Language Server Protocol implementation for STARLIMS Scripting Language.
package main

import (
	"fmt"
	"io"
	"os"
	"strings"

	"starlims-lsp/internal/constants"
	"starlims-lsp/internal/server"
)

var (
	version   = "dev"
	buildTime = "unknown"
)

// versionText is the --version output. The second line names the upstream
// commit the bundled SSL reference data was vendored from, so a stale
// snapshot is visible without reading the source (issue #254).
func versionText() string {
	return fmt.Sprintf("starlims-lsp version %s (built %s)\nSSL reference data: %s\n",
		version, buildTime, constants.DataProvenance.Reference)
}

func main() {
	code, serve := cliMain(os.Args[1:], os.Stdin, os.Stdout, os.Stderr)
	if !serve {
		os.Exit(code)
	}

	server.SetVersion(version)
	srv := server.NewSSLServer()
	if err := srv.Run(); err != nil {
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		os.Exit(1)
	}
}

// cliMain picks the mode from the first argument. It returns serve=true
// when the language server should start; otherwise the mode has run and
// code is the process exit code.
//
// The server starts with no arguments, with `--stdio` (which LSP clients
// append), or with `--clientProcessId[=<pid>]` (the other launcher
// convention). Any other dash-prefixed first argument is a usage error,
// exit 2: a mistyped mode such as `--valdiate` used to start the server,
// which then waited on stdin forever and hung CI (issue #258). A first
// argument without a dash still starts the server, as it always has.
func cliMain(args []string, stdin io.Reader, stdout, stderr io.Writer) (code int, serve bool) {
	if len(args) == 0 {
		return exitOK, true
	}
	switch first := args[0]; {
	case first == "--version" || first == "-v":
		fmt.Fprint(stdout, versionText())
		return exitOK, false
	case first == "--validate":
		return validateMain(args[1:], stdin, stdout, stderr), false
	case first == "--format":
		return formatMain(args[1:], stdin, stdout, stderr), false
	case first == "--export-signatures":
		runExportSignatures()
		return exitOK, false
	case first == "--help" || first == "-h":
		printMainHelp(stdout)
		return exitOK, false
	case isServerArg(first):
		return exitOK, true
	case isFlagLike(first):
		fmt.Fprintf(stderr, "Error: unknown flag %q\nRun 'starlims-lsp --help' for usage.\n", first)
		return exitUsage, false
	default:
		return exitOK, true
	}
}

// isServerArg reports whether arg is a launcher argument that starts the
// language server.
func isServerArg(arg string) bool {
	return arg == "--stdio" || arg == "--clientProcessId" || strings.HasPrefix(arg, "--clientProcessId=")
}

func printMainHelp(w io.Writer) {
	fmt.Fprint(w, `starlims-lsp - Language Server for STARLIMS Scripting Language (SSL)

Usage:
  starlims-lsp [--stdio]
  starlims-lsp --validate <file.ssl> [file2.ssl ...]
  starlims-lsp --validate --stdin
  starlims-lsp --format <file.ssl> [file2.ssl ...]
  starlims-lsp --format --stdin

Flags:
  --stdio               Use stdio for communication (default)
  --validate            Validate SSL files and output JSON diagnostics
  --format              Format SSL files and output to stdout
  --export-signatures   Export function signatures, classes, and keywords as JSON
  --version             Print version information
  --help                Print this help message

The server communicates via stdin/stdout using the Language Server Protocol.
Any other flag in first position is an error (exit 2).

Validate mode:
  Validates SSL files for syntax errors and outputs structured JSON.
  Designed for agent skills, CI pipelines, and programmatic use.
  Run 'starlims-lsp --validate --help' for detailed usage.

Format mode:
  Formats SSL source code using canonical style-guide rules.
  Run 'starlims-lsp --format --help' for detailed usage.

Editor integration:
  VS Code:  Use the vs-code-ssl-formatter extension
  Neovim:   Configure with nvim-lspconfig
  Other:    Any LSP-compatible editor can use this server
`)
}
