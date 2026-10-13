# SSL Language Server

A Language Server Protocol (LSP) implementation for STARLIMS Scripting Language (SSL).

This LSP server provides intelligent code editing features for SSL files across any editor that supports the Language Server Protocol, and a command-line validator and formatter for CI and agent tooling.

The LSP models **STARLIMS 12** SSL as verified by the published [SSL reference](https://github.com/mahoskye/starlims-ssl-reference) (written for STARLIMS 11, verified on STARLIMS 12). Where the two versions are known to differ, diagnostics report what fails on either one; see [D17](catalog/DECISIONS.md).

## Disclaimer

This is an unofficial community project and is not affiliated with, endorsed by, or sponsored by STARLIMS. STARLIMS and related trademarks are the property of their respective owners.

## Documentation

For comprehensive documentation, see the [`docs/`](docs/) directory:

| Document | Description |
|----------|-------------|
| [Documentation Hub](docs/README.md) | Complete documentation index, and which source wins when two disagree |
| [Behavior Catalog](catalog/README.md) | The normative definition of every diagnostic, feature, and formatter rule |
| [Feature Status](docs/STATUS.md) | Current implementation status (generated from the catalog) |
| [Diagnostic Rules](docs/reference/DIAGNOSTICS.md) | Every diagnostic code with its default severity (generated from the catalog) |
| [Configuration Reference](docs/configuration/CONFIGURATION.md) | All configuration options |
| [Roadmap](docs/vision/ROADMAP.md) | What has shipped and the current direction |

## Features

- **Auto-completion** for keywords, built-in functions, classes, procedures, and variables
  - Procedure completions are style-guide-aware: script contexts dispatch with `DoProc(...)`, class-method contexts suggest `Me:MethodName(...)`
- **Hover information** for keywords, functions, classes, and user-defined symbols
- **Signature help** for built-in functions, including dispatch helpers such as `DoProc` and `ExecFunction`
- **Go to Definition** for procedures and variables, including cross-file navigation for dotted `DoProc`/`ExecFunction` targets, `RunDS` data sources, and `:INCLUDE` paths
- **Find References** for procedures, variables, and parameters; procedure references include workspace-wide dispatch call sites
- **Rename Symbol** — variables within the document (scope-aware), procedures workspace-wide including their dispatch call sites
- **Document Symbols** (outline) for procedures and their parameters, `:PUBLIC` variables, and comment regions
- **Workspace Symbols** backed by a background workspace index
- **Diagnostics** with stable rule codes, per-rule severity overrides, and in-file suppression comments. Always-on rules cover code that does not compile or fails at run time, plus high-confidence style-guide rules. Checks prone to false positives (Hungarian notation, undeclared and unused variables, SQL parameters) are opt-in, and an opt-in **info tier** adds advisory detail aimed at assistant/LLM consumers. See [Diagnostic Rules](docs/reference/DIAGNOSTICS.md) for the full list.
- **Inlay Hints** for parameter names at call sites (configurable minimum parameter count)
- **Document and range formatting** for SSL and embedded SQL
- **Folding Ranges** for procedures, comment regions, control-flow blocks, and comments
- **Code Snippets** for common SSL patterns

## Installation

### Pre-built Binaries

Download the appropriate binary for your platform from the [releases page](https://github.com/mahoskye/starlims-lsp/releases).

The binaries statically link third-party Go modules; their license texts are
collected in [`THIRD-PARTY-NOTICES.md`](THIRD-PARTY-NOTICES.md), which is also
attached to each release.

### Build from Source

Requires the Go version named in [`go.mod`](go.mod) (currently 1.25) or later.

```bash
# Clone the repository
git clone https://github.com/mahoskye/starlims-lsp.git
cd starlims-lsp

# Build for the current platform (into bin/)
make build

# Or install into your Go bin directory
make install

# Or build for all platforms, or one
make build-all
make build-linux
make build-darwin
make build-windows
```

`go install github.com/mahoskye/starlims-lsp/...@latest` does not work, because the module path in `go.mod` is `starlims-lsp`. Build from a clone instead.

## Usage

### Command Line

The server communicates over stdio by default:

```bash
starlims-lsp --stdio
```

Or if built locally:

```bash
./bin/starlims-lsp --stdio
```

`starlims-lsp --version` prints the version and the SSL reference data the binary was built with. `starlims-lsp --export-signatures` prints the built-in function, class, operator, and keyword inventory as JSON.

### Validation Mode

Validate SSL files from the command line with structured JSON output. Designed for agent skills, CI pipelines, and programmatic use:

```bash
# Validate one or more files
starlims-lsp --validate script.ssl
starlims-lsp --validate file1.ssl file2.ssl

# Pipe content via stdin (add --ds for a data source)
echo ':PROCEDURE Test;:ENDPROC;' | starlims-lsp --validate --stdin

# Include the opt-in info tier (advisory detail; see CONFIGURATION.md §5.8)
starlims-lsp --validate --info script.ssl

# Strict profile for finished code: adds the undeclared-variable,
# unused-variable, and SQL-parameter checks the editor leaves off
starlims-lsp --validate --strict script.ssl

# Hungarian checks: both (--hungarian) or only the type check (--hungarian-types)
starlims-lsp --validate --hungarian-types script.ssl

# Get validation-specific help
starlims-lsp --validate --help
```

### Format Mode

Format SSL files (and the SQL embedded in their strings) from the command
line:

```bash
# Print formatted output
starlims-lsp --format script.ssl

# Rewrite files in place
starlims-lsp --format --write script.ssl

# CI gate: exit non-zero when any file is not already formatted
starlims-lsp --format --check *.ssl

# Format stdin; layout flags: --indent-style, --indent-size,
# --max-line-length, --no-sql
cat script.ssl | starlims-lsp --format --stdin

# Get format-specific help
starlims-lsp --format --help
```

`--validate` output is a JSON array with diagnostics per file:

```json
[
  {
    "file": "script.ssl",
    "valid": true,
    "diagnostics": []
  }
]
```

Exit code `0` means all inputs pass; `1` means errors were found.

### VS Code Integration

The companion extension [vs-code-ssl-formatter](https://github.com/mahoskye/vs-code-ssl-formatter) launches this server and is the supported way to use it in VS Code.

### Neovim Integration

With Neovim 0.11 or later, add to your `init.lua`:

```lua
-- Register the SSL file types
vim.filetype.add({
  extension = { ssl = 'ssl', srvscr = 'ssl', ds = 'ssl' },
  pattern = { ['.*%.ssl%.txt'] = 'ssl', ['.*%.ds%.txt'] = 'ssl' },
})

vim.lsp.config('starlims_lsp', {
  cmd = { 'starlims-lsp', '--stdio' },
  filetypes = { 'ssl' },
  root_markers = { '.git' },
})
vim.lsp.enable('starlims_lsp')
```

### Other Editors

Any editor supporting LSP can use this server. Configure it to:
1. Run `starlims-lsp --stdio`
2. Associate it with SSL files: `.srvscr`, `.ssl`, `.ssl.txt`, and data sources `.ds`, `.ds.txt`

## Configuration

The server accepts settings under the `ssl` key, both as `initializationOptions` and via `workspace/didChangeConfiguration`. Every field is optional; omitted fields keep their defaults. For example (`rules` here promotes one rule from the hidden info tier to a warning):

```json
{
  "ssl": {
    "format": {
      "indentStyle": "tab",
      "maxLineLength": 90,
      "sql": {
        "style": "canonicalCompact",
        "keywordCase": "upper"
      }
    },
    "diagnostics": {
      "hungarianTypes": true,
      "infoDiagnostics": false,
      "rules": { "negative_logic": "warning" }
    },
    "inlayHints": {
      "enabled": true,
      "minParameterCount": 2
    }
  }
}
```

[CONFIGURATION.md](docs/configuration/CONFIGURATION.md) documents every setting and its default.

## SSL Language Overview

SSL (STARLIMS Scripting Language) is a procedural scripting language used in STARLIMS LIMS (Laboratory Information Management System). Keywords are colon-prefixed and UPPERCASE (`:IF`, `:PROCEDURE`), every statement and comment ends with `;`, logical operators are period-wrapped (`.AND.`), and script procedures are called through `DoProc(...)` / `ExecFunction(...)` (inside a `:CLASS`, through `Me:` / `Base:`).

```ssl
:PROCEDURE CalculateTotal;
	:PARAMETERS nItems, nPrice;
	:DECLARE nTotal, i;

	nTotal := 0;

	:FOR i := 1 :TO nItems;
		nTotal := nTotal + nPrice;
	:NEXT;

	:RETURN nTotal;
:ENDPROC;
```

For the language itself, see [AGENTS.md](AGENTS.md) (conventions), [`docs/ssl-reference/`](docs/ssl-reference/) (a bundled summary), and the upstream sources they summarize: [`ssl-style-guide`](https://github.com/mahoskye/ssl-style-guide) for language and style rules and [`starlims-ssl-reference`](https://github.com/mahoskye/starlims-ssl-reference) for element behavior.

## Development

```bash
# Build
make build

# Run tests (includes the catalog conformance and spec tests)
make test

# Run only the catalog conformance and spec tests
make catalog-check

# Regenerate docs/STATUS.md and docs/reference/DIAGNOSTICS.md after a catalog edit
make generate-docs

# Run with coverage
make test-coverage

# Format code (requires goimports)
make fmt

# Lint (requires golangci-lint)
make lint

# Regenerate THIRD-PARTY-NOTICES.md when go.mod changes
make notices

# Clean build artifacts
make clean
```

Behavior changes start in the catalog: write or update the entry under [`catalog/`](catalog/README.md), then the code. [ARCHITECTURE.md](docs/vision/ARCHITECTURE.md) describes the packages and how a request flows through them.

## Project Structure

| Path | Contents |
|------|----------|
| `cmd/starlims-lsp/` | The binary: LSP server and the `--validate` / `--format` / `--export-signatures` CLI modes |
| `cmd/gen-inventory/`, `cmd/gen-catalog-docs/` | Code and doc generators |
| `internal/server/` | LSP handlers, document cache, settings, workspace index, cross-file references and rename |
| `internal/providers/` | Feature logic: diagnostics, completion, hover, navigation, formatting |
| `internal/parser/`, `internal/lexer/` | Tokenizer, statement tree, and expression AST |
| `internal/constants/` | The SSL inventory, generated from vendored reference data in `data/` |
| `internal/catalog/` | Catalog loader, conformance tests, and executable spec tests |
| `catalog/` | The behavior catalog |
| `docs/` | Documentation |

See [ARCHITECTURE.md](docs/vision/ARCHITECTURE.md) for details.

## Related Projects

- [vs-code-ssl-formatter](https://github.com/mahoskye/vs-code-ssl-formatter) - VS Code extension for SSL
- [ssl-style-guide](https://github.com/mahoskye/ssl-style-guide) - SSL language and style rules
- [starlims-ssl-reference](https://github.com/mahoskye/starlims-ssl-reference) - SSL reference (functions, classes, keywords, operators)

## License

MIT
