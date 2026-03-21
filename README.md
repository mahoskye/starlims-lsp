# SSL Language Server

A Language Server Protocol (LSP) implementation for STARLIMS Scripting Language (SSL).

This LSP server provides intelligent code editing features for SSL files across any editor that supports the Language Server Protocol.

## Disclaimer

This is an unofficial community project and is not affiliated with, endorsed by, or sponsored by STARLIMS. STARLIMS and related trademarks are the property of their respective owners.

## Documentation

For comprehensive documentation, see the [`docs/`](docs/) directory:

| Document | Description |
|----------|-------------|
| [Documentation Hub](docs/README.md) | Complete documentation index |
| [Feature Status](docs/STATUS.md) | Current implementation status |
| [Configuration Reference](docs/configuration/CONFIGURATION.md) | All configuration options |
| [Roadmap](docs/vision/ROADMAP.md) | Prioritized development plan |

## Features

- **Auto-completion** for keywords, built-in functions, classes, procedures, and variables
  - Procedure completions are source-aligned: script contexts dispatch with `DoProc(...)`, class-method contexts suggest `Me:MethodName(...)`
- **Hover information** for keywords, functions, classes, and user-defined symbols
- **Signature help** for built-in functions, including dispatch helpers such as `DoProc` and `ExecFunction`
- **Go to Definition** for procedures and variables
- **Find References** for all symbols
- **Document Symbols** (outline) for procedures, variables, and comment regions
- **Workspace Symbols** (open documents only; no workspace indexing)
- **Diagnostics** including:
  - Core block/keyword validation (`:IF`, `:TRY`, `:CLASS`, `:DEFAULT`, keyword form, loop control)
  - Style-guide enforcement for `DoProc`/`ExecFunction`, SQL placeholder usage, class layout, `Me`/`Base` class-context forms, `:PUBLIC`, `:INCLUDE`, and legacy keyword handling
  - Conservative inferred-type checks for `:FOR`, `NIL`, `$`, string `=`, and code-block comparison mistakes
  - SSL gotcha detection for direct procedure calls, dot-property access, zero-based arrays, assignment in conditions, and comment-semicolon hazards
  - Opt-in Hungarian notation warnings (configurable prefixes)
- **Document formatting** for SSL and embedded SQL
- **Folding Ranges** for procedures, comment regions, control-flow blocks, and comments
- **Code Snippets** for common SSL patterns

## Installation

### Pre-built Binaries

Download the appropriate binary for your platform from the [releases page](https://github.com/mahoskye/starlims-lsp/releases).

### Build from Source

Requires Go 1.21 or later.

```bash
# Clone the repository
git clone https://github.com/mahoskye/starlims-lsp.git
cd starlims-lsp

# Build
make build

# Or build for all platforms
make build-all
```

The binary will be created in the `bin/` directory.

### Install with Go

```bash
go install github.com/mahoskye/starlims-lsp/cmd/starlims-lsp@latest
```

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

### Validation Mode

Validate SSL files from the command line with structured JSON output. Designed for agent skills, CI pipelines, and programmatic use:

```bash
# Validate one or more files
starlims-lsp --validate script.ssl
starlims-lsp --validate file1.ssl file2.ssl

# Pipe content via stdin
echo ':PROCEDURE Test;:ENDPROC;' | starlims-lsp --validate --stdin

# Get validation-specific help
starlims-lsp --validate --help
```

Output is a JSON array with diagnostics per file:

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

To use with VS Code, you need a client extension that launches this server. The companion extension [vs-code-ssl-formatter](https://github.com/mahoskye/vs-code-ssl-formatter) can be configured to use this LSP.

### Neovim Integration

Add to your `init.lua`:

```lua
local lspconfig = require('lspconfig')
local configs = require('lspconfig.configs')

if not configs.starlims_lsp then
  configs.starlims_lsp = {
    default_config = {
      cmd = { 'starlims-lsp', '--stdio' },
      filetypes = { 'ssl' },
      root_dir = function(fname)
        return lspconfig.util.find_git_ancestor(fname) or vim.fn.getcwd()
      end,
      settings = {},
    },
  }
end

lspconfig.starlims_lsp.setup{}
```

### Other Editors

Any editor supporting LSP can use this server. Configure it to:
1. Run `starlims-lsp --stdio`
2. Associate it with `.ssl` files

## Configuration

The server accepts formatter settings via `workspace/didChangeConfiguration`:

```json
{
  "ssl": {
    "format": {
      "indentStyle": "tab",
      "indentSize": 4,
      "maxLineLength": 90,
      "operatorSpacing": true,
      "commaSpacing": true,
      "semicolonEnforcement": true,
      "blankLinesBetweenProcs": 1,
      "sql": {
        "enabled": true,
        "style": "canonicalCompact",
        "keywordCase": "upper",
        "indentSize": 4,
        "maxLineLength": 90
      }
    },
    "diagnostics": {
      "hungarianNotation": false,
      "hungarianPrefixes": ["a", "b", "d", "fn", "n", "o", "s", "v"],
      "globals": ["gCurrentUser", "gAppName"]
    },
    "inlayHints": {
      "enabled": true,
      "minParameterCount": 2
    }
  }
}
```

`indentStyle: "tab"` is the source-aligned default. `indentSize` is only used when `indentStyle` is `"space"`; the bundled value `4` is the fallback space width, not the width of a tab-indented SSL block.

Diagnostics settings default to server defaults. When `hungarianNotation` is enabled, the LSP warns on declared variables that do not use an allowed Hungarian prefix. Global variables can be declared via `globals` array; assignments to these variables will trigger an error. Always-on diagnostics enforce major rules from the authoritative material under `dev/ssl-style-guide/`; [`docs/ssl-reference/style-guide.md`](docs/ssl-reference/style-guide.md) is the bundled public summary.

## SSL Language Overview

SSL (STARLIMS Scripting Language) is a procedural scripting language used in STARLIMS LIMS (Laboratory Information Management System).

### Key Features

- Keywords prefixed with `:` (e.g., `:IF`, `:PROCEDURE`)
- Comments: `/* comment text;`
- Strings: `"double"`, `'single'`, or `[bracket]` notation
- Boolean literals: `.T.`, `.F.`
- Null literal: `NIL`
- Logical operators: `.AND.`, `.OR.`, `.NOT.`
- Assignment: `:=`
- Built-in classes use curly braces (`Email{}`); `CreateUdObject(...)` is for user-defined classes, empty dynamic objects, or anonymous property bags
- Custom procedures are invoked through `DoProc(...)` / `ExecFunction(...)`

### Example

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

## Development

```bash
# Build
make build

# Run tests
make test

# Run with coverage
make test-coverage

# Format code
make fmt

# Lint (requires golangci-lint)
make lint

# Clean build artifacts
make clean
```

## Project Structure

```
starlims-lsp/
├── cmd/
│   └── starlims-lsp/
│       ├── main.go           # Entry point
│       └── validate.go       # --validate CLI mode
├── internal/
│   ├── lexer/
│   │   └── lexer.go          # Tokenizer
│   ├── parser/
│   │   └── parser.go         # AST parser
│   ├── constants/
│   │   ├── constants.go      # Keywords, literals, operators, legacy inventories
│   │   ├── source_alignment.go # Source-aligned public function/class inventories
│   │   └── signatures.go     # Legacy signature corpus used by source alignment
│   ├── providers/
│   │   ├── completion.go     # Auto-completion
│   │   ├── hover.go          # Hover information
│   │   ├── diagnostics.go    # Error detection
│   │   ├── definition.go     # Go to definition
│   │   └── symbols.go        # Document symbols
│   └── server/
│       ├── server.go         # LSP server setup
│       ├── handler.go        # LSP request handlers
│       └── cache.go          # Document caching
├── go.mod
├── go.sum
├── Makefile
└── README.md
```

## Cross-Platform Builds

The Makefile supports building for multiple platforms:

```bash
# Build for current platform
make build

# Build for all platforms
make build-all

# Build for specific platforms
make build-linux
make build-darwin
make build-windows
```

## Related Projects

- [vs-code-ssl-formatter](https://github.com/mahoskye/vs-code-ssl-formatter) - VS Code extension for SSL

## License

MIT
