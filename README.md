# SSL Language Server

A Language Server Protocol (LSP) implementation for STARLIMS Scripting Language (SSL).

This LSP server provides intelligent code editing features for SSL files across any editor that supports the Language Server Protocol.

## Disclaimer

This is an unofficial community project and is not affiliated with, endorsed by, or sponsored by STARLIMS. STARLIMS and related trademarks are the property of their respective owners.

## Documentation

See `DOCUMENTATION.md` for a deeper usage and configuration guide.

## Features

- **Auto-completion** for keywords, built-in functions, classes, procedures, and variables
- **Hover information** for keywords, functions, classes, and user-defined symbols
- **Signature help** for built-in functions
- **Go to Definition** for procedures and variables
- **Find References** for all symbols
- **Document Symbols** (outline) for procedures, variables, and regions
- **Workspace Symbols** (open documents only; no workspace indexing)
- **Diagnostics** including:
  - Unclosed block detection (`:IF` without `:ENDIF`, etc.)
  - Unmatched parentheses and brackets
  - Block nesting depth warnings
  - Opt-in Hungarian notation warnings (configurable prefixes)
- **Document formatting** for SSL and embedded SQL
- **Folding Ranges** for procedures, regions, and comments
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
        "style": "standard",
        "keywordCase": "upper",
        "indentSize": 4,
        "maxLineLength": 90
      }
    },
    "diagnostics": {
      "hungarianNotation": false,
      "hungarianPrefixes": ["a", "b", "d", "n", "o", "s"]
    }
  }
}
```

Diagnostics settings default to server defaults and can opt in to Hungarian notation warnings using the style guide prefixes (`a`, `b`, `d`, `n`, `o`, `s`).

## SSL Language Overview

SSL (STARLIMS Scripting Language) is a procedural scripting language used in STARLIMS LIMS (Laboratory Information Management System).

### Key Features

- Keywords prefixed with `:` (e.g., `:IF`, `:PROCEDURE`)
- Comments: `/* comment text;`
- Strings: `"double"`, `'single'`, or `[bracket]` notation
- Boolean literals: `.T.`, `.F.`
- Logical operators: `.AND.`, `.OR.`, `.NOT.`
- Assignment: `:=`

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
│       └── main.go           # Entry point
├── internal/
│   ├── lexer/
│   │   └── lexer.go          # Tokenizer
│   ├── parser/
│   │   └── parser.go         # AST parser
│   ├── constants/
│   │   └── constants.go      # Language constants
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
