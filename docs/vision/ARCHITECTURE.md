# LSP Architecture

This document describes the technical architecture of the starlims-lsp language server.

---

## Overview

The starlims-lsp is a standalone executable that communicates with editors via the Language Server Protocol (LSP) over stdio. It receives JSON-RPC messages from clients, processes them, and returns responses.

```
┌─────────────────┐       stdio        ┌─────────────────┐
│                 │ ◄───────────────── │                 │
│   VS Code /     │                    │   starlims-lsp  │
│   Editor        │ ─────────────────► │                 │
│                 │     JSON-RPC       │                 │
└─────────────────┘                    └─────────────────┘
```

---

## Protocol Details

| Property | Value |
|----------|-------|
| LSP Version | 3.17 |
| Transport | stdio |
| Encoding | JSON-RPC 2.0 |
| Content-Type | application/vscode-jsonrpc; charset=utf-8 |

---

## Project Structure

```
starlims-lsp/
├── cmd/
│   └── starlims-lsp/
│       └── main.go              # Entry point
├── internal/
│   ├── server/
│   │   ├── server.go            # LSP server lifecycle
│   │   └── handler.go           # Request dispatch
│   ├── providers/
│   │   ├── completion.go        # textDocument/completion
│   │   ├── hover.go             # textDocument/hover
│   │   ├── signaturehelp.go     # textDocument/signatureHelp
│   │   ├── definition.go        # definition + references
│   │   ├── symbols.go           # documentSymbol + foldingRange
│   │   ├── formatting.go        # formatting + rangeFormatting
│   │   ├── sql_formatter.go     # SQL string formatting
│   │   └── diagnostics.go       # publishDiagnostics
│   ├── lexer/
│   │   └── lexer.go             # SSL tokenizer
│   ├── parser/
│   │   └── parser.go            # SSL parser
│   └── constants/
│       ├── constants.go         # Keywords, classes
│       └── signatures.go        # Function signatures
└── docs/                        # This documentation
```

---

## Component Architecture

### Server Layer (`internal/server/`)

Handles LSP protocol concerns:

```
┌─────────────────────────────────────────────────────────┐
│                       Server                            │
├─────────────────────────────────────────────────────────┤
│  server.go                                              │
│  ├── Initialize / Initialized                           │
│  ├── Shutdown / Exit                                    │
│  ├── Document Sync (Open/Change/Close/Save)             │
│  └── Configuration Change                               │
├─────────────────────────────────────────────────────────┤
│  handler.go                                             │
│  └── Request Dispatch → Provider Methods                │
└─────────────────────────────────────────────────────────┘
```

**Responsibilities:**
- LSP lifecycle management
- Document storage (in-memory)
- Configuration management
- Request routing to providers

### Provider Layer (`internal/providers/`)

Implements LSP features:

```
┌─────────────────────────────────────────────────────────┐
│                      Providers                          │
├───────────────┬───────────────┬───────────────┬─────────┤
│  completion   │    hover      │  signature    │ defn/   │
│               │               │    help       │  refs   │
├───────────────┼───────────────┼───────────────┼─────────┤
│   symbols     │  formatting   │ sql_formatter │  diags  │
└───────────────┴───────────────┴───────────────┴─────────┘
         │               │               │
         └───────────────┼───────────────┘
                         ▼
              ┌─────────────────────┐
              │  Lexer / Parser     │
              └─────────────────────┘
```

**Each provider:**
- Receives parsed document data
- Implements one or more LSP methods
- Returns structured LSP responses

### Lexer/Parser Layer (`internal/lexer/`, `internal/parser/`)

Handles SSL language processing:

```
┌─────────────────────────────────────────────────────────┐
│                    Text Processing                      │
├─────────────────────────────────────────────────────────┤
│  lexer.go                                               │
│  ├── Tokenization                                       │
│  ├── Token types (Keyword, Identifier, Operator, etc.) │
│  └── Position tracking (line, column)                  │
├─────────────────────────────────────────────────────────┤
│  parser.go                                              │
│  ├── Procedure extraction                               │
│  ├── Variable tracking                                  │
│  └── Block structure analysis                           │
└─────────────────────────────────────────────────────────┘
```

### Constants Layer (`internal/constants/`)

Static SSL language data:

```
┌─────────────────────────────────────────────────────────┐
│                      Constants                          │
├─────────────────────────────────────────────────────────┤
│  constants.go                                           │
│  ├── Keywords (37)                                      │
│  ├── Built-in Classes (30)                              │
│  ├── Literals (.T., .F., NIL)                           │
│  └── Operators (.AND., .OR., .NOT.)                     │
├─────────────────────────────────────────────────────────┤
│  signatures.go                                          │
│  └── Function Signatures (367)                          │
│      ├── Name                                           │
│      ├── Parameters (name, type, optional)              │
│      ├── Return type                                    │
│      └── Description                                    │
└─────────────────────────────────────────────────────────┘
```

---

## Request Flow

### Example: Completion Request

```
1. Editor sends: textDocument/completion
   ┌─────────────────────────────────────────┐
   │ { "textDocument": { "uri": "..." },     │
   │   "position": { "line": 5, "char": 10 } │
   │ }                                       │
   └─────────────────────────────────────────┘
                    │
                    ▼
2. server.go receives JSON-RPC message
                    │
                    ▼
3. handler.go routes to completion.go
                    │
                    ▼
4. completion.go:
   a. Gets document from storage
   b. Tokenizes content (lexer)
   c. Determines context (after ":", in expression, etc.)
   d. Filters appropriate completions
   e. Returns CompletionList
                    │
                    ▼
5. Response sent to editor
   ┌─────────────────────────────────────────┐
   │ { "isIncomplete": false,                │
   │   "items": [ { "label": ":IF", ... } ]  │
   │ }                                       │
   └─────────────────────────────────────────┘
```

### Example: Document Change → Diagnostics

```
1. Editor sends: textDocument/didChange
                    │
                    ▼
2. server.go:
   a. Updates stored document
   b. Re-parses document
   c. Triggers diagnostic analysis
                    │
                    ▼
3. diagnostics.go:
   a. Runs all enabled checks
   b. Collects diagnostics
                    │
                    ▼
4. Server sends: textDocument/publishDiagnostics
   ┌─────────────────────────────────────────┐
   │ { "uri": "...",                         │
   │   "diagnostics": [                      │
   │     { "range": {...},                   │
   │       "message": "Unclosed ':IF'",      │
   │       "severity": 1 }                   │
   │   ]                                     │
   │ }                                       │
   └─────────────────────────────────────────┘
```

---

## LSP Lifecycle

### Initialization

```
┌──────────┐                              ┌──────────┐
│  Client  │                              │  Server  │
└────┬─────┘                              └────┬─────┘
     │                                         │
     │  initialize(capabilities, folders)      │
     │ ───────────────────────────────────────►│
     │                                         │
     │  InitializeResult(serverCapabilities)   │
     │ ◄───────────────────────────────────────│
     │                                         │
     │  initialized()                          │
     │ ───────────────────────────────────────►│
     │                                         │
```

### Document Synchronization

```
     │  didOpen(uri, text, version)            │
     │ ───────────────────────────────────────►│
     │                                         │── Parse + Store
     │  publishDiagnostics(uri, diagnostics)   │
     │ ◄───────────────────────────────────────│
     │                                         │
     │  didChange(uri, changes, version)       │
     │ ───────────────────────────────────────►│
     │                                         │── Update + Re-parse
     │  publishDiagnostics(uri, diagnostics)   │
     │ ◄───────────────────────────────────────│
     │                                         │
     │  didClose(uri)                          │
     │ ───────────────────────────────────────►│
     │                                         │── Remove from storage
```

### Shutdown

```
     │  shutdown()                             │
     │ ───────────────────────────────────────►│
     │                                         │── Cleanup
     │  null                                   │
     │ ◄───────────────────────────────────────│
     │                                         │
     │  exit()                                 │
     │ ───────────────────────────────────────►│
     │                                         │── Process terminates
```

---

## Document Storage

Documents are stored in memory during the editing session:

```go
type DocumentStore struct {
    documents map[string]*Document
    mutex     sync.RWMutex
}

type Document struct {
    URI      string
    Version  int
    Content  string
    Tokens   []Token    // Cached tokenization
    Parsed   *ParseResult // Cached parse result
}
```

**Lifecycle:**
- `didOpen`: Create entry, parse, publish diagnostics
- `didChange`: Update content, re-parse, publish diagnostics
- `didSave`: Re-publish diagnostics (if includeText)
- `didClose`: Remove entry, clear diagnostics

---

## Configuration Flow

```
┌──────────┐                              ┌──────────┐
│  Client  │                              │  Server  │
└────┬─────┘                              └────┬─────┘
     │                                         │
     │  didChangeConfiguration(settings)       │
     │ ───────────────────────────────────────►│
     │                                         │
     │                                         │── Update format options
     │                                         │── Update diagnostic options
     │                                         │── Re-analyze open documents
     │                                         │
     │  publishDiagnostics (for each doc)      │
     │ ◄───────────────────────────────────────│
```

Configuration structure expected:

```json
{
  "ssl": {
    "format": {
      "indentStyle": "tab",
      "indentSize": 4,
      "maxLineLength": 90,
      "sql": {
        "enabled": true,
        "style": "standard"
      }
    },
    "diagnostics": {
      "hungarianNotation": false,
      "globals": ["gCurrentUser"]
    }
  }
}
```

---

## Error Handling

### Parser Errors

- Lexer continues after encountering unknown tokens
- Parser recovers at statement boundaries
- Partial results returned for incomplete constructs

### Provider Errors

- Individual provider failures don't crash server
- Errors logged but empty results returned
- User sees graceful degradation

### Protocol Errors

- Malformed messages logged and ignored
- Server continues operating
- No user-visible impact

---

## Performance Considerations

### Tokenization Caching

Tokens are cached per document and only re-computed on change.

### Incremental Updates

Currently using full document sync (`change: 2`). Future optimization could use incremental sync for large files.

### Lazy Parsing

Full parse only triggered when needed (not on every keystroke).

### Debouncing

Diagnostics may be debounced by the client; server processes all requests immediately.

---

## Testing Strategy

### Unit Tests

Each provider has isolated unit tests with mock documents.

### Integration Tests

Full LSP message flow tests with JSON-RPC.

### Specification Tests

Test scenarios from feature documentation executed as automated tests.

---

## Related Documents

- [VISION.md](./VISION.md) - Project goals and philosophy
- [ROADMAP.md](./ROADMAP.md) - Feature roadmap
- [../features/](../features/) - Individual feature specifications
