# starlims-lsp Integration Roadmap

A comprehensive roadmap for integrating starlims-lsp with the VS Code extension. This document tracks progress across work sessions.

**Last Updated:** 2026-01-10
**Status:** Phase 4 In Progress (Tests Complete)

---

## Overview

**Goal:** Enable multi-editor support via starlims-lsp while simplifying the VS Code extension.

**Scope:** Document-only operations (no workspace indexing)

**Key Files:**
- starlims-lsp: `/home/maho/dev/starlims-lsp/`
- VS Code Extension: `/home/maho/dev/vs-code-ssl-formatter/`
- Style Guide: `/home/maho/dev/silver-robot/DOCS/SSL Refactor Guidelines/STARLIMS_Style_Guide.md`

---

## Phase 1: SSL Formatting in starlims-lsp

### 1.1 Core Formatter Implementation
- [x] Create `internal/providers/formatting.go`
- [x] Implement `FormattingOptions` struct with configurable settings
- [x] Implement basic indentation (tab/space configurable)
- [x] Implement operator spacing (`:=`, `+`, `-`, `*`, `/`, etc.)
- [x] Implement comma spacing in parameter lists
- [x] Implement line length enforcement with smart wrapping
- [x] Implement semicolon enforcement
- [x] Implement blank lines between procedures

### 1.2 SQL String Formatting
SQL strings in SSL are detected inside these functions:
- `SQLExecute`, `GetDataSet`, `GetDataSetWithSchemaFromSelect`, `GetDataSetXMLFromSelect`, `GetNETDataSet`
- `RunSQL`, `LSearch`, `LSelect`, `LSelect1`, `LSelectC`, `GetDataSetEx`

- [x] Create `internal/providers/sql_formatter.go`
- [x] Create `internal/providers/sql_lexer.go`
- [x] Create `internal/providers/sql_constants.go`
- [x] Implement SQL lexer (keywords, identifiers, operators, placeholders)
- [x] Implement SQL keyword casing (`UPPER`, `lower`, `preserve`)
- [x] Implement basic SQL formatting style (clause breaks)
- [x] Integrate SQL formatter with SSL formatter

**SQL Formatting Styles:**
| Style | Description |
|-------|-------------|
| `standard` | Simple clause breaks per style guide. **Default.** |
| `canonicalCompact` | Balanced with indented AND/OR and smart wrapping |
| `compact` | Minimal breaks, fits on fewer lines |
| `expanded` | Each column/condition on own line |

- [x] Implement `standard` style (default, per style guide)
- [x] Implement `canonicalCompact` style
- [x] Implement `compact` style
- [x] Implement `expanded` style

### 1.3 LSP Integration
- [x] Register `DocumentFormattingProvider` capability in `server.go`
- [x] Add `TextDocumentFormatting` handler
- [x] Add `TextDocumentRangeFormatting` handler
- [x] Implement configuration sync via `workspace/didChangeConfiguration`

### 1.4 Configuration Options

| Setting | Type | Default | Description |
|---------|------|---------|-------------|
| `indentStyle` | string | `"tab"` | `"tab"` or `"space"` |
| `indentSize` | int | `4` | Spaces per indent level |
| `maxLineLength` | int | `90` | Max line length (0 = unlimited) |
| `operatorSpacing` | bool | `true` | Space around operators |
| `commaSpacing` | bool | `true` | Space after commas |
| `semicolonEnforcement` | bool | `true` | Ensure `;` termination |
| `blankLinesBetweenProcs` | int | `1` | Blank lines between procedures |
| `sql.enabled` | bool | `true` | Format SQL strings |
| `sql.style` | string | `"canonicalCompact"` | SQL formatting style |
| `sql.keywordCase` | string | `"upper"` | SQL keyword casing |

---

## Phase 2: VS Code Extension LSP Client

### 2.1 Add LSP Client Dependencies
- [x] Add `vscode-languageclient` to `package.json`
- [x] Add `vscode-languageserver-protocol` types (included with vscode-languageclient)
- [x] Update npm dependencies

### 2.2 Bundle starlims-lsp Binaries
- [x] Create `server/` directory in extension
- [x] Build binaries: `cd starlims-lsp && make build-all`
- [x] Copy binaries to `server/` directory:
  - `starlims-lsp-linux-amd64`
  - `starlims-lsp-linux-arm64`
  - `starlims-lsp-darwin-amd64`
  - `starlims-lsp-darwin-arm64`
  - `starlims-lsp-windows-amd64.exe`
- [x] Update `.vscodeignore` to include binaries
- [x] Update `package.json` to include binaries in package

### 2.3 Implement LSP Client
- [x] Create `src/lspClient.ts`
- [x] Implement platform detection for binary selection
- [x] Initialize `LanguageClient` in `extension.ts`
- [x] Configure server spawn: `starlims-lsp --stdio`
- [x] Handle client lifecycle (start, stop, restart)

### 2.4 Configuration Bridge
- [x] Map existing `ssl.format.*` settings to `sslLanguageServer.format.*`
- [x] Sync configuration changes to LSP server
- [x] Maintain backwards compatibility with existing settings

**Note:** Windows binary build was previously blocked by an upstream issue in the `github.com/tliron/glsp` library's dependency on `github.com/tliron/kutil`. This was resolved by upgrading kutil from v0.3.11 to v0.3.27, which fixed the Windows terminal color method compatibility issue with `muesli/termenv`.

---

## Phase 3: Migrate Features to LSP

### 3.1 Features Handled by LSP (disable in extension)
- [x] Completion (`sslCompletionProvider.ts`) - conditionally disabled when LSP active
- [x] Hover (`sslHoverProvider.ts`) - conditionally disabled when LSP active
- [x] Go to Definition (`sslDefinitionProvider.ts`) - conditionally disabled when LSP active
- [x] Find References (`sslReferenceProvider.ts`) - conditionally disabled when LSP active
- [x] Document Symbols (`sslSymbolProvider.ts`) - conditionally disabled when LSP active
- [x] Folding Ranges (`sslFoldingProvider.ts`) - conditionally disabled when LSP active
- [x] Signature Help (`sslSignatureHelpProvider.ts`) - conditionally disabled when LSP active
- [x] Document Formatting (`sslFormatter.ts`) - conditionally disabled when LSP active
- [x] Basic Diagnostics (block matching, parens) - conditionally disabled when LSP active

**Implementation:** Added `ssl.languageServer.enabled` configuration option (default: `true`). When enabled, the extension skips registering native providers for the above features, allowing the LSP to handle them. When disabled or if the LSP fails to start, native providers are registered as fallback.

### 3.2 Features Remaining in Extension
These stay in the VS Code extension:
- TextMate grammar (syntax highlighting)
- CodeLens (reference counts)
- Call Hierarchy
- Inlay Hints
- Rename Provider
- Code Actions (quick fixes)
- Document Highlight
- Workspace Symbols
- Extended Diagnostics (Hungarian notation, SQL injection)
- Commands (format SQL selection, configure namespaces)

---

## Phase 4: Testing & Verification

### 4.1 Unit Tests (starlims-lsp)
- [x] Formatter tests for each indentation option (26 tests in formatting_test.go)
- [x] SQL formatter tests for each style (11 tests in sql_formatter_test.go)
- [x] Configuration change tests (in formatting_test.go and server_test.go)
- [x] Range formatting tests (6 tests in formatting_test.go)
- [x] Comprehensive provider tests (30+ tests in providers_test.go)
- [x] Server and handler tests (15+ tests in server_test.go)

### 4.2 Integration Tests
- [x] LSP client initialization - verified via stdio protocol
- [x] Feature parity checks (completion, hover, formatting) - verified via protocol tests
- [x] Formatting via LSP produces correct indentation and operator spacing

### 4.3 Manual Verification Checklist
| Feature | Test | Status |
|---------|------|--------|
| Completion | Type `:` → keywords appear | ✅ Verified |
| Hover | Hover function → docs shown | ✅ Verified |
| Go to Definition | Ctrl+Click procedure | Pending |
| Find References | Right-click → Find All References | Pending |
| Document Symbols | Ctrl+Shift+O → outline | Pending |
| Folding | Collapse/expand procedures | Pending |
| Signature Help | Type `(` after function | Pending |
| Diagnostics | Unclosed `:IF` → error | Pending |
| Format (SSL) | Shift+Alt+F → proper indentation | ✅ Verified |
| Format (SQL) | SQL strings formatted correctly | Pending |
| Config | Change settings → immediate effect | Pending |

---

## Notes & Decisions

### Decided
- Bundle binaries in extension (not separate download)
- Formatter based on official STARLIMS Style Guide
- SQL formatting with 4 styles: standard, canonicalCompact, compact, expanded
- Default SQL style is "standard" (per style guide), canonicalCompact available via settings
- Document-only scope (no workspace indexing)
- Configurable formatting options with style guide defaults
- Dropped VS Code extension's extra SQL styles (hangingOperators, knr, knrCompact, ormFriendly)
- 2026-01-10: Internal refactor cleanup completed

### Deferred
- Workspace-wide features (rename, workspace symbols)
- Extended diagnostics in LSP (Hungarian notation, SQL injection)
- Call hierarchy in LSP
- CodeLens in LSP

---

## File Reference

**starlims-lsp files to create:**
- `internal/providers/formatting.go`
- `internal/providers/sql_formatter.go`

**starlims-lsp files to modify:**
- `internal/server/server.go` (add formatting handler)

**VS Code extension files to create:**
- `src/lspClient.ts`
- `server/` (binary directory)

**VS Code extension files to modify:**
- `package.json` (dependencies, bundled binaries)
- `src/extension.ts` (LSP client init)

**VS Code extension files to disable/remove:**
- `src/sslCompletionProvider.ts`
- `src/sslHoverProvider.ts`
- `src/sslDefinitionProvider.ts`
- `src/sslReferenceProvider.ts`
- `src/sslSymbolProvider.ts`
- `src/sslFoldingProvider.ts`
- `src/sslSignatureHelpProvider.ts`
- `src/formatting/formatter.ts`
