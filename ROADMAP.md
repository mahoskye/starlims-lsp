# starlims-lsp Integration Roadmap

A comprehensive roadmap for the starlims-lsp language server. This document tracks progress across work sessions.

**Last Updated:** 2026-01-10
**Status:** Phase 4 In Progress (Tests Complete, Workspace Symbols Added)

---

## Overview

**Goal:** Enable multi-editor support via starlims-lsp.

**Scope:** Document-only operations (no workspace indexing)

**Key Files:**
- starlims-lsp: `/home/maho/dev/starlims-lsp/`
- Style Guide: `/home/maho/dev/silver-robot/DOCS/abbot-starlims-style-guide.md`

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

## Phase 2: Core LSP Features

### 2.1 LSP Feature Coverage
- [x] Completion
- [x] Hover
- [x] Go to Definition
- [x] Find References
- [x] Document Symbols
- [x] Folding Ranges
- [x] Signature Help
- [x] Document Formatting
- [x] Basic Diagnostics (block matching, parens)
- [x] Hungarian notation warnings (opt-in)
- [x] Workspace Symbols (open documents only)

---

## Phase 3: Testing & Verification

### 3.1 Unit Tests (starlims-lsp)
- [x] Formatter tests for each indentation option (26 tests in formatting_test.go)
- [x] SQL formatter tests for each style (11 tests in sql_formatter_test.go)
- [x] Configuration change tests (in formatting_test.go and server_test.go)
- [x] Range formatting tests (6 tests in formatting_test.go)
- [x] Comprehensive provider tests (30+ tests in providers_test.go)
- [x] Server and handler tests (16+ tests in server_test.go)

### 3.2 Integration Tests
- [x] Feature parity checks (completion, hover, formatting) - verified via protocol tests
- [x] Formatting via LSP produces correct indentation and operator spacing

---

## Notes & Decisions

### Decided
- Formatter defaults follow `/home/maho/dev/silver-robot/DOCS/abbot-starlims-style-guide.md`
- SQL formatting with 4 styles: standard, canonicalCompact, compact, expanded
- Default SQL style is "standard" (per style guide), canonicalCompact available via settings
- Document-only scope (no workspace indexing)
- Configurable formatting options with style guide defaults
- 2026-01-10: Internal refactor cleanup completed

---

## File Reference

**starlims-lsp files to create:**
- `internal/providers/formatting.go`
- `internal/providers/sql_formatter.go`

**starlims-lsp files to modify:**
- `internal/server/server.go` (add formatting handler)
