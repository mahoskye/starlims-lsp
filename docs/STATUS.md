# Implementation Status Dashboard

This document provides a quick overview of all LSP features and their current implementation status.

**Last Updated:** 2025-02-02

---

## Status Legend

| Status | Meaning |
|--------|---------|
| IMPLEMENTED | Fully functional |
| PARTIAL | Core functionality works, some gaps remain |
| PLANNED | Specified but not yet implemented |
| NOT PLANNED | Explicitly excluded |

---

## Feature Status Summary

### Language Features

| Feature | Status | Gaps/Notes |
|---------|--------|------------|
| [Completion](./features/completion.md) | IMPLEMENTED | Custom functions/classes not configurable |
| [Hover](./features/hover.md) | IMPLEMENTED | SQL placeholders not yet supported |
| [Signature Help](./features/signature-help.md) | IMPLEMENTED | 367 built-in functions covered |
| [Go to Definition](./features/definition.md) | IMPLEMENTED | Single-file only |
| [Find References](./features/references.md) | IMPLEMENTED | Single-file only |
| [Document Symbols](./features/document-symbols.md) | IMPLEMENTED | Procedures, publics, regions |
| [Workspace Symbols](./features/workspace-symbols.md) | PARTIAL | Open documents only, no indexing |
| [Folding Ranges](./features/folding-ranges.md) | IMPLEMENTED | Procedures, regions, comments, control flow blocks |
| [Formatting](./features/formatting.md) | IMPLEMENTED | SSL + embedded SQL |
| [Diagnostics](./features/diagnostics.md) | PARTIAL | See diagnostic gaps below |
| [Snippets](./features/snippets.md) | IMPLEMENTED | 25+ code templates |

### Workspace Features

| Feature | Status | Notes |
|---------|--------|-------|
| Configuration | IMPLEMENTED | Via `workspace/didChangeConfiguration` |
| Workspace Folders | NOT PLANNED | Single-file focus |
| File Watching | NOT PLANNED | No background indexing |

### Not Implemented

| Feature | Reason |
|---------|--------|
| `textDocument/rename` | Future enhancement |
| `textDocument/codeAction` | No quick fixes defined |
| `textDocument/codeLens` | Not needed for SSL |
| `textDocument/inlayHint` | Future enhancement |
| `textDocument/semanticTokens` | Future enhancement |
| `callHierarchy/*` | Future enhancement |

---

## Diagnostic Capabilities

| Diagnostic | Status | Notes |
|------------|--------|-------|
| Unclosed blocks | IMPLEMENTED | `:IF` without `:ENDIF`, etc. |
| Unmatched delimiters | IMPLEMENTED | `(`, `[`, `{` matching |
| Block depth exceeded | IMPLEMENTED | Configurable max depth |
| Missing EXITCASE | IMPLEMENTED | SSL-specific requirement |
| Bare logical operators | IMPLEMENTED | `AND` vs `.AND.` |
| DEFAULT on DECLARE | IMPLEMENTED | Common SSL mistake |
| Global assignment | IMPLEMENTED | Protect configured globals |
| Hungarian notation | IMPLEMENTED | Optional style check |
| Undeclared variables | IMPLEMENTED | Disabled by default (opt-in) |
| Unused variables | PLANNED | Not yet implemented |
| SQL parameter validation | PLANNED | Not yet implemented |

### Known Diagnostic Gaps

The following behaviors are handled when undeclared variable checking is enabled (`CheckUndeclaredVars: true`), but the feature is disabled by default:

| Gap | Issue | Status |
|-----|-------|--------|
| `:INCLUDE` paths flagged as undeclared | #56 | Handled |
| Configured globals not recognized | #55 | Handled |
| `Me` keyword flagged as undeclared | #2 | Handled |
| Function calls flagged as undeclared | #53 | Handled |
| SQL parameter case-insensitive matching | #47, #25 | Open |
| Property access confused with variables | #22 | Handled |

---

## Formatting Capabilities

| Capability | Status | Notes |
|------------|--------|-------|
| Indentation | IMPLEMENTED | Tab or space, configurable |
| Operator spacing | IMPLEMENTED | Space around `:=`, `+`, etc. |
| Comma spacing | IMPLEMENTED | Space after commas |
| Semicolon enforcement | IMPLEMENTED | Add missing semicolons |
| Line wrapping | IMPLEMENTED | Configurable max length |
| Blank lines between procs | IMPLEMENTED | Configurable count |
| SQL formatting | IMPLEMENTED | Multiple styles available |

### Known Formatting Gaps

| Gap | Issue | Priority |
|-----|-------|----------|
| End-of-line comments moved | #11 | Medium |
| Multi-line structure collapsed | #33 | Medium |
| SQL function casing incorrect | #28 | Low |

---

## Completion Capabilities

| Category | Count | Status |
|----------|-------|--------|
| Keywords | 37 | IMPLEMENTED |
| Built-in Functions | 367 | IMPLEMENTED |
| Built-in Classes | 30 | IMPLEMENTED |
| Literals (`.T.`, `.F.`, `NIL`) | 3 | IMPLEMENTED |
| Operators (`.AND.`, `.OR.`, `.NOT.`) | 3 | IMPLEMENTED |
| Snippets | 25+ | IMPLEMENTED |
| Procedures (current document) | Dynamic | IMPLEMENTED |
| Variables (current scope) | Dynamic | IMPLEMENTED |
| Custom Functions | - | NOT SUPPORTED |
| Custom Classes | - | NOT SUPPORTED |

---

## Hover Capabilities

| Element | Status | Notes |
|---------|--------|-------|
| Keywords | IMPLEMENTED | Description and usage |
| Built-in Functions | IMPLEMENTED | Full signature + docs |
| Built-in Classes | IMPLEMENTED | Class description |
| Literals | IMPLEMENTED | `.T.`, `.F.`, `NIL` |
| Operators | IMPLEMENTED | `.AND.`, `.OR.`, `.NOT.` |
| Procedures | IMPLEMENTED | Signature with params |
| Variables | IMPLEMENTED | Declaration location |
| SQL `?param?` placeholders | PLANNED | Not yet implemented |
| SQL `?` positional placeholders | PLANNED | Not yet implemented |

---

## Cross-File Capabilities

| Capability | Status | Notes |
|------------|--------|-------|
| Go to definition (same file) | IMPLEMENTED | Works |
| Go to definition (other files) | NOT IMPLEMENTED | No workspace indexing |
| Find references (same file) | IMPLEMENTED | Works |
| Find references (workspace) | NOT IMPLEMENTED | No workspace indexing |
| `:INCLUDE` resolution | NOT IMPLEMENTED | Paths not followed |
| Namespace navigation | NOT IMPLEMENTED | `Namespace.Script.Proc` |

---

## Configuration Support

| Config Path | Status | Default |
|-------------|--------|---------|
| `ssl.format.indentStyle` | IMPLEMENTED | `"tab"` |
| `ssl.format.indentSize` | IMPLEMENTED | `4` |
| `ssl.format.maxLineLength` | IMPLEMENTED | `90` |
| `ssl.format.operatorSpacing` | IMPLEMENTED | `true` |
| `ssl.format.commaSpacing` | IMPLEMENTED | `true` |
| `ssl.format.semicolonEnforcement` | IMPLEMENTED | `true` |
| `ssl.format.blankLinesBetweenProcs` | IMPLEMENTED | `1` |
| `ssl.format.sql.enabled` | IMPLEMENTED | `true` |
| `ssl.format.sql.style` | IMPLEMENTED | `"standard"` |
| `ssl.format.sql.keywordCase` | IMPLEMENTED | `"upper"` |
| `ssl.diagnostics.hungarianNotation` | IMPLEMENTED | `false` |
| `ssl.diagnostics.hungarianPrefixes` | IMPLEMENTED | `["a","b","d","n","o","s"]` |
| `ssl.diagnostics.globals` | PARTIAL | `[]` |

---

## Version Milestones

### v1.0 (Current)
- Core LSP features implemented
- Single-file analysis
- Basic diagnostics
- SSL + SQL formatting

### v1.1 (Planned)
- Fix diagnostic gaps (undeclared variable handling)
- SQL placeholder hover support
- Improved formatting edge cases

### v2.0 (Future)
- Workspace indexing
- Cross-file navigation
- Inlay hints
- Rename support
