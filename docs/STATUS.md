# Implementation Status Dashboard

This document provides a quick overview of all LSP features and their current implementation status.

**Last Updated:** 2026-03-21

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
| [Completion](./features/completion.md) | IMPLEMENTED | Broad symbol/snippet list, excludes strings/comments, class-aware procedure dispatch snippets |
| [Hover](./features/hover.md) | IMPLEMENTED | Includes `Me` keyword and SQL placeholders |
| [Signature Help](./features/signature-help.md) | IMPLEMENTED | 330 canonical built-in functions and dispatch helpers such as `DoProc` / `ExecFunction` |
| [Go to Definition](./features/definition.md) | IMPLEMENTED | Single-file, scope precedence, DoProc/ExecFunction string targets |
| [Find References](./features/references.md) | IMPLEMENTED | Single-file, scope-aware for local vars |
| [Rename](./features/rename.md) | IMPLEMENTED | Single-file, scope-aware, validates new name |
| [Inlay Hints](./features/inlay-hints.md) | IMPLEMENTED | Parameter name hints for function calls |
| [Document Symbols](./features/document-symbols.md) | IMPLEMENTED | Hierarchical: comment regions contain procedures |
| [Workspace Symbols](./features/workspace-symbols.md) | IMPLEMENTED | Workspace-wide indexing with file watching |
| [Folding Ranges](./features/folding-ranges.md) | IMPLEMENTED | Procedures, comment regions, comments, control flow blocks |
| [Formatting](./features/formatting.md) | IMPLEMENTED | SSL + embedded SQL |
| [Diagnostics](./features/diagnostics.md) | IMPLEMENTED | Full diagnostic suite with opt-in checks |
| [Snippets](./features/snippets.md) | IMPLEMENTED | 25+ code templates |

### Workspace Features

| Feature | Status | Notes |
|---------|--------|-------|
| Configuration | IMPLEMENTED | Via `workspace/didChangeConfiguration` |
| Workspace Indexing | IMPLEMENTED | Background scan of `.srvscr`, `.ssl`, `.ssl.txt`, `.ds`, `.ds.txt` files |
| File Watching | IMPLEMENTED | Dynamic registration via `client/registerCapability` |
| Workspace Folders | IMPLEMENTED | Supports `rootURI` and `workspaceFolders` |

### Not Implemented

| Feature | Reason |
|---------|--------|
| `textDocument/codeAction` | No quick fixes defined |
| `textDocument/codeLens` | Not needed for SSL |
| `textDocument/semanticTokens` | Future enhancement |
| `callHierarchy/*` | Future enhancement |

---

## Diagnostic Capabilities

| Diagnostic | Status | Notes |
|------------|--------|-------|
| Unclosed blocks | IMPLEMENTED | `:IF` without `:ENDIF`, etc. |
| Unmatched delimiters | IMPLEMENTED | `(`, `[`, `{` matching |
| Block depth exceeded | IMPLEMENTED | Configurable max depth |
| Missing EXITCASE | IMPLEMENTED | Style-guide warning for `:CASE` / `:OTHERWISE` blocks |
| Bare logical operators | IMPLEMENTED | `AND` vs `.AND.` |
| DEFAULT on DECLARE | IMPLEMENTED | Common SSL mistake |
| CATCH clause form | IMPLEMENTED | `:CATCH` cannot name an exception variable |
| Branch target labels | IMPLEMENTED | Literal `Branch("...")` targets must include `LABEL` |
| Global assignment | IMPLEMENTED | Protect configured globals |
| Hungarian notation | IMPLEMENTED | Optional style check |
| Undeclared variables | IMPLEMENTED | Disabled by default (opt-in) |
| Unused variables | IMPLEMENTED | Disabled by default (opt-in) |
| SQL parameter validation | IMPLEMENTED | Disabled by default (opt-in) |

### Gotcha Diagnostics (v1.4)

These diagnostics detect common SSL mistakes documented in [gotchas.md](./ssl-reference/gotchas.md):

| Gotcha | Diagnostic | Status |
|--------|------------|--------|
| #1 | Direct procedure calls | IMPLEMENTED |
| #2 | Missing `:EXITCASE` | IMPLEMENTED |
| #3 | `:DEFAULT` with `:DECLARE` | IMPLEMENTED |
| #4 | Bare logical operators | IMPLEMENTED |
| #5 | Zero-based array indexing | IMPLEMENTED |
| #6 | Semicolon in comments | PARTIAL (same-line trailing-code heuristic) |
| #7 | Named SQL params in wrong functions | IMPLEMENTED |
| #8 | Dot property notation | IMPLEMENTED |
| #9 | Assignment in conditions | IMPLEMENTED |
| #10 | Loose string equality | IMPLEMENTED (conservative inferred-type coverage) |
| #11 | NIL vs Empty | IMPLEMENTED (conservative inferred-type coverage) |
| #12 | Lowercase keywords | IMPLEMENTED (parser) |
| #13 | Property as undeclared | IMPLEMENTED |
| #14 | `Str()` vs `LimsString()` | REFERENCE ONLY |
| #15 | Parentheses for class instantiation | IMPLEMENTED |
| #16 | `!=` asymmetry with `=` for strings | REFERENCE ONLY |
| #17 | Function name casing (PascalCase) | REFERENCE ONLY |
| #18 | SQL parameter names are case-insensitive | IMPLEMENTED |
| #19 | Default variable value is `""`, not `NIL` | IMPLEMENTED (conservative) |
| #20 | SQLExecute complex expression in placeholder | IMPLEMENTED |
| #21 | `:FOR` loop `:STEP` keyword spacing | IMPLEMENTED (formatter + diagnostic) |
| #22 | `:REGION` is functional, not a folding marker | IMPLEMENTED (info diagnostic) |
| #23 | `:ENDFOR` is not valid — use `:NEXT` | IMPLEMENTED (diagnostic + parser) |
| #24 | `:FINALLY` block restrictions | IMPLEMENTED |
| #25 | Visibility annotations on class methods | IMPLEMENTED |
| #26 | `:INCLUDE` placement | IMPLEMENTED |
| Branch label token text | IMPLEMENTED | Literal `Branch("...")` targets must include `LABEL` |

### Known Diagnostic Gaps

The following behaviors are handled when undeclared variable checking is enabled (`CheckUndeclaredVars: true`), but the feature is disabled by default:

| Gap | Issue | Status |
|-----|-------|--------|
| `:INCLUDE` paths flagged as undeclared | #56 | Handled |
| Configured globals not recognized | #55 | Handled |
| `Me` keyword flagged as undeclared | #2 | Handled |
| Function calls flagged as undeclared | #53 | Handled |
| SQL parameter case-insensitive matching | #47, #25 | Resolved |
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
| End-of-line comments | IMPLEMENTED | Preserved on same line |
| Multi-line structure | IMPLEMENTED | Continuation indentation |
| SQL function casing | IMPLEMENTED | COUNT, SUM, AVG, etc. |
| SQL string detection | IMPLEMENTED | Auto-detect SQL in any string |

### Known Formatting Gaps

*No known formatting gaps at this time.*

---

## Completion Capabilities

| Category | Count | Status |
|----------|-------|--------|
| Keywords | 38 | IMPLEMENTED |
| Built-in Functions | 330 | IMPLEMENTED |
| Built-in Classes | 22 | IMPLEMENTED |
| Literals (`.T.`, `.F.`, `NIL`) | 3 | IMPLEMENTED |
| Operators (`.AND.`, `.OR.`, `.NOT.`) | 3 | IMPLEMENTED |
| Snippets | 25+ | IMPLEMENTED |
| Procedures (current document) | Dynamic | IMPLEMENTED |
| Variables (current document) | Dynamic | IMPLEMENTED |
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
| `Me` keyword | IMPLEMENTED | Self-reference in classes |
| SQL `?param?` placeholders | IMPLEMENTED | Named parameter hover |
| SQL `?` positional placeholders | IMPLEMENTED | Position-aware hover |

---

## Cross-File Capabilities

| Capability | Status | Notes |
|------------|--------|-------|
| Workspace symbol search | IMPLEMENTED | All SSL files indexed, procedures searchable |
| Go to definition (same file) | IMPLEMENTED | Variables, procedures, DoProc/ExecFunction targets |
| Go to definition (other files) | PLANNED | Workspace index provides foundation; namespace path resolution needed |
| Find references (same file) | IMPLEMENTED | Works |
| Find references (workspace) | PLANNED | Workspace index provides foundation |
| `:INCLUDE` resolution | PLANNED | Paths not followed |
| Namespace navigation | PLANNED | `Category.Script.Procedure` path resolution |

---

## Configuration Support

| Config Path | Status | Default |
|-------------|--------|---------|
| `ssl.format.indentStyle` | IMPLEMENTED | `"tab"` |
| `ssl.format.indentSize` | IMPLEMENTED | `4` (space mode only; tabs remain the default style) |
| `ssl.format.maxLineLength` | IMPLEMENTED | `90` |
| `ssl.format.operatorSpacing` | IMPLEMENTED | `true` |
| `ssl.format.commaSpacing` | IMPLEMENTED | `true` |
| `ssl.format.semicolonEnforcement` | IMPLEMENTED | `true` |
| `ssl.format.blankLinesBetweenProcs` | IMPLEMENTED | `1` |
| `ssl.format.sql.enabled` | IMPLEMENTED | `true` |
| `ssl.format.sql.style` | IMPLEMENTED | `"canonicalCompact"` |
| `ssl.format.sql.keywordCase` | IMPLEMENTED | `"upper"` |
| `ssl.format.sql.detectSQLStrings` | IMPLEMENTED | `true` |
| `ssl.diagnostics.hungarianNotation` | IMPLEMENTED | `false` |
| `ssl.diagnostics.hungarianPrefixes` | IMPLEMENTED | `["a","b","d","fn","n","o","s","v"]` |
| `ssl.diagnostics.globals` | IMPLEMENTED | `[]` |
| `ssl.diagnostics.maxBlockDepth` | IMPLEMENTED | `4` |

---

## Version Milestones

### v1.0 (Complete)
- Core LSP features implemented
- Single-file analysis
- Basic diagnostics
- SSL + SQL formatting

### v1.1-v1.3 (Complete)
- Enhanced formatting (end-of-line comments, multi-line structure)
- SQL string detection and formatting
- Diagnostic improvements and gap fixes
- Hover enhancements (SQL placeholders, `Me` keyword)

### v1.4 (Complete)
- Gotcha diagnostics for common SSL mistakes
- 7 new diagnostic checks implemented
- See [gotchas.md](./ssl-reference/gotchas.md) for full list

### v1.5 (In Progress)
- Conservative local type inference for diagnostics implemented
- Class member metadata (properties/methods for 29 canonical built-in classes)
- Member completion after `object:`

### v2.0 (In Progress)
- Workspace indexing — IMPLEMENTED (background scan, file watching, workspace symbols)
- Cross-file go-to-definition — PLANNED (namespace path resolution)
- `:INCLUDE` resolution — PLANNED
