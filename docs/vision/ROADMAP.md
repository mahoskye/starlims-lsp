# Project Roadmap

This document outlines the prioritized feature roadmap for the starlims-lsp project.

**Last Updated:** 2025-02-02

---

## Versioning Strategy

- **Major versions (v2.0):** Significant new capabilities
- **Minor versions (v1.x):** Feature additions and improvements
- **Patch versions (v1.0.x):** Bug fixes

---

## Current Release: v1.0

### Implemented Features

| Category | Feature | Status |
|----------|---------|--------|
| Completion | Keywords, functions, classes, snippets | Complete |
| Hover | Function signatures, keyword docs | Complete |
| Signature Help | 367 built-in functions | Complete |
| Navigation | Go to definition, find references | Single-file |
| Symbols | Document symbols, folding ranges | Complete |
| Formatting | SSL code + embedded SQL | Complete |
| Diagnostics | Block matching, basic checks | Partial |

---

## v1.1 - Diagnostic Improvements

**Goal:** Fix known diagnostic gaps and improve accuracy.

**Target:** Q1 2025

### High Priority

| Issue | Description | Effort |
|-------|-------------|--------|
| #56 | `:INCLUDE` paths flagged as undeclared | Small |
| #55 | Configured globals not recognized as declared | Small |
| #2 | `Me` keyword flagged as undeclared | Small |
| #53 | Function calls flagged as undeclared | Small |

**Implementation Notes:**

For undeclared variable checking, skip these patterns:
- Lines starting with `:INCLUDE`
- The `Me` identifier (class self-reference)
- Identifiers followed by `(` (function calls)
- Identifiers preceded by `:` (property access)
- Variables in configured `globals` list

### Medium Priority

| Issue | Description | Effort |
|-------|-------------|--------|
| #47, #25 | SQL parameter case-insensitive matching | Medium |
| #22 | Property access confused with variables | Small |
| #52 | Comment block semicolon edge cases | Medium |

---

## v1.2 - Hover Enhancements

**Goal:** Improve hover information for SQL and special contexts.

**Target:** Q2 2025

### Features

| Issue | Description | Effort |
|-------|-------------|--------|
| #15 | Hover for named SQL parameters (`?varName?`) | Medium |
| #13 | Hover for positional SQL placeholders (`?`) | Medium |
| #27 | Skip hover in strings/comments | Small |
| #30 | Verify DoProc signature correctness | Small |

**Implementation Notes:**

SQL parameter hover should show:
- Parameter name
- Value (if statically determinable)
- Declaration location

---

## v1.3 - Formatting Refinements

**Goal:** Improve formatting edge cases.

**Target:** Q2 2025

### Features

| Issue | Description | Effort |
|-------|-------------|--------|
| #11 | Preserve end-of-line comments | Medium |
| #33 | Preserve multi-line function structure | Medium |
| #8 | Don't modify comment content | Small |
| #31 | Better continuation line indentation | Medium |

---

## v2.0 - Workspace Features

**Goal:** Enable cross-file analysis and navigation.

**Target:** Q3-Q4 2025

### Major Features

| Feature | Description | Effort |
|---------|-------------|--------|
| Workspace Indexing | Index all SSL files in workspace | Large |
| Cross-file Definition | Navigate to definitions in other files | Large |
| Cross-file References | Find references across workspace | Large |
| `:INCLUDE` Resolution | Follow include paths | Medium |
| Namespace Navigation | Navigate `Namespace.Script.Proc` | Medium |

**Prerequisites:**
- File system watching
- Background indexing
- Index persistence
- Incremental updates

### Additional Features

| Feature | Description | Effort |
|---------|-------------|--------|
| #16 | Go-to-definition for `ExecFunction`/`DoProc` | Medium |
| #36 | Context-aware reference finding | Medium |
| #40 | Scope-aware rename | Large |

---

## v2.1 - Advanced Features

**Goal:** Add sophisticated IDE features.

**Target:** 2026

### Features

| Feature | Description | Effort |
|---------|-------------|--------|
| Inlay Hints | Parameter name hints in calls | Large |
| Semantic Tokens | Enhanced syntax highlighting | Medium |
| Call Hierarchy | Incoming/outgoing call analysis | Large |
| Code Actions | Quick fixes for diagnostics | Large |

---

## Future Considerations

These features are under consideration but not yet scheduled:

| Feature | Notes |
|---------|-------|
| `textDocument/codeLens` | Reference counts, test status |
| `textDocument/documentLink` | Clickable `:INCLUDE` paths |
| `textDocument/selectionRange` | Smart selection expansion |
| Snippet customization | User-defined snippets |
| Custom function definitions | Project-specific function hints |
| Custom class definitions | Project-specific class hints |

---

## Known Limitations (No Plans to Address)

| Limitation | Reason |
|------------|--------|
| STARLIMS server connection | Out of scope - LSP is for editing, not runtime |
| Database schema awareness | Requires server connection |
| Real-time debugging | Separate tool concern |
| Build/deployment | Not a language server responsibility |

---

## Technical Debt

Items to address as part of ongoing maintenance:

| Item | Priority | Notes |
|------|----------|-------|
| Add more unit tests | Medium | Improve coverage |
| Incremental document sync | Low | Performance for large files |
| Structured logging | Low | Better debugging |
| Benchmark suite | Low | Performance regression detection |

---

## How to Contribute

See the [main README](../../README.md) for:
- Setting up development environment
- Running tests
- Submitting pull requests

### Priority for Contributors

1. **High-value, low-effort:** v1.1 diagnostic fixes
2. **Test coverage:** Add tests for existing features
3. **Documentation:** Improve examples and edge cases
4. **Bug fixes:** Issues labeled "good first issue"

---

## Revision History

| Date | Version | Changes |
|------|---------|---------|
| 2025-02-02 | 1.0 | Initial roadmap from ISSUE_ALIGNMENT analysis |
