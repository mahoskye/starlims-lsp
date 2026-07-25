# Project Roadmap

This document outlines the prioritized feature roadmap for the starlims-lsp project.

**Last Updated:** 2026-03-21

---

## Versioning Strategy

- **Major versions (v2.0):** Significant new capabilities
- **Minor versions (v1.x):** Feature additions and improvements
- **Patch versions (v1.0.x):** Bug fixes

---

## Current Release: v1.4

### Implemented Features

| Category | Feature | Status |
|----------|---------|--------|
| Completion | Keywords, functions, classes, snippets | ✅ Complete |
| Hover | Function signatures, keyword docs, SQL placeholders, `Me` keyword | ✅ Complete |
| Signature Help | 330 canonical built-in functions and dispatch helpers | ✅ Complete |
| Navigation | Go to definition, find references, rename, DoProc/ExecFunction targets | ✅ Cross-file |
| Symbols | Document symbols (hierarchical), folding ranges (including control flow) | ✅ Complete |
| Formatting | SSL code + embedded SQL, end-of-line comments, SQL string detection | ✅ Complete |
| Diagnostics | Full suite with gotcha checks + opt-in undeclared/unused/SQL param checks | ✅ Complete |

---

## v1.1 - Diagnostic Improvements (COMPLETED)

**Goal:** Fix known diagnostic gaps and improve accuracy.

**Status:** ✅ COMPLETED

### High Priority (All Resolved)

| Issue | Description | Status |
|-------|-------------|--------|
| #56 | `:INCLUDE` paths flagged as undeclared | ✅ Handled |
| #55 | Configured globals not recognized as declared | ✅ Handled |
| #2 | `Me` keyword flagged as undeclared | ✅ Handled |
| #53 | Function calls flagged as undeclared | ✅ Handled |

### Medium Priority (All Resolved)

| Issue | Description | Status |
|-------|-------------|--------|
| #47, #25 | SQL parameter case-insensitive matching | ✅ Resolved |
| #22 | Property access confused with variables | ✅ Handled |
| #52 | Comment block semicolon edge cases | Deferred |

---

## v1.2 - Hover Enhancements (COMPLETED)

**Goal:** Improve hover information for SQL and special contexts.

**Status:** ✅ COMPLETED

### Features (All Implemented)

| Issue | Description | Status |
|-------|-------------|--------|
| #15 | Hover for named SQL parameters (`?varName?`) | ✅ Implemented |
| #13 | Hover for positional SQL placeholders (`?`) | ✅ Implemented |
| #27 | Skip hover in strings/comments | ✅ Context filtering implemented |
| #30 | Verify DoProc signature correctness | ✅ Implemented |

---

## v1.3 - Formatting Refinements (COMPLETED)

**Goal:** Improve formatting edge cases.

**Status:** ✅ COMPLETED

### Features (All Implemented)

| Issue | Description | Status |
|-------|-------------|--------|
| #11 | Preserve end-of-line comments | ✅ Implemented |
| #33 | Preserve multi-line function structure | ✅ Implemented |
| #8 | Don't modify comment content | ✅ Implemented |
| #31 | Better continuation line indentation | ✅ Implemented |
| - | SQL function casing (COUNT, SUM, etc.) | ✅ Implemented |
| - | SQL string auto-detection | ✅ Implemented |

---

## v1.4 - Gotcha Diagnostics (COMPLETED)

**Goal:** Detect common SSL mistakes documented in gotchas.md.

**Status:** ✅ COMPLETED

### Features (All Implemented)

| Gotcha | Description | Status |
|--------|-------------|--------|
| #1 | Direct procedure calls (should use DoProc/ExecFunction) | ✅ Implemented |
| #5 | Zero-based array indexing (SSL is 1-based) | ✅ Implemented |
| #7 | Named SQL params (`?name?`) in wrong functions | ✅ Implemented |
| #8 | Dot property access (should use colon) | ✅ Implemented |
| #9 | Assignment (`:=`) in conditions | ✅ Implemented |
| #15 | Parentheses for class instantiation (should use `{}`) | ✅ Implemented |
| - | Missing quotes in ExecFunction arguments | ✅ Implemented |

See [gotchas.md](../ssl-reference/gotchas.md) for full documentation of all SSL gotchas and their LSP detection status.

---

## v1.5 - Type Inference & Class Members (IN PROGRESS)

**Goal:** Enable intelligent completion for object properties and methods.

**Status:** 🔄 IN PROGRESS

### Phase 1: Class Member Metadata (Next Up)

| Task | Description | Status |
|------|-------------|--------|
| Define class member structure | Properties/methods for 29 canonical built-in classes | ⏳ Pending |
| Add SSLDataset members | Most commonly used class | ⏳ Pending |
| Add Email members | Common utility class | ⏳ Pending |
| Add SSLExpando members | Dynamic object class | ⏳ Pending |
| Add remaining 23 classes | Complete coverage | ⏳ Pending |

### Phase 2: Type Inference System

| Task | Description | Status |
|------|-------------|--------|
| Track `ClassName{}` instantiation | `oEmail := Email{}` → local inferred object type for diagnostics | 🔄 Partial |
| Track `CreateUdObject()` calls | Local inferred object type for diagnostics | 🔄 Partial |
| Infer types from function returns | Coarse built-in return-type mapping for diagnostics | 🔄 Partial |
| SSLExpando dynamic properties | Track `oObj:propName := value` | ⏳ Pending |

### Phase 3: Member Completion

| Task | Description | Status |
|------|-------------|--------|
| Trigger on `:` after typed variable | Context-aware completion | ⏳ Pending |
| Filter by property vs method | Show appropriate icon | ⏳ Pending |
| Include inherited members | For classes with INHERIT | ⏳ Pending |

---

## v1.6 - Additional Gotcha Refinement (PLANNED)

**Goal:** Deepen currently conservative gotcha coverage and add low-cost reference checks.

**Status:** 📋 PLANNED

| Gotcha | Description | Complexity |
|--------|-------------|------------|
| #10 | Loose string equality (`=` vs `==`) | Medium - already implemented conservatively; future work is broader inference coverage |
| #11 | NIL vs Empty confusion | Medium - already implemented conservatively; future work is broader inference coverage |
| #14 | `Str()` vs `LimsString()` confusion | Low - pattern matching |
| #6 | Semicolon in comments | Partial heuristic implemented; full intent detection still open |

---

## v2.0 - Workspace Features

**Goal:** Enable cross-file analysis and navigation.

**Target:** 2026

### Major Features

| Feature | Description | Effort | Status |
|---------|-------------|--------|--------|
| Workspace Indexing | Index all SSL files in workspace | Large | DONE |
| File Watching | Dynamic registration via `client/registerCapability` | Medium | DONE |
| Workspace Symbols | Search procedures across all indexed files | Medium | DONE |
| Cross-file Definition | Navigate to definitions in other files | Large | Planned |
| Cross-file References | Find references across workspace | Large | Planned |
| `:INCLUDE` Resolution | Follow include paths | Medium | Planned |
| Namespace Navigation | Navigate `Category.Script.Procedure` paths | Medium | Planned |

**Completed prerequisites:**
- File system watching (dynamic registration for `.srvscr`, `.ssl`, `.ssl.txt`, `.ds`, `.ds.txt`)
- Background indexing (4-worker bounded concurrency)
- Incremental updates (file watcher events + re-index on document close)

**Remaining prerequisites for cross-file navigation:**
- Namespace path resolution (`Category.Script.Procedure` to filesystem path)
- STARLIMS filesystem layout mapping (`Server Scripts/CATEGORY/SCRIPT.srvscr`)

### Additional Features

| Feature | Description | Status |
|---------|-------------|--------|
| #16 | Go-to-definition for `ExecFunction`/`DoProc` | ✅ Same-file implemented |
| #36 | Context-aware reference finding | ✅ Implemented |
| #40 | Scope-aware rename | ✅ Implemented |

---

## v2.1 - Advanced Features

**Goal:** Add sophisticated IDE features.

**Target:** 2026

### Features

| Feature | Description | Effort |
|---------|-------------|--------|
| Inlay Hints | Parameter name hints in calls | ✅ Implemented |
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
| 2026-02-03 | 1.4 | Added v1.4 (gotcha diagnostics), v1.5 (type inference), v1.6 (additional gotchas) |
| 2026-02-02 | 1.1 | Updated to reflect completed v1.1, v1.2, v1.3 features |
| 2025-02-02 | 1.0 | Initial roadmap from ISSUE_ALIGNMENT analysis |
