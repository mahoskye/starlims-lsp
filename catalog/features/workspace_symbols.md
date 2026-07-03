---
id: feature.workspace_symbols
title: Workspace symbol search
kind: feature
status: active
authority: tool
schema_ref: null
config: []
tests:
  - internal/server/workspace_index_test.go
  - internal/server/handler_test.go
history:
  - date: 2026-01-10
    ref: "442fa69 (v0.1.0)"
    note: Initial workspace symbols over open documents only.
  - date: 2026-03-28
    ref: "be7a174"
    note: >-
      Background workspace indexing added: on-disk SSL files are scanned at
      initialization (4 bounded workers) and kept fresh via registered file
      watchers, so search covers the whole workspace, not just open
      documents. 500-result cap introduced.
issues: ["#45"]
---

## Behavior

Serves `workspace/symbol` across the entire workspace:

- Matching is case-insensitive substring match on procedure names; an empty
  query returns all known procedures. Fuzzy matching is NOT provided.
- Only procedures are returned; variables and regions MUST NOT appear.
  Index results use kind Function (12) for script files and kind Method (6)
  for files whose first significant token is `:CLASS`. (Open-document
  results are currently always Function — see Known gaps.)
- On initialization the server MUST index all workspace files with SSL
  extensions (`.srvscr`, `.ssl`, `.ssl.txt`, `.ds`, `.ds.txt`,
  case-insensitive) in the background, and register file watchers for those
  globs so the index tracks creates, changes, and deletes without a restart.
- Open documents are authoritative: when a file is both open and indexed,
  results come from the open document and the index entry is skipped — the
  same procedure MUST NOT appear twice.
- When a document is closed it is re-indexed from disk; when a watched file
  is deleted, its symbols are removed from the index and MUST NOT appear in
  later searches.
- If the client provides no `workspaceFolders`/`rootURI`/`rootPath`,
  indexing is skipped and only open documents are searched; the request
  still succeeds.
- Results are capped at 500 symbols. Each result carries the file URI and
  the procedure's line range.

## Acceptance

- A1: Given an open document defining `ProcA` and `ProcB`, when querying `procb` (lowercase), then `ProcB` is returned with kind Function (12) and the open document's URI — case-insensitive substring matching.
- A2: Given a workspace file on disk that is not open, when the background index has scanned the workspace roots and the file's procedure name is queried, then the symbol is returned with that file's URI; non-SSL files in the same tree are never indexed.
- A3: Given a file that is both open and indexed, when querying a procedure it defines, then exactly one result is returned (from the open document) — the index MUST NOT contribute a duplicate.
- A4: Given an indexed file starting with `:CLASS`, when querying one of its procedures, then the symbol kind is Method (6), not Function (12).
- A5: Given a query matching nothing, when searching, then an empty result is returned and no error is raised.
- A6: Given an indexed file that is removed from the index (deletion event), when the query is repeated, then its procedures MUST NOT appear.
- A7: Given a client that supplies no workspace root, when searching, then only open documents are consulted and the server does not error.

## Rationale

v0.1.0 searched open documents only; be7a174 (2026-03-28) added the
background `WorkspaceIndex` so Ctrl+T works across a whole STARLIMS
checkout. Open-document priority keeps unsaved edits authoritative over
stale disk state, and the skip-open-URIs rule (A3) is what prevents the
double-entry noise that plagues naive merge implementations. Procedures-only
keeps the index lightweight (name, parameters, line range per entry) so a
10K-file workspace stays in the tens of megabytes and searches in
microseconds; the 500-result cap protects clients from unbounded empty-query
responses. Substring rather than fuzzy matching is deliberate: STARLIMS
procedure names are long and prefix-heavy, and substring results are
predictable.

## Known gaps

- Open-document results are always kind Function (12): handleWorkspaceSymbol
  phase 1 (internal/server/handler.go) hardcodes `SymbolKindFunction`, so a
  `:CLASS` file that is currently open reports its procedures as Function
  while the same file reports Method (6) once closed and re-indexed. The
  index-side kind (A4) is the intended behavior; align the open-document
  path in a follow-up PR citing this entry.
- The deletion path is specified at the index level (A6 exercises
  RemoveFile); the `workspace/didChangeWatchedFiles` handler wiring that
  invokes it has no direct test.
