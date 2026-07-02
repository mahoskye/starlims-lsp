---
id: feature.workspace_symbols
title: Workspace symbol search
kind: feature
status: draft
authority: tool
schema_ref: null
config: []
tests:
  - internal/server/workspace_index_test.go
  - internal/server/handler_test.go
history:
  - date: 2026-01-10
    ref: "v0.1.0"
    note: Initial workspace symbols over open documents only.
  - date: 2026-03-28
    ref: "be7a174"
    note: >-
      Background workspace indexing added: on-disk SSL files are scanned at
      initialization and kept fresh via file watchers, so search covers the
      whole workspace, not just open documents.
---

## Behavior

Serves `workspace/symbol` across the entire workspace:

- Matching is case-insensitive substring match on procedure names; an empty
  query returns all known procedures. Fuzzy matching is NOT provided.
- Only procedures are returned: kind Function (12) for script files, kind
  Method (6) for files that begin with `:CLASS`. Variables and regions are
  not included.
- On initialization the server MUST index all workspace files with SSL
  extensions (`.srvscr`, `.ssl`, `.ssl.txt`, `.ds`, `.ds.txt`) in the
  background, and register file watchers so the index tracks creates,
  changes, and deletes without a restart.
- Open documents are authoritative: when a file is both open and indexed,
  results come from the open document and the index entry is skipped —
  no duplicates.
- When a document is closed, it is re-indexed from disk; when a watched file
  is deleted, its symbols are removed from the index.
- If the client provides no workspace root, indexing is skipped and only open
  documents are searched.
- Results are capped at 500 symbols. Each result carries the file URI and the
  procedure's line range.

## Acceptance

- A1: Given an open document defining `CalculateTotal` and `CalculateAverage`,
  when querying `calculate`, then both procedures are returned with kind 12
  (case-insensitive match).
- A2: Given a workspace file on disk that is not open and defines
  `FormatDate`, when querying `FormatDate` after indexing, then the symbol is
  returned with that file's URI.
- A3: Given a file that is both open and indexed, when querying a procedure it
  defines, then exactly one result is returned (from the open document) — the
  index MUST NOT contribute a duplicate.
- A4: Given an indexed file starting with `:CLASS`, when querying one of its
  procedures, then the symbol kind is Method (6), not Function (12).
- A5: Given a query matching nothing, when searching, then an empty result is
  returned and no error is raised.
- A6: Given an indexed file that is deleted on disk, when the watcher event is
  processed and the query is repeated, then its procedures MUST NOT appear.
- A7: Given a client that supplies no `rootURI` and no `workspaceFolders`,
  when searching, then only open documents are consulted and the server does
  not error.

## Rationale

v0.1.0 searched open documents only; be7a174 (2026-03-28) added the
background `WorkspaceIndex` so Ctrl+T works across a whole STARLIMS checkout.
Open-document priority keeps unsaved edits authoritative over stale disk
state; the 500-result cap and lightweight per-procedure index entries keep
large (10K-file) workspaces responsive.
