---
id: feature.document_symbols
title: Document symbols (outline)
kind: feature
status: draft
authority: tool
schema_ref: null
config: []
tests:
  - internal/providers/providers_test.go
  - internal/server/handler_test.go
history:
  - date: 2026-01-10
    ref: "v0.1.0"
    note: Initial document symbols for procedures, public variables, and regions.
---

## Behavior

Serves `textDocument/documentSymbol` with a hierarchical symbol tree:

- Every `:PROCEDURE` MUST appear as a symbol of kind Function (12), with its
  `:PARAMETERS` names as child symbols of kind Variable (13).
- Every `:PUBLIC` declaration MUST produce one Variable (13) symbol per
  declared name (`:PUBLIC a, b;` yields two symbols).
- `/* region Name;` ... `/* endregion;` comment markers MUST produce a
  Namespace (3) symbol containing the symbols declared inside the region.
  An unclosed region extends to end of file.
- Each symbol MUST carry a full `range` (declaration through its end, e.g.
  `:PROCEDURE` through `:ENDPROC`) and a `selectionRange` covering only the
  symbol name — never surrounding whitespace or punctuation.
- Symbols MUST be ordered by position in the file, not alphabetically.
- Procedures with no parameters still appear, with no children.
- Local `:DECLARE` variables and `:CLASS` blocks are NOT emitted as symbols.

## Acceptance

- A1: Given a document with `:PROCEDURE MyProc; :PARAMETERS sA, nB;`, when
  document symbols are requested, then one Function symbol named `MyProc` is
  returned with children `sA` and `nB` of kind Variable.
- A2: Given `:PUBLIC gName, gVersion;`, when symbols are requested, then two
  separate Variable symbols are returned.
- A3: Given procedures wrapped in `/* region Ops;` ... `/* endregion;`, when
  symbols are requested, then a Namespace symbol `Ops` is returned containing
  those procedures as children, and a procedure after the `endregion` marker
  is a sibling, not a child.
- A4: Given a procedure declaration with extra whitespace around the name,
  when symbols are requested, then `selectionRange` covers exactly the name
  and MUST NOT include whitespace or the trailing semicolon.
- A5: Given two procedures in reverse-alphabetical file order, when symbols
  are requested, then results follow file order.
- A6: Given a document containing only `:DECLARE` locals inside a procedure,
  when symbols are requested, then those locals MUST NOT appear as symbols.
- A7: Given an empty document, when symbols are requested, then the result is
  empty and the server does not error.

## Rationale

An outline restricted to procedures, publics, and regions matches how SSL
scripts are actually navigated (v0.1.0 initial design). File order and
name-only selection ranges are what editors expect for breadcrumbs and
Ctrl+Shift+O. Locals are excluded to keep the outline signal-dense.
