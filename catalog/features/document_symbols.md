---
id: feature.document_symbols
title: Document symbols (outline)
kind: feature
status: active
authority: tool
schema_ref: null
config: []
tests:
  - internal/providers/providers_test.go
  - internal/server/handler_test.go
history:
  - date: 2026-01-10
    ref: "442fa69 (v0.1.0)"
    note: Initial document symbols for procedures (with parameter children)
      and public variables.
  - date: 2026-02-02
    ref: "d56cbfe (v0.2.0), vs-code-ssl-formatter#12"
    note: Hierarchical symbols added — region comment markers become
      Namespace containers holding the procedures and publics declared
      inside them; fixes the empty-outline report.
  - date: 2026-07-02
    ref: "issue #44"
    note: selectionRange now covers exactly the procedure name identifier,
      located from the declaration line's tokens.
  - date: 2026-08-28
    ref: "issue #184 (expression AST consumers)"
    note: >-
      Declared names now come from statement-based declaration resolution
      (parser.CollectDeclarations). A declaration written as a bare
      `:DECLARE` / `:PARAMETERS` with its names on the following lines
      previously produced no names at all, so every name it declared was
      invisible to this behavior. No change on the production corpus's
      default-on output; the names it recovers were simply missing before.
issues: ["#44"]
---

## Behavior

Serves `textDocument/documentSymbol` with a hierarchical symbol tree:

- Every `:PROCEDURE` MUST appear as a symbol of kind Function (12), with its
  `:PARAMETERS` names as child symbols of kind Variable (13). Procedures
  with no parameters still appear, with no children.
- Every `:PUBLIC` declaration MUST produce one Variable (13) symbol per
  declared name (`:PUBLIC a, b;` yields two symbols).
- `/* region Name;` ... `/* endregion;` comment markers MUST produce a
  Namespace (3) symbol containing the procedure and public-variable symbols
  declared inside the region; a symbol after the `endregion` marker is a
  sibling, not a child. An unclosed region extends to end of file.
- Each symbol MUST carry a full `range` (declaration through its end, e.g.
  `:PROCEDURE` through `:ENDPROC`) and a `selectionRange` covering only the
  symbol name — never surrounding keywords, whitespace, or punctuation.
  (Procedure selectionRanges currently start at column 0 and span the
  `:PROCEDURE ` keyword too — see Known gaps / A4.)
- Symbols of the same kind MUST follow file order, never alphabetical
  order. Ungrouped procedures and public variables are listed before the
  region containers in the response.
- Local `:DECLARE` variables and `:CLASS` blocks MUST NOT be emitted as
  symbols — the outline stays signal-dense.
- An empty or symbol-free document returns an empty result, not an error.

## Acceptance

- A1: Given a document with `:PROCEDURE MyProc;` and `:PARAMETERS sA, nB;`, when document symbols are requested, then a Function symbol is returned for the procedure and any parameter children carry kind Variable.
- A2: Given `:PUBLIC gVar1, gVar2;`, when symbols are requested, then two separate Variable symbols are returned, one per declared name.
- A3: Given procedures wrapped in `/* region Helpers;` ... `/* endregion;`, when symbols are requested, then a Namespace symbol `Helpers` contains those procedures as children, and a procedure outside the markers is a top-level sibling, not a child.
- A4: Given a procedure declaration, when symbols are requested, then `selectionRange` covers exactly the procedure name — not the `:PROCEDURE` keyword, whitespace, or the trailing semicolon.
- A5: Given two procedures in reverse-alphabetical file order, when symbols are requested, then the results follow file order, not alphabetical order.
- A6: Given a procedure containing only `:DECLARE` locals, when symbols are requested, then those locals do NOT appear as symbols at any level.
- A7: Given an empty document, when symbols are requested, then the result is empty and the server does not error.

## Rationale

An outline restricted to procedures, publics, and regions matches how SSL
scripts are actually navigated: procedures are the unit of work, publics are
the file's surface, and regions are the author's own grouping (d56cbfe,
extension issue #12 — the original outline shipped flat and users asked for
their region structure back). Locals are excluded deliberately; a 2,000-line
script with hundreds of `:DECLARE`s would drown the ten procedures the user
actually jumps between. File order (not alphabetical) is what breadcrumbs
and Ctrl+Shift+O expect. Name-only selection ranges are the LSP contract for
rename/highlight anchoring; the current keyword-inclusive ranges are a
defect kept normative in A4 so the fix has a target.

## Known gaps

- Parameter children still carry a column-0 selection-range approximation
  (their names are not located on the declaration line); procedure
  selectionRange itself is exact per A4.
- Region containers are appended after ungrouped symbols instead of being
  interleaved at their file position, so the top level is grouped-by-kind
  rather than strictly position-sorted. Editors that re-sort by range hide
  this; entry keeps per-kind file order normative (A5) and leaves top-level
  interleaving unspecified.
