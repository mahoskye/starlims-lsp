---
id: feature.completion
title: Completion
kind: feature
status: active
authority: tool
schema_ref: null
config: []
tests:
  - internal/providers/providers_test.go
  - internal/providers/udobject_shapes_test.go
history:
  - date: 2026-05-06
    ref: "PR #10 (v0.7.4), issues #8/#9"
    note: >-
      Removed `,`, `.`, and `(` as auto-trigger characters — they fired
      during list/decimal/expression entry and Enter-selected wrong tokens
      (e.g. .AND. while typing a list literal). Only `:` auto-triggers.
  - date: 2026-05-09
    ref: "PR #13, issues #11/#12"
    note: >-
      `:` trigger gated: keyword completions only offered when the `:` is
      preceded by whitespace/start-of-line; accepting a keyword suggestion
      replaces the typed `:` instead of duplicating it.
issues: []
---

## Behavior

The server answers `textDocument/completion` under a deliberately narrow
trigger policy:

- The only advertised trigger character is `:`. `.`, `,`, and `(` must never
  auto-open completion; the full inventory is available on explicit
  invocation (Ctrl+Space).
- On `:` auto-trigger the response depends on context:
  - After `Me`/`Base` inside a `:CLASS` file, after a built-in class
    instance, or after a shape-inferred UDObject variable: only that
    receiver's members.
  - Otherwise, when the `:` is preceded by whitespace or start-of-line:
    keyword completions only (no procedures, variables, or snippets).
  - Accepting a keyword completion replaces the already-typed `:` — the
    result is `:DECLARE`, never `::DECLARE`.
- Explicit invocation returns the full set: keywords, built-in functions and
  classes, literals, operators, document-local procedures and variables, and
  snippets. Inside `:CLASS` methods it adds class-context forms (`Me`,
  `Base`, `Constructor`) and inserts sibling methods as `Me:Method(...)`.
- Class-only forms are never suggested outside a `:CLASS` method.
- Inside string literals and comments, no completions are returned.
- Variables initialized via `CreateUDObject({{...}})` get an inferred
  property shape (inherited through `:clone()`); member completion after
  `<var>:` lists exactly the inferred properties.

## Acceptance

- A1: Given the user types `:` at the start of a line, when completion
  auto-triggers, then only keyword items are returned, and accepting one
  yields a single leading `:`.
- A2: Given the user types `,` or `.` or `(` in ordinary code, when the
  client would consult trigger characters, then completion does NOT
  auto-open.
- A3: Given `oObj:` where oObj has a CreateUDObject-inferred shape, when
  completion triggers, then exactly the inferred properties are listed.
- A4: Given `Me:` inside a `:CLASS` method, when completion triggers, then
  the enclosing class's members are listed; given `Me` typed outside a class
  file, class-context forms are NOT suggested.
- A5: Given the cursor is inside a string literal or comment, when completion
  is requested, then no items are returned.
- A6: Given `x:` immediately after an identifier with no known shape, when
  completion auto-triggers, then procedures/variables/snippets are NOT
  included in the response.

## Rationale

Completion earns trust by staying out of the way: the v0.7.4/v0.7.6 noise
decisions (history) established that a wrong auto-popup is worse than no
popup, because Enter-acceptance corrupts code mid-flow. The trigger policy
is therefore a contract, not an implementation detail — broadening it again
requires editing this entry first.
