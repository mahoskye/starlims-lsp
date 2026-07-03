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
  - date: 2026-07-03
    ref: "feature.cross_file_resolution"
    note: >-
      Dispatch-string completion made segment-aware and cross-file
      (categories-only level 0 per the noise policy); the pre-existing
      vs-code-ssl-formatter#74 same-file exception is now documented
      here and A5 narrowed to non-dispatch strings.
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

- Inside the first string argument of `DoProc(...)`/`ExecFunction(...)`
  completion IS offered (the one string-context exception, added for
  vs-code-ssl-formatter#74 and previously undocumented here — a spec gap
  found during the cross-file design). The list is segment-aware
  (feature.cross_file_resolution): before any dot, same-file procedures
  plus workspace category names only; after `Category.`, that category's
  scripts; after `Category.Script.` or flat `Script.`, that script's
  procedures with `/*@private;`/`/*@protected;` ones excluded. All items
  insert plain text. `.` remains a non-trigger character (the trigger
  policy above is unchanged): segment completions appear on explicit
  invocation or while the popup is already open — do not "fix" this by
  re-adding `.` as a trigger.

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
- A5: Given the cursor is inside a comment or a non-dispatch string literal, when completion is requested, then no items are returned.
- A6: Given `x:` immediately after an identifier with no known shape, when
  completion auto-triggers, then procedures/variables/snippets are NOT
  included in the response.
- A7: Given the cursor inside `ExecFunction("")` with nothing typed, when completion is requested, then same-file procedures and workspace category names are offered — workspace script names are NOT offered before a dot is typed (the categories-only noise floor).
- A8: Given `ExecFunction("Cat.")` where `Cat` is a workspace category, when completion is requested, then the scripts in that category are offered.
- A9: Given `ExecFunction("Cat.Script.")` resolving to a workspace script, when completion is requested, then that script's procedures are offered, excluding `/*@private;` and `/*@protected;` ones.
- A10: Given `DoProc("Script.")` where `Script` matches a flat-layout script basename, when completion is requested, then that script's non-private procedures are offered.

## Rationale

Completion earns trust by staying out of the way: the v0.7.4/v0.7.6 noise
decisions (history) established that a wrong auto-popup is worse than no
popup, because Enter-acceptance corrupts code mid-flow. The trigger policy
is therefore a contract, not an implementation detail — broadening it again
requires editing this entry first.
