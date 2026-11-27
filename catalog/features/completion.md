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
  - internal/providers/class_members_test.go
  - internal/providers/udobject_shapes_test.go
  - internal/server/handler_test.go
  - internal/server/class_members_test.go
  - internal/server/cross_file_test.go
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
  - date: 2026-10-13
    ref: "#252"
    note: >-
      Built-in function, keyword, operator, and literal completion
      documentation now comes from the vendored reference (shared with
      hover), and function snippets use reference parameter names as
      placeholders (`SQLExecute(${1:sCommandString})`). The trigger and
      context contract below is unchanged.
  - date: 2026-10-13
    ref: "issue #255"
    note: >-
      ProcedureInfo.IsPrivate now requires an annotation in force — the
      exact lowercase form on the line immediately before :PROCEDURE —
      matching diag.visibility_annotation. A near-miss spelling or an
      annotation detached by a blank line or another comment used to mark
      the procedure private (and hide it from completion) although the
      runtime leaves it public.
  - date: 2026-10-13
    ref: "issue #258"
    note: >-
      Variable completions scoped: the cursor's procedure plus
      script-level and :PUBLIC declarations, one item per name
      case-insensitively (A12). Every declaration from every procedure
      was listed before, duplicates included.
  - date: 2026-11-27
    ref: "issue #379"
    note: >-
      The token before the cursor, and the text typed so far in a
      dispatch string, are found by character count (A13, A14). Both used a
      token's UTF-8 byte length against a character column. A string
      earlier on the line whose extra bytes outnumbered the characters
      between it and the cursor (Cyrillic, Greek or CJK text does it in
      a few words) hid the `:` before the cursor, and member completion
      answered nothing. A dispatch-string prefix was cut short after a
      non-ASCII character.
  - date: 2026-11-27
    ref: "issue #380"
    note: >-
      `Me:` and `Base:` list the members of the class the document
      declares (A4, A15-A19). The class name was looked up among the
      built-in classes only, so a class written in the workspace listed
      nothing, and one named like a built-in (`:CLASS Email`) listed the
      built-in's members. A4's test used that name and passed either
      way. `Me:` now lists the class's own fields and methods, then what
      it inherits and what it includes; `Base:` lists what it inherits.
      Inherited and included members are offered only where the parent
      or the include resolves to exactly one workspace file, and only as
      far as the behavior has been verified: the Behavior text records
      what is left out and why.
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
    receiver's members. For `Me` and `Base` these are the class members
    below, and the same list answers explicit invocation there.
  - Otherwise, when the `:` is preceded by whitespace or start-of-line:
    keyword completions only (no procedures, variables, or snippets).
  - Accepting a keyword completion replaces the already-typed `:` — the
    result is `:DECLARE`, never `::DECLARE`.
- Explicit invocation returns the full set: keywords, built-in functions and
  classes, literals, operators, document-local procedures and variables, and
  snippets. Variables are those visible at the cursor — the enclosing
  procedure's locals and parameters, script-level declarations, and
  `:PUBLIC` names — one item per name case-insensitively, the nearest
  scope winning; another procedure's locals are never offered. Inside `:CLASS` methods it adds class-context forms (`Me`,
  `Base`, `Constructor`) and inserts sibling methods as `Me:Method(...)`.
- Class-only forms are never suggested outside a `:CLASS` method.
- Inside string literals and comments, no completions are returned.
- Variables initialized via `CreateUDObject({{...}})` get an inferred
  property shape (inherited through `:clone()`); member completion after
  `<var>:` lists exactly the inferred properties.
- Class members. The class is the one the document declares with `:CLASS`,
  whatever its name: a class named like a built-in class (`:CLASS Email`)
  is still the document's class, and the built-in's members are not its
  members. `Me:` lists, one item per name case-insensitively, the first
  of these that declares the name:
  1. the class's own fields (the names declared by `:DECLARE` outside
     every procedure) and methods (its `:PROCEDURE`s);
  2. the procedures of each script the class file names in an `:INCLUDE`,
     which a method reaches as `Me:Name(...)` (verified on STARLIMS 12);
  3. the fields and methods of each ancestor, following `:INHERIT` up the
     chain, the nearer class first. `Me:` resolves against the current
     class and then its ancestors (reference, `Me`).

  `Base:` lists item 3 only, starting at the parent. A field inserts its
  name; a method inserts its name and its parameters as snippet
  placeholders (`Bump(${1:nBy})`), the form explicit invocation already
  inserts after `Me:`. `Constructor` is offered after `Base:` when an
  ancestor declares one (`Base:Constructor(args)` is the documented way
  to pass arguments to a parent constructor) and never after `Me:`.
- Items 2 and 3 are read from the workspace: the open buffer when the file
  is open, the index otherwise. Each step is taken only when the name
  resolves to exactly one file. A parent or an include that resolves to
  nothing, or to more than one file, contributes nothing, and an
  unresolved parent ends the walk up the chain: an accepted member that
  the class does not have is a compile-time error, so a guess is worse
  than a shorter list (D10). Classes that inherit each other in a circle
  end the walk. With no workspace index, `Me:` lists the class's own
  members and `Base:` has nothing to list.
- Left out because the behavior has not been run (D17), each a member that
  may exist and is not offered: a procedure of an included script whose
  `/*@private;` or `/*@protected;` annotation is in force; anything an
  included script includes in turn; what an ancestor includes; and the
  included script's own script-level variables. Where an included
  procedure and an inherited member share a name, which one `Me:` reaches
  has not been run either; the name is listed once, as the included
  procedure. The members every object has (`AddProperty`, `IsProperty`,
  ...) are not listed after `Me:` or `Base:`, as they are not for a
  shape-inferred object.
- When no member is known for `Me:` or `Base:` (an empty class, a `Base:`
  whose parent does not resolve), the response is the one an unknown
  receiver gets: nothing on the `:` trigger, the full set on explicit
  invocation.

- Inside the first string argument of `DoProc(...)`/`ExecFunction(...)`
  completion IS offered (the one string-context exception, added for
  vs-code-ssl-formatter#74 and previously undocumented here — a spec gap
  found during the cross-file design). The list is segment-aware
  (feature.cross_file_resolution): before any dot, same-file procedures
  plus workspace category names only; after `Category.`, that category's
  scripts; after `Category.Script.` or flat `Script.`, that script's
  procedures with `/*@private;`/`/*@protected;` ones excluded — an
  annotation counts only when it is in force (exact form, on the line
  immediately before `:PROCEDURE`; diag.visibility_annotation reports the
  inert ones, and an inert annotation never hides a procedure). All items
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
- A4: Given `Me:` inside a method of `:CLASS Counter;`, a class with the field `:DECLARE nCount;` and the methods `Bump` (`:PARAMETERS nBy;`) and `Current`, when completion triggers on the `:` or is invoked explicitly, then exactly `nCount`, `Bump` and `Current` are listed — the field as a field inserting its name, the methods as methods inserting `Bump(${1:nBy})` and `Current()` — and no keyword, built-in function, variable or snippet is; given `Me` typed outside a class file, class-context forms are NOT suggested (issue #380).
- A5: Given the cursor is inside a comment or a non-dispatch string literal, when completion is requested, then no items are returned.
- A6: Given `x:` immediately after an identifier with no known shape, when
  completion auto-triggers, then procedures/variables/snippets are NOT
  included in the response.
- A7: Given the cursor inside `ExecFunction("")` with nothing typed, when completion is requested, then same-file procedures and workspace category names are offered — workspace script names are NOT offered before a dot is typed (the categories-only noise floor).
- A8: Given `ExecFunction("Cat.")` where `Cat` is a workspace category, when completion is requested, then the scripts in that category are offered.
- A9: Given `ExecFunction("Cat.Script.")` resolving to a workspace script, when completion is requested, then that script's procedures are offered, excluding `/*@private;` and `/*@protected;` ones.
- A10: Given `DoProc("Script.")` where `Script` matches a flat-layout script basename, when completion is requested, then that script's non-private procedures are offered.
- A11: Given `aItems:` where aItems holds an SSL array, string, number, logical or date (no class, returns-object or CreateUdObject shape), when completion triggers, then no items are offered — in particular no SSL-object members (`GetProperty`, `IsProperty`, `AddProperty`, ...), which those values do not have, and no guessed .NET members, whose names are case-sensitive and unlisted (issue #267).
- A12: Given two procedures each declaring locals (one name repeated with different casing) plus script-level and `:PUBLIC` declarations, when completion is requested inside the second procedure, then its locals, the script-level variables, and the publics are offered, the first procedure's own locals are not, and each name appears once.
- A13: Given `sText := "Температура: " + oMail:` where `oMail := Email{};`, when completion triggers after the `:`, then the Email members are listed exactly as they are with an ASCII string of the same length in its place — a string's extent is counted in characters, never UTF-8 bytes, however much non-ASCII text it holds (issue #379).
- A14: Given `ExecFunction("Größe.")` where `Größe` is a workspace category, when completion is requested before the closing quote, then that category's scripts are offered — the text typed so far is cut by characters, so a non-ASCII character in it neither shortens nor splits the prefix (issue #379).
- A15: Given the class of A4 declared as `:CLASS Email;`, a built-in class's name, when completion triggers after `Me:`, then the same three members are listed and none of the built-in `Email` class's (`Subject`, `Send`) is (issue #380).
- A16: Given `:CLASS Child;` with `:INHERIT Lab.Parent;` and its own method `Bump`, where `Lab.Parent` is one workspace class file declaring the field `sName` and the methods `Describe`, `Bump` and `Constructor`, and itself inherits `Lab.Root` declaring `Audit`, when completion triggers after `Me:`, then Child's members are listed with `sName`, `Describe` and `Audit` marked as inherited from the class that declares them, `Bump` appears once as Child's own, and `Constructor` is not listed; after `Base:` exactly the inherited members are listed — `sName`, `Describe`, Parent's `Bump`, `Constructor` and `Audit` — and none of Child's own.
- A17: Given a class whose `:INHERIT` parent resolves to no workspace file, or to two, when completion triggers after `Me:`, then the class's own members are listed and nothing from either candidate is; after `Base:` no member is offered — the `:` trigger answers nothing.
- A18: Given a class file with `:INCLUDE Lab.Helpers;` resolving to one workspace script that declares `Normalize` and, under a `/*@private;` annotation in force, `Internal`, when completion triggers after `Me:`, then `Normalize` is listed as a method, marked as coming from the included script, and `Internal` is not; `Base:` lists neither.
- A19: Given the parent class open in the editor with an unsaved method the file on disk lacks, when completion triggers after `Me:` or `Base:` in the child, then the unsaved method is listed — an open file is read from its buffer.

## Rationale

Completion earns trust by staying out of the way: the v0.7.4/v0.7.6 noise
decisions (history) established that a wrong auto-popup is worse than no
popup, because Enter-acceptance corrupts code mid-flow. The trigger policy
is therefore a contract, not an implementation detail — broadening it again
requires editing this entry first.
