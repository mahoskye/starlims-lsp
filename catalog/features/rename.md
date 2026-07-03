---
id: feature.rename
title: Rename
kind: feature
status: active
authority: tool
schema_ref: null
config: []
tests:
  - internal/providers/rename_test.go
history:
  - date: 2025-12-05
    ref: "vs-code-ssl-formatter#40"
    note: Rename limited to procedure scope for local variables and
      parameters; only :PUBLIC variables and procedure names rename
      file-wide.
  - date: 2026-02-02
    ref: "fb59a52 (v0.2.0)"
    note: textDocument/rename and prepare-rename added — context/symbol
      validation, new-name validation, scope-aware edits via the shared
      reference search.
  - date: 2026-07-02
    ref: "issue #43"
    note: Rename edits no longer touch comments or unrelated strings — the
      shared reference search classifies every match against lexer tokens;
      DoProc/ExecFunction dispatch string targets are still renamed.
issues: ["#43"]
---

## Behavior

- Rename MUST support: `:DECLARE` variables, `:PARAMETERS` parameters,
  `:PUBLIC` variables, procedure names, and other user-defined identifiers,
  within the current file only.
- Prepare-rename MUST reject before a new name is requested when the cursor
  is on a keyword, built-in function, built-in class, literal (`.T.`, `.F.`,
  `NIL`), operator, `Me`, `Base`, or `Constructor`, or when the cursor is
  inside a string literal or comment.
- The new name MUST be validated: a legal SSL identifier (letter or `_`
  first, then alphanumerics/`_`), not a keyword, not a built-in function
  name, and not `Me`/`Base`/`Constructor`; otherwise the rename MUST be
  rejected with no edits.
- Edits MUST be scope-aware: renaming a local variable or parameter changes
  occurrences only within its declaring procedure; a same-named local in
  another procedure MUST NOT be touched. `:PUBLIC` variables and procedure
  names rename file-wide.
- Matching MUST be case-insensitive; every case variant of the old name is
  replaced with the exact casing the user supplied.
- Renaming a procedure MUST also update its `DoProc("Name")` /
  `ExecFunction("Name")` string targets — they are the only legal call
  syntax, so leaving them behind would silently break dispatch.
- Rename MUST NOT modify unrelated string literal or comment content that
  happens to contain the old name. Edits come from the shared reference
  search, which classifies each text match against the lexer tokens: comment
  matches are dropped, and string matches are dropped unless the string is
  the first argument of `DoProc`/`ExecFunction` (case-insensitive) and the
  match spans the entire string content — the dispatch-target case above.
- All edits MUST be returned as a single WorkspaceEdit against the current
  document; no other files are modified.

## Acceptance

- A1: Given `:DECLARE sOutput;` (or a `:PARAMETERS` name) used several times inside one procedure, when the user renames it, then the declaration and every in-procedure occurrence (any casing) are replaced with exactly the supplied new name.
- A2: Given two procedures each declaring a local `sValue`, when the user renames `sValue` inside the first procedure, then no occurrence in the second procedure is modified — matches outside the declaring procedure's scope must not be touched.
- A3: Given the cursor on a keyword, built-in function, or `Me`/`Base`/`Constructor`, when prepare-rename is invoked, then the rename is rejected before a new name can be entered.
- A4: Given the cursor inside a string literal or comment, when prepare-rename is invoked, then the rename is rejected.
- A5: Given a valid rename target, when the user supplies `IF`, `Len`, or `my-var` as the new name, then the rename is rejected and no edits are produced.
- A6: Given `:PROCEDURE CalcTotal;` or a `:PUBLIC` variable referenced across the file, when the user renames it, then all file-wide identifier occurrences are updated.
- A7: Given a procedure rename with a `DoProc("CalcTotal")` call present, when the rename executes, then the name inside the dispatch string is updated too (exactly the name, quotes untouched), keeping the call working.
- A8: Given an unrelated string such as `"sName is a variable"` or a comment mentioning the old name, when a rename of `sName` executes, then that string/comment content is NOT edited.

## Rationale

Scope-limited rename (vs-code-ssl-formatter#40, 2025-12-05) exists because
SSL locals are procedure-scoped: a file-wide textual rename of `sValue`
would corrupt unrelated procedures that happen to reuse the name. Rejecting
at prepare-rename keeps invalid targets (built-ins, keywords, string and
comment interiors) from ever reaching the edit stage, and new-name
validation prevents producing code that cannot parse or that shadows the
330-function built-in inventory. Dispatch strings are updated on procedure
rename for the same reason references counts them (feature.references/A7):
`DoProc("Name")` is the call site, and a rename that breaks every call is
worse than no rename. The flip side — unrelated strings and comments must
stay untouched — was the same context-awareness defect tracked for
references; issue #43 fixed it at the shared reference search (the same fix
resolves feature.references/A9), preserving A7 (dispatch strings ARE
renamed).
