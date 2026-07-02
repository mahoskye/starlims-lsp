---
id: feature.rename
title: Rename
kind: feature
status: draft
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
issues: []
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
  first, then alphanumerics/`_`), not a keyword, and not a built-in function
  name; otherwise the rename MUST be rejected with no edits.
- Edits MUST be scope-aware: renaming a local variable or parameter changes
  occurrences only within its declaring procedure; a same-named local in
  another procedure MUST NOT be touched. `:PUBLIC` variables and procedure
  names rename file-wide.
- Matching MUST be case-insensitive; every case variant of the old name is
  replaced with the exact casing the user supplied.
- Renaming a procedure MUST NOT rewrite `DoProc("Name")` /
  `ExecFunction("Name")` string arguments (current contract; callers must
  update dispatch strings themselves).
- All edits MUST be returned as a single WorkspaceEdit against the current
  document; no other files are modified.

## Acceptance

- A1: Given `:DECLARE sOutput;` used several times inside one procedure, when the user renames it to `sResult`, then the declaration and every in-procedure occurrence (any casing) are replaced with exactly `sResult`.
- A2: Given two procedures each declaring a local `sValue`, when the user renames `sValue` inside the first procedure, then no occurrence in the second procedure is modified — matches outside the declaring procedure's scope must not be touched.
- A3: Given the cursor on a keyword, built-in function, or `Me`/`Base`/`Constructor`, when prepare-rename is invoked, then the rename is rejected before a new name can be entered.
- A4: Given the cursor inside a string literal or comment, when prepare-rename is invoked, then the rename is rejected.
- A5: Given a valid rename target, when the user supplies `IF`, `Len`, or `my-var` as the new name, then the rename is rejected and no edits are produced.
- A6: Given `:PROCEDURE CalcTotal;` referenced elsewhere in the file, when the user renames it to `CalculateTotal`, then all file-wide identifier occurrences are updated.
- A7: Given a procedure rename as in A6 with a `DoProc("CalcTotal")` call present, when the rename executes, then the string argument is left unchanged (documented limitation, not silently rewritten with wrong quoting).

## Rationale

Scope-limited rename (vs-code-ssl-formatter#40, 2025-12-05) exists because
SSL locals are procedure-scoped: a file-wide textual rename of `sValue`
would corrupt unrelated procedures that happen to reuse the name. Rejecting
at prepare-rename keeps invalid targets (built-ins, keywords, string and
comment interiors) from ever reaching the edit stage, and new-name
validation prevents producing code that cannot parse or that shadows the
330-function built-in inventory. DoProc/ExecFunction strings are excluded
from edits because string rewriting without semantic call-site modeling
risks corrupting SQL and message text; the limitation is stated instead.
