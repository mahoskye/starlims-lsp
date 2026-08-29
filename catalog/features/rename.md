---
id: feature.rename
title: Rename
kind: feature
status: active
authority: tool
schema_ref: null
config: []
tests:
  - internal/providers/identifier_roles_test.go
  - internal/providers/rename_test.go
  - internal/server/cross_file_test.go
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
  - date: 2026-07-24
    ref: "issue #125"
    note: Cross-file rename for procedure subjects — the single-document
      rule is lifted. Dotted dispatch sites across the workspace are edited
      (last segment only), computed from each file's current content at
      request time. Conservative by design — ambiguous sites are skipped,
      and class-file procedures refuse the cross-file path because
      obj:Method()/Base:Method() call sites are invisible. Prepare-rename
      gains the dispatch-target last-segment carve-out (A4 narrowed to
      non-dispatch strings).
  - date: 2026-08-28
    ref: "issue #184 (expression AST consumers)"
    note: >-
      Identifier occurrences are now classified by role from the
      expression tree (parser.IdentifierRoles): a variable reference, a
      member name, a call callee, a class name, a declared name, or a
      procedure header. Word matching could not separate a variable
      `sName` from the property in `oRec:sName` or from a like-named
      procedure, so this behavior acted on occurrences of a different
      symbol. Positions the tree cannot resolve stay unclassified and
      keep the prior word-match behavior.
issues: ["#43", "#125"]
---

## Behavior

- Rename MUST support: `:DECLARE` variables, `:PARAMETERS` parameters,
  `:PUBLIC` variables, procedure names, and other user-defined identifiers.
  Variables and parameters rename within the current file only; procedure
  subjects extend cross-file through dispatch call sites (below).
- Prepare-rename MUST reject before a new name is requested when the cursor
  is on a keyword, built-in function, built-in class, literal (`.T.`, `.F.`,
  `NIL`), operator, `Me`, `Base`, or `Constructor`, or when the cursor is
  inside a comment or a NON-DISPATCH string literal. The one string
  carve-out: the LAST segment of a dotted `DoProc`/`ExecFunction` target
  string is renameable — prepare returns the segment range and the segment
  as placeholder. Cursor on earlier segments (category/script) still
  rejects: script rename is a file rename, out of scope. 1-part dispatch
  strings also still reject — a same-file rename starts from the
  identifier, not the string.
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
- Procedure renames extend cross-file: dotted `DoProc`/`ExecFunction`
  sites across the workspace whose target resolves UNAMBIGUOUSLY (a single
  resolution candidate, equal to the renamed definition) are edited — the
  last segment only; quotes and category/script segments untouched.
  Ambiguously-resolving sites MUST be skipped silently — editing a site
  that might target a different same-named procedure would corrupt that
  call. Dotted self-sites inside the definition file are edited too.
- Every cross-file edit MUST be computed from the file's current content
  at request time — open buffers from the live parse, closed files re-read
  from disk — never from indexed positions; a site that vanished on disk
  produces no edit.
- Procedures defined in `:CLASS` files MUST refuse the cross-file path:
  their methods are callable from other scripts via `obj:Method()` /
  `Base:Method()` bare identifiers that the LSP cannot see, so a
  workspace-wide rename would silently break those callers. Same-file
  rename inside the class file remains available (its blind spot predates
  this feature; see Known gaps).
- Without a workspace index, rename behaves exactly as the single-file
  contract: a single WorkspaceEdit against the current document.

## Acceptance

- A1: Given `:DECLARE sOutput;` (or a `:PARAMETERS` name) used several times inside one procedure, when the user renames it, then the declaration and every in-procedure occurrence (any casing) are replaced with exactly the supplied new name.
- A2: Given two procedures each declaring a local `sValue`, when the user renames `sValue` inside the first procedure, then no occurrence in the second procedure is modified — matches outside the declaring procedure's scope must not be touched.
- A3: Given the cursor on a keyword, built-in function, or `Me`/`Base`/`Constructor`, when prepare-rename is invoked, then the rename is rejected before a new name can be entered.
- A4: Given the cursor inside a comment or a non-dispatch string literal (including a 1-part dispatch string), when prepare-rename is invoked, then the rename is rejected.
- A5: Given a valid rename target, when the user supplies `IF`, `Len`, or `my-var` as the new name, then the rename is rejected and no edits are produced.
- A6: Given `:PROCEDURE CalcTotal;` or a `:PUBLIC` variable referenced across the file, when the user renames it, then all file-wide identifier occurrences are updated.
- A7: Given a procedure rename with a `DoProc("CalcTotal")` call present, when the rename executes, then the name inside the dispatch string is updated too (exactly the name, quotes untouched), keeping the call working.
- A8: Given an unrelated string such as `"sName is a variable"` or a comment mentioning the old name, when a rename of `sName` executes, then that string/comment content is NOT edited.
- A9: Given the cursor on the last segment of `ExecFunction("CAT.SCRIPT.CalcTotal")`, when prepare-rename is invoked, then the segment range and `CalcTotal` as placeholder are returned.
- A10: Given a procedure renamed from its declaration with a dotted dispatch site in another (closed) file, when the rename executes, then the WorkspaceEdit spans both files — the caller edit replaces the last segment only, with quotes and the `CAT.SCRIPT.` prefix intact and exactly the supplied casing.
- A11: Given a dispatch site whose target resolves to multiple candidate files, when the rename executes, then that site is skipped and receives no edit.
- A12: Given a procedure defined in a `:CLASS` file, when a rename is requested from its declaration, then edits stay within the class file; when requested from a dispatch string targeting it in another file, then the rename is refused with no edits.
- A13: Given no workspace index, when a procedure rename executes, then the WorkspaceEdit targets the current document only, identical to the single-file behavior.
- A14: Given a dotted dispatch self-site inside the definition file, when the rename executes, then its last segment is edited alongside the declaration and identifier uses.
- A15: Given the cursor on a category or script segment of a dotted dispatch string, when prepare-rename is invoked, then the rename is rejected.
- A16: Given a caller file rewritten on disk after indexing (site moved, no re-index), when the rename executes, then the edit is computed at the site's fresh position from current disk content.
- A17: Given a variable `sName` and a member access `oRec:sName` in the same file, when the variable is renamed, then the member access is NOT edited — a property belongs to the receiver's type, not to the file's variable of that spelling.
- A18: Given a variable `sName` and a `:PROCEDURE sName;` in the same file, when the variable is renamed then the procedure header and its call sites are NOT edited, and when the procedure is renamed then the variable's declaration and uses are NOT edited.

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

Cross-file rename (issue #125) inverts references' liberality: a missed
reference costs a click, a wrong edit corrupts a call site, so only
unambiguous resolutions are written (D1) and class-file procedures refuse
the cross-file path outright (D8) — an honest refusal beats a rename that
silently breaks every `obj:Method()` caller it cannot see. Edits are always
recomputed from current content because the index may be stale against
unsaved or external changes; emitting edits from indexed positions could
splice the new name into the wrong bytes.

## Known gaps

- **Class-method channel.** `obj:Method()` / `Base:Method()` calls into
  class scripts via bare identifiers are invisible; the D8 gate refuses
  cross-file rename for class-file procedures rather than break those
  callers. Relaxable if class-method resolution is ever modeled.
- **Include-spliced 1-part calls.** A 1-part `DoProc("Foo")` in an includer
  of the definition file is a genuine runtime call but is out of rename's
  sight (1-part scoping rule) — renaming the procedure breaks it silently.
- **Concatenated dispatch strings.** `DoProc("CAT." + sName)` is never
  edited.
- **Position encoding.** The server does not negotiate `positionEncoding`
  and computes byte columns; non-ASCII characters before a site on the same
  line can skew edit columns under UTF-16 clients. Pre-existing; multi-file
  edits widen the blast radius.
