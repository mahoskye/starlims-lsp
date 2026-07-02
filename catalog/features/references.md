---
id: feature.references
title: Find references
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
    ref: "f27f727 (v0.2.0)"
    note: FindReferences fixed to properly respect the includeDeclaration
      request option; coverage added for includeDeclaration=false.
  - date: 2026-02-02
    ref: "0b0acdb (v0.2.0)"
    note: Scope-aware reference finding — local variables and parameters are
      confined to their declaring procedure.
issues: []
---

## Behavior

- References MUST return all occurrences of the symbol at the cursor within
  the current document: procedures (declaration plus call sites, including
  `DoProc`/`ExecFunction` string targets), variables, and parameters
  (declaration plus uses).
- Matching MUST be case-insensitive: `myProc`, `MYPROC`, and `MyProc` are
  the same symbol.
- Matching MUST be whole-word only: searching `count` MUST NOT match
  `countAll` or `recount`.
- The `includeDeclaration` request option MUST be honored: when false, the
  `:DECLARE` / `:PARAMETERS` / `:PROCEDURE` declaration location is excluded
  and only uses are returned. (Currently honored only when the request
  originates on the declaration itself — see Known gaps / A8.)
- References MUST respect scope: a procedure-local variable's references are
  confined to its declaring procedure; a same-named local in another
  procedure MUST NOT appear in the results. `:PUBLIC` variables and
  procedure names are file-global.
- Results are single-file only; locations in other files MUST NOT be
  returned.
- Matches inside comments and non-dispatch strings MUST NOT be returned;
  the only legitimate string-context references are `DoProc`/`ExecFunction`
  first arguments. (Currently violated — the search is text-based after
  symbol identification; see Known gaps / A9.)

## Acceptance

- A1: Given a procedure with `:DECLARE counter;` and several uses, when references are requested with `includeDeclaration: true`, then the declaration and every use are returned; the same holds for a procedure name and its call sites.
- A2: Given the cursor on the declaration, when references are requested with `includeDeclaration: false`, then only the uses are returned and the `:DECLARE` / `:PARAMETERS` location is absent.
- A3: Given `:DECLARE MyVariable;` and uses spelled `myvariable` and `MYVARIABLE`, when references are requested, then all case variants are returned.
- A4: Given `:DECLARE count;` alongside identifiers `countAll` and `recount`, when references are requested on `count`, then `countAll` and `recount` do not appear in the results.
- A5: Given two procedures each declaring a local `localVar` (or parameter `sName`), when references are requested inside the first procedure, then no location from the second procedure is returned.
- A6: Given a `:PUBLIC` variable or a procedure name used across several procedures, when references are requested, then occurrences from the whole file are returned.
- A7: Given `:PROCEDURE TargetProc;` and a call `DoProc("TargetProc")`, when references are requested on the procedure, then the string target inside the DoProc call is included as a reference.
- A8: Given the cursor on a *use* of a symbol, when references are requested with `includeDeclaration: false`, then the declaration location is still excluded from the results. (planned)
- A9: Given a comment or a non-dispatch string containing the symbol name as a whole word, when references are requested, then those comment/string matches are NOT returned. (planned)

## Rationale

Case-insensitive, whole-word, scope-aware matching follows SSL's identifier
semantics — anything looser produces noise, anything stricter misses real
uses. `DoProc`/`ExecFunction` string arguments are counted as references
because they are the only legal call syntax for user procedures, so omitting
them would hide every real call site. `includeDeclaration` handling was
pinned in v0.2.0 (f27f727) after the option was silently ignored.
Comment/string leakage is acknowledged as a defect rather than contract
(vs-code-ssl-formatter#36 was the extension-side report of the same class of
noise): the entry keeps the intended behavior normative so the fix has a
target.

## Known gaps

- Declaration exclusion is cursor-dependent: the declaration is only
  detected (and excluded under `includeDeclaration: false`) when the request
  position is on the declaration line itself (definition.go
  FindReferencesWithScope, `declarationFound`). Requested from a use site,
  the declaration leaks back into the results. Covered by A8 (planned).
- The search is text-based after symbol identification, so whole-word
  matches inside comments and non-call strings currently appear in results.
  Intended behavior is code-context matches plus `DoProc`/`ExecFunction`
  first-argument strings only. Covered by A9 (planned); the fix must keep
  A7 (dispatch strings ARE references) intact.
