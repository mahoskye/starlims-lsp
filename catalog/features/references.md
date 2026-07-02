---
id: feature.references
title: Find references
kind: feature
status: draft
authority: tool
schema_ref: null
config: []
tests:
  - internal/providers/providers_test.go
history:
  - date: 2026-03-03
    ref: "v0.2.0"
    note: FindReferences fixed to properly respect the includeDeclaration
      request option; coverage added for includeDeclaration=false.
issues: ["#36"]
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
  and only uses are returned.
- References MUST respect scope: a procedure-local variable's references are
  confined to its declaring procedure; a same-named local in another
  procedure MUST NOT appear in the results. `:PUBLIC` variables and
  procedure names are file-global.
- Results are single-file only; locations in other files MUST NOT be
  returned.
- Known gap (issue #36): the search is text-based after symbol
  identification, so whole-word matches inside comments and non-call strings
  currently appear in results. Intended behavior is code-context matches
  plus `DoProc`/`ExecFunction` first-argument strings only.

## Acceptance

- A1: Given a procedure `:DECLARE counter;` with several assignments and uses, when references are requested on `counter` with `includeDeclaration: true`, then the declaration and every use are returned.
- A2: Given the same document, when references are requested with `includeDeclaration: false`, then only the uses are returned and the `:DECLARE` location is absent.
- A3: Given `:DECLARE MyVariable;` and uses spelled `myvariable` and `MYVARIABLE`, when references are requested, then all case variants are returned.
- A4: Given `:DECLARE count;` alongside identifiers `countAll` and `recount`, when references are requested on `count`, then `countAll` and `recount` do not appear in the results.
- A5: Given two procedures each declaring a local `localVar`, when references are requested on `localVar` inside the first procedure, then no location from the second procedure is returned.
- A6: Given `:PROCEDURE TargetProc;` and a call `DoProc("TargetProc")`, when references are requested on the procedure, then the string target inside the DoProc call is included as a reference.
- A7: Given a multi-file workspace, when references are requested, then every returned location is in the current document — no cross-file locations.

## Rationale

Case-insensitive, whole-word, scope-aware matching follows SSL's identifier
semantics — anything looser produces noise, anything stricter misses real
uses. `DoProc`/`ExecFunction` string arguments are counted as references
because they are the only legal call syntax for user procedures, so omitting
them would hide every real call site. `includeDeclaration` handling was
pinned in v0.2.0 after the option was silently ignored. Comment/string
leakage is acknowledged as a defect (issue #36) rather than contract: the
entry keeps the intended behavior normative so the fix has a target.
