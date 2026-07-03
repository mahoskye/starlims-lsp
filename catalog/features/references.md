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
  - date: 2026-07-02
    ref: "issue #42"
    note: Declaration resolution moved from the cursor line to the parsed
      symbol — includeDeclaration=false now excludes the declaration when
      the request originates on a use site, not just on the declaration.
  - date: 2026-07-02
    ref: "issue #43"
    note: Text matches are classified against lexer tokens — matches inside
      comments and non-dispatch strings are no longer returned; DoProc/
      ExecFunction first-argument dispatch targets remain references.
issues: ["#42", "#43"]
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
  and only uses are returned. This MUST hold regardless of where the request
  originates — the declaration is resolved from the parsed symbol
  (procedures/variables), not from the cursor line, so requesting from a use
  site excludes the declaration too. The cursor-line heuristic remains only
  as a fallback when no parsed symbol info is available.
- References MUST respect scope: a procedure-local variable's references are
  confined to its declaring procedure; a same-named local in another
  procedure MUST NOT appear in the results. `:PUBLIC` variables and
  procedure names are file-global.
- Results are single-file only; locations in other files MUST NOT be
  returned.
- Matches inside comments and non-dispatch strings MUST NOT be returned;
  the only legitimate string-context references are `DoProc`/`ExecFunction`
  first arguments. Each text match is classified against the lexer tokens:
  comment-token matches are dropped; string-token matches are dropped unless
  the string is the first argument of `DoProc`/`ExecFunction`
  (case-insensitive) and the match spans the entire string content — the
  dispatch-target case. Code-context matches (including code blocks) are
  kept.

## Acceptance

- A1: Given a procedure with `:DECLARE counter;` and several uses, when references are requested with `includeDeclaration: true`, then the declaration and every use are returned; the same holds for a procedure name and its call sites.
- A2: Given the cursor on the declaration, when references are requested with `includeDeclaration: false`, then only the uses are returned and the `:DECLARE` / `:PARAMETERS` location is absent.
- A3: Given `:DECLARE MyVariable;` and uses spelled `myvariable` and `MYVARIABLE`, when references are requested, then all case variants are returned.
- A4: Given `:DECLARE count;` alongside identifiers `countAll` and `recount`, when references are requested on `count`, then `countAll` and `recount` do not appear in the results.
- A5: Given two procedures each declaring a local `localVar` (or parameter `sName`), when references are requested inside the first procedure, then no location from the second procedure is returned.
- A6: Given a `:PUBLIC` variable or a procedure name used across several procedures, when references are requested, then occurrences from the whole file are returned.
- A7: Given `:PROCEDURE TargetProc;` and a call `DoProc("TargetProc")`, when references are requested on the procedure, then the string target inside the DoProc call is included as a reference.
- A8: Given the cursor on a *use* of a symbol, when references are requested with `includeDeclaration: false`, then the declaration location is still excluded from the results.
- A9: Given a comment or a non-dispatch string containing the symbol name as a whole word, when references are requested, then those comment/string matches are NOT returned.

## Rationale

Case-insensitive, whole-word, scope-aware matching follows SSL's identifier
semantics — anything looser produces noise, anything stricter misses real
uses. `DoProc`/`ExecFunction` string arguments are counted as references
because they are the only legal call syntax for user procedures, so omitting
them would hide every real call site. `includeDeclaration` handling was
pinned in v0.2.0 (f27f727) after the option was silently ignored, and made
cursor-independent for issue #42 by resolving the declaration from the
parsed symbol. Comment/string leakage was a long-standing defect
(vs-code-ssl-formatter#36 was the extension-side report of the same class of
noise); issue #43 fixed it by classifying every text match against the lexer
tokens, keeping `DoProc`/`ExecFunction` dispatch targets (A7) as the only
string-context references.
