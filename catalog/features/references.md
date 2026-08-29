---
id: feature.references
title: Find references
kind: feature
status: active
authority: tool
schema_ref: null
config: []
tests:
  - internal/providers/identifier_roles_test.go
  - internal/providers/providers_test.go
  - internal/providers/crossfile_test.go
  - internal/server/handler_test.go
  - internal/server/cross_file_test.go
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
  - date: 2026-07-24
    ref: "issue #125"
    note: Cross-file references for procedure subjects — the single-file-only
      rule is lifted. Dotted dispatch call sites across the workspace are
      returned via a token-based call-site index (candidate discovery) with
      per-site re-resolution through the dispatch resolver; open documents
      scan from the live parse. Dotted self-sites in the definition file,
      previously invisible to the whole-content match, are included.
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
issues: ["#42", "#43", "#125"]
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
- Procedure subjects extend cross-file: when the cursor names a procedure
  (its declaration, an identifier use, or a dotted dispatch-target string
  that resolves to it), dotted `DoProc`/`ExecFunction` call sites across
  the workspace whose target resolves to that procedure MUST be returned,
  at the string-content range. Site→definition matching uses the dispatch
  resolver (`feature.cross_file_resolution` — same degradation chain,
  uniqueness gate, open-document overlay), so references agree exactly
  with go-to-definition. Ambiguously-resolving sites that include the
  target among their candidates are returned (a reference listing is
  read-only and liberal; rename is the conservative side).
- A 1-part dispatch target (`DoProc("Foo")`) in ANOTHER file is NOT a
  reference — 1-part targets are same-file calls by the resolver's scoping
  rule (`feature.cross_file_resolution` A14). This is a deliberate scoping
  decision, not a semantic fact; see Known gaps for the include-splice
  consequence.
- Dotted self-sites — a dispatch string inside the definition file whose
  target resolves to a procedure of that same file — MUST be returned
  exactly once (the same-file whole-content match cannot see them).
- Non-procedure subjects (variables, parameters) remain single-file.
  Without a workspace index the whole feature behaves exactly as the
  single-file contract above.
- Cross-file locations are capped at 500 per request.
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
- A10: Given `:PROCEDURE CalculateTotal;` in one file and `ExecFunction("CAT.SCRIPT.CalculateTotal")` in another, when references are requested from the declaration (or from the call-site string), then the call site is returned at its string-content range (and the declaration is returned per `includeDeclaration`).
- A11: Given a 1-part `DoProc("CalculateTotal")` in a different file, when references are requested on the procedure, then that site is NOT returned (same-file scoping rule for 1-part targets).
- A12: Given a caller file open with unsaved edits that removed its dispatch site, when references are requested, then the stale indexed site is NOT returned — open documents are scanned from the live buffer.
- A13: Given no workspace index, when references are requested, then results are byte-identical to the single-file behavior.
- A14: Given a dotted dispatch self-site inside the definition file, when references are requested on the procedure, then that site appears exactly once in the results.
- A15: Given a variable `sName` and a member access `oRec:sName`, when references are requested on the variable, then the member-access occurrence is NOT returned.
- A16: Given a variable and a procedure sharing a name, when references are requested from one of them, then only that symbol's own occurrences are returned.

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

Cross-file references (issue #125) reuse the dispatch resolver per site
rather than trusting the index's positions as answers: the index only
discovers candidate files (last-segment match), and each candidate re-runs
the same resolution chain go-to-definition uses, so the two features can
never disagree about what a dispatch string means. Ambiguous sites are
included because a references listing is read-only — the cost of a false
positive is a click, while rename (the write side) skips them.

## Known gaps

- **Class-method channel.** `obj:Method()` on a `CreateUdObject`-
  instantiated class and `Base:Method()` after `:INHERIT` reference
  procedures in other scripts via bare identifiers. The LSP does not model
  this channel (definition/hover share the blind spot), so references
  under-report for class files.
- **Include-spliced 1-part calls.** `:INCLUDE` splices the included file's
  text, so a 1-part `DoProc("Foo")` in an includer of the definition file
  is a genuine runtime call — excluded by the 1-part scoping rule (A11).
  Closable later via a reverse-include walk.
- **Concatenated dispatch strings.** `DoProc("CAT." + sName)` is never
  extracted as a call site; only the leading string literal is seen and its
  partial content does not resolve.
- **Candidate cap.** Resolution candidate sets cap at 10; in a degenerate
  workspace with more than 10 same-named candidates, a target capped out of
  the set is omitted from references (and safely skipped by rename).
