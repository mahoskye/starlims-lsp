---
id: feature.hover
title: Hover
kind: feature
status: active
authority: tool
schema_ref: null
config:
  - ssl.diagnostics.endpointPatterns
tests:
  - internal/providers/identifier_roles_test.go
  - internal/providers/providers_test.go
  - internal/providers/element_reference_test.go
  - internal/server/handler_test.go
history:
  - date: 2026-02-02
    ref: "814b42d / d56cbfe (v0.2.0)"
    note: SQL placeholder hover (named `?var?` and positional `?`) added, and
      hover suppressed inside strings/comments via token context filtering.
  - date: 2026-05-01
    ref: "v0.3.0"
    note: Hover content re-sourced from the published element reference; added
      class member tables, operator type-behavior tables, core-type and
      special-form hovers.
  - date: 2026-04-30
    ref: "af5dd0e (v0.6.0)"
    note: Documented exceptions, caveats, and don't-lists from ssl-docs
      surfaced inline in function and class hovers.
  - date: 2026-05-13
    ref: "bc06d0c (v0.7.6) / vs-code-ssl-formatter#75"
    note: Leading procedure docblocks (Description/Parameters/Returns) parsed
      and woven into user-procedure hover.
  - date: 2026-05-13
    ref: "60c10bd (v0.7.7)"
    note: Request/Response endpoint ambients get documentation hover in files
      recognized as endpoints (ssl.diagnostics.endpointPatterns or docblock
      Endpoint marker).
  - date: 2026-07-03
    ref: "feature.cross_file_resolution"
    note: >-
      Cross-file hover added for dispatch-target strings (second
      string-context exception after SQL placeholders) and :INCLUDE
      paths. Same-file hover output unchanged.
  - date: 2026-07-03
    ref: "RunDS navigation PR"
    note: >-
      RunDS target strings join the string-context exceptions: a
      resolvable target shows the data-source summary (A14);
      unresolvable targets keep the string suppression.
  - date: 2026-07-03
    ref: "UDObject member navigation PR"
    note: >-
      Member hover for shape-inferred UDObject receivers (A15-A16),
      closing the long-standing "property access after ':' has no
      hover" gap for receivers whose shape completion already infers
      (issues #7/#19). Unshaped receivers keep prior behavior.
  - date: 2026-07-22
    ref: "#78"
    note: >-
      Bare 1-part DoProc/ExecFunction targets join the dispatch-string
      hover exception: a target naming a same-file procedure shows that
      procedure's docblock hover, mirroring go-to-definition's same-file
      semantics for 1-part targets (A17-A18).
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
issues: []
---

## Behavior

- Hover MUST return Markdown content (`contents.kind: "markdown"`) for:
  keywords, built-in functions, built-in classes, core SSL value types,
  special forms (`Me`, `Base`, `Constructor`, ...), literals (`.T.`, `.F.`,
  `NIL`), operators, user-defined procedures, and declared variables.
- Built-in function hover MUST show the canonical signature, parameter list,
  return type, and description; documented exceptions/caveats from the
  element metadata MUST be included when present.
- Built-in class hover MUST enumerate published constructors, properties,
  and methods; binary-operator hover MUST include the type-behavior table.
- User-procedure hover MUST show the procedure name, its `:PARAMETERS` list,
  and the declaration location, weaving in a leading docblock
  (`Description:`/`Parameters:`/`Returns:`) when one precedes the procedure.
- Variable hover MUST show the declaration location and scope.
- Lookup MUST be case-insensitive (`sqlexecute` resolves to `SQLExecute`).
- Hover MUST NOT activate inside comments, and MUST NOT activate for general
  symbols inside string literals. String-context hover has exactly three
  exceptions: SQL placeholders — named `?varName?` (SQLExecute only) shows
  the parameter name and runtime-substitution note, positional `?` shows
  its 1-based position — DoProc/ExecFunction dispatch targets — dotted
  targets that resolve through the workspace resolver
  (feature.cross_file_resolution) show the target procedure's
  signature/docblock with a "defined in" origin, or the target script's
  entry-point summary for 2-part targets, while bare 1-part targets keep
  same-file semantics (mirroring go-to-definition) and show the matching
  local procedure's hover, case-insensitively — and RunDS string targets
  that resolve to a workspace data source, which show the data-source
  summary (identity and entry parameters). An unresolvable dispatch or
  RunDS target falls through to the normal string suppression (null).
- In endpoint files (matched via `ssl.diagnostics.endpointPatterns` or a
  leading `Endpoint:` docblock marker), `Request` and `Response` MUST hover
  with their ambient documentation; in non-endpoint files they get no
  special hover.
- When no information exists for the position, the response MUST be null.
- Hovering the member in `<recv>:<member>` where `<recv>` has a
  CreateUDObject-inferred shape (the same inference completion uses —
  initializer literals, `:prop :=` augmentation, `:clone()`,
  cross-procedure propagation) shows the property's name, inferred value
  type, receiver, and definition line. A shaped receiver whose shape
  lacks the member hovers as null — the shape is the best available
  knowledge, and hover must not fall back to unrelated same-named
  symbols. Receivers with no inferred shape keep the prior word-based
  behavior unchanged.

- Hovering an `:INCLUDE` statement (keyword or path) shows the resolved
  script's summary (identity, entry parameters, procedure count) when the
  workspace resolver finds it; unresolvable includes hover as before
  (word-based fallthrough or null). Ambiguous resolutions show the first
  candidate plus a match count — choosing between candidates is
  go-to-definition's job.

## Acceptance

- A1: Given `result := SQLExecute(query, "ds");`, when the user hovers over `SQLExecute`, then the response is Markdown containing the signature label, the parameter list, and the return type.
- A2: Given a built-in with documented runtime exceptions (e.g. `ExecFunction`), when the user hovers over it, then the documented-exceptions section with the canonical exception message is included.
- A3: Given a document containing `:PROCEDURE CalculateTotal;` with `:PARAMETERS nPrice, nQuantity;`, when the user hovers over the procedure name, then the hover shows the procedure name, its parameters (with docblock descriptions when present), and the declaration location.
- A4: Given `:DECLARE nCounter;` followed by a use of `nCounter`, when the user hovers over the use, then the hover reports the declaration line and scope.
- A5: Given `sqlexecute` typed in lowercase, when the user hovers over it, then the hover for `SQLExecute` is returned (case-insensitive lookup).
- A6: Given `sSQL := "... WHERE name = ?sCustomer?";`, when the user hovers over `sCustomer` inside the placeholder, then the hover identifies it as a named SQL parameter substituted at runtime; hovering a bare `?` in a positional-parameter SQL string identifies its 1-based position.
- A7: Given `x := "SQLExecute is a function";` or a comment `/* SQLExecute would be here;`, when the user hovers over `SQLExecute` inside the string or comment, then no hover is returned (null) — general symbol hover must not fire in either context.
- A8: Given a plain undeclared identifier with no known information, when the user hovers over it, then the response is null rather than an empty or fabricated hover.
- A9: Given a file recognized as an endpoint script, when the user hovers over `Request` or `Response`, then their endpoint-ambient documentation is shown; a file with no endpoint signal is not classified as an endpoint, so the ambients get no special hover there.
- A10: Given `ExecFunction("Cat.Script.Proc")` where the workspace resolves the procedure, when the user hovers inside the target string, then the hover shows the procedure's signature/docblock and names the defining script.
- A11: Given `ExecFunction("Cat.Script")` resolving to a script, when the user hovers inside the string, then the hover shows the script's entry-point summary (entry :PARAMETERS).
- A12: Given `:INCLUDE SharedLib;` resolving to a workspace file, when the user hovers the include path, then the hover shows the resolved script's summary.
- A13: Given a dispatch-target string that resolves nowhere, when the user hovers inside it, then the response is null — the string suppression (A7) holds.
- A14: Given `RunDS("QUERIES.ORDERS")` resolving to a workspace data source, when the user hovers inside the string, then the hover shows the data-source summary (identity and entry parameters); a RunDS target resolving nowhere hovers as null.
- A15: Given `oObj := CreateUDObject({{"Name", "x"}});` followed by `oObj:Total := 5;`, when the user hovers `Name` or `Total` after `oObj:`, then the hover shows the property with its inferred type and definition line.
- A16: Given the same shaped `oObj` and a hover on the member in `oObj:Unknown`, when the member is not in the inferred shape, then the response is null — even if an unrelated variable named `Unknown` exists in the file.
- A17: Given `oResult := DoProc("BuildShell", {oContext});` and a same-file `:PROCEDURE BuildShell;` preceded by a docblock, when the user hovers inside the target string, then the hover shows the procedure's docblock hover (description, parameters, returns, declaration location), matching the name case-insensitively.
- A18: Given `DoProc("NoSuchProc", {})` naming no procedure in the current file, when the user hovers inside the string, then the response is null — 1-part targets never resolve cross-file, and the string suppression (A7) holds.
- A19: Given the cursor on the member name of `oRec:sName` with a local variable `sName` declared in the same procedure, when hover is requested, then the local variable's hover is NOT shown.

## Rationale

Hover is the primary discovery surface for SSL's 330 built-in functions and
29 classes, so its content tracks the published element reference verbatim
(v0.3.0) rather than hand-curated text, and documented runtime exceptions
ride along (v0.6.0) so users see failure modes before running code. String
and comment suppression (v0.2.0, token context filtering in the hover
handler) exists because SQL strings legitimately contain function-like
words (vs-code-ssl-formatter#27 class of false hovers); the SQL-placeholder
carve-out is deliberate since `?var?` substitution is real runtime behavior
worth explaining in place. Endpoint-ambient hover (v0.7.7) mirrors the
diagnostics decision: `Request`/`Response` are real only in endpoint
scripts, so hover follows the same file classification.

## Known gaps

- Member hover covers shape-inferred UDObject receivers only. `:` access
  on built-in value types (`sName:Length` — .NET passthrough) and on
  receivers shape inference cannot see (built in another file, passed
  through untracked bindings) still follows the legacy word-based path
  or returns null. Per DECISIONS.md D10 (issue #22), absent knowledge is
  silence: any future receiver typing must exempt the six .NET
  passthrough value types from unknown-member treatment.
- Cross-file hover covers dispatch strings and `:INCLUDE` paths; a bare
  identifier that happens to be defined in another workspace file still
  gets no hover (word-based hover stays same-file — deliberate, to avoid
  guessing on plain words).
