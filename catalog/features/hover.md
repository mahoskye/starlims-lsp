---
id: feature.hover
title: Hover
kind: feature
status: draft
authority: tool
schema_ref: null
config:
  - ssl.diagnostics.endpointPatterns
tests:
  - internal/providers/providers_test.go
  - internal/providers/element_reference_test.go
history:
  - date: 2026-05-01
    ref: "v0.3.0"
    note: Hover content re-sourced from the published element reference; added
      class member tables, operator type-behavior tables, core-type and
      special-form hovers.
  - date: 2026-04-30
    ref: "v0.6.0"
    note: Documented exceptions, caveats, and don't-lists from ssl-docs
      surfaced inline in function and class hovers.
  - date: 2026-05-13
    ref: "v0.7.6 / vs-code-ssl-formatter#75"
    note: Leading procedure docblocks (Description/Parameters/Returns) parsed
      and woven into user-procedure hover.
  - date: 2026-05-13
    ref: "v0.7.7"
    note: Request/Response endpoint ambients get documentation hover in files
      recognized as endpoints (ssl.diagnostics.endpointPatterns or docblock
      Endpoint marker).
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
  symbols inside string literals. The only string-context hover is SQL
  placeholders: named `?varName?` (SQLExecute only) shows the parameter name
  and runtime-substitution note; positional `?` shows its 1-based position.
- In endpoint files (matched via `ssl.diagnostics.endpointPatterns` or a
  leading `Endpoint:` docblock marker), `Request` and `Response` MUST hover
  with their ambient documentation; in non-endpoint files they get no
  special hover.
- When no information exists for the position, the response MUST be null.

## Acceptance

- A1: Given `result := SQLExecute(query, "ds");`, when the user hovers over `SQLExecute`, then the response is Markdown containing the signature, parameter documentation, and return type.
- A2: Given a document containing `:PROCEDURE CalculateTotal;` with `:PARAMETERS nPrice, nQuantity;`, when the user hovers over the procedure name, then the hover shows the procedure signature with its parameters and declaration location.
- A3: Given `:DECLARE nCounter;` followed by a use of `nCounter`, when the user hovers over the use, then the hover reports the declaration line and scope.
- A4: Given `sqlexecute` typed in lowercase, when the user hovers over it, then the hover for `SQLExecute` is returned (case-insensitive lookup).
- A5: Given `sSQL := "... WHERE name = ?sCustomer?";`, when the user hovers over `sCustomer` inside the placeholder, then the hover identifies it as a named SQL parameter substituted at runtime; hovering a bare `?` in a positional-parameter SQL string identifies its position.
- A6: Given `x := "SQLExecute is a function";`, when the user hovers over `SQLExecute` inside the string, then no hover is returned (null) — general symbol hover must not fire inside strings.
- A7: Given a comment `/* SQLExecute would be here;`, when the user hovers over `SQLExecute` inside the comment, then no hover is returned.
- A8: Given a plain undeclared identifier with no known information, when the user hovers over it, then the response is null rather than an empty or fabricated hover.

## Rationale

Hover is the primary discovery surface for SSL's 330 built-in functions and
29 classes, so its content tracks the published element reference verbatim
(v0.3.0) rather than hand-curated text, and documented runtime exceptions
ride along (v0.6.0) so users see failure modes before running code. String
and comment suppression exists because SQL strings legitimately contain
function-like words (vs-code-ssl-formatter#27 class of false hovers); the
SQL-placeholder carve-out is deliberate since `?var?` substitution is real
runtime behavior worth explaining in place. Endpoint-ambient hover (v0.7.7)
mirrors the diagnostics decision: `Request`/`Response` are real only in
endpoint scripts, so hover follows the same file classification.
