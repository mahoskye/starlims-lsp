---
id: diag.procedure_declaration_syntax
title: Malformed procedure declaration
kind: diagnostic
status: active
authority: tool
schema_ref: null
default_severity: error
severity_overridable: true
suppressible: true
tests:
  - internal/providers/providers_test.go
history:
  - date: 2026-05-01
    ref: "commit d134334 (v0.7.0)"
    note: >-
      Introduced (checkProcedureDeclarationSyntax) so C-style declaration
      typos get a syntax error instead of the misleading
      direct_procedure_call message; runs ahead of that check by design.
issues: []
---

## Behavior

Flags two malformed procedure-declaration shapes, common typos from C-style
languages, both at `error` severity:

- **Missing colon**: a bare identifier `PROCEDURE` (any casing, no leading
  `:`) followed by a name and then `(`. The range covers the `PROCEDURE`
  token. Requiring the `(` is the false-positive guard: without it the
  identifier might be ordinary code, and the shape is left to other checks.
- **Parenthesized parameters**: `:PROCEDURE Name(` — the keyword form
  followed by a name and then `(`. SSL procedures take no parameter list;
  arguments are declared with a separate `:PARAMETERS` statement. The range
  covers the `(` token.

Whitespace and comments between the keyword, name, and `(` are skipped when
matching. Exactly one diagnostic is emitted per malformed declaration, and
the same construct is deliberately NOT also reported as
`diag.direct_procedure_call`.

It must NOT flag:

- a correct declaration `:PROCEDURE Name;` with arguments via
  `:PARAMETERS`;
- a bare `PROCEDURE Name;` without parentheses (no `(` signal — not
  provably a declaration attempt);
- `PROCEDURE` or `:PROCEDURE` not followed by an identifier.

## Examples

### Flags

```ssl
PROCEDURE Demo(nValue);
:RETURN .T.;
ENDPROC;
```

### Flags

```ssl
:PROCEDURE Demo(nValue);
:RETURN .T.;
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE Demo;
:PARAMETERS nValue;
:RETURN nValue;
:ENDPROC;
```

## Rationale

The style guide's grammar defines the only legal declaration form
(`:PROCEDURE Name;` + `:PARAMETERS`), but has no lint slug for the typo
patterns, so this is a tool-authored rule (`authority: tool`). It exists
because the previous behavior was worse than silence: `Demo(nValue)` after
a bare `PROCEDURE` fell through to checkDirectProcedureCalls and produced
"custom procedures cannot be called directly" — a confusing message for
what is really a declaration syntax error (d134334). Error severity matches
the fact that neither shape compiles as a declaration.
