---
id: diag.try_structure
title: TRY body with no statements before its handler
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
  - date: 2026-03-21
    ref: "commit cdbfee6"
    note: >-
      Introduced in the full alignment pass with ssl-style-guide, as part
      of the TRY/CATCH/FINALLY structure rules (checkTryStructure).
  - date: 2026-04-30
    ref: "PR #3 (v0.4.0, commit d744511)"
    note: Stable diagnostic code assigned; rule behavior unchanged.
issues: []
---

## Behavior

Flags a `:TRY` block whose body contains no statements before its first
handler clause — the schema requires the protected body to hold at least
one statement. Three emit sites share the same message (`':TRY' requires
at least one statement before ':CATCH' or ':FINALLY'`), error severity,
and range (the opening `:TRY` token):

- when a `:CATCH` is reached with an empty body;
- when a `:FINALLY` is reached with an empty body;
- at `:ENDTRY`, if the block has a handler and no body statement ever
  appeared (so `:TRY; :CATCH; :ENDTRY;` is reported twice — once at the
  empty-body `:CATCH`, once at close; consumers should de-duplicate by
  range if needed).

Semicolons, whitespace, and comments do not count as statements; any other
token does, including a nested `:TRY` keyword. Nested blocks are tracked
on a stack — each body is judged independently.

It must NOT flag:

- a `:TRY` with at least one statement (even a single one) before its
  first `:CATCH`/`:FINALLY`;
- a bare `:TRY; :ENDTRY;` with an empty body and no handler at all — that
  is reported only as `diag.try_requires_handler`, not additionally as an
  empty body;
- an empty `:CATCH` body (explicitly legal — "zero or more statements");
  an empty `:FINALLY` body is the separate rule `diag.finally_empty`.

## Examples

### Flags

```ssl
:PROCEDURE Demo;
:DECLARE nCount;
:TRY;
:CATCH;
nCount := 0;
:ENDTRY;
:ENDPROC;
```

### Flags

```ssl
:PROCEDURE Demo;
:DECLARE nCount;
:TRY;
/* a comment is not a statement;
:FINALLY;
nCount := 2;
:ENDTRY;
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE Demo;
:DECLARE nCount;
:TRY;
nCount := 1;
:CATCH;
:ENDTRY;
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE Demo;
:DECLARE nCount;
:TRY;
nCount := 1;
:ENDTRY;
:ENDPROC;
```

## Rationale

The schema's error-handling structure marks the `:TRY` body as "requires
at least one statement" (authoritative section, but no lints slug — hence
`authority: tool`, like the sibling TRY rules). An empty protected body
means the handler guards nothing, which is almost always a refactoring
leftover; error severity matches the schema's structural framing. The
empty-`:CATCH` fence pins the asymmetry that matters most in practice:
the body must not be empty, the catch may be. The bare-`:TRY;:ENDTRY;`
fence pins the single-report contract with `diag.try_requires_handler`.
