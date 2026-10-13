---
id: diag.try_requires_handler
title: TRY block with neither CATCH nor FINALLY
kind: diagnostic
status: active
authority: authoritative
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
  - date: 2026-10-13
    ref: "issue #255"
    note: >-
      Catalog audit: authority corrected to authoritative to mirror the
      style-guide rule try_requires_handler (level authoritative, severity
      error), which names this slug in enforced_by. Behavior unchanged.
issues: []
---

## Behavior

Flags a `:TRY` block that reaches its `:ENDTRY` containing neither a
`:CATCH` nor a `:FINALLY` clause — bare `:TRY ... :ENDTRY` is rejected by
SSL. The diagnostic is emitted when the `:ENDTRY` is seen; the range covers
the opening `:TRY` token. Nested blocks are tracked on a stack, so each
block is judged only by its own clauses.

It must NOT flag:

- a `:TRY` with a `:CATCH`, a `:FINALLY`, or both;
- a `:TRY` that never reaches an `:ENDTRY` — the diagnostic fires at
  close, so unclosed blocks are left to the unclosed-block check rather
  than double-reported here;
- a stray `:ENDTRY` with no open `:TRY`. Conversely, an inner nested
  `:TRY` without a handler is still flagged even when the outer block has
  one — each block is judged alone.

## Examples

### Flags

```ssl
:PROCEDURE Demo;
:DECLARE nCount;
:TRY;
nCount := 1;
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
nCount := 0;
:ENDTRY;
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE Demo;
:DECLARE nCount;
:TRY;
nCount := 1;
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
```

## Rationale

The style-guide rule `try_requires_handler` (authoritative, error) states
the constraint — a bare `:TRY ... :ENDTRY` is rejected — and names this
check as its enforcement, so the authority mirrors it. Error severity
matches the runtime rejecting the construct. Emitting at `:ENDTRY` (rather
than at `:TRY`) is what keeps unclosed blocks single-reported — the fourth
fence pins that.
