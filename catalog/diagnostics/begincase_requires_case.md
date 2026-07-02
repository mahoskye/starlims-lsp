---
id: diag.begincase_requires_case
title: BEGINCASE block with no CASE clause
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
      Introduced in the full alignment pass with ssl-style-guide
      (checkBeginCaseHasCase).
  - date: 2026-04-30
    ref: "PR #3 (v0.4.0)"
    note: Stable diagnostic code assigned; rule behavior unchanged.
issues: []
---

## Behavior

Flags a `:BEGINCASE` block that closes with `:ENDCASE` without containing
a single `:CASE` clause — for example a block holding only an
`:OTHERWISE`, or an empty block. The range covers the `:BEGINCASE` token.
Nested `:BEGINCASE` blocks are tracked on a stack, so each block is judged
only by its own clauses.

It must NOT flag:

- blocks containing at least one `:CASE`, regardless of whether an
  `:OTHERWISE` is present (a missing `:OTHERWISE` is the separate hint
  `diag.missing_otherwise`);
- a `:BEGINCASE` that never reaches its `:ENDCASE` — the diagnostic is
  emitted at close, so unclosed blocks are left to the unclosed-block
  check rather than double-reported here;
- a stray `:ENDCASE` with no open `:BEGINCASE`.

## Examples

### Flags

```ssl
:PROCEDURE Demo;
:DECLARE nMode;
:BEGINCASE;
:OTHERWISE;
nMode := 0;
:EXITCASE;
:ENDCASE;
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE Demo;
:DECLARE nMode, nOut;
:BEGINCASE;
:CASE nMode = 1;
nOut := 1;
:EXITCASE;
:OTHERWISE;
nOut := 0;
:EXITCASE;
:ENDCASE;
:ENDPROC;
```

## Rationale

A `:BEGINCASE` exists to select among `:CASE` branches; with none, the
construct either does nothing or unconditionally runs its `:OTHERWISE`,
which is always clearer written as plain statements. The style guide's
structural rules treat this as malformed, so it reports as an error.
Emitting on the closing `:ENDCASE` keeps the rule from piling onto blocks
that are already broken for a different reason (unclosed). Introduced in
the style-guide alignment pass (commit cdbfee6); the code slug was
stabilized in PR #3 (v0.4.0).
