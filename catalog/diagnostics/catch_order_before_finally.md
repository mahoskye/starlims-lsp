---
id: diag.catch_order_before_finally
title: CATCH appearing after FINALLY in a TRY block
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
    ref: "PR #3 (v0.4.0)"
    note: Stable diagnostic code assigned; rule behavior unchanged.
issues: []
---

## Behavior

Flags a `:CATCH` that appears after a `:FINALLY` within the same `:TRY`
block. SSL requires the fixed clause order `:TRY` body, optional `:CATCH`,
optional `:FINALLY`, `:ENDTRY`; a catch clause following the finally
clause is malformed. The range covers the misplaced `:CATCH` token.

Nested `:TRY` blocks are tracked on a stack, so a `:CATCH` is judged
against its own block's `:FINALLY`, not an outer one. When this rule
fires, the misplaced `:CATCH` is not additionally counted for the
same-block single-catch rule.

It must NOT flag:

- the correct order `:TRY ... :CATCH ... :FINALLY ... :ENDTRY`;
- `:TRY`/`:CATCH` without any `:FINALLY`;
- a `:CATCH` outside any `:TRY` block — that placement is owned by the
  block-structure checks, not this ordering rule.

## Examples

### Flags

```ssl
:PROCEDURE Demo;
:DECLARE nCount;
:TRY;
nCount := 1;
:FINALLY;
nCount := 2;
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
:CATCH;
nCount := 0;
:FINALLY;
nCount := 2;
:ENDTRY;
:ENDPROC;
```

## Rationale

A `:CATCH` after `:FINALLY` will not behave as error handling — the
runtime's clause order is fixed — so code that compiles in the author's
head silently doesn't catch, which merits an error. The dedicated code
(rather than folding into a generic try-structure error) lets the message
state the exact fix: move `:CATCH` before `:FINALLY`. Introduced in the
style-guide alignment pass (commit cdbfee6); the code slug was stabilized
in PR #3 (v0.4.0).
