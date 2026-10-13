---
id: diag.single_catch
title: More than one CATCH in a TRY block
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
    ref: "issue #255 (diagnostics audit, batch B)"
    note: >-
      Authority tool -> authoritative to mirror the style-guide record
      single_catch_per_try. The stray-:CATCH bullet no longer claims an owner
      that does not exist (issue #292).
  - date: 2026-10-13
    ref: "issue #292"
    note: >-
      The stray-placement bullet named "the block-structure checks" as
      owner, but no check reported it. It now points at
      stray_block_keyword, added for exactly that case.
issues: ["#292"]
---

## Behavior

Flags a second (or later) `:CATCH` within the same `:TRY` block — SSL has
no multi-catch; exactly one `:CATCH` is allowed per `:TRY`. The range
covers the extra `:CATCH` token, one diagnostic per extra clause.

Nested `:TRY` blocks are tracked on a stack, so each `:CATCH` is counted
only against its own innermost block.

It must NOT flag:

- the first `:CATCH` of any `:TRY` block, including one `:CATCH` in each
  of several nested blocks;
- a `:CATCH` that appears after a `:FINALLY` — that clause is reported as
  `diag.catch_order_before_finally` instead, and is deliberately not
  double-counted here;
- a stray `:CATCH` outside any `:TRY` block — that is
  `stray_block_keyword`.

## Examples

### Flags

```ssl
:PROCEDURE Demo;
:DECLARE nCount;
:TRY;
nCount := 1;
:CATCH;
nCount := 0;
:CATCH;
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
:TRY;
nCount := 2;
:CATCH;
nCount := 3;
:ENDTRY;
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
:CATCH;
nCount := 3;
:ENDTRY;
:ENDPROC;
```

## Rationale

The style-guide record `single_catch_per_try` (level authoritative, from
the schema's error-handling structure) states "Only one :CATCH block is
allowed per :TRY; there is no multi-catch", so a second clause is a
structural error, matching the error severity. The
catch-after-finally carve-out keeps one malformed clause from producing two
overlapping diagnostics; the nested-block fence pins the stack-based
scoping that keeps sibling blocks independent.
