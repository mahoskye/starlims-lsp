---
id: diag.single_finally
title: More than one FINALLY in a TRY block
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

Flags a second (or later) `:FINALLY` within the same `:TRY` block — a
`:TRY` allows at most one `:FINALLY`. The range covers the extra `:FINALLY`
token, one diagnostic per extra clause. Statements after the extra
`:FINALLY` are not counted toward the first `:FINALLY`'s body.

Nested `:TRY` blocks are tracked on a stack, so each `:FINALLY` is counted
only against its own innermost block.

It must NOT flag:

- the first `:FINALLY` of any `:TRY` block, including one `:FINALLY` in
  each of several nested blocks;
- a `:FINALLY` in a block that also has a `:CATCH` — one of each is the
  normal full form;
- a stray `:FINALLY` outside any `:TRY` block (owned by the
  block-structure checks).

## Examples

### Flags

```ssl
:PROCEDURE Demo;
:DECLARE nCount;
:TRY;
nCount := 1;
:FINALLY;
nCount := 2;
:FINALLY;
nCount := 3;
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

### Does not flag

```ssl
:PROCEDURE Demo;
:DECLARE nCount;
:TRY;
nCount := 1;
:TRY;
nCount := 2;
:FINALLY;
nCount := 3;
:ENDTRY;
:FINALLY;
nCount := 0;
:ENDTRY;
:ENDPROC;
```

## Rationale

The schema's error-handling structure (authoritative, but with no lints
slug — hence `authority: tool` for the transcription) allows the clause
sequence body / optional `:CATCH` / optional `:FINALLY` / `:ENDTRY`;
a duplicate `:FINALLY` has no defined execution order and is a structural
error, matching the error severity. The nested-block fence pins the
stack-based scoping under which each block owns exactly its own clauses.
