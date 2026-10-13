---
id: diag.catch_order_before_finally
title: CATCH appearing after FINALLY in a TRY block
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
    ref: "PR #3 (v0.4.0)"
    note: Stable diagnostic code assigned; rule behavior unchanged.
  - date: 2026-10-13
    ref: "issue #255"
    note: >-
      authority corrected to authoritative: the style-guide record
      catch_before_finally (level authoritative) names this rule in
      enforced_by, so the guide is not silent. Rationale cites the
      reference's clause order instead of guessing at runtime behavior;
      a nested-TRY fence pins per-block judging.
issues: ["#292"]
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
- a `:CATCH` outside any `:TRY` block — not this ordering rule's concern,
  and currently reported by no rule (issue #292).

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

### Does not flag

A nested `:TRY` inside an outer `:FINALLY` has its own clause order.

```ssl
:PROCEDURE Demo;
:DECLARE nCount;
:TRY;
nCount := 1;
:FINALLY;
:TRY;
nCount := 2;
:CATCH;
nCount := 0;
:ENDTRY;
:ENDTRY;
:ENDPROC;
```

## Rationale

The reference (keywords/TRY, CATCH) fixes the clause order: when both
handlers are present, `:CATCH` must precede `:FINALLY`. A `:CATCH` after
`:FINALLY` is a malformed block, which merits an error. The dedicated code
(rather than folding into a generic try-structure error) lets the message
state the exact fix: move `:CATCH` before `:FINALLY`. Introduced in the
style-guide alignment pass (commit cdbfee6); the code slug was stabilized
in PR #3 (v0.4.0).
