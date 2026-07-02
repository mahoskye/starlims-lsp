---
id: diag.finally_empty
title: Empty :FINALLY block
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
    ref: "commit cdbfee6 (v0.3.0)"
    note: >-
      Introduced in the TRY/CATCH/FINALLY structure checks during full
      alignment with ssl-style-guide.
  - date: 2026-04-30
    ref: "PR #3 (v0.4.0), commit d744511"
    note: Stable code finally_empty assigned.
issues: []
---

## Behavior

Always-on structural check (part of the `:TRY` structure family): when a
`:TRY` block is closed by `:ENDTRY`, flags its `:FINALLY` clause if the
clause contains no statement. The diagnostic range is the `:FINALLY` token
itself. Any significant token between `:FINALLY` and `:ENDTRY` other than
`;` counts as a statement; comments and whitespace do NOT count, so a
`:FINALLY` holding only a comment still flags.

It must NOT flag when:

- the `:FINALLY` body contains at least one statement;
- the `:TRY` block has no `:FINALLY` clause at all (that case belongs to
  `try_requires_handler`);
- the `:TRY` is never closed by `:ENDTRY` — the check is evaluated at
  `:ENDTRY` (the unclosed block is reported separately).

## Examples

### Flags

```ssl
:TRY;
	nCount := 1;
:FINALLY;
:ENDTRY;
```

### Flags

```ssl
:TRY;
	nCount := 1;
:FINALLY;
	/* cleanup goes here someday;
:ENDTRY;
```

### Does not flag

```ssl
:TRY;
	nCount := 1;
:FINALLY;
	nCount := 0;
:ENDTRY;
```

### Does not flag

```ssl
:TRY;
	nCount := 1;
:CATCH;
	nCount := -1;
:ENDTRY;
```

## Rationale

An empty `:FINALLY` is always a leftover — either cleanup code was removed
without removing the clause, or the author stubbed it and forgot. Introduced
with the TRY-structure family (commit cdbfee6) at error severity to match
the sibling structural rules (`try_structure`, `single_finally`), which
treat a malformed handler skeleton as a real defect rather than style.
