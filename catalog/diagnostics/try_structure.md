---
id: diag.try_structure
title: TRY body with no statements before its handler
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
      Catalog audit: one report per empty body. The emit at :ENDTRY could
      never be the only report (the body flag only ever turns true, so the
      handler-clause emit had already fired for the same :TRY) and it fired
      inconsistently — twice for `:TRY; :CATCH; :ENDTRY;` and for an empty
      body before a non-empty :FINALLY, once when the :CATCH held a
      statement. Removed. Authority corrected to authoritative to mirror
      the style-guide rule try_and_finally_bodies_not_empty.
issues: []
---

## Behavior

Flags a `:TRY` block whose body contains no statements before its first
handler clause — the protected body must hold at least one statement.
Exactly one diagnostic per empty body, emitted when the first `:CATCH` or
`:FINALLY` is reached, with the message `':TRY' requires at least one
statement before ':CATCH' or ':FINALLY'`, error severity, and the opening
`:TRY` token as its range.

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

### Flags

```ssl
:PROCEDURE Demo;
:DECLARE nCount;
:TRY;
:FINALLY;
nCount := 2;
:ENDTRY;
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE Demo;
:TRY;
:ENDTRY;
:ENDPROC;
```

## Rationale

The style-guide rule `try_and_finally_bodies_not_empty` (authoritative,
error) requires a non-empty `:TRY` body, and its notes name this code as
the LSP's report of it, so the authority mirrors it. An empty protected
body means the handler guards nothing, which is almost always a
refactoring leftover. The empty-`:CATCH` fence pins the asymmetry that
matters most in practice: the body must not be empty, the catch may be.
One report per empty body (issue #255): the second Flags fence, an empty
body before a non-empty `:FINALLY`, used to be reported twice. The
bare-`:TRY;:ENDTRY;` fence pins the single-report contract with
`diag.try_requires_handler`.
