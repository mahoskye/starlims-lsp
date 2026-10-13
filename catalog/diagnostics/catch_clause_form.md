---
id: diag.catch_clause_form
title: CATCH takes no exception variable or clause content
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
      Introduced in the full alignment pass with ssl-style-guide
      (checkCatchClauseForm).
  - date: 2026-04-30
    ref: "PR #3 (v0.4.0)"
    note: Stable diagnostic code assigned; rule behavior unchanged.
  - date: 2026-10-13
    ref: "issue #255"
    note: >-
      A :CATCH whose next token starts a new line has lost its `;`; it
      now reports a missing-terminator message on the :CATCH keyword
      instead of the exception-variable message. authority set to
      authoritative (reference keywords/CATCH).
issues: []
---

## Behavior

Flags a `:CATCH` keyword whose next significant token is anything other
than the terminating `;`. SSL's `:CATCH` is a bare clause — it binds no
exception variable and takes no filter; error details are retrieved inside
the block via `GetLastSSLError()`. The range covers the offending token
after `:CATCH` (typically the would-be exception variable), and the
message points at `GetLastSSLError()` as the replacement.

A `:CATCH` missing its `;` entirely also flags. When the next token
starts a new line it is the handler's first statement, not an exception
variable, so the message says the terminator is missing and the range
covers the `:CATCH` keyword instead (issue #255). Either way the clause
is malformed; the only accepted form is `:CATCH;`.

The check applies to every `:CATCH` keyword in the file, whether or not it
sits in a well-formed `:TRY` block — misplaced `:CATCH` is reported
separately by the TRY-structure rules.

It must NOT flag:

- the bare form `:CATCH;`;
- a `:CATCH` that is the last token of the file (nothing to judge).

## Examples

### Flags

```ssl
:PROCEDURE Demo;
:DECLARE nCount;
:TRY;
nCount := 1;
:CATCH oError;
nCount := 0;
:ENDTRY;
:ENDPROC;
```

### Flags

The `;` is missing; the next line is the handler body.

```ssl
:PROCEDURE Demo;
:DECLARE nCount;
:TRY;
nCount := 1;
:CATCH
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
:ENDTRY;
:ENDPROC;
```

## Rationale

`:CATCH e` is the single most common try/catch porting mistake from
C#/JavaScript, and it is guaranteed-broken SSL, hence an error. The
message teaches the idiomatic replacement (`GetLastSSLError()`) instead of
just rejecting the syntax; the reference (keywords/CATCH) states
`:CATCH` accepts no exception variable or filter. A missing `;` reports
under this code too, with its own message, because the clause is
malformed the same way. Introduced in the style-guide alignment pass (commit
cdbfee6); the code slug was stabilized in PR #3 (v0.4.0).
