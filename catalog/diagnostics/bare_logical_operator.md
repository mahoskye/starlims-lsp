---
id: diag.bare_logical_operator
title: Bare AND/OR/NOT instead of .AND./.OR./.NOT.
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
  - date: 2026-01-14
    ref: "commit 567b287"
    note: >-
      Introduced with the first SSL-specific diagnostic batch (exit cases,
      bare logical operators, declare/default usage).
  - date: 2026-04-30
    ref: "PR #3 (v0.4.0)"
    note: Stable diagnostic code assigned; rule behavior unchanged.
issues: []
---

## Behavior

Flags any identifier token spelled `AND`, `OR`, or `NOT`
(case-insensitive) anywhere in the file. SSL's logical operators are the
period-delimited forms `.AND.`, `.OR.`, `.NOT.`; the bare words are not
operators and the runtime rejects them. The message suggests the correct
period-delimited replacement, and the range covers the bare word.

The rule is deliberately position-blind: it does not try to prove the word
sits in an expression. Any identifier spelled `and`/`or`/`not` flags, which
also (intentionally) discourages using these words as variable or procedure
names.

It must NOT flag:

- the correct operators `.AND.`, `.OR.`, `.NOT.` — the lexer produces
  operator tokens for these, never identifiers;
- the words inside string literals (e.g. SQL text `"a = 1 AND b = 2"`) or
  inside comments, which are single non-identifier tokens.

## Examples

### Flags

```ssl
:PROCEDURE Demo;
:DECLARE bReady, bDone;
:IF bReady AND bDone;
:ENDIF;
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE Demo;
:DECLARE bReady, bDone;
:IF bReady .AND. .NOT. bDone;
:ENDIF;
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE Demo;
:DECLARE sSql;
/* AND appears in prose and in SQL text below;
sSql := "SELECT 1 WHERE A = 1 AND B = 2";
:ENDPROC;
```

## Rationale

Bare `AND`/`OR`/`NOT` is guaranteed-broken code — the SSL runtime has no
such operators — so this is an error, not a style nit. The position-blind
identifier scan trades a theoretical false positive (a variable literally
named `And`) for zero false negatives; in practice SSL code never names
things after logical operators. Introduced in commit 567b287; the code slug
was stabilized in PR #3 (v0.4.0).
