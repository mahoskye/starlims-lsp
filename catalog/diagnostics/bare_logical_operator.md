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
  - date: 2026-08-12
    ref: "issue #165"
    note: >-
      Position-blindness retired: the "SSL code never names things after
      logical operators" premise was refuted by WSDL-generated proxy
      classes declaring members named And/Or. The check now fires only in
      expression-operator positions (And/Or between two operands, Not as
      prefix before an operand); declaration lists, member access, and
      assignment targets are identifier slots and never flag.
issues: []
---

## Behavior

Flags an identifier token spelled `AND`, `OR`, or `NOT`
(case-insensitive) used in an expression-operator position. SSL's logical
operators are the period-delimited forms `.AND.`, `.OR.`, `.NOT.`; the
bare words are not operators and the runtime rejects them there. The
message suggests the correct period-delimited replacement, and the range
covers the bare word.

Operator position (issue #165):

- `And`/`Or` flag when between two operands — the previous significant
  token can end an operand (identifier, number, string, literal, closing
  `)`/`]`/`}`) and the next can start one (identifier, number, string,
  literal, opening `(`/`{`, prefix `!`/`.NOT.`).
- `Not` flags in prefix position — an operand follows and no operand
  precedes.

It must NOT flag:

- the correct operators `.AND.`, `.OR.`, `.NOT.` — the lexer produces
  operator tokens for these, never identifiers;
- the words inside string literals (e.g. SQL text `"a = 1 AND b = 2"`) or
  inside comments, which are single non-identifier tokens;
- identifiers named `And`/`Or`/`Not` in identifier slots (issue #165):
  `:DECLARE`/`:PARAMETERS` lists, assignment targets (`And := 1`), and
  member access (`oProxy:And`) — WSDL-generated proxy classes really do
  declare such members.

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

### Does not flag

```ssl
:DECLARE And;
And := 1;
```

### Does not flag

```ssl
:DECLARE oProxy, x;
x := oProxy:And;
```

## Rationale

Bare `AND`/`OR`/`NOT` in an operator position is guaranteed-broken code —
the SSL runtime has no such operators — so this is an error, not a style
nit. The original position-blind identifier scan traded "a theoretical
false positive (a variable literally named `And`)" for zero false
negatives; issue #165 showed the false positive is real (WSDL-generated
proxy classes declare members named `And`/`Or`), so the check narrowed to
operator positions, where the operand-adjacency test keeps every genuine
misuse flagged. Introduced in commit 567b287; the code slug was stabilized
in PR #3 (v0.4.0).
