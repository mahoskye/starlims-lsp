---
id: diag.parenthesize_negated_comparison
title: ".NOT. before a comparison without parentheses"
kind: diagnostic
status: active
authority: authoritative
schema_ref: null
default_severity: warning
severity_overridable: true
suppressible: true
tests:
  - internal/providers/literal_semantics_test.go
history:
  - date: 2026-10-14
    ref: "issue #339"
    note: >-
      Introduced from the atoms-of-confusion review. Verified on STARLIMS
      12 (test batch P22): `.NOT.` binds before `=`, so `.NOT. s = 'AB'`
      with a string `s` raises ("the operator/method: .NOT. is not
      implemented on type: SSLString"), and with a logical `b`, `.NOT. b =
      .F.` is `(.NOT. b) = .F.`. Severity set by ssl-style-guide's
      parenthesize_negated_comparison (warning), with error where the
      negated operand is a string or number literal, since that always
      raises.
issues: []
---

## Behavior

Flags a comparison whose left operand is an unparenthesized `.NOT.` or `!`
expression: `.NOT. a = b`, and likewise `==`, `!=`, `<>`, `#`, `$`, `<`,
`>`, `<=`, `>=`. Unary operators bind before every binary operator
(ssl-ebnf-grammar.md), so the expression is `(.NOT. a) = b`, not the
negated comparison its layout suggests. The range covers the whole
comparison, and the message suggests `.NOT. (a = b)`.

Severity is warning. Where the negated operand is a string or number
literal (`.NOT. 'AB' $ sList`), `.NOT.` always raises on it, so that case
is reported at error severity under the same code.

It must NOT flag:

- a parenthesized comparison, `.NOT. (a = b)`;
- an explicitly grouped negation, `(.NOT. a) = b`, which says what it
  does;
- `.NOT.` before an operand of `.AND.` / `.OR.`, which bind after it as a
  reader expects;
- `!=` and `#`, which are comparisons of their own, not negations.

## Examples

### Flags

```ssl
:DECLARE sCode, bMatch;
bMatch := .NOT. sCode = "AB";
```

### Flags

A literal operand always raises.

```ssl
:DECLARE sList;
:IF .NOT. 'AB' $ sList;
:ENDIF;
```

### Does not flag

```ssl
:DECLARE sCode, bMatch, bA, bB;
bMatch := .NOT. (sCode = "AB");
bMatch := (.NOT. bA) = .F.;
bMatch := .NOT. bA .AND. bB;
bMatch := sCode != "AB";
```

## Rationale

Implicit precedence is the most-confused "atom" in the reading studies
issue #339 cites, and in SSL it is worse than a misreading: on a string
`.NOT.` raises, and on a logical it silently computes something else. The
style guide sets the severity (warning). `!=` is not a drop-in fix for
`.NOT. (a = b)`, because it negates `==`, not `=`'s prefix match, so the
message suggests parentheses instead.
