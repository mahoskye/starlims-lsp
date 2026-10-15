---
id: diag.prefer_explicit_trim_concat
title: String minus instead of Trim() and +
kind: diagnostic
status: active
authority: style_only
schema_ref: null
default_severity: info
config:
  - ssl.diagnostics.infoDiagnostics
  - ssl.diagnostics.hungarianTypes
severity_overridable: true
suppressible: true
spec_options:
  include_info_diagnostics: true
tests:
  - internal/providers/atoms_test.go
history:
  - date: 2026-10-14
    ref: "issues #339, #347"
    note: >-
      Introduced from ssl-style-guide's prefer_explicit_trim_concat
      (style_only, info). Verified on STARLIMS 12 (test batch P22): string
      `-` trims trailing spaces from the left operand only, then appends
      the right operand unchanged; leading spaces, the right operand's
      spaces and a trailing tab stay.
issues: []
---

## Behavior

Flags a binary `-` whose operands are both known strings, on the `-`
token. On strings, `-` trims trailing spaces from the left operand and
then appends the right one, which reads like subtraction; the message
suggests `Trim(a) + b` and notes that `Trim()` also removes trailing tabs
and line breaks, which `-` keeps.

An operand is a known string when it is a string literal, a builtin
documented to return a string (`AllTrim(sName)`), or an expression of
those. With `ssl.diagnostics.hungarianTypes` on (`--hungarian-types`), an
`s`-prefixed name counts as a string too.

It must NOT flag:

- numeric or date `-`;
- an operand whose type is unknown, such as an unprefixed name, or an
  `s`-prefixed name when `hungarianTypes` is off.

## Examples

### Flags

```ssl
:DECLARE sJoined;
sJoined := "Sample   " - "Analysis";
```

### Does not flag

```ssl
:DECLARE sFirst, sLast, sJoined, nDiff;
sJoined := sFirst - sLast;
nDiff := 5 - 3;
sJoined := AllTrim(sFirst) + sLast;
```

## Rationale

String minus is valid and sometimes useful, but it hides a trim behind an
arithmetic operator. The style guide asks for the explicit form at info,
and only where both operands are known strings, so numeric and date
subtraction never flag. Hungarian prefixes count only when the consumer has
opted into reading them as types, as for `hungarian_type_mismatch`.
