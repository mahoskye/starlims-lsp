---
id: diag.equals_empty_string
title: "`= \"\"` comparison that is always true"
kind: diagnostic
status: active
authority: authoritative
schema_ref: null
default_severity: warning
severity_overridable: true
suppressible: true
tests:
  - internal/providers/expr_operator_checks_test.go
history:
  - date: 2026-10-13
    ref: "issue #255"
    note: >-
      Split from equals_vs_strict_equals, whose info-tier prefix-match
      note was the only report of an always-true condition. Two findings
      on the private corpus, both in conditions.
issues: []
---

## Behavior

Flags a `=` comparison whose right operand is an empty string literal
(`""`, `''`, or `[]`). For strings, `=` is a prefix match that is true
whenever the right operand is empty (reference operators/equals), so
`x = ""` is true for every string `x` — it is not an emptiness test.
It is false only when `x` is `NIL`. The range covers the comparison; the
message suggests `Empty(x)` or `x == ""`.

Operands come from the expression AST, so the right operand is exactly
the empty literal, not a longer expression that starts with one.

It must NOT flag:

- `"" = x` — with the empty literal on the left the comparison is true
  only when `x` is empty, a correct emptiness test;
- `x == ""` or `x != ""` — exact comparison;
- a left operand known not to be a string (a number, date, ...) — that
  comparison is false or raises, a different finding;
- a statement that is nothing but `x = "";` — that is a mistyped
  assignment, not a test of anything, and not this rule's finding;
- empty strings inside SQL text or other string content.

## Examples

### Flags

```ssl
:PROCEDURE Demo;
:PARAMETERS sStatus;
:IF sStatus = "";
	sStatus := "Logged";
:ENDIF;
:ENDPROC;
```

### Flags

```ssl
:PROCEDURE Demo;
:PARAMETERS aRow;
:DECLARE bBlank;
bBlank := aRow[2] = '' .OR. aRow[3] = '';
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE Demo;
:PARAMETERS sStatus;
:DECLARE bBlank;
bBlank := Empty(sStatus);
bBlank := sStatus == "";
bBlank := "" = sStatus;
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE Demo;
:DECLARE sName;
sName = "";
:ENDPROC;
```

## Rationale

The reference documents `=` on strings as true when the right operand is
empty, so `:IF sStatus = "";` takes its branch for every string value.
Authors write it as an emptiness test, and it never is one: a likely bug,
hence warning. `equals_vs_strict_equals` already noted the prefix
semantics of every `=` against a string literal, but at info severity
(the opt-in advisory tier), where an always-true condition was invisible
by default. Issue #255 split this shape out at warning. The left-literal
form is exempt because it is the correct test, and the bare statement
form because its defect is the missing `:`, not the comparison.
