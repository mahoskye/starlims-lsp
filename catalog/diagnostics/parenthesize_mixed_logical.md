---
id: diag.parenthesize_mixed_logical
title: ".AND. and .OR. mixed without parentheses"
kind: diagnostic
status: active
authority: style_only
schema_ref: null
default_severity: warning
severity_overridable: true
suppressible: true
tests:
  - internal/providers/atoms_test.go
history:
  - date: 2026-10-14
    ref: "issues #339, #347"
    note: >-
      Introduced from ssl-style-guide's parenthesize_mixed_logical
      (style_only, warning). Verified on STARLIMS 12 (test batch P22):
      .AND. binds tighter than .OR., so `.T. .OR. .T. .AND. .F.` is .T.,
      and in `.T. .OR. X .AND. Y` neither X nor Y runs.
issues: []
---

## Behavior

Flags each `.AND.` group that is an operand of `.OR.` without parentheses
around it: in `bA .OR. bB .AND. bC` the range covers `bB .AND. bC`, which
`.AND.`'s tighter binding groups first. `bA .AND. bB .OR. bC .AND. bD`
reports both groups. The message asks for parentheses that state the
grouping.

It must NOT flag:

- an expression that uses only `.AND.` or only `.OR.`;
- an `.AND.` group already in parentheses (`bA .OR. (bB .AND. bC)`);
- an `.OR.` in parentheses under `.AND.` (`(bA .OR. bB) .AND. bC`), which
  is explicit.

## Examples

### Flags

```ssl
:DECLARE bA, bB, bC, bOk;
bOk := bA .OR. bB .AND. bC;
```

### Does not flag

```ssl
:DECLARE bA, bB, bC, bOk;
bOk := bA .OR. (bB .AND. bC);
bOk := (bA .OR. bB) .AND. bC;
bOk := bA .AND. bB .AND. bC;
```

## Rationale

Implicit precedence between logical operators is the most-confused atom in
both reading studies issue #339 cites, and in SSL short-circuiting adds to
it: a reader who groups left to right expects an operand to run that never
does. Parentheses never change what a correct expression does, so the fix
is free. The style guide sets warning.
