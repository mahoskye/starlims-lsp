---
id: diag.one_assignment_per_statement
title: Assignment used inside a larger expression
kind: diagnostic
status: active
authority: style_only
schema_ref: null
default_severity: info
config:
  - ssl.diagnostics.infoDiagnostics
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
      Introduced from ssl-style-guide's one_assignment_per_statement
      (style_only, info). Verified on STARLIMS 12 (test batch P22):
      assignment is an expression whose value is the assigned value, so
      `a := b := 7`, `{n := 1, n + 1}` and `LimsString(n := 3)` all work,
      and `(n += 3)` yields the updated value.
issues: []
---

## Behavior

Flags an assignment (`:=` or a compound assignment) whose value is used by
a larger expression: an operand (`(nCount := 5) * 2`), an array element
(`{n := 1}`), a call argument (`LimsString(n := 3)`), a parenthesized
assignment inside a condition (`:WHILE (i += 1) <= n;`), or a `:RETURN`
value. The range covers the assignment.

It must NOT flag:

- a statement's own assignment (`nCount := 5;`);
- a pure chain, where every target receives the same value
  (`sStatus := sDefault := "PENDING";`);
- an assignment that is a whole condition (`:IF n := 1;`), which
  `assignment_in_condition` reports.

## Examples

### Flags

```ssl
:DECLARE nTotal, nCount;
nTotal := (nCount := 5) * 2;
```

### Flags

```ssl
:DECLARE aPair, n;
aPair := {n := 1, n + 1};
```

### Does not flag

```ssl
:DECLARE sStatus, sDefault, nCount;
sStatus := sDefault := "PENDING";
nCount := 5;
nCount += 1;
```

## Rationale

An assignment hidden inside another statement is easy to miss when a
reader scans for where a variable changes. The style guide accepts one
form of more than one assignment per statement, the same-value chain, and
asks for info on the rest. The `:WHILE (i += 1) <= n;` loop idiom counts
too; info keeps it out of the default surface.
