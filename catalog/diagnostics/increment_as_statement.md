---
id: diag.increment_as_statement
title: "++ or -- inside a larger expression"
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
      Introduced from ssl-style-guide's increment_as_statement (style_only,
      info). Verified on STARLIMS 12 (test batch P22): postfix yields the
      old value and prefix the new, operands evaluate left to right, and
      each increment lands before the next operand is read, so with
      i = 1, `i++ + i++` is 3 and leaves i at 3. Two tiers, as the style
      guide asked: info for a single use, warning when the expression
      uses the variable again.
issues: []
---

## Behavior

Flags a `++` or `--` used inside a larger expression: an array subscript
(`aRows[i++]`), an operand (`n := i++ + 1`), a call argument
(`DoProc("P", {n++})`), a condition (`:WHILE ++i <= n;`), an assignment's
value (`nNext := nCount++;`). The range covers the increment expression.

Two tiers under one code:

- **info** for a single increment of a variable in its statement. The
  reader has to work out whether the old or the new value is used.
- **warning** when the same statement increments that variable more than
  once, or also reads it (`i++ + i++`, `aSeen[i++] := i;`). Operands
  evaluate left to right and each increment lands before the next operand
  is read, so the result depends on an order a reader rarely gets right.
  One finding per variable per statement.

The info tier is visible with `ssl.diagnostics.infoDiagnostics`; the
warning tier is always visible.

It must NOT flag:

- an increment that is a whole statement (`i++;`, `++i;`, `aCounts[1]++;`);
- an increment that is a `:FOR` loop's `:STEP` expression.

## Examples

### Flags

```ssl
:DECLARE aRows, i, vItem;
i := 1;
vItem := aRows[i++];
```

### Flags

The variable is used twice in one expression (warning).

```ssl
:DECLARE aSeen, nIndex;
nIndex := 1;
aSeen[nIndex++] := nIndex;
```

### Does not flag

```ssl
:DECLARE nCount, aCounts;
nCount := 0;
nCount++;
++nCount;
aCounts[1]++;
```

## Rationale

An increment inside an expression is among the best-evidenced "atoms of
confusion" (issue #339): readers misjudge which value is used. The style
guide makes it a style rule at info, with the stronger warning where the
expression reuses the variable, since then even a reader who knows the
pre/post rule must also know the evaluation order. The common
`:WHILE ++i <= n;` loop idiom is reported at info, which the info gate
hides by default.
