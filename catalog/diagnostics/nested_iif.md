---
id: diag.nested_iif
title: IIF call nested inside another IIF call
kind: diagnostic
status: active
authority: advisory
schema_ref: lints.style_rules.no_nested_ternaries
default_severity: info
severity_overridable: true
suppressible: true
tests:
  - internal/providers/providers_test.go
history:
  - date: 2026-03-28
    ref: "commit be7a174"
    note: >-
      Introduced in the diagnostics expansion pass, implementing the style
      guide's no_nested_ternaries lint.
  - date: 2026-04-30
    ref: "PR #3 (v0.4.0, commit d744511)"
    note: Stable diagnostic code assigned; behavior unchanged.
issues: []
---

## Behavior

Flags an `IIF(` call that appears anywhere inside the parentheses of
another `IIF(` call — in any argument position, at any depth. Info
severity, ranged on the inner `IIF` token. Function-name matching is
case-insensitive (`iif(` counts); a bare `IIF` identifier not followed by
`(` is not a call and neither anchors nor triggers the rule.

It must NOT flag:

- a single, un-nested `IIF()` call, however complex its arguments;
- sibling `IIF()` calls in the same statement that are not inside one
  another (`sA := IIF(b1, "x", "y") + IIF(b2, "u", "v");`);
- other function calls nested inside an `IIF()`.

Note: with three or more nested levels, an inner call is reported once per
enclosing `IIF` that contains it, so deeply nested chains produce multiple
diagnostics — the noise is proportional to the offense.

## Examples

### Flags

```ssl
:DECLARE sResult;
sResult := IIF(bCond1, IIF(bCond2, "A", "B"), "C");
```

### Does not flag

```ssl
:DECLARE sResult;
sResult := IIF(bCond1, "Yes", "No");
```

### Does not flag

```ssl
:DECLARE sFirst, sSecond;
sFirst := IIF(bCond1, "A", "B");
sSecond := IIF(bCond2, "C", "D");
```

## Rationale

The style guide's lints set `no_nested_ternaries: true` at advisory level.
Nested `IIF()` is legal and sometimes compact, but each level doubles the
reader's branch tracking; `:BEGINCASE`/`:CASE` or `:IF`/`:ELSE` states the
same logic linearly. Info severity (one notch above the pure-style hints)
reflects that nested ternaries measurably hide bugs while still being a
readability rule with no runtime consequence. Pinned in providers_test.go
(TestGetDiagnostics_NestedIIF*).
