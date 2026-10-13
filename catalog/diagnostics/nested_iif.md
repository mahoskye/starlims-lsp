---
id: diag.nested_iif
title: IIF call nested inside another IIF call
kind: diagnostic
status: active
authority: style_only
schema_ref: lints.style_rules.no_nested_ternaries
default_severity: info
config:
  - ssl.diagnostics.infoDiagnostics
severity_overridable: true
suppressible: true
spec_options:
  include_info_diagnostics: true
tests:
  - internal/providers/providers_test.go
  - internal/providers/diagnostics_audit_test.go
history:
  - date: 2026-03-28
    ref: "commit be7a174"
    note: >-
      Introduced in the diagnostics expansion pass, implementing the style
      guide's no_nested_ternaries lint.
  - date: 2026-04-30
    ref: "PR #3 (v0.4.0, commit d744511)"
    note: Stable diagnostic code assigned; behavior unchanged.
  - date: 2026-10-13
    ref: "issue #255"
    note: >-
      Each nested IIF is reported once. At three or more levels an inner
      call had been reported once per enclosing IIF, as identical
      diagnostics on one range. Authority corrected advisory ->
      style_only to mirror the style guide record (no_nested_iif).
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

Each nested call is reported once, however many `IIF` calls enclose it:
`IIF(a, IIF(b, IIF(c, 1, 2), 3), 4)` produces two diagnostics, one per
nested call. An `IIF` inside a code block literal (`{|x| IIF(...)}`) is
not seen — the block is a single opaque token.

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

The style guide's `no_nested_iif` record (style_only) forbids nesting.
Nested `IIF()` is legal and sometimes compact, but each level doubles the
reader's branch tracking; `:BEGINCASE`/`:CASE` or `:IF`/`:ELSE` states the
same logic linearly. Info severity: D13's opt-in advisory tier, for a
readability rule with no runtime consequence. Pinned in providers_test.go
(TestGetDiagnostics_NestedIIF*).
