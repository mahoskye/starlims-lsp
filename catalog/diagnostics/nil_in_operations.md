---
id: diag.nil_in_operations
title: NIL literal in an arithmetic or string operation
kind: diagnostic
status: active
authority: style_only
schema_ref: lints.type_safety.nil_in_operations
default_severity: warning
config:
  - ssl.diagnostics.rules
severity_overridable: true
suppressible: true
tests:
  - internal/providers/providers_test.go
history:
  - date: 2026-03-21
    ref: "commit cdbfee6"
    note: >-
      Introduced (checkLiteralTypeSafety) in the style-guide alignment
      pass that added schema-backed rule enforcement.
  - date: 2026-04-30
    ref: "PR #3 (v0.4.0)"
    note: Stable diagnostic code assigned when Code was populated on every diagnostic.
issues: []
---

## Behavior

Flags the operators `+`, `-`, `*`, `/` when the nearest significant token
on either side is the literal `NIL`, at the operator's range. Detection
is deliberately literal-only: it fires exactly when `NIL` itself is
written as an operand, which is always a runtime error in SSL.

It must NOT flag:

- operations on a *variable* that merely holds NIL — no data-flow
  analysis is attempted, so `nValue + 1` never fires this rule even right
  after `nValue := NIL;` (mixed-type inference is `mixed_type_operator`'s
  business);
- `NIL` used with comparison operators (`=`, `==`, `!=`) — comparisons
  against NIL are legal and covered by `nil_not_empty_string` where
  relevant;
- assignments of NIL (`x := NIL;`), which are legal.

## Examples

### Flags

```ssl
x := NIL + 1;
```

### Flags

```ssl
sMsg := "Total: " + NIL;
```

### Does not flag

```ssl
:DECLARE nValue;
x := nValue + 1;
```

### Does not flag

```ssl
x := NIL;
:IF x = NIL;
    x := 0;
:ENDIF;
```

## Rationale

The schema lists `nil_in_operations` under `lints.type_safety` with
`severity: warning` (style_only): "Using NIL in arithmetic or string
operations causes error. Use Empty() to check for NIL first." A literal
NIL operand can never be intentional working code, but the check stays a
warning (not an error) because the pipeline's type checks are heuristic
by policy, and its literal-only scope keeps the false-positive rate at
zero at the cost of missing NIL-bearing variables. Introduced in the
2026-03-21 alignment pass (history).
