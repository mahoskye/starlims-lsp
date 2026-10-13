---
id: diag.nil_in_operations
title: NIL literal in an arithmetic or string operation
kind: diagnostic
status: active
authority: authoritative
schema_ref: lints.type_safety.nil_in_operations
default_severity: warning
config:
  - ssl.diagnostics.rules
severity_overridable: true
suppressible: true
tests:
  - internal/providers/providers_test.go
  - internal/providers/diagnostics_audit_test.go
history:
  - date: 2026-03-21
    ref: "commit cdbfee6"
    note: >-
      Introduced (checkLiteralTypeSafety) in the style-guide alignment
      pass that added schema-backed rule enforcement.
  - date: 2026-04-30
    ref: "PR #3 (v0.4.0)"
    note: Stable diagnostic code assigned when Code was populated on every diagnostic.
  - date: 2026-10-13
    ref: "issue #260"
    note: >-
      Re-checked against the reference verified on STARLIMS 12
      (operators/equals.md, strict-equals.md, not-equals.md, hash.md): a
      number or date compared with NIL returns a logical instead of
      raising, so a guard such as `nValue == NIL` is safe. Behavior
      already matches; a fence pins NIL comparisons against numbers and
      dates.
issues: []
  - date: 2026-10-13
    ref: "issue #255"
    note: >-
      Moved onto the expression tree: the operand must be the NIL literal
      itself. `1 + NIL:ToString()` had been reported here as well as by
      nil_method_call, because the nearest token to the `+` was NIL; its
      operand is a member call. No change on the production corpus.
      Authority corrected style_only -> authoritative to mirror the style
      guide record (nil_in_operations). Whether the literal-only finding
      should be an error is an open question on #255 (see Rationale).
issues: ["#255"]
---

## Behavior

Flags the operators `+`, `-`, `*`, `/` (and a unary minus) when an
operand, read from the expression tree, is the literal `NIL` itself, at
the operator's range. Detection is deliberately literal-only: it fires
exactly when `NIL` itself is written as an operand, which is always a
runtime error in SSL.

It must NOT flag:

- operations on a *variable* that merely holds NIL — no data-flow
  analysis is attempted, so `nValue + 1` never fires this rule even right
  after `nValue := NIL;` (mixed-type inference is `mixed_type_operator`'s
  business);
- `NIL` used with comparison operators (`=`, `==`, `!=`) — comparisons
  against NIL are legal and covered by `nil_not_empty_string` where
  relevant;
- assignments of NIL (`x := NIL;`), which are legal;
- a member chain on NIL (`1 + NIL:ToString()`) — the operand is the
  member call, not NIL, and the defect is `nil_method_call`'s;
- compound assignments (`x += NIL;`), which are statements rather than
  operations in the expression grammar.

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

### Does not flag

```ssl
:PROCEDURE Check;
	:PARAMETERS nValue, dWhen;
	:IF nValue == NIL .OR. dWhen != NIL;
		:RETURN 5 = NIL;
	:ENDIF;
	:RETURN Today() = NIL;
:ENDPROC;
```

### Does not flag

```ssl
x := 1 + NIL:ToString();
```

## Rationale

The style guide's `nil_in_operations` record (authoritative, warning),
from the schema's `lints.type_safety`: "Using NIL in arithmetic or string
operations causes error." A literal
NIL operand can never be intentional working code. The literal-only scope
keeps the false-positive rate at zero at the cost of missing NIL-bearing
variables. Introduced in the 2026-03-21 alignment pass (history).

Open question (#255): by the severity-encodes-kind rule, an operand that
is the NIL literal itself is a guaranteed runtime failure, which argues
for `error`. The entry stays at `warning` — the style guide record's
severity — until that is decided, because a severity change is breaking
for consumers that gate on it.
