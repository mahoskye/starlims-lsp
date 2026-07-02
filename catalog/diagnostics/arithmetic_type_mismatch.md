---
id: diag.arithmetic_type_mismatch
title: Mismatched operand types in arithmetic
kind: diagnostic
status: active
authority: tool
schema_ref: null
default_severity: warning
severity_overridable: true
suppressible: true
tests:
  - internal/providers/providers_test.go
history:
  - date: 2026-03-21
    ref: "commit cdbfee6"
    note: >-
      Introduced in the full alignment pass with ssl-style-guide, as part
      of the literal-type-safety operator checks (checkLiteralTypeSafety).
  - date: 2026-04-30
    ref: "PR #3 (v0.4.0)"
    note: Stable diagnostic code assigned; rule behavior unchanged.
issues: []
---

## Behavior

Flags the arithmetic operators `-`, `*`, and `/` when the types of both
adjacent operands can be inferred and they differ. Two message variants
share this code, both at warning severity:

- **String operand** — either side infers as string
  (`"String in arithmetic operation ..."`);
- **Other non-numeric operand** — the types differ and at least one is
  not numeric, e.g. boolean or date
  (`"Non-numeric type in arithmetic operation ..."`).

The range covers the operator token. Operand types come from the shared
simple-type inference: literals (strings, numbers, `.T.`/`.F.`, array and
codeblock constructors), Hungarian-prefixed names (`s`→string,
`n`→numeric, `b`→boolean, ...), types tracked from local declarations and
assignments, built-in class constructors, and known built-in function
return types.

It must NOT flag:

- `+` — mixed-type `+` is the separate rule `diag.mixed_type_operator`,
  because `+` legally overloads to string concatenation;
- same-type operands (`2 - 1`, `nCount * nFactor`);
- expressions where either operand's type cannot be inferred — unknown
  types never flag;
- `NIL` literals in arithmetic, which report as `diag.nil_in_operations`
  before type comparison happens;
- member-access operands (`obj:prop`) and array elements on the
  operator's far side, which inference deliberately leaves untyped.

## Examples

### Flags

```ssl
:PROCEDURE Demo;
:DECLARE nTotal;
nTotal := "abc" - 1;
:ENDPROC;
```

### Flags

```ssl
:PROCEDURE Demo;
:DECLARE bDone, nTotal;
bDone := .T.;
nTotal := bDone * 2;
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE Demo;
:DECLARE nTotal, nCount;
nCount := 4;
nTotal := nCount - 1;
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE Demo;
:DECLARE sLabel;
sLabel := "total: " + 5;
:ENDPROC;
```

## Rationale

`-`, `*`, `/` are numeric-only in SSL; a string or boolean operand raises
a runtime error. The check still reports at warning, not error, because it
rests on heuristic type inference (Hungarian prefixes and simple
assignment tracking) rather than real type checking — the never-flag
guarantee for uninferable operands is what keeps it quiet on real code.
`+` is deliberately excluded and owned by `diag.mixed_type_operator` so
its message can explain the concatenation overload. Introduced in the
style-guide alignment pass (commit cdbfee6); the code slug was stabilized
in PR #3 (v0.4.0).
