---
id: diag.nil_not_empty_string
title: NIL compared against a default-value literal
kind: diagnostic
status: active
authority: advisory
schema_ref: lints.type_safety.nil_not_empty_string
default_severity: info
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

Flags the operators `=` and `==` when one operand (nearest significant
token) is the literal `NIL` and the other is a default-value literal —
an empty string (`""`, `''`, `[]`), the number `0`, or `.F.` — at the
operator's range. Such a comparison is always `.F.` in SSL: declared
variables initialize to empty string, not NIL, so testing NIL against a
default value is almost certainly a misunderstanding of initialization.

It must NOT flag:

- `NIL` compared against `NIL`, a variable, or any non-default literal —
  those comparisons are meaningful;
- variables compared against default-value literals (`sName == ""` is
  the normal initialized-value test; no data-flow analysis is attempted);
- `!=` comparisons, assignments (`x := NIL;`), or `NIL` in arithmetic
  (that is `nil_in_operations`).

## Examples

### Flags

```ssl
:IF NIL = "";
:ENDIF;
```

### Flags

```ssl
:IF NIL == 0;
:ENDIF;
```

### Does not flag

```ssl
:IF x = NIL;
:ENDIF;
```

### Does not flag

```ssl
:DECLARE sName;
:IF sName == "";
:ENDIF;
```

## Rationale

The schema lists `nil_not_empty_string` under `lints.type_safety` with
`severity: info` (advisory): "NIL is not the same as empty string, zero,
or .F. — all declared variables initialize to empty string, not NIL.
NIL = NIL is .T., NIL = \"\" is .F." The code is legal and merely always
false, so this is an info-level teaching diagnostic rather than a
warning. Introduced in the 2026-03-21 alignment pass (history).
