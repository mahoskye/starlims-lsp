---
id: diag.dollar_string_only
title: "$ containment operator with non-string operands"
kind: diagnostic
status: active
authority: style_only
schema_ref: lints.type_safety.dollar_string_only
default_severity: warning
severity_overridable: true
suppressible: true
tests:
  - internal/providers/providers_test.go
history:
  - date: 2026-03-21
    ref: "commit cdbfee6"
    note: Introduced during the full style-guide alignment pass.
  - date: 2026-04-30
    ref: "PR #3 (v0.4.0)"
    note: Stable diagnostic code assigned when every diagnostic gained a Code.
  - date: 2026-05-01
    ref: "commit d134334"
    note: >-
      False-positive guard: operands that are indexed access (arr[i]) or
      member access (obj:prop) are never classified — element and member
      types are unknowable from names alone.
issues: []
---

## Behavior

Fires a warning on the `$` containment operator when either operand's
conservatively inferred type is known and is not `string`. Inference draws
from literals, tracked local `:=` assignments, strict Hungarian name
prefixes (`s`, `n`, `b`, `a`, `o`, `d`, `fn`), and built-in function
return types.

It must NOT flag:

- `$` between two string operands;
- operands whose type is unknown (bare names with no inferable prefix or
  tracked assignment stay silent);
- indexed-access (`arr[i]`) or member-access (`obj:prop`) operands, even
  when the element/member name carries a non-string Hungarian prefix —
  their runtime types are unknowable (commit d134334).

## Examples

### Flags

```ssl
:DECLARE nCount, sText;
nCount := 5;
sText := "abcdef";
:IF nCount $ sText;
	nCount := 1;
:ENDIF;
```

### Does not flag

```ssl
:DECLARE sNeedle, sHay;
sNeedle := "a";
sHay := "abc";
:IF sNeedle $ sHay;
	sNeedle := "b";
:ENDIF;
```

### Does not flag

```ssl
:DECLARE sHay, oRecord;
sHay := "abc";
:IF oRecord:nCode $ sHay;
	sHay := "b";
:ENDIF;
```

## Rationale

The schema rule (`lints.type_safety.dollar_string_only`, severity
`warning`) records that `$` only works on strings and non-string operands
cause a runtime error; the catalog maps schema warnings to `style_only`
authority and the emit site uses warning severity to match. The
member-access Does-not-flag fence pins the d134334 false-positive guard: a
member named `nCode` must not be presumed numeric just because of its
prefix.
