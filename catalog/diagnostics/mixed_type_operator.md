---
id: diag.mixed_type_operator
title: Mixed operand types in a '+' operation
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
  - date: 2026-04-05
    ref: "commit de97e03"
    note: Introduced within the literal type-safety check.
  - date: 2026-04-30
    ref: "PR #3 (v0.4.0, commit d744511)"
    note: Stable diagnostic code assigned; behavior unchanged.
  - date: 2026-05-01
    ref: "commits d134334 / 2a74704"
    note: >-
      False positives from user code fixed by tightening inference:
      strict-case Hungarian prefix matching (uppercase-leading acronyms are
      untyped), indexed access and member access are opaque, and the
      operator scan became paren-aware so operators inside call arguments
      are not read as the outer expression's operator.
issues: []
---

## Behavior

Flags a `+` operator whose two operands infer to different types — `+` in
SSL is overloaded for string concatenation and arithmetic, but both
operands must be the same type (both strings or both numeric) or the
operation errors at runtime. Warning severity, ranged on the operator
token. Operand types come from conservative local inference: literals,
Hungarian-prefix names (strict lowercase prefix, e.g. `sName` is string,
`nCount` is numeric), tracked assignments, and known function return
types.

The inference is deliberately conservative; the rule stays silent whenever
either operand's type is unknown. In particular it must NOT flag:

- operands with matching types (`sFirst + sLast`, `nBase + 1`);
- identifiers whose leading capital letter merely resembles a Hungarian
  prefix (`DCUparseCat` is an acronym, not a `d`-prefixed date) —
  prefix matching is strictly lowercase;
- indexed access (`aCols[nX]`) — the element type of an array is unknown;
- member access (`Me:Foo`, `oObj:Bar`) — member types are unknown;
- expressions where the mismatched operator sits inside a call argument
  (`SubStr(sText, 1, Len(sText) - 1)`) rather than at the top level;
- mismatches under `-`, `*`, `/` — those report as
  `arithmetic_type_mismatch`, and NIL operands report as
  `nil_in_operations`.

## Examples

### Flags

```ssl
:DECLARE sMessage;
sMessage := "Total: " + 5;
```

### Flags

```ssl
:DECLARE sLabel, nCount;
sLabel := sLabel + nCount;
```

### Does not flag

```ssl
:DECLARE DCUparseCat, parsingScript;
DCUparseCat := "category";
parsingScript := DCUparseCat + "." + "leaf";
```

### Does not flag

```ssl
:PROCEDURE BuildCols;
	:PARAMETERS aCols;
	:DECLARE sCols, X;
	sCols := "";
	:FOR X := 1 :TO 5;
		sCols := sCols + aCols[X] + " end";
	:NEXT;
:ENDPROC;
```

### Does not flag

```ssl
:CLASS Box;
:PROCEDURE Build;
	Me:Foo := Me:Bar + " z";
:ENDPROC;
```

## Rationale

A mixed-type `+` is a genuine runtime error in SSL, but static typing in a
loosely-typed language is guesswork — this rule's history is a catalogue of
inference overreach trimmed back one false positive at a time (d134334,
2a74704: acronym names, array elements, class members, operators inside
call arguments). Each Does-not-flag fence pins one of those fixes; the
matching regression tests live in providers_test.go
(TestGetDiagnostics_MixedTypes_*). Warning rather than error because the
inference is heuristic even after tightening.
