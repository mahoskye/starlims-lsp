---
id: diag.mixed_type_operator
title: Mixed operand types in a '+' operation
kind: diagnostic
status: active
authority: authoritative
schema_ref: null
default_severity: warning
severity_overridable: true
suppressible: true
tests:
  - internal/providers/providers_test.go
  - internal/providers/diagnostics_audit_test.go
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
  - date: 2026-10-13
    ref: "issue #255"
    note: >-
      Moved onto the expression tree. Operands are the operator's actual
      operands rather than the adjacent tokens; the file-wide assignment
      tracking that typed a name from its last assignment anywhere in the
      file (another procedure included) is gone; constant-style and
      loop-counter names no longer read as numeric; date + number, the
      one supported mixed pairing, is no longer flagged. On the
      production corpus 3,723 reports became 108, the bulk of the drop
      being constant-style names (3,554) that had been typed numeric.
      Authority corrected tool -> authoritative to mirror the style guide
      record (explicit_type_conversion).
issues: []
---

## Behavior

Flags a `+` operator whose two operands infer to different types — `+` in
SSL adds two numbers, concatenates two strings, or offsets a date by a
number of days, and any other pairing errors at runtime. Warning
severity, ranged on the operator token. The operands are the operator's
operands in the expression tree, so precedence and grouping are honored
(`nA * 2 + sB` compares the product with `sB`). Operand types come from
conservative local evidence: literals, strict Hungarian prefixes on bare
identifiers (a lowercase prefix followed by an uppercase letter, e.g.
`sName` is string, `nCount` is numeric), documented builtin return
types, and the result of a nested operation whose operands are known. A
`+` chain that has a string operand is a string (`oRec:Name + " - "`),
since the only `+` that accepts a string yields one. Nothing is inferred
from assignments: a value's type at one point of a file says nothing
about another.

The inference is deliberately conservative; the rule stays silent whenever
either operand's type is unknown. In particular it must NOT flag:

- operands with matching types (`sFirst + sLast`, `nBase + 1`);
- `date + number` (`dDue + 7`) — the one supported mixed pairing. The
  reverse, `number + date`, is not supported and is flagged;
- identifiers whose leading capital letter merely resembles a Hungarian
  prefix (`DCUparseCat` is an acronym, not a `d`-prefixed date) —
  prefix matching is strictly lowercase;
- loop-counter names (`i`, `j`, `k`, `x`, `y`, `z`) and constant-style
  names (`MAX_ROWS`) — exempt from the prefix convention, so they carry
  no type;
- a name typed only by an assignment elsewhere in the file — including
  one in another procedure;
- indexed access (`aCols[nX]`) — the element type of an array is unknown;
- member access (`Me:Foo`, `oObj:Bar`) — member types are unknown;
- expressions where the mismatched operator sits inside a call argument
  (`SubStr(sText, 1, Len(sText) - 1)`) rather than at the top level;
- mismatches under `-`, `*`, `/` — those report as
  `arithmetic_type_mismatch`, and NIL operands report as
  `nil_in_operations`.

A chain such as `"a" + nCount + "b"` reports once, on the `+` that
introduces the mismatched operand.

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

### Does not flag

```ssl
:PROCEDURE Label;
	:PARAMETERS vValue;
	:RETURN vValue + " units";
:ENDPROC;

:PROCEDURE Reset;
	:DECLARE vValue;
	vValue := 0;
:ENDPROC;
```

### Does not flag

```ssl
:DECLARE dDue, nDays;
dDue := dDue + nDays;
```

### Does not flag

```ssl
:DECLARE sLabel;
sLabel := "Limit: " + MAX_ROWS;
```

### Flags

```ssl
:DECLARE sLabel, nCount;
sLabel := nCount * 2 + sLabel;
```

### Flags

```ssl
:DECLARE sLabel, nCount;
sLabel := oRec:Name + " - " + nCount;
```

## Rationale

A mixed-type `+` is a genuine runtime error in SSL, but static typing in a
loosely-typed language is guesswork — this rule's history is a catalogue of
inference overreach trimmed back one false positive at a time (d134334,
2a74704: acronym names, array elements, class members, operators inside
call arguments; issue #255: assignments elsewhere in the file,
constant-style names, `date + number`). Each Does-not-flag fence pins
one of those fixes; the matching regression tests live in
providers_test.go (TestGetDiagnostics_MixedTypes_*). The style guide's
`explicit_type_conversion` record (authoritative, warning) is the rule
being enforced. Warning rather than error because Hungarian prefixes are
evidence of intent, not of the runtime value — the inference is
heuristic even after tightening.
