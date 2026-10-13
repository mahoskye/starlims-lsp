---
id: diag.arithmetic_type_mismatch
title: Mismatched operand types in arithmetic
kind: diagnostic
status: active
authority: authoritative
schema_ref: null
default_severity: warning
severity_overridable: true
suppressible: true
tests:
  - internal/providers/providers_test.go
  - internal/providers/expr_operator_checks_test.go
history:
  - date: 2026-03-21
    ref: "commit cdbfee6"
    note: >-
      Introduced in the full alignment pass with ssl-style-guide, as part
      of the literal-type-safety operator checks (checkLiteralTypeSafety).
  - date: 2026-04-30
    ref: "PR #3 (v0.4.0)"
    note: Stable diagnostic code assigned; rule behavior unchanged.
  - date: 2026-09-21
    ref: "issue #249"
    note: >-
      Fenced against unary signs. A leading negative in an array literal
      (`{-1}`) inferred its left operand from the literal's own opening
      brace and reported "array - numeric".
  - date: 2026-10-13
    ref: "issue #255"
    note: >-
      Ported to the expression AST. Operand pairs are judged against the
      operator type tables the reference documents (operators/minus,
      multiply, divide, modulo, power), so date - number and date - date
      no longer flag, and `%`, `^`, `**` and same-type invalid pairs
      (`.T. - .F.`) now do. The legacy name heuristic read loop-counter
      names and UPPER_SNAKE constants as numbers; tree inference leaves
      them unknown. Member reads make no claim. authority set to
      authoritative: the operand tables are documented language
      behavior.
issues:
  - 249
---

## Behavior

Flags a binary `-`, `*`, `/`, `%`, `^`, or `**` whose two operand types
are both known and form a pair the operator's documented type table does
not accept. The pair table is the reference's (operators/minus,
multiply, divide, modulo, power, double-star-power), taken from the
generated element inventory (`operatorResults`):

- `-` accepts number - number, string - string (trim-then-concatenate),
  date - number (a date that many days earlier), and date - date (the
  day difference);
- `*`, `/`, `%`, `^`, `**` accept number with number only — so
  same-type pairs such as `.T. - .F.` or `{1} * {2}` also flag.

Two message variants share this code, both at warning severity: one when
either operand is a string, one for other non-numeric pairs. The range
covers the operator token.

Operands are whole subtrees of the expression AST, so precedence and
parentheses are real: in `Len(sA) * "2"` the left operand is the call,
and in `(sA) - 1` it is the group. Types come from the tree inference in
`expr_types.go`: literals, operator results, builtin return types, and
Hungarian prefixes on bare identifiers in their documented shape (a
lowercase prefix followed by an uppercase letter).

It must NOT flag:

- `+` — mixed-type `+` is the separate rule `diag.mixed_type_operator`,
  because `+` legally overloads to string concatenation;
- documented pairs, including the date arithmetic above (`dDue - 1`,
  `dEnd - dStart`) and string - string;
- an operand whose type is unknown — a name without a typed prefix,
  loop-counter names (`i`, `j`, `k`, `x`, `y`, `z`), UPPER_SNAKE
  constants, user-procedure returns, array elements;
- an operand that reads a `:` member anywhere in its subtree — a
  member's prefix says nothing reliable about the value;
- `NIL` operands, which report as `diag.nil_in_operations`;
- a unary sign (`{-1}`, `n := -1`, `2 - -1`) — the tree parses it as a
  unary node, not as a binary operator.

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

### Flags

Operators the token scan never covered, and a same-type pair the
operator still rejects.

```ssl
:PROCEDURE Demo;
:DECLARE nRest, bFlag;
nRest := 10 % "3";
bFlag := .T. - .F.;
:ENDPROC;
```

### Flags

The left operand is the whole call, typed by its documented return.

```ssl
:PROCEDURE Demo;
:PARAMETERS sText;
:DECLARE nTotal;
nTotal := Len(sText) * "2";
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

### Does not flag

Date arithmetic is documented: a date minus days is a date, and a date
minus a date is the day difference.

```ssl
:PROCEDURE Demo;
:PARAMETERS dDue, dStart;
:DECLARE dReminder, nDays;
dReminder := dDue - 3;
nDays := dDue - dStart;
:ENDPROC;
```

### Does not flag

Loop counters and constants carry no type claim, and string - string is
the trim-then-concatenate form.

```ssl
:PROCEDURE Demo;
:PARAMETERS MAX_ROWS, sFirst, sSecond;
:DECLARE x, nLeft, sJoined;
x := DoProc("Measure");
nLeft := MAX_ROWS - x;
sJoined := sFirst - sSecond;
:ENDPROC;
```

### Does not flag

A unary sign, wherever it appears — after `{`, `(`, `,`, `:=`, or
another operator.

```ssl
:PROCEDURE Demo;
:DECLARE aFlags, nOffset;
aFlags := {-1, 2};
nOffset := -1;
:ENDPROC;
```

## Rationale

The reference documents the operand types each arithmetic operator
accepts, and an undocumented pair raises a runtime error. The check
reports at warning, not error, because type evidence includes Hungarian
prefixes — a naming convention, not a runtime fact — and the
never-flag guarantee for unknown operands is what keeps it quiet on
real code. The original token scan (commit cdbfee6) compared adjacent
tokens and typed loop-counter names and UPPER_SNAKE constants as
numbers, which reported valid date arithmetic and constants; issue #255
moved it onto the expression AST and the documented pair tables. `+` is
deliberately excluded and owned by `diag.mixed_type_operator` so its
message can explain the concatenation overload.
