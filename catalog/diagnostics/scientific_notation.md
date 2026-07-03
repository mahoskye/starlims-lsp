---
id: diag.scientific_notation
title: Scientific notation missing its decimal point
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
  - date: 2026-03-28
    ref: "commit be7a174"
    note: >-
      Introduced (checkScientificNotation), transcribing the schema's
      numbers rules (require_decimal_for_scientific, with 7e2 / .5e1 / 9E+1
      as its invalid examples).
  - date: 2026-04-30
    ref: "PR #3 (v0.4.0, commit d744511)"
    note: Stable diagnostic code assigned; rule behavior unchanged.
issues: ["#47"]
---

## Behavior

Flags number literals that look like scientific notation but are not valid
SSL numbers, in three token shapes (SSL requires a digit, a decimal point,
then the exponent — `7.0e2`, `1.2e-3`). Because the lexer cannot join these
malformed forms into one number token, they surface as a number token
immediately followed (no whitespace) by an identifier:

- **Missing decimal point**: a number without `.` directly followed by an
  identifier `e<digits>`, `e+…`, or `e-…` (e.g. `7e2` lexes as `7` + `e2`).
- **Missing decimal point, split exponent sign**: a number without `.`
  directly followed by a bare `e`/`E` identifier and then a `+`/`-`
  operator (e.g. `9E+1` lexes as `9` + `E` + `+` + `1`).
- **Missing leading digit**: a number starting with `.` directly followed
  by an `e…` identifier (e.g. `.5e1` lexes as `.5` + `e1`).

The range covers the number token; severity is warning for all three
shapes, and the message shows the corrected literal.

It must NOT flag:

- valid scientific notation such as `7.0e2`, `1.2e-3`, `9.0E1`;
- a number separated from a following identifier by whitespace or an
  operator — adjacency is the signal;
- ordinary identifiers that merely start with `e`.

## Examples

### Flags

```ssl
:PROCEDURE Demo;
:DECLARE nValue;
nValue := 7e2;
:ENDPROC;
```

### Flags

```ssl
:PROCEDURE Demo;
:DECLARE nValue;
nValue := 9E+1;
:ENDPROC;
```

### Flags

```ssl
:PROCEDURE Demo;
:DECLARE nValue;
nValue := .5e1;
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE Demo;
:DECLARE nValue, nSmall, eCount;
nValue := 7.0e2;
nSmall := 1.2e-3;
eCount := nValue + 1;
:ENDPROC;
```

## Rationale

The schema's numbers section (`require_decimal_for_scientific: true`) lists
exactly these invalid examples — `7e2`, `.5e1`, `9E+1` — but defines no
lints slug for them, so the rule is tool-authored. Warning (not error)
severity because the code still lexes and may even run with the exponent
part silently misread as an identifier — which is precisely why it deserves
a loud nudge: `7e2` is almost never an intentional variable reference.
Note the fix-it message for the `9E+1` shape suggests `9.0E+1`, although
the schema also says explicit `+` exponent signs are unsupported; the
schema-correct target is `90.0` or `9.0E1` (message-wording nit, tracked in
the review notes, not a behavioral gap).
