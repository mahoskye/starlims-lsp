---
id: diag.for_numeric_values
title: ":FOR loop parts must be numeric"
kind: diagnostic
status: active
authority: style_only
schema_ref: lints.type_safety.for_numeric_values
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
    ref: "commit 2a74704"
    note: >-
      Expression inference made paren-aware: operators inside nested call
      arguments / index lookups no longer misclassify the outer expression
      (fixed false positives on values like Len(s) - 1 chains).
issues: []
---

## Behavior

For each complete `:FOR ... ;` statement, fires a warning on every part
whose type local inference can prove non-numeric: the loop variable (the
identifier after `:FOR`), the start expression (after `:=`), the limit
(after `:TO`), and the step (after `:STEP`) — one diagnostic per offending
part. Inference draws from literals, tracked local `:=` assignments,
strict Hungarian name prefixes, and built-in return types; compound
expressions are classified conservatively (paren-aware since 2a74704).
Tracked assignments take precedence over name prefixes, and the loop's own
start assignment counts: a string-prefixed variable initialized from a
numeric start value is treated as numeric and does not flag as loop
variable.

It must NOT flag:

- parts whose type is unknown (bare names with no inferable type stay
  silent — the rule prefers false negatives);
- fully numeric loops, including numeric variables and expressions in any
  of the four positions;
- `:FOR` statements without a terminating `;` (the malformed-statement
  case is left to the block/delimiter rules).

## Examples

### Flags

```ssl
:PROCEDURE Demo;
	:DECLARE nIdx;
	:FOR nIdx := 1 :TO "ten";
	:NEXT;
:ENDPROC;
```

### Flags

```ssl
:PROCEDURE Demo;
	:DECLARE sName, sStart;
	:FOR sName := sStart :TO 10;
	:NEXT;
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE Demo;
	:DECLARE nIdx, nMax;
	nMax := 10;
	:FOR nIdx := 1 :TO nMax :STEP 2;
	:NEXT;
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE Demo;
	:DECLARE vStart, vEnd, i;
	:FOR i := vStart :TO vEnd;
	:NEXT;
:ENDPROC;
```

## Rationale

The schema rule (`lints.type_safety.for_numeric_values`, severity
`warning`) records that non-numeric `:FOR` values cause a runtime error;
the catalog maps schema warnings to `style_only` authority and the emit
site uses warning severity to match. Because detection rests on
name-and-assignment inference, the unknown-type Does-not-flag fence pins
the conservatism that keeps this rule quiet on untyped variables, and
2a74704 (history) is the false-positive class — operators inside nested
sub-expressions — that must never return.
