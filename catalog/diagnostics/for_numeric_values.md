---
id: diag.for_numeric_values
title: ":FOR loop parts must be numeric"
kind: diagnostic
status: active
authority: authoritative
schema_ref: lints.type_safety.for_numeric_values
default_severity: warning
severity_overridable: true
suppressible: true
tests:
  - internal/providers/providers_test.go
  - internal/providers/expr_operator_checks_test.go
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
  - date: 2026-10-13
    ref: "issue #255"
    note: >-
      Ported to the expression AST: the :FOR header's own trees (target,
      start, limit, step), so values are whole expressions. authority
      corrected to authoritative: the style-guide record
      for_numeric_values (level authoritative) names this rule in
      enforced_by, so the guide is not silent. The stale 'schema
      warnings map to style_only' sentence is removed.
issues: []
---

## Behavior

For each `:FOR` header the expression AST parses, fires a warning on
every part whose type the tree inference can prove non-numeric: the
start expression (after `:=`), the limit (after `:TO`), and the step
(after `:STEP`) — each a whole expression — and the loop variable when
its Hungarian prefix promises a non-number and the start value is not a
number. One diagnostic per offending part, ranged over that part. Types
come from `expr_types.go`: literals, operator results, builtin return
types, and Hungarian prefixes on bare identifiers in their documented
shape; an operand that reads a `:` member makes no claim. A
string-prefixed loop variable started from a number is treated as
numeric and does not flag.

It must NOT flag:

- parts whose type is unknown (bare names with no inferable type stay
  silent — the rule prefers false negatives);
- fully numeric loops, including numeric variables and expressions in any
  of the four positions;
- loop-counter names (`i`, `j`, `k`, `x`, `y`, `z`) and UPPER_SNAKE
  constants, which carry no type claim;
- `:FOR` headers the expression parser cannot read (the malformed-statement
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

The reference (keywords/FOR) requires the start, limit, and step to be
numeric at runtime, and the style-guide record for_numeric_values
records a non-numeric value as a runtime error. Because the evidence
includes Hungarian prefixes, the rule warns rather than errors, and the
unknown-type Does-not-flag fence pins the conservatism that keeps it
quiet on untyped variables. 2a74704 (history) fixed a false-positive
class — operators inside nested sub-expressions — that the AST port
(issue #255) removes by construction: each part is a parsed expression.
