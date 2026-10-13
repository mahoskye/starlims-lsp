---
id: diag.dollar_string_only
title: "$ containment operator with non-string operands"
kind: diagnostic
status: active
authority: authoritative
schema_ref: lints.type_safety.dollar_string_only
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
    ref: "commit d134334"
    note: >-
      False-positive guard: operands that are indexed access (arr[i]) or
      member access (obj:prop) are never classified — element and member
      types are unknowable from names alone.
  - date: 2026-10-13
    ref: "issue #255"
    note: >-
      Ported to the expression AST. The legacy heuristic typed loop-
      counter names and UPPER_SNAKE constants as numbers (`"A" $
      ALLOWED_CODES` flagged), and tracked assignments read the last
      token of an expression. Operands are now whole subtrees, so
      precedence is real: `.NOT. "a" $ s` is `(.NOT. "a") $ s` and
      flags. authority corrected to authoritative: the style-guide
      record dollar_string_only (level authoritative) names this rule in
      enforced_by, so the guide is not silent.
issues: []
---

## Behavior

Fires a warning on a binary `$` (containment) when either operand's type
is known and is not string. Operands are whole subtrees of the
expression AST, typed by the tree inference in `expr_types.go`:
literals, operator results, builtin return types, and Hungarian
prefixes on bare identifiers in their documented shape (`sName`,
`nCount`). The range covers the `$` token.

Precedence is the grammar's: unary operators bind tighter than `$`, so
`.NOT. "a" $ sList` is `(.NOT. "a") $ sList` — a boolean on the left —
and flags. The intended test is `.NOT. ("a" $ sList)`.

It must NOT flag:

- `$` between two string operands;
- an operand whose type is unknown — a name without a typed prefix,
  loop-counter names (`i`, `j`, `k`, `x`, `y`, `z`), UPPER_SNAKE
  constants, user-procedure returns, array elements (`arr[i]`);
- an operand that reads a `:` member (`obj:nCode`), even when the member
  name carries a non-string prefix — its runtime type is unknowable
  (commit d134334).

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

### Flags

`.NOT.` binds tighter than `$`, so the left operand is a boolean.

```ssl
:PROCEDURE Demo;
:PARAMETERS sList;
:IF .NOT. "a" $ sList;
	sList := "a" + sList;
:ENDIF;
:ENDPROC;
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

### Does not flag

Loop-counter names and constants make no type claim.

```ssl
:PROCEDURE Demo;
:PARAMETERS ALLOWED_CODES;
:DECLARE x, bHit;
x := DoProc("NextCode");
bHit := "A" $ ALLOWED_CODES .OR. "B" $ x;
:ENDPROC;
```

## Rationale

The reference (operators/dollar) documents `$` for two strings only; any
other operand raises a runtime error. Warning rather than error because
type evidence includes Hungarian prefixes, a naming convention. The
original token scan typed loop-counter names and UPPER_SNAKE constants
as numbers and so reported valid containment tests; issue #255 moved the
rule onto the expression AST, where unknown operands make no claim. The
member-access Does-not-flag fence pins the d134334 guard: a member named
`nCode` must not be presumed numeric just because of its prefix.
