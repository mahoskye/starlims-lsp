---
id: diag.code_block_comparison
title: Code blocks compared with = or ==
kind: diagnostic
status: active
authority: authoritative
schema_ref: lints.type_safety.code_block_comparison
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
  - date: 2026-10-13
    ref: "issue #255"
    note: >-
      Ported to the expression AST and widened to `!=`, `<>` and `#`:
      the reference (types/codeblock, operators/not-equals) makes every
      equality comparison of a code block a runtime error. authority
      corrected to authoritative: the style-guide record
      code_block_comparison (level authoritative) names this rule in
      enforced_by, so the guide is not silent. Assignment tracking no
      longer feeds it; the fn prefix and literal shapes do.
issues: []
---

## Behavior

Fires a warning on an equality comparison — `=`, `==`, `!=`, `<>`, or
`#` — when either operand is known to be a code block: a `{|...| ...}`
literal, a name with the `fn` Hungarian prefix in its documented shape
(`fnCallback`), or any expression the tree inference types as a code
block. Operands are whole subtrees of the expression AST. The range
covers the operator token, and the message names the operator.

Comparing a code block with `NIL` flags too: the code-block operand
raises before any comparison happens.

It must NOT flag:

- operands whose type is unknown (unknown types stay silent);
- an operand that reads a `:` member — a member's prefix says nothing
  reliable about the value;
- comparisons between non-codeblock types (those may hit other rules such
  as `equals_vs_strict_equals`, never this one);
- `:=` assignment of a code-block literal.

## Examples

### Flags

```ssl
:DECLARE fnCallback, fnOther;
fnCallback := {|x| x + 1};
fnOther := {|x| x + 2};
:IF fnCallback == fnOther;
	nMatched := 1;
:ENDIF;
```

### Flags

Inequality negates `==`, so it raises the same way.

```ssl
:PROCEDURE Demo;
:PARAMETERS fnLeft, fnRight;
:DECLARE bSame;
bSame := fnLeft != fnRight;
bSame := fnLeft == NIL;
:ENDPROC;
```

### Does not flag

```ssl
:DECLARE nLeft, nRight;
nLeft := 1;
nRight := 2;
:IF nLeft == nRight;
	nLeft := 3;
:ENDIF;
```

### Does not flag

```ssl
:DECLARE fnCallback;
fnCallback := {|x| x + 1};
```

## Rationale

The reference (types/codeblock, operators/equals, strict-equals,
not-equals) documents that every equality comparison with a code block
raises a runtime error — `!=` included, since it negates `==`. The rule
originally covered `=` and `==` only; issue #255 widened it and moved it
onto the expression AST. Warning rather than error because the `fn`
prefix is a naming convention, not a runtime fact; unknown operands stay
silent.
