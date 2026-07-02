---
id: diag.code_block_comparison
title: Code blocks compared with = or ==
kind: diagnostic
status: active
authority: style_only
schema_ref: lints.type_safety.code_block_comparison
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
issues: []
---

## Behavior

Fires a warning on a `=` or `==` comparison when either immediate operand
is inferred to be a code block: a `{| ... |}` literal adjacent to the
operator, an identifier whose tracked local assignment was a code-block
expression, an identifier with the strict Hungarian `fn` prefix, or a call
to a built-in whose signature returns a code block.

It must NOT flag:

- operands whose type cannot be conservatively inferred (unknown types
  stay silent);
- comparisons between non-codeblock types (those may hit other rules such
  as `equals_vs_strict_equals`, never this one);
- `:=` assignment of a code-block literal — only the comparison operators
  `=` and `==` are checked (`!=` is likewise outside this rule's scope).

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

The schema rule (`lints.type_safety.code_block_comparison`, severity
`warning`) records that comparing code blocks with `=` or `==` causes a
runtime error in SSL. The catalog maps schema warnings to `style_only`
authority and the emit site uses warning severity to match. Detection
rests on conservative local type inference, so the rule prefers false
negatives (unknown operands stay silent) over false positives.
