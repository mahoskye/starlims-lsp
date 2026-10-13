---
id: diag.code_block_structure
title: Code block literal without a bound variable
kind: diagnostic
status: active
authority: authoritative
schema_ref: expressions.code_block_literals.min_parameters
default_severity: error
severity_overridable: true
suppressible: true
tests:
  - internal/providers/providers_test.go
history:
  - date: 2026-03-28
    ref: "commit be7a174"
    note: >-
      Introduced during the diagnostics expansion; sourced from
      ssl-ebnf-grammar.md, which requires at least one parameter between the
      pipes of a code block literal.
  - date: 2026-04-30
    ref: "PR #3 (v0.4.0, commit d744511)"
    note: Stable diagnostic code assigned; behavior unchanged.
  - date: 2026-10-13
    ref: "issue #255"
    note: >-
      Raised to error: the reference (special-forms/code-block) states
      `{|| ...}` is a compile-time error. The private corpus has zero
      findings, so no valid document changes. authority corrected to
      authoritative: the style-guide record code_block_structure (level
      authoritative) names this rule in enforced_by, so the guide is not
      silent.
issues: []
---

## Behavior

Flags a code block literal `{|params| expr}` whose parameter list between
the pipes is empty — `{|| expr}`, including whitespace-only variants such as
`{| | expr}` or a tab between the pipes. Per the SSL grammar, code blocks
require at least one bound variable, and `{|| expr}` does not compile, so
the finding is an error. The range covers the whole code block token.

It must NOT flag:

- code blocks with at least one bound variable (`{|x| x * 2}`);
- array literals and other brace constructs that the lexer does not tokenize
  as a code block (`{1, 2, 3}`, `{}`) — the check only inspects lexed
  code-block tokens, so ordinary braces never reach it.

## Examples

### Flags

```ssl
:PROCEDURE Demo;
	:DECLARE fnGet;
	fnGet := {|| 42};
:ENDPROC;
```

### Flags

```ssl
:PROCEDURE Demo;
	:DECLARE fnGet;
	fnGet := {| | 42};
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE Demo;
	:DECLARE fnDouble;
	fnDouble := {|x| x * 2};
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE Demo;
	:DECLARE aList;
	aList := {1, 2, 3};
:ENDPROC;
```

## Rationale

The SSL grammar (ssl-ebnf-grammar.md, cited at the be7a174 introduction)
requires a bound variable between the pipes, and the reference
(special-forms/code-block) states that `{|| ...}` is a compile-time
error. The rule shipped at warning while the style guide was silent;
issue #255 raised it to error, the severity for a compile error, after
the private corpus showed zero findings — no document that validated
before fails now. The lexer captures a code block as one token, so the
shape test is exact.
