---
id: diag.code_block_structure
title: Code block literal without a bound variable
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
      Introduced during the diagnostics expansion; sourced from
      ssl-ebnf-grammar.md, which requires at least one parameter between the
      pipes of a code block literal.
  - date: 2026-04-30
    ref: "PR #3 (v0.4.0, commit d744511)"
    note: Stable diagnostic code assigned; behavior unchanged.
issues: []
---

## Behavior

Flags a code block literal `{|params| expr}` whose parameter list between
the pipes is empty — `{|| expr}`, including whitespace-only variants such as
`{| | expr}` or a tab between the pipes. Per the SSL grammar, code blocks
require at least one bound variable. The range covers the whole code block
token.

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
requires a bound variable between the pipes; `{|| ...}` is malformed.
Warning rather than error because the check is lexer-shape-based and the
style guide itself is silent (`authority: tool`).
