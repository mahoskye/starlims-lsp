---
id: diag.default_on_declare_line
title: ":DEFAULT on the same line as :DECLARE"
kind: diagnostic
status: active
authority: tool
schema_ref: null
default_severity: error
severity_overridable: true
suppressible: true
tests:
  - internal/providers/providers_test.go
history:
  - date: 2026-01-14
    ref: "commit 567b287"
    note: >-
      Introduced with the declare/default usage checks: :DEFAULT pairs with
      :PARAMETERS, not :DECLARE (ssl_agent_instructions.md gotcha #3).
  - date: 2026-04-30
    ref: "PR #3 (v0.4.0, commit d744511)"
    note: Stable diagnostic code assigned; behavior unchanged.
issues: []
---

## Behavior

Flags a `:DECLARE` statement when a `:DEFAULT` keyword appears later on the
same source line (in non-data-source files; data source files route to their
own `:DEFAULT` rules instead). `:DEFAULT` provides defaults for
`:PARAMETERS` variables — combining it with `:DECLARE` on one line is the
signature of trying to default a declared local. The range covers the
`:DECLARE` keyword.

It must NOT flag:

- `:DECLARE` and `:DEFAULT` on separate lines;
- `:DEFAULT` used with `:PARAMETERS` (the correct pairing), with or without
  a `:DECLARE` elsewhere in the file;
- data source files, where this check does not run.

## Examples

### Flags

```ssl
:PROCEDURE Demo;
	:DECLARE sName; :DEFAULT sName, "";
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE Demo;
	:PARAMETERS sName;
	:DEFAULT sName, "";
	:DECLARE sLocal;
:ENDPROC;
```

## Rationale

Defaulting a `:DECLARE`d local is a language constraint violation, not a
style preference, so this is an error (567b287). The rule is line-based by
design and owns only the one-liner shape: a `:DEFAULT` targeting a declared
variable on a *different* line is instead caught by the placement rule
`default_after_parameters` (a `:DEFAULT` not immediately following
`:PARAMETERS`), so between the two rules no misuse escapes while neither
needs cross-line data-flow guessing.
