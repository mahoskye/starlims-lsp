---
id: diag.parameters_first
title: PARAMETERS not first in its scope
kind: diagnostic
status: active
authority: authoritative
schema_ref: lints.coding_standards.parameters_first
default_severity: error
severity_overridable: true
suppressible: true
tests:
  - internal/providers/providers_test.go
history:
  - date: 2026-03-21
    ref: "commit cdbfee6"
    note: >-
      Introduced in the full alignment pass with ssl-style-guide
      (checkParameterPlacement), covering both procedure-level and
      script-level placement.
  - date: 2026-04-30
    ref: "PR #3 (v0.4.0, commit d744511)"
    note: >-
      Stable diagnostic code assigned; this slug is the canonical example in
      the rule-override and suppression test suites.
  - date: 2026-08-12
    ref: "issue #170"
    note: >-
      Shared false positive fixed alongside diag.default_after_parameters:
      a comment token mid-statement reset the statement tracker, so a
      parameter after an inline comment in a multi-line :PARAMETERS list
      registered as a body/top-level statement. Comments no longer touch
      statement tracking — only `;` ends a statement.
issues: []
---

## Behavior

Enforces `:PARAMETERS` placement at two levels, one emit site each, both
`error` severity with the range on the `:PARAMETERS` keyword:

- **Procedure level**: a `:PARAMETERS` inside a `:PROCEDURE` body flags
  unless it is the first statement after `:PROCEDURE`. Any earlier
  statement in the body — including a `:DECLARE` or a previous
  `:PARAMETERS` — disqualifies it (message: `':PARAMETERS' must appear
  immediately after ':PROCEDURE'`).
- **Script level**: a top-level `:PARAMETERS` flags once any top-level
  executable statement has been seen. Leading `:PROCEDURE ... :ENDPROC`
  blocks do not count as top-level statements, so a script may define
  procedures first and still declare its parameters after them (message:
  `Script-level ':PARAMETERS' must appear before top-level statements
  (leading ':PROCEDURE' blocks are allowed)`).

It must NOT flag:

- `:PARAMETERS` immediately after `:PROCEDURE`, or as the first top-level
  statement of a script;
- `:PARAMETERS` separated from `:PROCEDURE` only by comments — comments are
  structurally transparent and do not break adjacency;
- statements following a multi-line `:PARAMETERS` list with inline
  comments — a comment inside a statement does not end it, so the later
  parameters are not statements of their own (issue #170);
- anything in data-source files (`IsDataSourceFile`): the whole check is
  skipped there, since data-source scripts have their own statement rules.

## Examples

### Flags

```ssl
:PROCEDURE Demo;
:DECLARE nCount;
:PARAMETERS nValue;
:ENDPROC;
```

### Flags

```ssl
:DECLARE nCount;
nCount := 1;
:PARAMETERS nValue;
```

### Does not flag

```ssl
:PROCEDURE Demo;
:PARAMETERS nValue;
:DECLARE nCount;
nCount := nValue;
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE Demo;
/* explains the parameters below;
:PARAMETERS nValue;
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE Demo;
:PARAMETERS uP0, /* dsName;
 uP1, /* filter;
 uP2;
:DECLARE nCount;
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE Helper;
:RETURN .T.;
:ENDPROC;
:PARAMETERS nValue;
:DECLARE nCount;
nCount := nValue;
```

## Rationale

The schema rule `lints.coding_standards.parameters_first` is declared with
severity `error` (`:PARAMETERS must appear before any other statements in a
script or procedure body`), so this entry is authoritative and keeps error
severity. The comment-transparency and leading-procedure allowances encode
real SSL layout patterns (doc-header comments between `:PROCEDURE` and
`:PARAMETERS`; scripts that define helpers before their parameter block)
that a naive "first statement" check would false-positive on.
