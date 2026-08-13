---
id: diag.default_after_parameters
title: ":DEFAULT must immediately follow :PARAMETERS"
kind: diagnostic
status: active
authority: authoritative
schema_ref: lints.coding_standards.default_after_parameters
default_severity: error
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
  - date: 2026-08-12
    ref: "issue #170"
    note: >-
      False positive fixed: a comment token mid-statement reset the
      statement tracker even though the enclosing statement's `;` had not
      been seen, so a multi-line :PARAMETERS list with inline comments
      "ended" at the first comment and the following :DEFAULT flagged.
      Comment transparency now means comments never touch statement
      tracking — only `;` ends a statement.
issues: []
---

## Behavior

Fires an error on each statement-initial `:DEFAULT` whose immediately
preceding statement is neither a `:PARAMETERS` statement nor another
`:DEFAULT`. Comments are structurally transparent — they do not break the
`:PARAMETERS` -> `:DEFAULT` sequence. The check is purely sequential over
the token stream (no per-procedure scoping) and does not run for data
source files, where `:DEFAULT` usage is governed by the datasource rules
(`no_default_statements_in_datasource` / `datasource_default_required`)
instead.

It must NOT flag:

- a `:DEFAULT` directly after `:PARAMETERS`;
- a run of several consecutive `:DEFAULT` statements after one
  `:PARAMETERS`;
- comments (including whole comment lines) between `:PARAMETERS` and
  `:DEFAULT`;
- a `:DEFAULT` after a multi-line `:PARAMETERS` list carrying inline
  comments — a comment inside a statement does not end it (issue #170).

## Examples

### Flags

```ssl
:PROCEDURE Demo;
:PARAMETERS nValue;
:DECLARE nOther;
:DEFAULT nValue, 1;
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE Demo;
:PARAMETERS nValue, sName;
:DEFAULT nValue, 1;
:DEFAULT sName, "abc";
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE Demo;
:PARAMETERS nValue;
/* fallback when the caller omits the argument;
:DEFAULT nValue, 1;
:ENDPROC;
```

### Does not flag

```ssl
:PARAMETERS uP0, /* dsName;
 uP1, /* filter;
 uP2;
:DEFAULT uP2, "";
:RETURN uP0;
```

## Rationale

The schema rule (`lints.coding_standards.default_after_parameters`,
severity `error`) states that `:DEFAULT` must immediately follow
`:PARAMETERS` — zero or more `:DEFAULT` lines, but only right after
`:PARAMETERS` — so the entry is authoritative with error severity to
match. The comment-transparency Does-not-flag fence pins the deliberate
decision that documenting a default must never break the sequence.
