---
id: diag.redeclare_is_noop
title: Variable re-declared in the same scope
kind: diagnostic
status: active
authority: authoritative
schema_ref: lints.variable_behavior.redeclare_is_noop
default_severity: hint
config:
  - ssl.diagnostics.rules
severity_overridable: true
suppressible: true
tests:
  - internal/providers/providers_test.go
history:
  - date: 2026-03-28
    ref: "commit be7a174"
    note: >-
      Introduced (checkRedeclaredVariables) in the diagnostics expansion
      that followed the style-guide alignment pass.
  - date: 2026-04-30
    ref: "PR #3 (v0.4.0)"
    note: Stable diagnostic code assigned when Code was populated on every diagnostic.
issues: []
---

## Behavior

Flags an identifier in a `:DECLARE` or `:PARAMETERS` statement whose name
(case-insensitive) was already introduced by an earlier `:DECLARE` or
`:PARAMETERS` in the same scope, at the repeated identifier's range, with
the first declaration's line in the message. Scopes are the file's
top-level script body plus one scope per `:PROCEDURE`...`:ENDPROC`;
`:PARAMETERS` names count as declarations, so `:DECLARE`-ing a parameter
again in the same procedure also fires.

It must NOT flag:

- the first declaration of a name, or distinct names in one `:DECLARE`
  list;
- the same name declared in *different* scopes — two procedures may each
  declare `nCount`, and a procedure may re-use a name declared at script
  level (procedure scopes start empty);
- assignments to an already-declared variable — only identifiers inside
  `:DECLARE`/`:PARAMETERS` statements are examined.

## Examples

### Flags

```ssl
:DECLARE nCount;
:DECLARE nCount;
```

### Flags

```ssl
:PROCEDURE Demo;
:PARAMETERS nCount;
:DECLARE nTotal, nCount;
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE First;
:DECLARE nCount;
:ENDPROC;
:PROCEDURE Second;
:DECLARE nCount;
:ENDPROC;
```

### Does not flag

```ssl
:DECLARE nCount;
nCount := 1;
nCount := 2;
```

## Rationale

The schema records `redeclare_is_noop` under `lints.variable_behavior`
(`level: authoritative`): "Re-declaring an existing variable with
:DECLARE is silently ignored — no error, existing value preserved." The
runtime accepts the code and preserves the existing value, so nothing is
broken — but the author probably believed they were getting a fresh
variable. A hint marks the spot without shouting, matching the
silently-ignored runtime semantics. Introduced 2026-03-28 (history).
