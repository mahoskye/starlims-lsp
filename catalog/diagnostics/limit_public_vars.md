---
id: diag.limit_public_vars
title: PUBLIC variables discouraged
kind: diagnostic
status: active
authority: style_only
schema_ref: lints.coding_standards.limit_public_vars
default_severity: warning
config:
  - ssl.diagnostics.rules
severity_overridable: true
suppressible: true
tests:
  - internal/providers/providers_test.go
history:
  - date: 2026-03-21
    ref: "commit cdbfee6"
    note: >-
      Introduced (checkPublicVariables) in the style-guide alignment pass
      that added schema-backed rule enforcement.
  - date: 2026-04-30
    ref: "PR #3 (v0.4.0)"
    note: Stable diagnostic code assigned when Code was populated on every diagnostic.
issues: []
---

## Behavior

Flags every `:PUBLIC` keyword token, unconditionally — one diagnostic per
`:PUBLIC` statement, at the keyword's range. Despite the "limit" in the
slug there is no threshold: the first `:PUBLIC` in a file already fires,
because the style guide treats public variables as risky shared state to
be avoided, not rationed.

It must NOT flag:

- `:DECLARE` or `:PARAMETERS` statements;
- reads or writes of a variable that happens to have been declared
  `:PUBLIC` — only the declaration keyword itself is flagged (assignment
  to configured globals is `global_assignment`'s business).

## Examples

### Flags

```ssl
:PUBLIC gShared;
```

### Does not flag

```ssl
:DECLARE nLocal;
nLocal := 1;
```

## Rationale

The schema lists `limit_public_vars` under `lints.coding_standards` with
`severity: warning` (style_only): ":PUBLIC variables persist across all
procedures and risk namespace pollution. Prefer :DECLARE with parameter
passing." Valid SSL — the code compiles and runs — hence a warning, not
an error, and per-occurrence so every shared-state declaration is visible
in review. Introduced in the 2026-03-21 alignment pass (history).
