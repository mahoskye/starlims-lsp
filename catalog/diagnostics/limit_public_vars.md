---
id: diag.limit_public_vars
title: PUBLIC variables discouraged
kind: diagnostic
status: active
authority: advisory
schema_ref: lints.coding_standards.limit_public_vars
default_severity: info
config:
  - ssl.diagnostics.infoDiagnostics
severity_overridable: true
suppressible: true
spec_options:
  include_info_diagnostics: true
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
  - date: 2026-08-27
    ref: "issue #208 discussion (info-tier expansion)"
    note: >-
      Moved warning -> info in the info-tier expansion: a stance on
      shared state fired unconditionally — advisory, not actionable at
      each site. Info is the opt-in advisory tier
      (ssl.diagnostics.infoDiagnostics); explicit ssl.diagnostics.rules
      entries still promote or disable per rule.
  - date: 2026-10-13
    ref: "issue #255"
    note: >-
      authority corrected to advisory: the style-guide record
      limit_public_vars (level advisory) names this rule in enforced_by,
      so the guide is not silent. Rationale corrected: the severity is
      info by D13 (advisory tier), where it said warning; the record's
      warning is a deliberate difference.
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

The schema lists `limit_public_vars` under `lints.coding_standards`
(style-guide record limit_public_vars, advisory, severity warning):
":PUBLIC variables persist across all procedures and risk namespace
pollution. Prefer :DECLARE with parameter passing." Valid SSL — the code
compiles and runs — so it is advisory, and per-occurrence so every
shared-state declaration is visible in review. The severity is info, not
the record's warning: DECISIONS.md D13 moved this rule into the opt-in
advisory tier (PR #213), a deliberate difference from the record. Introduced in the 2026-03-21 alignment pass (history).
