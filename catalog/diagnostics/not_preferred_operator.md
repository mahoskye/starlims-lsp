---
id: diag.not_preferred_operator
title: Not preferred operator
kind: diagnostic
status: draft
authority: tool
schema_ref: null
default_severity: info
severity_overridable: true
suppressible: true
tests:
  - internal/providers/providers_test.go
history:
  - date: 2026-05-01
    ref: "PR #3 (v0.4.0); schema 'not preferred operators'"
    note: Schema-backed preference for != over <> and #.
  - date: 2026-05-01
    ref: "vs-code-ssl-formatter PR #58"
    note: Extension quick-fix keyed on this slug rewrites <>/# to !=.
issues: []
---

## Behavior

TODO: normative statement — what this rule flags, and the boundaries of what
it must not flag.

## Examples

### Flags

```ssl
/* TODO: minimal SSL that must produce not_preferred_operator; */
```

### Does not flag

```ssl
/* TODO: nearby-but-valid SSL that must NOT produce not_preferred_operator; */
```

## Rationale

TODO: why this behavior and this severity; cite history refs.
