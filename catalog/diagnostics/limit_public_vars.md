---
id: diag.limit_public_vars
title: Limit public vars
kind: diagnostic
status: draft
authority: advisory
schema_ref: lints.coding_standards.limit_public_vars
default_severity: warning
severity_overridable: true
suppressible: true
tests: []
history: []
issues: []
---

## Behavior

TODO: normative statement — what this rule flags, and the boundaries of what
it must not flag.

## Examples

### Flags

```ssl
/* TODO: minimal SSL that must produce limit_public_vars; */
```

### Does not flag

```ssl
/* TODO: nearby-but-valid SSL that must NOT produce limit_public_vars; */
```

## Rationale

TODO: why this behavior and this severity; cite history refs.
