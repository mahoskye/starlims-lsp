---
id: diag.class_member_order
title: Class member order
kind: diagnostic
status: draft
authority: advisory
schema_ref: lints.class_rules.class_member_order
default_severity: info
severity_overridable: true
suppressible: true
tests:
  - internal/providers/providers_test.go
history: []
issues: []
---

## Behavior

TODO: normative statement — what this rule flags, and the boundaries of what
it must not flag.

## Examples

### Flags

```ssl
/* TODO: minimal SSL that must produce class_member_order; */
```

### Does not flag

```ssl
/* TODO: nearby-but-valid SSL that must NOT produce class_member_order; */
```

## Rationale

TODO: why this behavior and this severity; cite history refs.
