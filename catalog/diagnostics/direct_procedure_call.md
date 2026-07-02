---
id: diag.direct_procedure_call
title: Direct procedure call
kind: diagnostic
status: draft
authority: tool
schema_ref: null
default_severity: error
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
/* TODO: minimal SSL that must produce direct_procedure_call; */
```

### Does not flag

```ssl
/* TODO: nearby-but-valid SSL that must NOT produce direct_procedure_call; */
```

## Rationale

TODO: why this behavior and this severity; cite history refs.
