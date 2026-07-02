---
id: diag.exitfor_in_finally
title: Exitfor in finally
kind: diagnostic
status: draft
authority: authoritative
schema_ref: lints.compile_errors.exitfor_in_finally
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
/* TODO: minimal SSL that must produce exitfor_in_finally; */
```

### Does not flag

```ssl
/* TODO: nearby-but-valid SSL that must NOT produce exitfor_in_finally; */
```

## Rationale

TODO: why this behavior and this severity; cite history refs.
