---
id: diag.exitfor_outside_loop
title: Exitfor outside loop
kind: diagnostic
status: draft
authority: authoritative
schema_ref: lints.compile_errors.exitfor_outside_loop
default_severity: error
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
/* TODO: minimal SSL that must produce exitfor_outside_loop; */
```

### Does not flag

```ssl
/* TODO: nearby-but-valid SSL that must NOT produce exitfor_outside_loop; */
```

## Rationale

TODO: why this behavior and this severity; cite history refs.
