---
id: diag.class_or_script
title: Class or script
kind: diagnostic
status: draft
authority: authoritative
schema_ref: lints.compile_errors.class_or_script
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
/* TODO: minimal SSL that must produce class_or_script; */
```

### Does not flag

```ssl
/* TODO: nearby-but-valid SSL that must NOT produce class_or_script; */
```

## Rationale

TODO: why this behavior and this severity; cite history refs.
