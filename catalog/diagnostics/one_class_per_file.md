---
id: diag.one_class_per_file
title: One class per file
kind: diagnostic
status: draft
authority: authoritative
schema_ref: lints.compile_errors.one_class_per_file
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
/* TODO: minimal SSL that must produce one_class_per_file; */
```

### Does not flag

```ssl
/* TODO: nearby-but-valid SSL that must NOT produce one_class_per_file; */
```

## Rationale

TODO: why this behavior and this severity; cite history refs.
