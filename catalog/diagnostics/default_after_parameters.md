---
id: diag.default_after_parameters
title: Default after parameters
kind: diagnostic
status: draft
authority: advisory
schema_ref: lints.coding_standards.default_after_parameters
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
/* TODO: minimal SSL that must produce default_after_parameters; */
```

### Does not flag

```ssl
/* TODO: nearby-but-valid SSL that must NOT produce default_after_parameters; */
```

## Rationale

TODO: why this behavior and this severity; cite history refs.
