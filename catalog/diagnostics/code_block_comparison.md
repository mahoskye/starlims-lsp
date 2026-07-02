---
id: diag.code_block_comparison
title: Code block comparison
kind: diagnostic
status: draft
authority: advisory
schema_ref: lints.type_safety.code_block_comparison
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
/* TODO: minimal SSL that must produce code_block_comparison; */
```

### Does not flag

```ssl
/* TODO: nearby-but-valid SSL that must NOT produce code_block_comparison; */
```

## Rationale

TODO: why this behavior and this severity; cite history refs.
