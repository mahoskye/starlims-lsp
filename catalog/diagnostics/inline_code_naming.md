---
id: diag.inline_code_naming
title: Inline code naming
kind: diagnostic
status: draft
authority: tool
schema_ref: null
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
/* TODO: minimal SSL that must produce inline_code_naming; */
```

### Does not flag

```ssl
/* TODO: nearby-but-valid SSL that must NOT produce inline_code_naming; */
```

## Rationale

TODO: why this behavior and this severity; cite history refs.
