---
id: diag.procedure_declaration_syntax
title: Procedure declaration syntax
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
/* TODO: minimal SSL that must produce procedure_declaration_syntax; */
```

### Does not flag

```ssl
/* TODO: nearby-but-valid SSL that must NOT produce procedure_declaration_syntax; */
```

## Rationale

TODO: why this behavior and this severity; cite history refs.
