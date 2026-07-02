---
id: diag.no_default_statements_in_datasource
title: No default statements in datasource
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
/* TODO: minimal SSL that must produce no_default_statements_in_datasource; */
```

### Does not flag

```ssl
/* TODO: nearby-but-valid SSL that must NOT produce no_default_statements_in_datasource; */
```

## Rationale

TODO: why this behavior and this severity; cite history refs.
