---
id: diag.udobject_array_in_clause
title: Udobject array in clause
kind: diagnostic
status: draft
authority: tool
schema_ref: null
default_severity: warning
severity_overridable: true
suppressible: true
tests:
  - internal/providers/providers_test.go
history:
  - date: 2026-04-03
    ref: "ssl-style-guide 635ff9f; LSP de07b4e"
    note: >-
      Runtime discovery: expanding ?obj:prop? inside a SQL IN clause raises
      "The current array has more than 1 dimmension." — the value must be
      copied to a local first. Documented in the style guide and enforced
      here.
  - date: 2026-05-01
    ref: "vs-code-ssl-formatter PR #58"
    note: Extension quick-fix keyed on this slug (copy-to-local rewrite).
issues: []
---

## Behavior

TODO: normative statement — what this rule flags, and the boundaries of what
it must not flag.

## Examples

### Flags

```ssl
/* TODO: minimal SSL that must produce udobject_array_in_clause; */
```

### Does not flag

```ssl
/* TODO: nearby-but-valid SSL that must NOT produce udobject_array_in_clause; */
```

## Rationale

TODO: why this behavior and this severity; cite history refs.
