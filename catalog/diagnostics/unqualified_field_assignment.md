---
id: diag.unqualified_field_assignment
title: Unqualified field assignment
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
  - date: 2026-05-13
    ref: "starlims-ssl-reference #4/#5 -> PR #6; ssl-style-guide 386d57e"
    note: >-
      Runtime correction upstream: inside a class method a bare identifier
      resolves to a local, not the :DECLAREd field — unqualified assignment
      silently creates a local and leaves the field untouched.
  - date: 2026-05-14
    ref: "PR #23 (v0.7.7)"
    note: >-
      Rule added: flag bare assignment to a declared class field in a
      method; suppressed by a local shadow or Me:/Base: qualification.
issues: []
---

## Behavior

TODO: normative statement — what this rule flags, and the boundaries of what
it must not flag.

## Examples

### Flags

```ssl
/* TODO: minimal SSL that must produce unqualified_field_assignment; */
```

### Does not flag

```ssl
/* TODO: nearby-but-valid SSL that must NOT produce unqualified_field_assignment; */
```

## Rationale

TODO: why this behavior and this severity; cite history refs.
