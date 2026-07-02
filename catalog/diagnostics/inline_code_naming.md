---
id: diag.inline_code_naming
title: ":BEGININLINECODE requires a name"
kind: diagnostic
status: active
authority: tool
schema_ref: null
default_severity: error
severity_overridable: true
suppressible: true
tests:
  - internal/providers/providers_test.go
history:
  - date: 2026-03-21
    ref: "commit cdbfee6 (v0.3.0)"
    note: >-
      Introduced during full alignment with ssl-style-guide, which requires
      inline-code blocks to be named with a bare or quoted identifier
      (schema keywords.inline_code.identifier_required).
  - date: 2026-04-30
    ref: "PR #3 (v0.4.0), commit d744511"
    note: Stable code inline_code_naming assigned.
issues: []
---

## Behavior

Always-on check: flags a `:BEGININLINECODE` keyword whose next significant
token is not a name, in two forms:

- **Missing name** (range on the keyword): the next significant token is
  `;` or the file ends — the block has no name at all.
- **Wrong token type** (range on the offending token): the next significant
  token is neither an identifier nor a quoted string (e.g. a number).

Comments and whitespace between the keyword and the name are skipped.

It must NOT flag when the block is named with a bare identifier
(`:BEGININLINECODE MyBlock;`) or a quoted string
(`:BEGININLINECODE "MyBlock";`) — the two forms the style guide allows.

## Examples

### Flags

```ssl
:BEGININLINECODE;
:ENDINLINECODE;
```

### Flags

```ssl
:BEGININLINECODE 123;
:ENDINLINECODE;
```

### Does not flag

```ssl
:BEGININLINECODE MyBlock;
:ENDINLINECODE;
```

### Does not flag

```ssl
:BEGININLINECODE "MyBlock";
:ENDINLINECODE;
```

## Rationale

The style guide schema marks the name as required
(`keywords.inline_code.identifier_required: true`, forms `bare_identifier`
and `quoted_identifier`); an unnamed block is invalid, not merely unstyled,
hence error severity. Following the catalog convention, `schema_ref` stays
null because the schema encodes this under `keywords`, not as a `lints`
rule slug. Introduced in cdbfee6 with the style-guide alignment pass.
