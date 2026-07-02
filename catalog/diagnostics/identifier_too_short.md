---
id: diag.identifier_too_short
title: Identifier too short
kind: diagnostic
status: draft
authority: tool
schema_ref: null
default_severity: warning
severity_overridable: true
suppressible: true
tests: []
history:
  - date: 2026-04-30
    ref: "PR #3 (v0.4.0), commit d744511"
    note: >-
      Constant CodeIdentifierTooShort defined alongside
      CodeIdentifierTooLong, but no check was ever wired to it.
issues: []
---

## Behavior

NOT PROMOTABLE — dead code. `identifier_too_short` has a code constant
(`CodeIdentifierTooShort`, internal/providers/diagnostic_codes.go) but no
emit site anywhere in internal/providers/. `checkNameLengths` only enforces
maximum lengths (see the `identifier_too_long` entry); no minimum-length
check exists, and neither the style-guide schema's lints nor
CONFIGURATION.md define one.

A `### Flags` fence for this slug cannot fire, so this entry stays `draft`
until either (a) a minimum-length check is implemented and specified here,
or (b) the constant is removed and this entry becomes `status: removed`.

## Examples

### Flags

```ssl
/* unreachable: no emit site uses code identifier_too_short — see Behavior; */
```

### Does not flag

```ssl
:DECLARE nCount;
```

## Rationale

Kept as draft to preserve the catalog's bijection with
diagnostic_codes.go. Note the style guide deliberately allows very short
names (loop counters i, j, k, x, y, z are exempt from Hungarian notation),
so a too-short rule may never be wanted; removal of the constant is the
likelier resolution.
