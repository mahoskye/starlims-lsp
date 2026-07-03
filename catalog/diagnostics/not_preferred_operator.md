---
id: diag.not_preferred_operator
title: Discouraged inequality operator form
kind: diagnostic
status: active
authority: advisory
schema_ref: lints.not_preferred_operators
default_severity: info
severity_overridable: true
suppressible: true
tests:
  - internal/providers/providers_test.go
history:
  - date: 2026-03-21
    ref: "commit cdbfee6"
    note: >-
      Introduced in the full alignment pass with ssl-style-guide
      (checkNotPreferredOperators), mirroring the schema's
      not_preferred_operators list.
  - date: 2026-04-30
    ref: "PR #3 (v0.4.0, commit d744511)"
    note: Stable diagnostic code assigned; rule behavior unchanged.
  - date: 2026-05-01
    ref: "vs-code-ssl-formatter PR #58 (ext v1.6.0)"
    note: >-
      Extension quick-fix code action keyed on this slug rewrites `<>` / `#`
      to `!=`; message wording is load-bearing for that fix.
issues: []
---

## Behavior

Flags every operator token whose text is exactly `#` or `<>` — both are
valid SSL inequality operators, but the style guide prefers `!=`. The range
covers the operator token, and the message names the concrete replacement
(`Use '!=' instead of '<>' for inequality` / `Use '!=' instead of '#' for
inequality`). Each occurrence is reported individually.

It must NOT flag:

- the preferred `!=` operator, or any other comparison operator (`=`, `==`,
  `<`, `>`, `<=`, `>=`, `$`);
- `#` or `<>` appearing inside string literals or comments — only tokens
  lexed as operators are inspected.

## Examples

### Flags

```ssl
:PROCEDURE Demo;
:DECLARE nCount;
nCount := 1;
:IF nCount <> 2;
nCount := 3;
:ENDIF;
:ENDPROC;
```

### Flags

```ssl
:PROCEDURE Demo;
:DECLARE nCount;
nCount := 1;
:IF nCount # 2;
nCount := 3;
:ENDIF;
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE Demo;
:DECLARE nCount, sLabel;
nCount := 1;
sLabel := "value <> other # note";
:IF nCount != 2;
nCount := 3;
:ENDIF;
:ENDPROC;
```

## Rationale

The schema's `lints.not_preferred_operators` list declares `#` and `<>`
"not preferred" with severity `info` and replacement `!=`; this rule is a
direct transcription (authority: advisory, severity info — the operators
work, this is purely a consistency nudge). The extension's quick-fix
(vs-code-ssl-formatter PR #58) depends on this slug and on the message
naming the replacement operator, so both are part of the contract.
