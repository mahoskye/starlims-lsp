---
id: diag.identifier_too_long
title: Identifier exceeds style-guide length limit
kind: diagnostic
status: active
authority: tool
schema_ref: null
default_severity: info
severity_overridable: true
suppressible: true
config:
  - ssl.diagnostics.infoDiagnostics
  - ssl.diagnostics.hungarianPrefixes
spec_options:
  include_info_diagnostics: true
tests:
  - internal/providers/providers_test.go
history:
  - date: 2026-03-28
    ref: "commit be7a174"
    note: >-
      Introduced in the diagnostics expansion: variables max 20 chars
      (excluding Hungarian prefix), procedures max 30.
  - date: 2026-04-30
    ref: "PR #3 (v0.4.0), commit d744511"
    note: Stable code identifier_too_long assigned.
  - date: 2026-08-28
    ref: "issue #184 (expression AST consumers)"
    note: >-
      Declared names now come from statement-based declaration resolution
      (parser.CollectDeclarations). A declaration written as a bare
      `:DECLARE` / `:PARAMETERS` with its names on the following lines
      previously produced no names at all, so every name it declared was
      invisible to this behavior. No change on the production corpus's
      default-on output; the names it recovers were simply missing before.
issues: []
---

## Behavior

Always-on check with two limits:

- **Variables** (declared via `:DECLARE`/`:PUBLIC`/`:PARAMETERS`): flags
  when the *effective* name exceeds 20 characters. The effective name strips
  leading underscores and one Hungarian prefix from
  `ssl.diagnostics.hungarianPrefixes` (default `a, b, d, fn, n, o, s, v`),
  provided the character after the prefix is uppercase — `sCustomerName`
  is measured as `CustomerName`.
- **Procedures**: flags when the full procedure name exceeds 30 characters
  (no prefix stripping).

It must NOT flag:

- a variable whose effective name is 20 characters or fewer, even if the
  raw name (with prefix) is longer;
- a procedure name of 30 characters or fewer;
- usage sites — only declarations are measured.

## Examples

### Flags

```ssl
:DECLARE sCustomerAccountReconciliationTotal;
```

### Does not flag

```ssl
:DECLARE sAbcdefghijklmnopqrst;
```

### Does not flag

```ssl
:DECLARE nCount;
```

## Rationale

The style guide caps variable names at 20 characters (excluding the
mandatory Hungarian prefix) and procedure names at 30 (commit be7a174).
Long names are a readability concern, not a defect, so severity is info.
The second fence pins the prefix exclusion: `sAbcdefghijklmnopqrst` is 21
raw characters but exactly 20 after stripping `s`, so it passes — measuring
the raw name would double-penalize the prefix convention.
