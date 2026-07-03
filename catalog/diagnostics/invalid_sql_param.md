---
id: diag.invalid_sql_param
title: Named SQL parameter matches no declared variable
kind: diagnostic
status: active
authority: tool
schema_ref: null
default_severity: warning
severity_overridable: true
suppressible: true
config:
  - ssl.diagnostics.globals
spec_options:
  check_sql_params: true
tests:
  - internal/providers/providers_test.go
history:
  - date: 2026-02-02
    ref: "commit 0935ab2"
    note: SQL parameter validation introduced.
  - date: 2026-03-28
    ref: "commit be7a174"
    note: >-
      Predefined globals treated as declared; base-name extraction for
      property/array access placeholders.
  - date: 2026-04-30
    ref: "PR #3 (v0.4.0), commit d744511"
    note: Stable code invalid_sql_param assigned.
issues: []
---

## Behavior

Opt-in check (provider option `CheckSQLParams`, default off, not exposed via
LSP client settings): scans every string literal in the file for named SQL
placeholders (`?name?`) and flags one whose base variable name is not
declared. The base name is the placeholder text up to the first `:`, `[`, or
`(` — so `?oUser:ID?` is checked as `oUser`. Reported once per parameter
name per line; the range covers the placeholder including both `?` marks.

Treated as declared: names from `:DECLARE`/`:PUBLIC`/`:PARAMETERS`
statements, procedure parameters, built-in predefined globals and status
keywords, and names configured in `ssl.diagnostics.globals`.

It must NOT flag:

- positional placeholders (bare `?`);
- placeholders containing function calls (`?Today()?`) — not variables;
- complex expressions that are not simple named placeholders (those belong
  to `complex_sql_placeholder`);
- anything when the check is disabled (the default).

Note the check is purely lexical over string literals: the string does not
need to be passed to a SQL function to be scanned.

## Examples

### Flags

```ssl
:DECLARE sSql;
sSql := "SELECT * FROM samples WHERE id = ?nSampleId?";
```

### Does not flag

```ssl
:DECLARE sSql, nSampleId;
sSql := "SELECT * FROM samples WHERE id = ?nSampleId?";
```

### Does not flag

```ssl
:DECLARE sSql;
sSql := "SELECT * FROM samples WHERE id = ?";
```

## Rationale

A `?name?` placeholder is substituted with the named variable at runtime;
if no such variable exists the query fails or silently binds nothing, so a
mismatch is warning-worthy but the detection (any string literal, no data
flow) is heuristic enough that error would overclaim. Opt-in per the
noisy-checks policy — endpoint files and included scripts reference
variables the scanner cannot see. History: introduced 0935ab2; be7a174
added the globals/base-name exemptions that the Does-not-flag fences
partially pin.
