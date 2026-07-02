---
id: diag.hungarian_notation
title: Missing Hungarian notation prefix
kind: diagnostic
status: active
authority: advisory
schema_ref: lints.hungarian_notation
default_severity: warning
severity_overridable: true
suppressible: true
config:
  - ssl.diagnostics.hungarianNotation
  - ssl.diagnostics.hungarianPrefixes
spec_options:
  check_hungarian_notation: true
tests:
  - internal/providers/providers_test.go
  - internal/server/server_test.go
history:
  - date: 2026-01-10
    ref: "commit c5c7e68"
    note: Introduced as an opt-in diagnostic with configurable prefixes.
  - date: 2026-03-21
    ref: "commit cdbfee6 (v0.3.0)"
    note: >-
      Aligned with the style-guide lint: loop-counter and ALL_CAPS-constant
      exemptions, prefix must be followed by an uppercase letter.
  - date: 2026-04-30
    ref: "PR #3 (v0.4.0), commit d744511"
    note: Stable code hungarian_notation assigned.
issues: []
---

## Behavior

Opt-in check (`ssl.diagnostics.hungarianNotation`, default off): flags a
name declared via `:DECLARE`, `:PUBLIC`, or `:PARAMETERS` that does not
carry an allowed Hungarian prefix. A name passes when, after stripping
leading underscores, it starts (case-insensitively) with a prefix from
`ssl.diagnostics.hungarianPrefixes` (default
`a, b, d, fn, n, o, s, v`) and the first character after the prefix (and any
underscores following it) is uppercase — so `nCount` passes but `notes`
does not.

It must NOT flag:

- loop-counter names `i`, `j`, `k`, `x`, `y`, `z` (leading underscores
  ignored);
- ALL-CAPS names containing an underscore (`MAX_RETRIES`) — the constant
  convention;
- names consisting only of underscores;
- identifiers that are not declarations — usage sites are never checked;
- anything when the check is disabled (the default) or the configured
  prefix list is empty.

## Examples

### Flags

```ssl
:DECLARE Total;
```

### Flags

```ssl
:PROCEDURE Demo;
	:PARAMETERS notes;
:ENDPROC;
```

### Does not flag

```ssl
:DECLARE nCount, sName, i;
```

### Does not flag

```ssl
:DECLARE MAX_RETRIES;
```

## Rationale

The style guide requires Hungarian prefixes (schema:
`lints.hungarian_notation`, `naming_conventions.variables`), but existing
codebases are full of legacy names, so the check is opt-in per the
noisy-checks policy and warns rather than errors. The exemption list
(cdbfee6) mirrors the schema's `exceptions` block: loop counters and
constants are idiomatic without prefixes. `notes` failing while `nCount`
passes pins the uppercase-after-prefix requirement — a lowercase
continuation means the "prefix" is just the start of a word.
