---
id: diag.unjustified_collate
title: COLLATE in embedded SQL without a justification comment
kind: diagnostic
status: active
authority: style_only
schema_ref: null
default_severity: info
config:
  - ssl.diagnostics.infoDiagnostics
severity_overridable: true
suppressible: true
spec_options:
  include_info_diagnostics: true
tests:
  - internal/providers/providers_test.go
history:
  - date: 2026-08-26
    ref: "issue #197"
    note: >-
      Introduced from the runtime-verification batch as an opt-in style
      rule, default off: forcing collation is occasionally necessary but
      should carry a documented reason.
  - date: 2026-08-27
    ref: "issue #208 discussion (info-tier expansion)"
    note: >-
      Moved hint -> info in the info-tier expansion: the dedicated
      ssl.diagnostics.collateJustification toggle (never released) is
      subsumed by the tier. Info is the opt-in advisory tier
      (ssl.diagnostics.infoDiagnostics); explicit ssl.diagnostics.rules
      entries still promote or disable per rule.
issues: []
---

## Behavior

Info tier (`ssl.diagnostics.infoDiagnostics`, default off). Flags a
string token containing the SQL keyword `COLLATE` (case-insensitive,
word-bounded) when the token is part of the first argument of a
recognized embedded-SQL function call and no comment precedes the
containing statement: scanning backwards from the call's function name,
a `/* ...;` comment must appear before the previous statement's `;`
terminator is reached. The range covers the string token.

It must NOT flag:

- anything when the info tier is off — the default;
- a `COLLATE` whose statement carries a comment directly above it (or
  trailing the previous statement's line) — the justification;
- words containing collate as a substring (`COLLATERAL`) — word
  boundary;
- `COLLATE` in strings that are not SQL-function arguments.

## Examples

### Flags

```ssl
:PROCEDURE Main;
	:DECLARE aRows;
	aRows := LSelect("SELECT NAME FROM SAMPLES ORDER BY NAME COLLATE Latin1_General_CI_AS", "", "CONN");
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE Main;
	:DECLARE aRows;
	/* server default is case-sensitive so the report must sort case-insensitively;
	aRows := LSelect("SELECT NAME FROM SAMPLES ORDER BY NAME COLLATE Latin1_General_CI_AS", "", "CONN");
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE Main;
	:DECLARE aRows;
	aRows := LSelect("SELECT NAME FROM SAMPLES WHERE KIND = 'COLLATERAL'", "", "CONN");
:ENDPROC;
```

## Rationale

An unexplained `COLLATE` is usually cargo-culted from a query that needed
it against a different server (issue #197); the occasional legitimate one
deserves a sentence saying why. Because "has a comment above it" is a
convention no runtime enforces, this lives in the opt-in info tier.
Info severity: the SQL is correct — the rule polices documentation,
not behavior. Any comment above the statement satisfies the rule; judging
comment *content* is out of scope.
