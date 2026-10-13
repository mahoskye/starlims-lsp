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
  - date: 2026-10-13
    ref: "issue #255 (catalog audit)"
    note: >-
      COLLATE is now matched on the SQL lexer's stream instead of a raw regex,
      so literal or commented-out COLLATE text no longer flags. The fences
      used LSelect with a "" aFieldList, a call the function reference says
      raises at runtime; they now use SQLExecute. Data-source coverage gap
      recorded (issue #294). Corpus count unchanged.
issues: ["#294"]
---

## Behavior

Info tier (`ssl.diagnostics.infoDiagnostics`, default off). Flags a
string token whose SQL holds the word `COLLATE` (case-insensitive,
matched on the SQL lexer's token stream, so never inside a SQL character
literal or comment) when the token is part of the first argument of a
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
- `COLLATE` inside a SQL character literal or a SQL comment;
- `COLLATE` in strings that are not SQL-function arguments.

## Examples

### Flags

```ssl
:PROCEDURE Main;
	:DECLARE aRows;
	aRows := SQLExecute("SELECT NAME FROM SAMPLES ORDER BY NAME COLLATE Latin1_General_CI_AS");
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE Main;
	:DECLARE aRows;
	/* server default is case-sensitive so the report must sort case-insensitively;
	aRows := SQLExecute("SELECT NAME FROM SAMPLES ORDER BY NAME COLLATE Latin1_General_CI_AS");
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE Main;
	:DECLARE aRows;
	aRows := SQLExecute("SELECT NAME FROM SAMPLES WHERE KIND = 'COLLATERAL'");
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE Main;
	:DECLARE aRows;
	aRows := SQLExecute("SELECT NAME FROM SAMPLES WHERE KIND = 'COLLATE' -- collate not needed here
");
:ENDPROC;
```

## Known gaps

- Not evaluated on a SQL-mode data-source body (D15). That path runs only
  `datasource_sql_semicolon` and `datasource_undeclared_placeholder`,
  although the style-guide record lists `sql_data_source` in
  `applies_to`. An SSL-mode data source is checked like any script.
  Tracked in issue #294.

## Rationale

An unexplained `COLLATE` is usually cargo-culted from a query that needed
it against a different server (issue #197); the occasional legitimate one
deserves a sentence saying why. Because "has a comment above it" is a
convention no runtime enforces, this lives in the opt-in info tier.
Info severity: the SQL is correct — the rule polices documentation,
not behavior. Any comment above the statement satisfies the rule; judging
comment *content* is out of scope.
