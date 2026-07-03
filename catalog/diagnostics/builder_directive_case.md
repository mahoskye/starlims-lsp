---
id: diag.builder_directive_case
title: Data source builder directive not uppercase
kind: diagnostic
status: active
authority: tool
schema_ref: null
default_severity: error
severity_overridable: true
suppressible: true
spec_options:
  is_data_source_file: true
tests:
  - internal/providers/providers_test.go
history:
  - date: 2026-03-30
    ref: "commit f6e78ef"
    note: >-
      Introduced with data source file support (.ds/.ds.txt), which added a
      data-source-specific keyword-form check recognizing builder
      directives.
  - date: 2026-04-30
    ref: "PR #3 (v0.4.0)"
    note: Stable diagnostic code assigned; rule behavior unchanged.
issues: []
---

## Behavior

In data source files only, flags a colon-prefixed builder directive —
`:DSN`, `:TABLENAME`, `:NULLASBLANK`, `:INVARIANTDATECOLUMNS` — written in
anything other than all-uppercase (e.g. `:dsn`, `:TableName`). The data
source builder matches these directives case-sensitively, so a
wrong-cased directive is silently ignored rather than applied. The message
gives the canonical uppercase form; the range covers the directive token.

Data source mode is keyed off the document, not a setting: the server
treats URIs ending in `.ds` or `.ds.txt` as data source files (the spec
fences below run with `is_data_source_file: true`).

It must NOT flag:

- correctly uppercased directives (`:DSN LimsMain;`);
- the same text in a regular `.ssl` file — there the general keyword-form
  checks own the token (an unknown `:dsn` reports as
  `diag.unknown_keyword` instead);
- non-directive keywords in data source files, which are owned by
  `diag.keyword_uppercase` / `diag.unknown_keyword`.

## Examples

### Flags

```ssl
:dsn LimsMain;
SELECT 1;
```

### Does not flag

```ssl
:DSN LimsMain;
SELECT 1;
```

## Rationale

Builder directives configure how the data source SQL is executed; a
wrong-cased directive doesn't error at runtime — it simply never takes
effect, which surfaces as mysterious data behavior far from the typo.
That silent-misconfiguration mode justifies error severity even though the
file "runs". Introduced with data source support (commit f6e78ef); the
code slug was stabilized in PR #3 (v0.4.0).
