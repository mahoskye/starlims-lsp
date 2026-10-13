---
id: diag.builder_directive_outside_datasource
title: Data source builder directive outside a data source
kind: diagnostic
status: active
authority: authoritative
schema_ref: null
default_severity: error
config:
  - ssl.diagnostics.rules
severity_overridable: true
suppressible: true
tests:
  - internal/providers/providers_test.go
history:
  - date: 2026-10-13
    ref: "issue #269"
    note: >-
      Split out of unknown_keyword (warning). The builder directives are
      known names that are valid only in data source files, so outside one
      they are a definite error, not a possible gap in the keyword list.
      The style guide's builder_directives_datasource_only rule is an
      authoritative error. Zero findings on the production corpus.
issues: []
---

## Behavior

In a file that is not a data source, flags a builder directive keyword
(`:DSN`, `:TABLENAME`, `:NULLASBLANK`, `:INVARIANTDATECOLUMNS`, in any
case) at error severity. The range covers the token, and the message names
the directive. `unknown_keyword` does not also fire on it.

It must NOT flag the directives in a data source file (`.ds`/`.ds.txt`),
where they are the builder's header; their case there is
`builder_directive_case`'s concern.

## Examples

### Flags

```ssl
:DSN := MyConnection;
```

### Does not flag

```ssl
:DECLARE sDsn;
sDsn := "MyConnection";
```

## Rationale

The data source builder reads these directives before the SQL runs; they
are not SSL keywords, so a script that contains one does not compile. The
style guide's `builder_directives_datasource_only` rule is authoritative at
error. The schema has no lint path for it, so `schema_ref` is null.
