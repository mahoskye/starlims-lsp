---
id: diag.invariant_date_columns_quoted
title: "Quoted :INVARIANTDATECOLUMNS column name"
kind: diagnostic
status: active
authority: authoritative
schema_ref: null
default_severity: warning
config:
  - ssl.diagnostics.rules
severity_overridable: true
suppressible: true
spec_options:
  is_data_source_file: true
tests:
  - internal/providers/datasource_header_rules_test.go
history:
  - date: 2026-10-13
    ref: "issue #260"
    note: >-
      Introduced from the reference's data-sources guide: the directive
      takes bare column names; a quoted name keeps its quotes and matches
      no column. Zero findings on the production corpus.
issues: []
---

## Behavior

In a data source, flags each string-literal column name in an
`:INVARIANTDATECOLUMNS := ...;` directive, at warning severity, ranged on
the string.

It must NOT flag bare column names (`:INVARIANTDATECOLUMNS := logdate,
releasedate;`).

## Examples

### Flags

```ssl
:INVARIANTDATECOLUMNS := "logdate";
:PARAMETERS sStatus := "A";
SELECT logdate FROM item WHERE status = @sStatus
```

### Does not flag

```ssl
:INVARIANTDATECOLUMNS := logdate, releasedate;
:PARAMETERS sStatus := "A";
SELECT logdate, releasedate FROM item WHERE status = @sStatus
```

## Rationale

The builder keeps the quotes as part of the column name, so a quoted name
matches no column and the date is not treated as invariant. No error is
raised; the behavior silently differs, so the severity is warning.
