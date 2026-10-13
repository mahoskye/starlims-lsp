---
id: diag.datasource_sql_question_placeholder
title: "?name? marker in a SQL data source"
kind: diagnostic
status: active
authority: authoritative
schema_ref: module_structure.data_source_modules.file_types.sql_data_source.body_parameters.syntax
default_severity: error
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
      Introduced. The reference (guides/data-sources.md) says a SQL-mode
      data source references parameters as @name and that SQLExecute's
      ?name? markers do not work there; the style guide's
      datasource_sql_at_name_placeholders rule is authoritative/error.
      Zero findings on the production corpus.
issues: []
---

## Behavior

In the body of a SQL-mode data source (DECISIONS.md D15), flags each
`?name?` placeholder, at error severity, ranged on the marker. The marker
is read with the SQL lexer, so text inside SQL string literals and
comments is never flagged. Positional `?` markers are not this rule's
concern.

It must NOT flag `@name` placeholders, or `?name?` markers in an SSL-mode
data source, where `SQLExecute` substitutes them.

## Examples

### Flags

```ssl
:PARAMETERS sStatus := "A";
SELECT id FROM item WHERE status = ?sStatus?
```

### Does not flag

```ssl
:PARAMETERS sStatus := "A";
SELECT id FROM item WHERE status = @sStatus AND note <> '?sStatus?'
```

### Does not flag

```ssl
:PARAMETERS sStatus := "A";
:DECLARE aRows;
aRows := SQLExecute("SELECT id FROM item WHERE status = ?sStatus?");
:RETURN aRows;
```

## Rationale

A SQL-mode data source binds its `:PARAMETERS` values to `@name`
placeholders; the `?name?` substitution is `SQLExecute`'s, in SSL code,
and does not happen in a SQL body, so the query runs with the marker
text in it. The style guide makes `@name` the required form
(`datasource_sql_at_name_placeholders`, authoritative, error).
