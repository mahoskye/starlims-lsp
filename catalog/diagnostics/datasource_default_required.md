---
id: diag.datasource_default_required
title: Data source parameter missing its inline default
kind: diagnostic
status: active
authority: authoritative
schema_ref: module_structure.data_source_modules.lint_rules.datasource_default_required
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
      Introduced with data source file support (.ds/.ds.txt): data sources
      declare parameter defaults inline in :PARAMETERS rather than via
      separate :DEFAULT statements.
  - date: 2026-04-30
    ref: "PR #3 (v0.4.0, commit d744511)"
    note: Stable diagnostic code assigned; behavior unchanged.
issues: []
---

## Behavior

Data-source-only rule (the file must be detected as a data source — URI
ending in `.ds` or `.ds.txt`; the spec fences run with
`is_data_source_file: true`). Flags each parameter name in a `:PARAMETERS`
statement that is not immediately followed by an inline `:=` default value.
Expected data source syntax is `:PARAMETERS p1 := val1, p2 := val2;`. One
diagnostic per defaultless parameter, ranged on the parameter name.

It must NOT flag:

- parameters that carry an inline `:=` default — the default value itself is
  consumed whole (identifiers inside it, such as function calls, are never
  mistaken for parameter names);
- anything in ordinary (non-data-source) SSL files, where `:PARAMETERS`
  without inline defaults is the normal form and defaults belong in
  `:DEFAULT` statements.

## Examples

### Flags

```ssl
:PARAMETERS dtStart, sStatus := "A";
```

### Does not flag

```ssl
:PARAMETERS dtStart := Today(), sStatus := "A";
```

## Rationale

The data source builder executes `:PARAMETERS` declarations directly and
requires every parameter to have an inline default; a missing default breaks
the data source at runtime, hence error severity (f6e78ef). The complementary
rule `no_default_statements_in_datasource` rejects the `:DEFAULT`-statement
form in the same files, so together they force the inline syntax.
