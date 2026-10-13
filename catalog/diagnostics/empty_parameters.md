---
id: diag.empty_parameters
title: ":PARAMETERS with no names"
kind: diagnostic
status: active
authority: authoritative
schema_ref: module_structure.data_source_modules.file_types.ssl_data_source.parameters.empty_parameters_error
default_severity: error
config:
  - ssl.diagnostics.rules
severity_overridable: true
suppressible: true
tests:
  - internal/providers/datasource_header_rules_test.go
history:
  - date: 2026-10-13
    ref: "issue #260"
    note: >-
      Introduced. The reference (keywords/PARAMETERS.md) says a parameter
      list must contain at least one name, and the data-sources guide says
      `:PARAMETERS;` is an error; the style guide's
      datasource_empty_parameters rule is authoritative/error. Zero
      findings on the production corpus.
issues: []
---

## Behavior

Flags a `:PARAMETERS` keyword followed directly by `;` (or by nothing), in
any file — a script, a procedure or a data source in either mode — at
error severity, ranged on the keyword.

It must NOT flag a `:PARAMETERS` statement that names at least one
parameter, however it is laid out.

## Examples

### Flags

```ssl
:PARAMETERS;
:DECLARE nCount;
nCount := 1;
```

### Does not flag

```ssl
:PARAMETERS sName;
:DEFAULT sName, "";
:RETURN sName;
```

## Rationale

The reference states that a parameter list must contain at least one name;
a script or data source that takes no parameters leaves the line out. The
style guide records the data-source case as `datasource_empty_parameters`
(authoritative, error). The rule applies to every file because the
reference states the requirement for `:PARAMETERS` itself.
