---
id: diag.no_default_statements_in_datasource
title: DEFAULT statement used in a data source file
kind: diagnostic
status: active
authority: authoritative
schema_ref: module_structure.data_source_modules.lint_rules.no_default_statements_in_datasource
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
      Introduced with data source file support (.ds/.ds.txt): data source
      builders rewrite inline :PARAMETERS defaults themselves, so a
      hand-written :DEFAULT never belongs in these files.
  - date: 2026-04-30
    ref: "PR #3 (v0.4.0, commit d744511)"
    note: Stable diagnostic code assigned; behavior unchanged.
  - date: 2026-08-07
    ref: "issue #147, ssl-style-guide#48"
    note: >-
      Companion rule datasource_default_required removed — the builder
      accepts :PARAMETERS without inline defaults. This rule stays active,
      but whether hand-written :DEFAULT statements are truly an error in
      data source files is an open question on ssl-style-guide#48.
issues: ["ssl-style-guide#48"]
---

## Behavior

Data-source-only rule (the file must be detected as a data source — URI
ending in `.ds` or `.ds.txt`; the spec fences run with
`is_data_source_file: true`). Flags every `:DEFAULT` keyword token in the
file, in any casing, as an error ranged on the keyword. Data source files
declare parameter defaults inline (`:PARAMETERS p1 := val1, p2 := val2;`);
the server-side builder itself rewrites that form into `:DEFAULT`
statements during preprocessing, so a source-level `:DEFAULT` is always
wrong there.

It must NOT flag:

- the inline `:=` default form inside `:PARAMETERS` — that is the correct
  way to express a default in a data source (a parameter without one is
  also valid; the former complementary rule `datasource_default_required`
  was removed, issue #147);
- a `:PARAMETERS` statement whose parameters carry no defaults at all;
- anything in ordinary (non-data-source) SSL files, where `:DEFAULT`
  statements are the normal way to set parameter defaults — the check
  simply does not run outside data source files.

## Examples

### Flags

```ssl
:PARAMETERS sStatus := "A";
:DEFAULT sStatus, "B";
```

### Does not flag

```ssl
:PARAMETERS dtStart := Today(), sStatus := "A";
```

```ssl
:PARAMETERS dtStart, sStatus;
```

## Rationale

The style guide defines this rule verbatim at error level
(data_source_modules.lint_rules.no_default_statements_in_datasource):
data source files are preprocessed by server-side builders that expect
inline `:PARAMETERS` defaults and generate the `:DEFAULT` form themselves,
so a hand-written `:DEFAULT` breaks the data source. (The former
companion rule `datasource_default_required`, which additionally made the
inline default mandatory, was removed in issue #147 — the builder accepts
defaultless parameters; whether this rule's own premise fully holds is an
open question tracked on ssl-style-guide#48.) The non-data-source
exemption is structural — the check is
only wired into the data-source diagnostic path (f6e78ef) — which is why
no fence can demonstrate it; providers_test.go
(TestGetDiagnostics_DataSource_DefaultStatementFlagged) covers the
positive case with the real option wiring.
