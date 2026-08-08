---
id: diag.datasource_default_required
title: Data source parameter missing its inline default (removed — premise was wrong)
kind: diagnostic
status: removed
authority: authoritative
schema_ref: module_structure.data_source_modules.lint_rules.datasource_default_required
default_severity: error
severity_overridable: true
suppressible: true
spec_options:
  is_data_source_file: true
tests: []
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
  - date: 2026-08-07
    ref: "issue #147, ssl-style-guide#48"
    note: >-
      Removed. The rule's premise was wrong: the data source builder
      accepts `:PARAMETERS p1;` with no inline default (confirmed against
      runtime behavior), so flagging every defaultless parameter as an
      error false-flagged valid production .ds files. The rule faithfully
      implemented the style-guide schema's `default_required: true`, which
      is being corrected in ssl-style-guide#48. Constant, check, and tests
      deleted.
issues: ["#147"]
---

## Behavior

Removed. The rule flagged each parameter name in a data-source
`:PARAMETERS` statement that was not immediately followed by an inline
`:=` default value, at error severity — but the data source builder does
not require a default per parameter: `:PARAMETERS p1;` is valid and in
active production use. The style-guide schema's
`data_source_modules.parameters.default_required: true`, which this rule
enforced, is itself the error (spec fix tracked in ssl-style-guide#48).

Inline `:=` defaults remain the correct way to express a default when one
is wanted, and the complementary rule
[`no_default_statements_in_datasource`](no_default_statements_in_datasource.md)
(the `:DEFAULT`-statement form is an error in data source files) is
unaffected — its correctness is an open question on ssl-style-guide#48,
and it stays active until answered.

If the schema correction lands as "defaults recommended but optional", a
future advisory/style rule may be specced as a new `planned` entry; a
mandatory-default error must not return.

## Rationale

The removal follows the `region_legacy` precedent: a rule whose premise
is factually wrong about the runtime produces pure noise on correct code,
and no severity level makes it acceptable. See history (2026-08-07) and
issue #147 for the confirming evidence.
