---
id: diag.nullasblank_value
title: ":NULLASBLANK value other than true or false"
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
      Introduced from the reference's data-sources guide: :NULLASBLANK
      takes the words true or false; an SSL literal such as .F. raises no
      error but is ignored. Zero findings on the production corpus.
issues: []
---

## Behavior

In a data source, flags the value of a `:NULLASBLANK := value;` directive
when it is anything but the single word `true` or `false` (matched without
regard to case), at warning severity, ranged on the value.

It must NOT flag `true`, `false`, `TRUE` or `False`.

## Examples

### Flags

```ssl
:NULLASBLANK := .F.;
:PARAMETERS sStatus := "A";
SELECT id FROM item WHERE status = @sStatus
```

### Does not flag

```ssl
:NULLASBLANK := false;
:PARAMETERS sStatus := "A";
SELECT id FROM item WHERE status = @sStatus
```

## Rationale

The builder reads only the words `true` and `false`. Another value — most
often the SSL literal `.F.` — raises no error, but it is ignored, so
null-to-blank stays on and `NULL` values still come back blank. The code
runs, with behavior the author did not ask for, so the severity is warning.
