---
id: diag.max_params_warning
title: Procedure has too many parameters
kind: diagnostic
status: active
authority: advisory
schema_ref: lints.class_rules.max_params_warning
default_severity: info
config:
  - ssl.diagnostics.infoDiagnostics
severity_overridable: true
suppressible: true
spec_options:
  include_info_diagnostics: true
tests:
  - internal/providers/providers_test.go
  - internal/providers/diagnostics_audit_test.go
history:
  - date: 2026-03-21
    ref: "commit cdbfee6"
    note: >-
      Introduced (checkProcedureParameterCounts) in the style-guide
      alignment pass, with the two-tier 8/20 thresholds from the start.
  - date: 2026-04-30
    ref: "PR #3 (v0.4.0)"
    note: Stable diagnostic code assigned when Code was populated on every diagnostic.
  - date: 2026-08-27
    ref: "issue #208 discussion (info-tier expansion)"
    note: >-
      Moved hint -> info in the info-tier expansion: threshold-based
      API-shape advice. Info is the opt-in advisory tier
      (ssl.diagnostics.infoDiagnostics); explicit ssl.diagnostics.rules
      entries still promote or disable per rule.
  - date: 2026-10-13
    ref: "issue #255"
    note: >-
      Range moved onto the procedure name (it had started at column 0 of
      the :PROCEDURE line), and the Behavior text brought up to date: both
      tiers have been info since the info-tier expansion. Authority
      corrected style_only -> advisory to mirror the style guide record
      (max_params_warning). That record's severity (warning) disagrees with
      this entry; a parameter count is API-shape advice, not a likely bug,
      so info stands and the record is the side to correct.
issues: []
---

## Behavior

Flags a `:PROCEDURE` whose `:PARAMETERS` list has more than 8 names,
reported once per procedure, ranged on the procedure name. Both tiers are
info severity under one code and differ only in message:

- **More than 20 parameters**: the message cites the platform's own
  threshold — STARLIMS itself warns past 20 — and says the procedure
  should be refactored.
- **9 to 20 parameters**: the message cites the style guide's
  recommendation of at most 8 per procedure.

It must NOT flag procedures with 8 or fewer parameters, including
procedures with no `:PARAMETERS` statement at all. A script-level
`:PARAMETERS` statement (outside any `:PROCEDURE`) is not counted.

## Examples

### Flags

```ssl
:PROCEDURE BigProc;
:PARAMETERS p01, p02, p03, p04, p05, p06, p07, p08, p09, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, p21;
:ENDPROC;
```

### Flags

```ssl
:PROCEDURE WideProc;
:PARAMETERS a1, a2, a3, a4, a5, a6, a7, a8, a9;
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE OkProc;
:PARAMETERS a1, a2, a3, a4, a5, a6, a7, a8;
:ENDPROC;
```

## Rationale

The style guide's `max_params_warning` record (advisory) merges two
schema values: the recommended maximum of 8
(`lints.style_rules.max_params_per_procedure`) and the platform threshold
of 20 (`lints.class_rules.max_params_warning`: "Procedures with more than
20 parameters generate a warning."). The tool folds both into one code
with a threshold-specific message. Info severity (D13's opt-in advisory
tier): a long parameter list is API-shape advice, never a likely bug, so
the record's `severity: warning` is not mirrored here. Both tiers have
been present since the rule was introduced (history).
