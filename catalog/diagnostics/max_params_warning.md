---
id: diag.max_params_warning
title: Procedure has too many parameters
kind: diagnostic
status: active
authority: style_only
schema_ref: lints.class_rules.max_params_warning
default_severity: warning
config:
  - ssl.diagnostics.rules
severity_overridable: true
suppressible: true
tests:
  - internal/providers/providers_test.go
history:
  - date: 2026-03-21
    ref: "commit cdbfee6"
    note: >-
      Introduced (checkProcedureParameterCounts) in the style-guide
      alignment pass, with the two-tier 8/20 thresholds from the start.
  - date: 2026-04-30
    ref: "PR #3 (v0.4.0)"
    note: Stable diagnostic code assigned when Code was populated on every diagnostic.
issues: []
---

## Behavior

Flags a `:PROCEDURE` whose `:PARAMETERS` list exceeds a size threshold,
reported once per procedure at the procedure name. This rule deliberately
emits two severities under one code:

- **More than 20 parameters** (warning): mirrors the STARLIMS threshold —
  the platform itself warns at 20; the procedure "should be refactored".
- **9 to 20 parameters** (hint): the style guide recommends at most 8 per
  procedure, so counts of 9–20 get a gentle nudge only.

`default_severity: warning` records the over-20 path; a severity override
via `ssl.diagnostics.rules` remaps both paths.

It must NOT flag procedures with 8 or fewer parameters, including
procedures with no `:PARAMETERS` statement at all.

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

The schema defines `max_params_warning` under `lints.class_rules` with
`threshold: 20` and `severity: warning` (style_only): "Procedures with
more than 20 parameters generate a warning." The style guide separately
recommends at most 8 (`lints.style_rules.max_params_per_procedure: 8`);
the implementation folds both into one code, warning at the platform
threshold and hinting at the style threshold, so the schema-backed limit
stays prominent while the softer recommendation stays quiet. Both tiers
have been present since the rule was introduced (history).
