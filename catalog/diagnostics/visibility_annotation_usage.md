---
id: diag.visibility_annotation_usage
title: Visibility annotation used at all (team-preference rule)
kind: diagnostic
status: active
authority: style_only
schema_ref: null
default_severity: info
config:
  - ssl.diagnostics.infoDiagnostics
severity_overridable: true
suppressible: true
spec_options:
  include_info_diagnostics: true
tests:
  - internal/providers/providers_test.go
history:
  - date: 2026-08-26
    ref: "issue #198"
    note: >-
      Introduced as the opt-in half of issue #198 (the class no-op half
      was already covered by visibility_annotation): some teams prefer
      procedures unannotated; default off.
  - date: 2026-08-27
    ref: "issue #208 discussion (info-tier expansion)"
    note: >-
      Moved hint -> info in the info-tier expansion: the dedicated
      ssl.diagnostics.visibilityAnnotationUsage toggle (never released)
      is subsumed by the tier. Info is the opt-in advisory tier
      (ssl.diagnostics.infoDiagnostics); explicit ssl.diagnostics.rules
      entries still promote or disable per rule.
issues: []
---

## Behavior

Info tier (`ssl.diagnostics.infoDiagnostics`, default off). Flags
every `/*@private;` / `/*@protected;` annotation that the always-on
`visibility_annotation` rule leaves alone — the effective, correctly
placed ones. Exactly one of the two rules speaks per annotation: a
misplaced or class-file annotation gets the always-on warning only, an
effective annotation gets this hint only. The range covers the comment.

It must NOT flag:

- anything when the info tier is off — the default;
- annotations already flagged by `visibility_annotation` (class no-op or
  misplaced) — no double report;
- comments that are not visibility annotations (`/* @private;` with a
  space, other `/*@...;` markers).

## Examples

### Flags

```ssl
/*@private;
:PROCEDURE Helper;
	:RETURN 1;
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE Helper;
	:RETURN 1;
:ENDPROC;
```

### Does not flag

```ssl
:CLASS Widget;

/*@private;
:PROCEDURE Helper;
	:RETURN 1;
:ENDPROC;
```

## Rationale

The annotation is rarely used and some teams prefer script procedures
unannotated (issue #198); that is a team convention, not a language
fact, so the rule lives in the opt-in info tier. Info severity: the
annotation works — this only notes the house style. The
one-rule-speaks invariant keeps a class-file annotation from stacking a
style hint on top of the no-op warning (the last example flags
`visibility_annotation` instead of this rule).
