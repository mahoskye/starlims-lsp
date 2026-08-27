---
id: diag.visibility_annotation_usage
title: Visibility annotation used at all (team-preference rule)
kind: diagnostic
status: active
authority: style_only
schema_ref: null
default_severity: hint
config:
  - ssl.diagnostics.visibilityAnnotationUsage
severity_overridable: true
suppressible: true
spec_options:
  check_visibility_annotation_usage: true
tests:
  - internal/providers/providers_test.go
history:
  - date: 2026-08-26
    ref: "issue #198"
    note: >-
      Introduced as the opt-in half of issue #198 (the class no-op half
      was already covered by visibility_annotation): some teams prefer
      procedures unannotated; default off.
issues: []
---

## Behavior

Opt-in (`ssl.diagnostics.visibilityAnnotationUsage`, default off). Flags
every `/*@private;` / `/*@protected;` annotation that the always-on
`visibility_annotation` rule leaves alone — the effective, correctly
placed ones. Exactly one of the two rules speaks per annotation: a
misplaced or class-file annotation gets the always-on warning only, an
effective annotation gets this hint only. The range covers the comment.

It must NOT flag:

- anything when the setting is off — the default;
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
fact, so the rule is opt-in and default off. Hint severity: the
annotation works — this only nudges toward the house style. The
one-rule-speaks invariant keeps a class-file annotation from stacking a
style hint on top of the no-op warning (the last example flags
`visibility_annotation` instead of this rule).
