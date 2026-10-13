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
  - date: 2026-10-13
    ref: "issue #255"
    note: >-
      Now fires only on effective annotations. It used to note every
      annotation visibility_annotation left alone, including inert ones —
      near-miss spellings, an annotation detached from `:PROCEDURE` by a
      blank line or another comment, one before an expression statement or
      at end of file — so it told the author an annotation was in force
      when it was not.
issues: []
---

## Behavior

Info tier (`ssl.diagnostics.infoDiagnostics`, default off). Flags
every effective annotation: the exact `/*@private;` or `/*@protected;`
on its own line immediately before a script `:PROCEDURE`. At most one of
the two rules speaks per annotation: an inert annotation (class file,
near-miss spelling, detached or misplaced) gets the always-on
`visibility_annotation` warning only, an effective one gets this note
only, and an annotation-shaped comment before an expression statement or
at end of file gets neither. The range covers the comment.

It must NOT flag:

- anything when the info tier is off — the default;
- annotations already flagged by `visibility_annotation` (class no-op,
  near miss, detached, or misplaced) — no double report;
- an annotation that attaches to nothing (before an expression statement
  or at end of file) — it is not in force, so there is no convention to
  note;
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

### Does not flag

```ssl
/*@Private;
:PROCEDURE Helper;
	:RETURN 1;
:ENDPROC;
```

### Does not flag

```ssl
:DECLARE nCount;
/*@private;
nCount := 1;
```

## Rationale

The annotation is rarely used and some teams prefer script procedures
unannotated (issue #198); that is a team convention, not a language
fact, so the rule lives in the opt-in info tier. Info severity: the
annotation works — this only notes the house style. The
one-rule-speaks invariant keeps a class-file annotation from stacking a
style hint on top of the no-op warning (the class-file example flags
`visibility_annotation` instead of this rule). Firing only on effective
annotations (issue #255) is what makes the note truthful: the near-miss
and unattached fences used to get it, telling the author an annotation
was in force when the procedure was still public.
