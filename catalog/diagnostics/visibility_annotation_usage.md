---
id: diag.visibility_annotation_usage
title: Visibility annotation the convention does not require (team-preference rule)
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
  - internal/providers/visibility_raise_only371_test.go
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
  - date: 2026-11-25
    ref: "issue #371"
    note: >-
      No longer fires on the `/*@private;` of a raise-only helper, the one
      annotation the convention requires (style guide
      private_raise_only_helpers). It used to note every effective
      annotation and leave a reviewer to confirm the exception, so correct
      code carried a note that could not be cleared and that told an
      assistant to remove an annotation the guide asks for. The message
      now names the exception.
  - date: 2026-11-26
    ref: "PR #376"
    note: >-
      The legacy `:ERROR` handler is modelled. A procedure that raises
      ahead of its own `:ERROR;` marker catches that raise, so it is not
      raise-only and its annotation is flagged; the first version counted
      any procedure with a handler as raise-only when it raised.
issues: []
---

## Behavior

Info tier (`ssl.diagnostics.infoDiagnostics`, default off). Flags an
effective annotation — the exact `/*@private;` or `/*@protected;` on its
own line immediately before a script `:PROCEDURE` — unless it is the one
the convention requires: `/*@private;` on a raise-only helper. At most one
of the two rules speaks per annotation: an inert annotation (class file,
near-miss spelling, detached or misplaced) gets the always-on
`visibility_annotation` warning only, an effective one gets this note or,
on a raise-only helper, nothing, and an annotation-shaped comment before
an expression statement or at end of file gets neither. The range covers
the comment.

A raise-only helper, as the server sees one, is a procedure that reports
failure by raising: its body holds a `RaiseError(...)` call that leaves
the procedure, which is one no `:TRY` body of that procedure with a
`:CATCH` encloses. A raise in a `:TRY` that has only `:FINALLY`, in a
`:CATCH`, or after the `:ENDTRY` leaves the procedure; a raise the
procedure catches itself does not, and neither does a member call
`oObject:RaiseError(...)`, which is not the built-in. A legacy `:ERROR;`
marker in the procedure catches like a `:CATCH`: it goes last and protects
the statements before it, so a raise ahead of it does not leave the
procedure, while a raise inside the handler does.
A helper that raises nothing itself and only lets an error from a call
pass through is not one the server can tell from any other procedure, so
its annotation is flagged.

It must NOT flag:

- anything when the info tier is off — the default;
- `/*@private;` on a raise-only helper — the convention requires it
  (`/*@protected;` on one is still flagged: the convention names
  `/*@private;`);
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

A helper that catches its own raise reports failure by its return value,
so it is not the exception:

### Flags

```ssl
/*@private;
:PROCEDURE TryRequire;
	:PARAMETERS sSampleId;

	:TRY;
		:IF Empty(sSampleId);
			RaiseError("A sample is required");
		:ENDIF;
	:CATCH;
		ClearLastSSLError();
		:RETURN .F.;
	:ENDTRY;

	:RETURN .T.;
:ENDPROC;
```

The convention's exception is `/*@private;`:

### Flags

```ssl
/*@protected;
:PROCEDURE RequireSample;
	:PARAMETERS sSampleId;

	:IF Empty(sSampleId);
		RaiseError("A sample is required");
	:ENDIF;
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE Helper;
	:RETURN 1;
:ENDPROC;
```

A raise-only helper is marked private so that only the entry point that
catches its error can call it:

### Does not flag

```ssl
/*@private;
:PROCEDURE RequireSample;
	:PARAMETERS sSampleId;

	:IF Empty(sSampleId);
		RaiseError("A sample is required");
	:ENDIF;
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

The convention has one required exception (style guide
prefer_unannotated_procedures and private_raise_only_helpers): a helper
that reports failure only by raising is marked `/*@private;`, so no caller
can reach it without the entry point that catches its error. A note on
that annotation is wrong twice over (issue #371): the code is as the
convention asks, so the note can never be cleared, and the tier's readers
are assistants (D13), which act on a note by removing the annotation. The
server exempts what it can see, an explicit `RaiseError` that leaves the
procedure: past every `:TRY` with a `:CATCH` and past the procedure's own
`:ERROR` handler. It does not guess at a helper that only lets an
error pass through: every procedure without a `:TRY` does that, and the
convention's own example of a procedure to leave unannotated is one.
