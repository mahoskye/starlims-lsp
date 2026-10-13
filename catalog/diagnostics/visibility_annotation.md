---
id: diag.visibility_annotation
title: Misplaced or ineffective visibility annotation
kind: diagnostic
status: active
authority: authoritative
schema_ref: null
default_severity: warning
severity_overridable: true
suppressible: true
tests:
  - internal/providers/providers_test.go
history:
  - date: 2026-03-28
    ref: "commit be7a174"
    note: >-
      Introduced with the style-guide docs expansion: validates /*@private;
      and /*@protected; against the schema's placement and
      class-restriction rules (procedures.visibility_annotations).
  - date: 2026-04-30
    ref: "PR #3 (v0.4.0, commit d744511)"
    note: Stable diagnostic code assigned; behavior unchanged.
  - date: 2026-10-13
    ref: "issue #255"
    note: >-
      Catalog audit. The check passed inert annotations as effective: a
      near-miss spelling (`/*@Private;`, `/* @private;`, `/*@ private;`),
      a blank line before `:PROCEDURE`, and another comment in between all
      leave the procedure public (style-guide visibility_annotation_form
      and visibility_annotation_exact_form), yet got only the info-tier
      usage note. Each is now reported when it stands before a
      `:PROCEDURE`. The shared matcher became exact, so ProcedureInfo
      .IsPrivate (cross-file completion's filter) agrees with this
      verdict; it used to take the nearest comment across blank lines and
      accept any casing. Authority corrected to authoritative to mirror the
      style-guide rules. Production: 5 annotations, all exact and adjacent,
      no change.
issues: []
---

## Behavior

Validates the visibility annotations `/*@private;` and `/*@protected;`.
The one effective form is exact — lowercase, no space anywhere — on its
own line immediately before `:PROCEDURE`, with only a line break between
them, in a script. It is matched byte for byte; this rule also recognizes
an annotation-shaped comment (`/*`, `@`, private/protected, `;` in any
casing or spacing) as a near miss. Every situation below is a warning
with the range on the comment, because the annotation is inert and the
procedure silently stays public:

- **Inside a class** (anywhere after the `:CLASS` keyword): an exact
  annotation, or a near miss before a `:PROCEDURE`, has no effect — class
  methods are always Public|Virtual (ssl-style-guide
  `visibility_annotations_script_only`) — and the message says so.
- **Near-miss spelling before `:PROCEDURE`** (`/*@Private;`,
  `/*@PRIVATE;`, `/* @private;`, `/*@ private;`): not recognized
  (ssl-style-guide `visibility_annotation_exact_form`); the message gives
  the exact form.
- **Detached from `:PROCEDURE`**: a `:PROCEDURE` follows, but a blank
  line, another comment, or a shared line separates them
  (ssl-style-guide `visibility_annotation_form`: "a blank line breaks the
  association").
- **Followed by another keyword** (e.g. `:DECLARE`): not in its required
  position, so it attaches to nothing.

The verdict is shared with `ProcedureInfo.IsPrivate`, which feeds
cross-file completion's private-procedure filter (feature.completion), so
an annotation this rule calls inert never hides a procedure there.

It must NOT flag:

- an exact annotation on the line immediately before a script
  `:PROCEDURE` — the effective placement (`visibility_annotation_usage`
  speaks for it in the info tier);
- comments that are not annotation-shaped, including other `/*@...;`
  markers;
- a near-miss spelling that is not followed by a `:PROCEDURE` — ordinary
  comment text;
- an annotation followed by a **non-keyword** token (an expression
  statement) or by nothing at all (end of file). Annotation-style
  comments are also used as prose markers, and flagging those would be
  noise.

## Examples

### Flags

```ssl
:CLASS Widget;

/*@private;
:PROCEDURE Render;
:ENDPROC;
```

### Flags

```ssl
/*@private;
:DECLARE nCount;

:PROCEDURE Helper;
:ENDPROC;
```

### Does not flag

```ssl
/*@private;
:PROCEDURE Helper;
:ENDPROC;
```

### Flags

```ssl
/*@Private;
:PROCEDURE Helper;
:ENDPROC;
```

### Flags

```ssl
/* @private;
:PROCEDURE Helper;
:ENDPROC;
```

### Flags

```ssl
/*@private;

:PROCEDURE Helper;
:ENDPROC;
```

### Flags

```ssl
/*@private;
/* Helper does the work;
:PROCEDURE Helper;
:ENDPROC;
```

### Does not flag

```ssl
/* regular comment;
:DECLARE nCount;
```

### Does not flag

```ssl
/* @private;
:DECLARE nCount;
nCount := 1;
/*@private;
nCount := 2;
```

## Rationale

Visibility annotations are load-bearing security surface — they are what
keeps a helper procedure out of DoProc/ExecFunction reach — and they fail
silently: misplaced, misspelled, detached by a blank line, or used in a
class, the comment is ordinary text and the procedure stays public with
nothing to say so. The style guide states each failure mode
(`visibility_annotation_form`, `visibility_annotation_exact_form`,
`visibility_annotations_script_only`, all authoritative, warning), so the
authority mirrors them. Every flag path is a warning: the annotation is
inert, but the code still runs. Near misses are reported only before a
`:PROCEDURE`, where the intent is unmistakable; elsewhere an
annotation-shaped comment may be prose, as the last fence pins.
