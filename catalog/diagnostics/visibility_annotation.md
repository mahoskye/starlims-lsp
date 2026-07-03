---
id: diag.visibility_annotation
title: Misplaced or ineffective visibility annotation
kind: diagnostic
status: active
authority: tool
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
issues: []
---

## Behavior

Validates the visibility annotations `/*@private;` and `/*@protected;`
(content matched case-insensitively; the comment must literally start
`/*@` — `/* @private;` with a space is not an annotation and is ignored
entirely). Two situations flag, both warnings, range on the comment:

- **Inside a class** (anywhere after the `:CLASS` keyword): the annotation
  has no effect — class methods are always Public|Virtual (ssl-style-guide
  `procedures.visibility_annotations.class_restriction`) — and the message
  says so.
- **In a script**, when the next significant token is a colon keyword
  other than `:PROCEDURE` (e.g. `:DECLARE`): the annotation is not in its
  required position — its own line immediately before `:PROCEDURE` — so
  it will not attach to anything.

It must NOT flag:

- an annotation immediately preceding `:PROCEDURE` in a script — the
  correct, effective placement (comments and whitespace between them are
  skipped);
- comments that are not visibility annotations, including other `/*@...;`
  markers and near-miss spellings that fail the exact `/*@` +
  private/protected match;
- a script annotation followed by a **non-keyword** token (an expression
  statement) or by nothing at all (end of file). The check only treats a
  following colon keyword as proof of misplacement; anything else is left
  alone deliberately — annotation-style comments are also used as prose
  markers, and flagging those would be noise.

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

### Does not flag

```ssl
/* regular comment;
:DECLARE nCount;
```

## Rationale

Visibility annotations are load-bearing security surface — they are what
keeps a helper procedure out of DoProc/ExecFunction reach — and they fail
silently when misplaced or used in a class, so the tool checks the two
provable failure modes. The style guide is authoritative on the underlying
rule, but it defines no lint slug for it; the check itself (which
positions count as misplaced, warning severity) is an LSP product
decision, hence `authority: tool` with `schema_ref: null`. Both flag paths
share warning severity: the annotation is inert in each case, but the code
still runs.
