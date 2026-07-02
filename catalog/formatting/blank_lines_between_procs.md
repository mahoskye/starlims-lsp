---
id: fmt.blank_lines_between_procs
title: Blank lines between procedures
kind: formatter
status: active
authority: style_only
schema_ref: null
config:
  - ssl.format.blankLinesBetweenProcs
tests:
  - internal/providers/formatting_test.go
history:
  - date: 2026-01-10
    ref: "v0.1.0 initial release"
    note: >-
      Part of the original document formatter; blank-line count exposed as
      ssl.format.blankLinesBetweenProcs (default 1).
issues: ["#33"]
---

## Behavior

When a `:PROCEDURE` (or `:REGION`) keyword follows a `:ENDPROC;` (or
`:ENDREGION;`), the formatter emits `ssl.format.blankLinesBetweenProcs`
additional newlines (default 1) at the opening keyword. This guarantees at
least one blank line between procedures, but it is additive, not
normalizing: the streaming formatter separately preserves up to one blank
line from the source, so procedures that are already separated by a blank
line come out separated by two.

A standalone comment between `:ENDPROC;` and the next `:PROCEDURE` does not
suppress the insertion; the extra blank line lands between the comment and
the `:PROCEDURE` line, so the comment visually attaches to the procedure
above it (see Known gaps).

## Examples

Procedures butted together gain one blank line:

### Before

```ssl
:PROCEDURE First;
:ENDPROC;
:PROCEDURE Second;
:ENDPROC;
```

### After

```ssl
:PROCEDURE First;
:ENDPROC;

:PROCEDURE Second;
:ENDPROC;
```

Procedures already separated by one blank line gain a second (actual,
additive behavior):

### Before

```ssl
:PROCEDURE First;
:ENDPROC;

:PROCEDURE Second;
:ENDPROC;
```

### After

```ssl
:PROCEDURE First;
:ENDPROC;


:PROCEDURE Second;
:ENDPROC;
```

A doc comment for the next procedure gets separated from it (actual
behavior; the intent is the opposite — see Known gaps):

### Before

```ssl
:PROCEDURE First;
:ENDPROC;
/* Documentation for Second;
:PROCEDURE Second;
:ENDPROC;
```

### After

```ssl
:PROCEDURE First;
:ENDPROC;
/* Documentation for Second;

:PROCEDURE Second;
:ENDPROC;
```

## Rationale

Procedure boundaries are the file's primary structure; consistent separation
makes them scannable (style_only, configurable count). The additive quirk is
stable under repeated formatting (two blank lines is the fixed point, since
the formatter preserves at most one source blank line and adds one), but it
means the option does not deliver "exactly N blank lines" — that and the
comment-attachment behavior are recorded below as gaps rather than intent.

## Known gaps

- The setting reads as "ensure exactly N blank lines between procedures",
  but the implementation adds N newlines on top of whatever (capped-at-one)
  blank line survives from the source. Already-separated procedures end up
  with two blank lines instead of one.

### Before

```ssl expect=fail
:PROCEDURE First;
:ENDPROC;

:PROCEDURE Second;
:ENDPROC;
```

### After

```ssl
:PROCEDURE First;
:ENDPROC;

:PROCEDURE Second;
:ENDPROC;
```

- A leading doc comment should stay attached to the procedure it documents,
  with the separating blank line placed above the comment.

### Before

```ssl expect=fail
:PROCEDURE First;
:ENDPROC;
/* Documentation for Second;
:PROCEDURE Second;
:ENDPROC;
```

### After

```ssl
:PROCEDURE First;
:ENDPROC;

/* Documentation for Second;
:PROCEDURE Second;
:ENDPROC;
```
