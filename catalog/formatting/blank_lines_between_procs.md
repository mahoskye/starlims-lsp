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
  - date: 2026-07-02
    ref: "issue #33"
    note: >-
      Separation is now normalized, not additive: exactly N blank lines at
      the boundary regardless of source whitespace, and the blank lines are
      placed above a doc-comment block attached to the next procedure
      instead of between the comment and its :PROCEDURE line.
issues: ["#33"]
---

## Behavior

When a `:PROCEDURE` (or `:REGION`) keyword follows a `:ENDPROC;` (or
`:ENDREGION;`), the formatter normalizes the separation at that boundary to
exactly `ssl.format.blankLinesBetweenProcs` blank lines (default 1) —
whatever blank lines the source had there are replaced, never stacked on. A
setting of 0 disables the normalization entirely (source whitespace at the
boundary is left to fmt.max_consecutive_blank_lines).

A standalone comment block immediately preceding the `:PROCEDURE` with no
blank line between the comments and the keyword is *attached* to that
procedure: the separating blank lines are placed above the comment block,
so a doc comment stays with the procedure it documents. A comment that is
already separated from the `:PROCEDURE` by a blank line is not attached;
normalization then applies between that comment and the `:PROCEDURE` line.

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

Procedures already separated by one blank line keep exactly one (the count
is normalized, not additive):

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

A doc comment attached to the next procedure stays attached; the blank line
goes above the comment block:

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
makes them scannable (style_only, configurable count). Until issue #33 the
insertion was additive — N newlines on top of whatever blank line survived
from the source — so already-separated procedures came out with two blank
lines, and the insertion point at the `:PROCEDURE` keyword detached leading
doc comments from their procedure. Normalizing at the boundary (and placing
the blank lines above an attached comment block) makes the setting deliver
"exactly N blank lines" and keeps documentation with its procedure.
