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
  - date: 2026-10-13
    ref: "issue #255 (catalog audit)"
    note: >-
      Mixed boundaries (:ENDPROC; then :REGION, :ENDREGION; then
      :PROCEDURE) are now normalized too; only same-kind pairs were,
      leaving a procedure and an adjacent region unseparated against the
      schema's blank_lines.before_procedure / before_region of 1.
  - date: 2026-10-13
    ref: "issue #326"
    note: >-
      A top-level :PROCEDURE or :REGION after script-level code or a
      class header gets the separating blank lines too; only the boundary
      after :ENDPROC;/:ENDREGION; used to be normalized, so a procedure
      directly after top-level statements stayed butted against them.
issues: ["#33", "#326"]
---

## Behavior

When a `:PROCEDURE` or `:REGION` keyword follows a `:ENDPROC;` or
`:ENDREGION;` — any of the four pairings — or opens a top-level unit
after any other code (script-level statements, a `:CLASS` header;
schema `blank_lines.before_procedure` / `before_region`, issue #326),
the formatter normalizes the separation at that boundary to exactly `ssl.format.blankLinesBetweenProcs`
blank lines (default 1) — whatever blank lines the source had there are
replaced, never stacked on. The boundary holds across intervening
statements that do not start with a keyword (a stray top-level assignment
between two procedures): the blank lines go directly above the
`:PROCEDURE` / `:REGION`. A setting of 0 disables the normalization
entirely (source whitespace at the boundary is left to
fmt.max_consecutive_blank_lines).

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

A procedure and an adjacent region are separated the same way, in either
order:

### Before

```ssl
:PROCEDURE First;
:ENDPROC;
:REGION Payload;
raw text
:ENDREGION;
:PROCEDURE Second;
:ENDPROC;
```

### After

```ssl
:PROCEDURE First;
:ENDPROC;

:REGION Payload;
raw text
:ENDREGION;

:PROCEDURE Second;
:ENDPROC;
```

A procedure after script-level code is a boundary too (issue #326):

### Before

```ssl
:DECLARE nCount;
nCount := 1;
:PROCEDURE Demo;
:RETURN nCount;
:ENDPROC;
```

### After

```ssl
:DECLARE nCount;
nCount := 1;

:PROCEDURE Demo;
	:RETURN nCount;
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
