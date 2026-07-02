---
id: fmt.blank_lines_between_procs
title: Blank lines between procedures
kind: formatter
status: draft
authority: style_only
schema_ref: null
config:
  - ssl.format.blankLinesBetweenProcs
tests:
  - internal/providers/formatting_test.go
history: []
issues: []
---

## Behavior

The formatter ensures exactly `ssl.format.blankLinesBetweenProcs` blank
lines (default 1) between one procedure's `:ENDPROC;` and the next
`:PROCEDURE`. Comments attached above a procedure stay with the procedure
(the blank lines go above the comment block).

## Examples

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

## Rationale

Procedure boundaries are the file's primary structure; consistent separation
makes them scannable (style_only, configurable count).
