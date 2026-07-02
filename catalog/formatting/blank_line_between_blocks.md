---
id: fmt.blank_line_between_blocks
title: Blank line between sibling control-flow blocks
kind: formatter
status: draft
authority: tool
schema_ref: null
config:
  - ssl.format.blankLineBetweenBlocks
tests:
  - internal/providers/formatting_test.go
history:
  - date: 2026-05-14
    ref: "PR #20 (v0.7.6), issue #15"
    note: >-
      Introduced default-on: consecutive sibling blocks at the same indent
      read as one wall of code without separation.
issues: []
---

## Behavior

When two control-flow blocks (`:IF`, `:WHILE`, `:FOR`, `:BEGINCASE`,
`:TRY`) are siblings at the same indent level, the formatter inserts exactly
one blank line between the closing keyword of the first and the opening
keyword of the second. Default on (`ssl.format.blankLineBetweenBlocks`).
No blank line is inserted inside a block, before the first block in a
scope, or when a blank line is already present.

## Examples

### Before

```ssl
:PROCEDURE Demo;
:IF bFirst;
sResult := "one";
:ENDIF;
:IF bSecond;
sResult := "two";
:ENDIF;
:ENDPROC;
```

### After

```ssl
:PROCEDURE Demo;
	:IF bFirst;
		sResult := "one";
	:ENDIF;

	:IF bSecond;
		sResult := "two";
	:ENDIF;
:ENDPROC;
```

## Rationale

Issue #15: back-to-back `:ENDIF; :IF ...` at the same indent is hard to scan
in long procedures. One blank line makes each block a visual unit without
inviting arbitrary vertical whitespace (see fmt.max_consecutive_blank_lines).
