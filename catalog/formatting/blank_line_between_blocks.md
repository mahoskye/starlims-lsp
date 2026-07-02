---
id: fmt.blank_line_between_blocks
title: Blank line between sibling control-flow blocks
kind: formatter
status: active
authority: tool
schema_ref: null
config:
  - ssl.format.blankLineBetweenBlocks
tests:
  - internal/providers/formatting_test.go
history:
  - date: 2026-05-13
    ref: "PR #20 (v0.7.6), issue #15, vs-code-ssl-formatter#77"
    note: >-
      Introduced default-on: consecutive sibling blocks at the same indent
      read as one wall of code without separation.
issues: []
---

## Behavior

With `ssl.format.blankLineBetweenBlocks` on (default), a post-format pass
inserts exactly one blank line between two adjacent control-flow blocks at
the same indentation: the previous non-blank line must end an inner block
(`:ENDIF`, `:ENDWHILE`, `:NEXT`, `:ENDCASE`, `:ENDTRY`) and the current line
must open one (`:IF`, `:WHILE`, `:FOR`, `:BEGINCASE`, `:TRY`), with
identical leading indentation. No blank line is inserted:

- when any blank line already separates the two blocks;
- between a block and an ordinary statement on either side;
- between lines at different indent levels (a closer followed by its
  parent's closer, a nested opener);
- around `:PROCEDURE` / `:REGION` boundaries — those are handled by
  fmt.blank_lines_between_procs.

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

Already-separated siblings, statement/block adjacency, and closer-inside-
closer stay untouched:

### Idempotent

```ssl
:PROCEDURE Demo;
	:IF bFirst;
		sResult := "one";
	:ENDIF;

	:WHILE bLoop;
		sResult := "two";
	:ENDWHILE;
	nCount := 1;
:ENDPROC;
```

## Rationale

Issue #15 (vs-code-ssl-formatter#77): back-to-back `:ENDIF; :IF ...` at the
same indent is hard to scan in long procedures. One blank line makes each
block a visual unit without inviting arbitrary vertical whitespace (see
fmt.max_consecutive_blank_lines).
