---
id: fmt.blank_lines_between_sections
title: Blank line between logical sections
kind: formatter
status: active
authority: style_only
schema_ref: null
config:
  - ssl.format.blankLinesBetweenSections
tests:
  - internal/providers/formatting_sections_test.go
history:
  - date: 2026-10-14
    ref: "formatter preview review"
    note: >-
      Introduced default-on. Formatted code read as a wall: the formatter
      kept whatever blank lines the source had and only separated
      procedures and back-to-back blocks. The style guide's
      blank_line_between_sections rule (one blank line between logical
      sections) was left to reviewers; the formatter now applies it at the
      boundaries the rule's example shows, plus before a section comment.
issues: []
---

## Behavior

With `ssl.format.blankLinesBetweenSections` on (default), a post-format
pass inserts one blank line at each boundary between logical sections of a
body — a procedure, a block's body, a branch, or top-level script code:

- **after the declarations**: between a `:PARAMETERS`, `:DEFAULT`,
  `:DECLARE`, `:PUBLIC` or `:INCLUDE` statement and the first following
  statement that is not one (consecutive declarations stay together, and a
  comment between two declarations stays with them);
- **before a control block**: before an `:IF`, `:WHILE`, `:FOR`,
  `:BEGINCASE` or `:TRY` that follows code;
- **after a control block**: after `:ENDIF`, `:ENDWHILE`, `:NEXT`,
  `:ENDCASE` or `:ENDTRY` when more statements follow;
- **before a section comment**: before a standalone comment that follows
  code, so a comment heading opens its section and stays attached to the
  code below it (a comment directly above a block or a `:RETURN` takes the
  blank line, not the gap between comment and code);
- **before the final `:RETURN`**: before a procedure's last `:RETURN` (the
  one followed by `:ENDPROC`, or ending the script) when code precedes it.

Nothing is inserted:

- at the start of a body (after `:PROCEDURE`, an opener, `:ELSE`, `:CASE`,
  `:CATCH`, `:ERROR` and the like) or at its end (before a closer, a middle
  keyword, `:EXITCASE` or `:RESUME`);
- where any blank line already separates the two lines;
- between lines at different indentation (a malformed mix);
- inside a multi-line string, comment, code block or `:REGION` body, or
  between a statement and its own continuation lines.

The pass only adds blank lines; it never removes the author's.
fmt.max_consecutive_blank_lines caps long runs. Procedure boundaries keep
their own normalization (fmt.blank_lines_between_procs), and adjacent
blocks are also fmt.blank_line_between_blocks' boundary; where two rules
meet, one blank line results. Range formatting does not run this
whole-document pass (feature.formatting).

## Examples

The style guide's example: declarations, a statement, a loop and the
final return each become their own section:

### Before

```ssl
:PROCEDURE Total;
:PARAMETERS aItems;
:DECLARE nTotal, i;
nTotal := 0;
:FOR i := 1 :TO ALen(aItems);
nTotal := nTotal + aItems[i];
:NEXT;
:RETURN nTotal;
:ENDPROC;
```

### After

```ssl
:PROCEDURE Total;
	:PARAMETERS aItems;
	:DECLARE nTotal, i;

	nTotal := 0;

	:FOR i := 1 :TO ALen(aItems);
		nTotal := nTotal + aItems[i];
	:NEXT;

	:RETURN nTotal;
:ENDPROC;
```

A comment heading opens its section, and a block's body is not split:

### Before

```ssl
:PROCEDURE Apply;
:DECLARE nTotal;
nTotal := 100;
/* apply the discount;
:IF nTotal > 50;
nTotal := nTotal * 0.9;
nTotal := Round(nTotal, 2);
:ENDIF;
:ENDPROC;
```

### After

```ssl
:PROCEDURE Apply;
	:DECLARE nTotal;

	nTotal := 100;

	/* apply the discount;
	:IF nTotal > 50;
		nTotal := nTotal * 0.9;
		nTotal := Round(nTotal, 2);
	:ENDIF;
:ENDPROC;
```

Formatted output stays as it is: consecutive declarations keep a comment
between them, and a block's last statement does not gain a line before the
closer:

### Idempotent

```ssl
:PROCEDURE Load;
	:PARAMETERS sKey;
	:DEFAULT sKey, "";
	/* working values;
	:DECLARE aRows, nCount;

	aRows := {};
	nCount := 0;

	:WHILE nCount < 3;
		nCount += 1;
	:ENDWHILE;
:ENDPROC;
```

## Rationale

The style guide asks for one blank line between logical sections and shows
these boundaries in its example; it is a layout rule (style_only), so the
formatter may apply it mechanically and a setting turns it off. Only adding
lines keeps the author's own grouping intact, and anchoring every boundary
to the statement structure rather than raw text keeps continuation lines
and literal text out of it.
