---
id: fmt.indent_style
title: Indentation style and width
kind: formatter
status: active
authority: style_only
schema_ref: null
config:
  - ssl.format.indentStyle
  - ssl.format.indentSize
tests:
  - internal/providers/formatting_test.go
history:
  - date: 2026-01-10
    ref: "v0.1.0 initial release"
    note: >-
      Part of the original document formatter; tab default matches the
      style guide's tabs-preferred guidance.
issues: ["#36"]
---

## Behavior

Code lines are re-indented one level per enclosing block (`:PROCEDURE`,
`:IF`, `:WHILE`, `:FOR`, `:BEGINCASE`, `:TRY`, `:REGION`,
`:BEGININLINECODE`; middle keywords `:ELSE`, `:CASE`, `:OTHERWISE`,
`:CATCH`, `:FINALLY` dedent themselves and indent their content; the
scope-based `:ERROR` / `:RESUME` also indent their bodies). With the
default `ssl.format.indentStyle: "tab"` each level is one tab character;
with `"space"` each level is `ssl.format.indentSize` spaces (default 4).
For line-length accounting a tab is counted as `indentSize` columns.

Continuation lines inside an unclosed `(`/`{`/`[` get one fixed extra level
(not proportional to nesting depth).

Standalone comments are NOT indented: the formatter writes comment tokens
without leading indentation, so a comment inside a block lands at column 0
(actual behavior — see Known gaps). Content *inside* a multi-line comment is
preserved verbatim (modulo trailing-whitespace trimming, see
fmt.trim_trailing_whitespace).

## Examples

### Before

```ssl
:PROCEDURE Demo;
:DECLARE sName;
:IF bReady;
sName := "x";
:ELSE;
sName := "y";
:ENDIF;
:ENDPROC;
```

### After

```ssl
:PROCEDURE Demo;
	:DECLARE sName;
	:IF bReady;
		sName := "x";
	:ELSE;
		sName := "y";
	:ENDIF;
:ENDPROC;
```

A comment inside a procedure is written at column 0 even when the source
indented it (actual behavior):

### Before

```ssl
:PROCEDURE Demo;
	/* explains the assignment;
	nValue := 1;
:ENDPROC;
```

### After

```ssl
:PROCEDURE Demo;
/* explains the assignment;
	nValue := 1;
:ENDPROC;
```

### Space mode (illustration)

With `"ssl.format.indentStyle": "space"` the first example would indent
with four spaces per level instead (illustration only; the executable
fences above run with the default tab style):

```text
:PROCEDURE Demo;
    :DECLARE sName;
    ...
:ENDPROC;
```

## Rationale

Tabs-by-default matches the style guide's indentation guidance while
letting space-standardized teams configure width (style_only). The fixed
single-level continuation indent follows the schema's
`continuation_indent: 1`.

## Known gaps

- Standalone comments should be indented with the code they sit in;
  today `writeIndentIfNeeded` skips comment tokens entirely, flushing every
  standalone comment to column 0.

### Before

```ssl expect=fail
:PROCEDURE Demo;
	/* explains the assignment;
	nValue := 1;
:ENDPROC;
```

### After

```ssl
:PROCEDURE Demo;
	/* explains the assignment;
	nValue := 1;
:ENDPROC;
```
