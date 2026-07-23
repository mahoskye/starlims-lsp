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
  - date: 2026-07-02
    ref: "issue #36"
    note: >-
      Standalone comments are now indented at the enclosing block depth
      like statements; previously the indent writer skipped comment tokens,
      flushing every standalone comment to column 0.
  - date: 2026-07-22
    ref: "issue #89"
    note: >-
      Continuation anchoring unified as lexical (statement line + 1) for
      all continuation forms, including lines following a trailing ':='
      or binary operator.
  - date: 2026-07-22
    ref: "issue #86"
    note: >-
      Expression continuations that begin with a binary operator take the
      same one-level extra indent the line wrapper emits; previously the
      wrapped form lost its indent level on the next format pass.
issues: ["#36", "#86"]
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

Continuation lines — inside an unclosed `(`/`{`/`[`, beginning with a
binary operator, or following a line that ended in `:=` or a binary
operator — sit exactly one level past the line that opened the statement
(issues #86/#89). The anchor is lexical, not block depth: an `:IF`
condition's continuation indents one level past the `:IF` line even though
the body will indent further. The extra level is fixed, never proportional
to nesting depth, and a closing delimiter that leads a line aligns with
the statement line itself.

Standalone comments are indented at the enclosing block depth like
statements. Only the first line of a multi-line comment is indented:
content *inside* a multi-line comment is preserved verbatim (modulo
trailing-whitespace trimming, see fmt.trim_trailing_whitespace).
End-of-line comments stay attached to their statement's line and are not
separately indented.

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

A comment inside a procedure is indented with the code it sits in, even
when the source left it at column 0:

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
`continuation_indent: 1`. Until issue #36 the indent writer skipped comment
tokens entirely, flushing standalone comments to column 0; comments
document the code they sit next to, so they take the same block indent as
statements.
