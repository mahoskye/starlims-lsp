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
  - date: 2026-07-23
    ref: "issue #101"
    note: >-
      A statement following a standalone comment on its line is split to
      its own line; previously it stayed glued after the comment text.
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
  - date: 2026-09-20
    ref: "issue #240 follow-up"
    note: >-
      A line beginning with a member-access `:` (a call chain continued
      from the previous line) is a continuation and takes the same one
      extra level; the lexer now reads that colon as member access rather
      than a keyword, which used to make the formatter treat the line as
      a new statement.
  - date: 2026-10-13
    ref: "issue #255 (catalog audit)"
    note: >-
      Behavior text corrected: :REGION bodies are opaque and pass through
      verbatim (issue #164) rather than being indented; an :IF
      condition's continuation sits at the body's column; the configured
      style is applied whatever the file's existing indentation. A line
      that is the operand of a keyword ending the previous line
      (`:RETURN ⏎ nValue;`) is now a continuation too.
  - date: 2026-10-13
    ref: "ssl-style-guide#120"
    note: >-
      A wrapped declaration list (:DECLARE, :PARAMETERS, :PUBLIC) aligns
      its hanging lines with the list's first item, per the style guide's
      wrapped_list_alignment; it used to take the one-level continuation
      indent like any other statement.
issues: ["#36", "#86", "#101"]
---

## Behavior

Code lines are re-indented one level per enclosing block (`:PROCEDURE`,
`:IF`, `:WHILE`, `:FOR`, `:BEGINCASE`, `:TRY`, `:BEGININLINECODE`; middle
keywords `:ELSE`, `:CASE`, `:OTHERWISE`, `:CATCH`, `:FINALLY` dedent
themselves and indent their content; the scope-based `:ERROR` / `:RESUME`
also indent their bodies). A `:REGION` body is opaque payload and passes
through byte-for-byte (issue #164); only its `:REGION` / `:ENDREGION` lines
are indented. With the default `ssl.format.indentStyle: "tab"` each level
is one tab character; with `"space"` each level is `ssl.format.indentSize`
spaces (default 4). The configured style is applied whatever indentation
the file already uses — there is no detection of an existing
space-indented file. For line-length accounting a tab is counted as
`indentSize` columns.

Continuation lines — inside an unclosed `(`/`{`/`[`, beginning with a
binary operator or with a member-access `:` continuing a call chain,
following a line that ended in `:=`, a comma or a binary operator, or
supplying the operand of a keyword that ended the previous line
(`:RETURN ⏎ nValue;`, fmt.semicolon_enforcement) — sit exactly one level
past the line that opened the statement (issues #86/#89; the member-access
form is the issue #240 follow-up; the keyword-operand form is issue #255).
The anchor is lexical, not block depth: an `:IF` condition's continuation
indents one level past the `:IF` line — the same column as the body; the
leading operator is what distinguishes it. The extra level is fixed, never proportional
to nesting depth, and a closing delimiter that leads a line aligns with
the statement line itself.

A declaration list is the exception (style guide wrapped_list_alignment,
ssl-style-guide#120): every hanging line of a `:DECLARE`, `:PARAMETERS` or
`:PUBLIC` statement whose first item shares the keyword's line aligns with
that first item — the statement's indent in the configured style, then
spaces up to the item's column (9 for `:DECLARE`, 12 for `:PARAMETERS`, 8
for `:PUBLIC`). A list that starts on the line after its keyword has no
first item to align with and keeps the rules above.

Standalone comments are indented at the enclosing block depth like
statements. Only the first line of a multi-line comment is indented:
content *inside* a multi-line comment is preserved verbatim (modulo
trailing-whitespace trimming, see fmt.trim_trailing_whitespace).
End-of-line comments stay attached to their statement's line, separated
by exactly two spaces regardless of the source spacing, and are not
separately indented. A statement written on the same line *after* a
standalone comment moves to its own line — code must not hide behind a
comment (issue #101).

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

### Before

```ssl
:DECLARE txt, sTitle, sUser, sOut;
sOut := txt:ToString()
:Replace("##TITLE##", sTitle)
:Replace("##USER##", sUser);
```

### After

```ssl
:DECLARE txt, sTitle, sUser, sOut;
sOut := txt:ToString()
	:Replace("##TITLE##", sTitle)
	:Replace("##USER##", sUser);
```

A wrapped `:IF` condition anchors on the `:IF` line, so its continuation
shares the body's column:

### Before

```ssl
:PROCEDURE Demo;
:IF bAlphaConditionValue .AND. oConfigurationManager:databaseSettings:connectionIsAvailable;
nValue := 1;
:ENDIF;
:ENDPROC;
```

### After

```ssl
:PROCEDURE Demo;
	:IF bAlphaConditionValue
		.AND. oConfigurationManager:databaseSettings:connectionIsAvailable;
		nValue := 1;
	:ENDIF;
:ENDPROC;
```

A wrapped declaration list aligns its hanging lines with the first item:

### Before

```ssl
:PROCEDURE Demo;
:DECLARE sFirstName, sLastName, sMiddleName,
sSuffix, sTitle;
:ENDPROC;
```

### After

```ssl
:PROCEDURE Demo;
	:DECLARE sFirstName, sLastName, sMiddleName,
	         sSuffix, sTitle;
:ENDPROC;
```

A `:REGION` body is opaque and keeps its own layout:

### Idempotent

```ssl
:PROCEDURE Demo;
	:REGION Payload;
SELECT a FROM t
  WHERE b = 1
	:ENDREGION;
:ENDPROC;
```

## Rationale

Tabs-by-default matches the style guide's indentation guidance while
letting space-standardized teams configure width (style_only). The fixed
single-level continuation indent follows the schema's
`continuation_indent: 1`; declaration lists follow the style guide's
`wrapped_list_alignment` instead, so the hanging names read as one list
rather than as statements of their own. Until issue #36 the indent writer skipped comment
tokens entirely, flushing standalone comments to column 0; comments
document the code they sit next to, so they take the same block indent as
statements.
