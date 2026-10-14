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
  - date: 2026-10-13
    ref: "issue #326"
    note: >-
      A block nested in an :ERROR handler body keeps its depth: any block
      end used to close the handler scope too, so a nested :ENDIF and
      everything after it dedented one level too far. Only the end of the
      block holding the :ERROR closes it.
  - date: 2026-10-13
    ref: "issue #326"
    note: >-
      The invalid :ENDFOR closes its :FOR's indentation, as the validator
      pairs them (endfor_invalid reports it); it used to leave every
      following line, the next :PROCEDURE included, one level deep.
  - date: 2026-10-13
    ref: "issue #326"
    note: >-
      A declaration list that starts on the line after its keyword takes
      one continuation level throughout, and a comma-led line is a
      continuation; the list's first line used to sit at the keyword's
      depth and the rest one deeper.
  - date: 2026-10-14
    ref: "formatter preview review"
    note: >-
      Brackets align: a continuation inside an unclosed ( { [ whose first
      item shares the bracket's line aligns with that item; a bracket that
      ends its line hangs one level past the line it opened on, and a
      leading closer returns to that line's indentation. Continuations
      inside brackets used to sit one fixed level past the statement line,
      whatever the nesting.
issues: ["#36", "#86", "#101", "#326"]
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
binary operator, a comma (a comma-led list line, issue #326) or with a
member-access `:` continuing a call chain,
following a line that ended in `:=`, a comma or a binary operator, or
supplying the operand of a keyword that ended the previous line
(`:RETURN ⏎ nValue;`, or a declaration list after a bare `:PARAMETERS`,
`:DECLARE`, `:PUBLIC`, `:DEFAULT`, `:INCLUDE` or `:INHERIT` — issue #326;
fmt.semicolon_enforcement) — sit exactly one level
past the line that opened the statement (issues #86/#89; the member-access
form is the issue #240 follow-up; the keyword-operand form is issue #255).
The anchor is lexical, not block depth: an `:IF` condition's continuation
indents one level past the `:IF` line — the same column as the body; the
leading operator is what distinguishes it.

Brackets show their structure by alignment. Inside an unclosed `(`, `{`
or `[` whose first item shares the bracket's line, a continuation line
aligns with that first item — the opening line's indentation, then spaces
up to the item's column:

```text
:RETURN {sSampleNo, sMatrix, sSite,
         sRemarks};
```

A bracket that ends its line hangs: its items sit one level past the line
the bracket opened on, so nested hanging brackets step in one level each.
So does a bracket whose first item sits more than half the line limit
past its line's indentation (the alignment cap), or nested expressions
would march off to the right. After alignment spaces the extra level is
spaces, so a tab never follows a space. The innermost open bracket
decides. A closing delimiter that leads a line
returns to the indentation of the line its bracket opened on. The line
wrapper hangs a bracket — breaking right after it — only when one of its
items would not fit at the aligned column (fmt.max_line_length).

A declaration list is the exception (style guide wrapped_list_alignment,
ssl-style-guide#120): every hanging line of a `:DECLARE`, `:PARAMETERS` or
`:PUBLIC` statement whose first item shares the keyword's line aligns with
that first item — the statement's indent in the configured style, then
spaces up to the item's column (9 for `:DECLARE`, 12 for `:PARAMETERS`, 8
for `:PUBLIC`). A list that starts on the line after its keyword has no
first item to align with: all its lines take one continuation level.

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

A block nested in an `:ERROR` handler body keeps the handler's depth
(issue #326):

### Before

```ssl
:PROCEDURE Demo;
:DECLARE nCount;
:ERROR;
:IF nCount > 1;
nCount := 2;
:ENDIF;
nCount := 3;
:ENDPROC;
```

### After

```ssl
:PROCEDURE Demo;
	:DECLARE nCount;
	:ERROR;
		:IF nCount > 1;
			nCount := 2;
		:ENDIF;
		nCount := 3;
:ENDPROC;
```

The invalid `:ENDFOR` (reported by `endfor_invalid`) closes its `:FOR`'s
indentation, as the validator pairs them (issue #326):

### Before

```ssl
:PROCEDURE Demo;
:DECLARE i;
:FOR i := 1 :TO 3;
i := i;
:ENDFOR;
:ENDPROC;
```

### After

```ssl
:PROCEDURE Demo;
	:DECLARE i;
	:FOR i := 1 :TO 3;
		i := i;
	:ENDFOR;
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
