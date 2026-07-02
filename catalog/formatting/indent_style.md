---
id: fmt.indent_style
title: Indentation style and width
kind: formatter
status: draft
authority: style_only
schema_ref: null
config:
  - ssl.format.indentStyle
  - ssl.format.indentSize
tests:
  - internal/providers/formatting_test.go
history: []
issues: []
---

## Behavior

Block contents are indented one level per enclosing block. Default is tabs
(`ssl.format.indentStyle: "tab"`); with `"space"`, each level is
`ssl.format.indentSize` spaces (default 4). Indentation inside multi-line
comments and string literals is literal text and is not adjusted.

## Examples

### Before

```ssl
:PROCEDURE Demo;
:DECLARE sName;
:IF bReady;
sName := "x";
:ENDIF;
:ENDPROC;
```

### After

```ssl
:PROCEDURE Demo;
	:DECLARE sName;

	:IF bReady;
		sName := "x";
	:ENDIF;
:ENDPROC;
```

## Rationale

Matches the style guide's tabs-preferred guidance (style_only — teams may
configure). The comment/string exclusion mirrors the "strings and comments
are literal" rule (see fmt.sql_in_strings for the sole exception).
