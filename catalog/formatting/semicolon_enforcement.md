---
id: fmt.semicolon_enforcement
title: Statement semicolon enforcement
kind: formatter
status: active
authority: authoritative
schema_ref: null
config:
  - ssl.format.semicolonEnforcement
tests:
  - internal/providers/formatting_test.go
history:
  - date: 2025-11-19
    ref: "vs-code-ssl-formatter v1.1.0, issues #3/#26"
    note: >-
      Multi-line expressions continuing with logical operators or inside
      unclosed brackets must NOT get a semicolon appended mid-expression.
  - date: 2026-01-10
    ref: "v0.1.0 initial release"
    note: LSP formatter carries the same continuation-aware enforcement.
  - date: 2026-07-02
    ref: "issue #38"
    note: >-
      Enforcement now also runs at end-of-file, so a final statement with
      no trailing newline gets its semicolon.
  - date: 2026-07-22
    ref: "issue #89"
    note: >-
      Continuation lines after a trailing operator now take one level of
      continuation indent (see fmt.indent_style); the example here shows
      the indented form.
  - date: 2026-08-28
    ref: "issue #216 (production-corpus formatting review residual)"
    note: >-
      Declaration keywords (:PARAMETERS, :DECLARE, :PUBLIC, :DEFAULT,
      :INCLUDE, :INHERIT) ending a line no longer receive a forced
      semicolon — the operand list on the following lines is the same
      statement; the forced ';' truncated it (5 default_after_parameters
      errors materialized on one corpus file).
  - date: 2026-09-20
    ref: "issue #240 follow-up"
    note: >-
      A call chain continued on the next line with the colon leading it
      no longer gets a `;` forced after the receiver line: the lexer now
      reads that colon as member access. Previously `:Replace(...)` lexed
      as a keyword, so the formatter ended the statement before it and
      recased it to `:REPLACE`, splitting valid code into three broken
      statements.
  - date: 2026-10-13
    ref: "issue #255 (catalog audit)"
    note: >-
      An operand-taking keyword ending its line (`:IF`, `:WHILE`, `:CASE`,
      `:FOR`, `:RETURN`, ...) no longer gets a `;` when its operand starts
      the next line. The forced semicolon turned `:IF ⏎ bReady;` into a
      compile error and `:RETURN ⏎ nValue;` into a bare return — the same
      truncation #216 fixed for the declaration keywords. Behavior text
      now also lists the #99 postfix, :BEGINCASE and region-body rules.
  - date: 2026-10-13
    ref: "issue #326"
    note: >-
      A statement left without ';' before :ELSE, :CASE, :OTHERWISE,
      :CATCH or :FINALLY is now terminated. Those keywords were listed as
      continuations, so the formatter kept the missing ';' — a compile
      error — and range formatting, which terminated it, disagreed with a
      full format.
  - date: 2026-10-13
    ref: "issue #326"
    note: >-
      Stray semicolons stay on their statement's line; the statement
      break after a ';' used to put each extra ';' on a line of its own.
issues: ["#38", "#89", "#326"]
---

## Behavior

With `ssl.format.semicolonEnforcement` on (default), the formatter appends
a `;` at the end of a line that finishes a complete statement but lacks
one. A semicolon is only added when the line ends in statement content (an
identifier, number, string, code-block literal, closing delimiter, postfix
`++`/`--` (issue #99), or keyword) AND the next significant token starts a
new statement (an identifier or a statement-starting keyword).
`:BEGINCASE`, which takes no operand and is always followed by `:CASE` or
`:OTHERWISE`, always ends with a semicolon. Raw `:REGION` body text never
receives one (issue #164).

No semicolon is added when the expression continues past the line break:

- the line ends with an operator (including `:=`), a comma, or an opening
  delimiter;
- the line ends inside an unclosed `(` / `{` / `[`;
- the line ends with `:TO` / `:STEP`, or the next line starts with
  `:TO` / `:STEP` (a `:FOR` header split across lines). The middle
  keywords `:ELSE`, `:CASE`, `:OTHERWISE`, `:CATCH` and `:FINALLY` are not
  continuations: each begins a clause, so a statement before one is
  terminated (issue #326);
- the line ends with a declaration keyword (`:PARAMETERS`, `:DECLARE`,
  `:PUBLIC`, `:DEFAULT`, `:INCLUDE`, `:INHERIT`) — its operand list is on
  the following lines (issue #216);
- the line ends with a keyword that takes an operand (`:IF`, `:WHILE`,
  `:CASE`, `:FOR`, `:RETURN`, `:PROCEDURE`, `:CLASS`, `:LABEL`, `:REGION`,
  `:BEGININLINECODE`) and the next line does not start with a keyword —
  that line is the operand, and it takes one level of continuation indent
  (fmt.indent_style). When a keyword does follow, the operand really is
  absent and the statement is terminated as usual (issue #255);
- the next line starts with a member-access `:` continuing a call chain
  (`txt:ToString()` then `:Replace(...)`). The lexer reads that colon as
  member access, not a keyword, so the chain is one statement and its
  member names are never recased (issue #240 follow-up).

An empty statement — a stray `;` directly after a `;` — stays on its
statement's line (`:IF bReady;;` keeps both); the formatter neither
deletes it nor breaks before it, which used to turn `;;;` into lines
holding a lone `;` (issue #326).

Semicolons are never inserted inside strings or comments. The check runs at
every line break and at end-of-file, so a final statement with no trailing
newline is terminated the same way as one followed by a newline (the
continuation guards above apply identically: a document ending
mid-expression gets no semicolon).

## Examples

### Before

```ssl
:DECLARE nValue;
nValue := 1
:RETURN nValue;
```

### After

```ssl
:DECLARE nValue;
nValue := 1;
:RETURN nValue;
```

An expression continuing across lines (trailing `.AND.`, unclosed brace)
gets no mid-expression semicolon; the continuation line takes one level of
continuation indent (fmt.indent_style, issue #89):

### Before

```ssl
bResult := bFirst .AND.
bSecond;
```

### After

```ssl
bResult := bFirst .AND.
	bSecond;
```

### Before

```ssl
aList := {1,
2};
```

### After

```ssl
aList := {1,
	2};
```

A final statement with no trailing newline is terminated:

### Before

```ssl
nValue := 1
```

### After

```ssl
nValue := 1;
```

A declaration keyword ending its line takes its operand list from the
following lines — no semicolon is forced after the bare keyword, which
would truncate the statement and orphan the list (production-corpus
shape, issue #216 review residual):

### Before

```ssl
:PARAMETERS
chartNo, strRules, Mean
, STD, CV;
:DEFAULT chartNo, "";
```

### After

```ssl
:PARAMETERS
	chartNo, strRules, Mean
	, STD, CV;
:DEFAULT chartNo, "";
```

(The list's lines take one continuation level throughout, a comma-led
line included — fmt.indent_style, issue #326. The fence also pins the
no-forced-';' guarantee.)

An operand-taking keyword whose operand starts the next line keeps that
operand — a forced `;` would make `:IF` a compile error and `:RETURN` a
bare return (issue #255). A keyword on the next line means the operand is
absent, and the bare `:RETURN` is terminated:

### Before

```ssl
:PROCEDURE Demo;
:IF
bReady;
nValue := 1;
:ENDIF;
:RETURN
nValue;
:ENDPROC;
:PROCEDURE Bare;
:RETURN
:ENDPROC;
```

### After

```ssl
:PROCEDURE Demo;
	:IF
		bReady;
		nValue := 1;
	:ENDIF;
	:RETURN
		nValue;
:ENDPROC;

:PROCEDURE Bare;
	:RETURN;
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

A statement before a middle keyword is terminated — `:ELSE`, `:CASE`,
`:OTHERWISE`, `:CATCH` and `:FINALLY` each begin a clause (issue #326):

### Before

```ssl
:DECLARE bReady, sOut;
:IF bReady
sOut := "ready"
:ELSE
sOut := "waiting";
:ENDIF;
```

### After

```ssl
:DECLARE bReady, sOut;
:IF bReady;
	sOut := "ready";
:ELSE;
	sOut := "waiting";
:ENDIF;
```

Stray semicolons stay where they are (issue #326):

### Idempotent

```ssl
:DECLARE nCount;;
nCount := 1;;
```

## Rationale

Semicolon termination is SSL syntax (authoritative), so the formatter may
complete it mechanically — but the v1.1.0 continuation rules (history)
bound the enforcement: inserting a semicolon mid-expression changes program
meaning, which the formatter must never do. That is why enforcement keys on
both what the line ends with and what the next line starts with. The
end-of-file case (issue #38) closes the one hole in the trigger: enforcement
used to fire only on newline tokens, so a document whose last line had no
trailing newline kept its unterminated final statement.
