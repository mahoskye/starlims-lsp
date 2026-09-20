---
id: diag.unterminated_string
title: String literal never closed
kind: diagnostic
status: active
authority: authoritative
schema_ref: null
default_severity: error
severity_overridable: true
suppressible: true
tests:
  - internal/lexer/lexer_test.go
  - internal/providers/unterminated_string_test.go
history:
  - date: 2026-09-20
    ref: "issue #240 (follow-up to diag.unexpected_token)"
    note: >-
      Specified after #240: the apostrophe in "doesn't" opened a
      single-quoted string that ran to end of file, hiding the `.` that
      unknown_token would have caught and every statement after it, and
      nothing said so — the lexer's readString returns a string token
      whether or not it saw the closer. Measured over the production
      corpus before writing: 143,861 string literals in 4,620 scripts, of
      which 18,006 close across a line break and 0 never close; 6,403 in
      the 751 SSL-mode data sources, 1,320 across lines, 1 never closed —
      a retired data source whose SQL is wrapped in a C-style comment
      block, routed to SSL by the pipeline's unterminated-leading-comment
      rule, already reported by other rules for the same reason. So:
      strings span lines, which rules out cutting an unterminated one at
      the line break; the opener is the single fix location; and the rule
      is quiet on code that compiles.
  - date: 2026-09-20
    ref: "issue #240 (implementation)"
    note: >-
      Implemented as the lexer's verdict: readString now sets
      Token.Unterminated when it reaches end of input without the closer,
      and checkUnterminatedStrings reports on the opener. No data-source
      gate — the pipeline's own SQL-mode routing decides what is lexed.
issues: ["#240"]
---

## Behavior

Flags a string literal that reaches end of file without its closing
delimiter. SSL strings open with `"`, `'`, or `[` and end at the matching
`"`, `'`, or `]`. They may span lines — a SQL statement laid out across
several lines is the everyday case — so a string with no closer swallows
everything after its opener. The range covers the opening delimiter, the
one place the fix goes, and the message names the closer that never came
(`Unterminated string literal - expected a closing '"' before end of
file`).

Delimiters:

- `"` and `'` close at the next same character. The other quote kind
  inside is content (`"it's"`, `'say "hi"'`).
- `[` opens a string only where an array subscript cannot stand: after a
  keyword, an operator, an opener, a comma, or at statement start. After
  an identifier, a number, a string, `)`, or `]` it is a subscript, and an
  unclosed subscript belongs to `unclosed_delimiter`. A bracket string
  allows one nested `[...]` span (`[[a]b]` is the string `[a]b`) and the
  outer `]` closes it, so `[[a]` is unterminated even though its text ends
  in `]`. The lexer, which knows whether it saw the closer, is the
  authority — not the token's last character.
- There are no escape sequences. A backslash is content, and a doubled
  quote is two adjacent strings (`unexpected_token`'s business), never an
  escaped quote.

One diagnostic per unterminated string, and since the string runs to end
of file, at most one per document. Nothing after the opener is lexed as
code: statements inside the swallowed text are not checked, and a block
whose closer sits inside it is still reported by `unclosed_block` on its
opener. That is a consequence of the one cause and resolves with the one
fix, exactly as an unterminated comment (`comment_termination`) behaves.
There is no line-bounded recovery, because strings legitimately span
lines.

It applies to whatever the pipeline lexes as SSL: a script, an SSL-mode
data source in full, a hybrid data source's directive header. A SQL-mode
body is never lexed (feature.diagnostics_pipeline) and cannot flag.

It must NOT flag:

- a closed string, however many lines it spans;
- a quote character inside a comment (`/* don't;`) — comments are read
  before strings and their text is literal (DECISIONS.md D7);
- a quote of the other kind inside a string;
- a bracket string with one nested span, closed;
- an array subscript, closed (`aItems[1]`) or not (`aItems[1;`, which is
  `unclosed_delimiter`'s);
- two adjacent strings (`"a""b"`).

## Examples

### Flags

```ssl
:DECLARE sText;
sText := "never closed;
```

### Flags

```ssl
:DECLARE nCount;
nCount := 1;
This is a bunch of text, but it doesn't evaluate as wrong.
nCount := 2;
```

### Flags

```ssl
:DECLARE sName;
sName := 'never closed;
```

### Flags

```ssl
:DECLARE sSql;
sSql := [select 1 from dual;
```

### Flags

```ssl
:DECLARE sText;
sText := [[a]
```

### Does not flag

```ssl
:DECLARE sSql, aRows;
sSql := "select ITEMID, DISPLAYTEXT
	from ITEMS
	where DONE = '0'";
aRows := SQLExecute(sSql);
```

### Does not flag

```ssl
:DECLARE sA, sB, sC;
/* don't flag a quote in a comment;
sA := "it's";
sB := 'say "hi"';
sC := [it's "quoted" [and nested]];
```

### Does not flag

```ssl
:DECLARE aItems, sFirst, sBoth;
sFirst := aItems[1];
sBoth := "a""b";
```

### Does not flag

```ssl
:DECLARE aItems, sFirst;
sFirst := aItems[1;
```

## Rationale

The grammar requires the closer, and the failure mode is silent
destruction: from the opener to end of file, nothing is code any more, so
every later mistake disappears along with every later statement. In #240
that is exactly what hid the issue's own example — the apostrophe in
"doesn't" swallowed the trailing `.` that `unknown_token` would have
reported, and the file came back clean. An error on the opener is the
only honest report, and the opener is the only place the fix goes, which
is also why `comment_termination` anchors an unterminated comment where
it opens.

The corpus settled two design questions. Strings span lines — 18,006 of
the 143,861 literals in production scripts do — so the lexer cannot end
an unterminated string at the line break and let later lines recover;
the swallow is the language's, not the tool's, and the entry says so
rather than promising a recovery it cannot deliver. And the rule is
quiet on code that compiles: zero unterminated strings in 4,620 scripts.

Exactness matters at one edge: a bracket string's text can end in `]`
without being closed (`[[a]`), so the implementation must carry the
lexer's verdict — it alone knows whether the closer was seen — rather
than inspect the token's last character. The fifth Flags fence pins that.
