---
id: diag.comment_text_after_terminator
title: Comment terminated earlier than its layout suggests
kind: diagnostic
status: active
authority: authoritative
schema_ref: null
default_severity: error
severity_overridable: true
suppressible: true
tests:
  - internal/providers/comment_termination_test.go
  - internal/providers/fp_triage_304_test.go
history:
  - date: 2026-03-30
    ref: "commits 764a8de -> fe7a3aa -> 0d46f55"
    note: >-
      Tuning reversed three times in one day, settling on: same-line
      continuation warns; multi-line fires only on the bare-keyword signal.
  - date: 2026-04-XX
    ref: "issue #6, commits bccfe51/a501293/a99938d"
    note: >-
      False positive on benign comment-after-comment chains; suppressed when
      a paragraph break (blank line or standalone comment) separates the
      comment from the next code token.
  - date: 2026-06-XX
    ref: "issue #25 (open)"
    note: >-
      Dispute: the bare-keyword-only tuning misses a multi-line comment
      whose interior line ends in `;` when the orphaned lines are plain
      prose. Target semantics recorded under Known gaps; fix is a follow-up
      PR citing this entry.
  - date: 2026-07-02
    ref: "issue #25"
    note: >-
      Gap closed: fires (warning) when the next significant line — no
      paragraph break in between — begins with two consecutive bare
      identifiers, the signature of orphaned prose. Applies to multi-line
      comments AND to a comment whose ';' lands on its first line (the
      issue's original shape). Paragraph-break suppression (issue #6)
      still applies unchanged.
  - date: 2026-10-13
    ref: "issue #269"
    note: >-
      Severity re-checked against the style guide, which maps
      no_semicolon_in_comment_text (error) to this rule. Kept at warning: when the stranded text does
      not parse, unexpected_token already reports the compile error at
      error severity, and this rule explains its cause. When the text
      happens to parse, it is a detection heuristic, so error would
      overstate certainty about intent.
  - date: 2026-10-13
    ref: "issue #304"
    note: >-
      The bare-keyword break-out no longer fires when the keyword-named
      word is the target of an assignment on its line
      (`label := ...`). Shipped product code names a local after a
      keyword and assigns it on the line after a two-line comment;
      a keyword name followed by `:=` is a statement, not a doc header
      cut in half.
  - date: 2026-10-13
    ref: "issue #328"
    note: >-
      On a comment spanning lines the range now ends at the comment's last
      character, on its last line; it used to end on the first line at the
      start column plus the whole comment's length, past that line's end.
  - date: 2026-10-14
    ref: "issue #343"
    note: >-
      Reworked as an unexpected-next-token rule at error severity, the
      style guide's decision (no_semicolon_in_comment_text is error and
      the LSP follows). The LSP cannot see a semicolon "inside" a comment;
      it sees that the next token is unexpected. Stranded text that does
      not parse is now one error at the comment's `;`, and
      unexpected_token, unknown_token, and undeclared_variable defer inside
      that statement (it used to draw a warning here, an unexpected_token
      error, and one undeclared_variable per word). The same-line path is
      dropped: code after a comment's `;` that parses is valid SSL. A
      stranded doc line that parses (`Parameters: none` reads as member
      access) is caught after a single-line comment too. The orphaned-prose
      path is subsumed: two bare words never parse. authority corrected to
      authoritative: the style-guide record names this rule in
      enforced_by. The retired banner_has_no_semicolon is folded into
      no_semicolon_in_comment_text.
issues: []
---

## Behavior

An SSL comment runs from `/*` to its first `;`. When a `;` written as
punctuation ends a comment early, the rest of the comment text is read as
code. The tool cannot see a semicolon "inside" a comment, since the comment
simply ends there; what it sees is that the next token is unexpected. SSL
has no construct in which two bare words follow each other, so stranded
text almost never parses. This rule reports, as an error ranged on the
comment's `;`, a terminated `/*` comment followed with no paragraph break
by:

- **Stranded text that does not parse.** The statement that starts right
  after the comment fails the statement grammar at its first or second
  token — words that are not code (`then reset it`, a `*****` banner
  line) — and the failure is on the statement's first line, or the
  statement starts on the comment's own last line (`/* Set the count; then
  reset it;`). The message says the comment
  ended there and the rest is not SSL. Inside that statement
  `unexpected_token`, `unknown_token`, and `undeclared_variable` defer to
  this rule: one finding per cause.
- **A stranded doc line that parses.** The next line starts with an SSL
  keyword name or a doc-field name (`Parameters`, `Returns`, `Purpose`,
  `Description`, `Author`, `Notes`, …) followed by `:` and a space.
  `Parameters: none;` parses as member access, but member access is
  written with no space after the colon, so the line reads as a doc header
  cut in half. This applies whether the comment spans lines or not.
- **A bare-keyword break-out.** After a comment spanning several lines, the
  next token is an identifier named like an SSL keyword (`Parameters`,
  `Default`) that is not the target of an assignment (`label := ...` is a
  statement, issue #304).

Real code that is merely malformed is not blamed on the comment: a
statement that fails further along (an unclosed call after an ordinary
comment) or only on a later line (`nCount := 1` with its `;` missing before
the next statement) is `unexpected_token`'s to report. Separately
from comments, `undeclared_variable` is also silent inside any statement
that fails to parse on its first line (`check the totals;`), whose words
are not variables.

It must NOT flag when:

- code after the comment's `;` parses, on the same line or the next
  (`/* note; nX := 1;`) — that is valid SSL;
- a paragraph break (blank line or another standalone comment) separates the
  comment from the following code — the comment ended deliberately
  (issue #6);
- the comment is a `/* region` / `/* endregion` marker, whose `;` is
  intentional;
- the next statement starts with a colon keyword;
- the next line assigns a variable whose name happens to be a keyword
  name (`label := ...`) — that is a statement (issue #304);
- the next line is ordinary member access (`oRec:Name;`).

## Examples

### Flags

```ssl
:DECLARE nCount;
/* Set the count; then reset it;
nCount := 0;
```

### Flags

A banner's doc block, cut by a `;` in its text.

```ssl
/* ********************************************************************************
Description.. : Records a status change; for one sample.
Returns...... : {.T., NIL} on success
********************************************************************************;
:RETURN {.T., NIL};
```

### Flags

The stranded doc line parses as member access.

```ssl
:DECLARE nCount;
/*
Resets the count;
Parameters: none
;
nCount := 0;
```

### Flags

```ssl
/* Purpose: totals;
   Parameters: none;
```

### Flags

```ssl
/* Documentation for this script
Parameters are described here;
Default values apply to each one
;
```

### Flags

```ssl
/* This header explains the module
and this line accidentally ends with one;
so these words are now parsed as code
;
```

### Flags

```ssl
/* Client address: treat as a single composite object;
   if any component changes, the whole address is flagged;
```

### Flags

A `*****` banner line stranded after a comment.

```ssl
:DECLARE nX;
/* Totals for the day;
**********************
nX := 1;
```

### Does not flag

Valid code after the `;` on the same line.

```ssl
:DECLARE nX;
/* note; nX := 1;
```

### Does not flag

```ssl
/* explains the assignment below;
nCount := 1;
```

### Does not flag

```ssl
/* a multi-line note
that spans two lines;
nCount := 1;
DoProc("X");
```

### Does not flag

```ssl
/* a multi-line note
that spans two lines;
orphan
nCount := 1;
```

### Does not flag

```ssl
:PROCEDURE Demo;
	:DECLARE label;
	/* collect the request values
	before they are parsed;
	label := GetFromSession("Values");
	:RETURN label;
:ENDPROC;
```

### Does not flag

```ssl
:DECLARE oRec;
/* read the name;
oRec:Name;
```

### Does not flag

Malformed code after an ordinary comment is `unexpected_token`'s.

```ssl
:DECLARE sRow, aVals;
/* Convert the row count to a string;
sRow := AllTrim(Str(Len(aVals) + 1);
```

### Does not flag

```ssl
/* a multi-line note
that spans two lines;

these words follow a paragraph break
;
```

### Does not flag

```ssl
/* first comment block;

/* second comment block;
:PROCEDURE Foo;
:ENDPROC;
```

### Does not flag

```ssl
/* region Helpers;
:PROCEDURE Helper;
:ENDPROC;
/* endregion;
```

## Rationale

The style guide decides this rule's severity, and it set
`no_semicolon_in_comment_text` to error (issue #343). The framing that makes
error right is the parse: stranded comment text that does not parse cannot
compile, so reporting it once, at its cause, is better than three findings
for one mistake. Stranded text that does parse runs silently, and only two
shapes are reported there — a keyword or doc-field name followed by `: `,
and a keyword-named word after a multi-line comment — because both read as
a doc header and as nothing a developer writes as code. Valid code after a
comment's `;` on the same line used to warn; it is valid SSL, so it is no
longer this rule's concern. Every suppression above encodes a real false
positive that must never regress (issues #6, #304).
