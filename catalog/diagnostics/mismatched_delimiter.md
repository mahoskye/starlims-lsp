---
id: diag.mismatched_delimiter
title: Closing delimiter does not match the open one
kind: diagnostic
status: active
authority: tool
schema_ref: null
default_severity: error
severity_overridable: true
suppressible: true
tests:
  - internal/providers/audit255_test.go
history:
  - date: 2026-01-10
    ref: "commit 442fa69 (initial commit)"
    note: Present since the first commit as part of the delimiter scanner.
  - date: 2026-04-30
    ref: "PR #3 (v0.4.0, commit d744511)"
    note: Stable diagnostic code assigned; behavior unchanged.
  - date: 2026-10-13
    ref: "issue #255"
    note: >-
      Catalog audit: one mistake, one finding. A closer that does not
      match the innermost opener now resolves three ways — a matching
      opener deeper in the stack reports the openers above it as
      unclosed_delimiter and resumes there (`Len(aV[1);`, missing `]`); a
      closer with no matching opener anywhere that is followed by the
      innermost opener's own closer is a stray extra, unmatched_delimiter
      with nothing popped (`{a, b}});`); otherwise it is a crossed pair,
      mismatched_delimiter, and the opener is popped (`(1 + 2];`). Each of
      the first two used to produce two errors.
issues: []
---

## Behavior

Part of the delimiter-pairing scanner (`CheckUnmatchedParens`, on by
default). Flags a closing `)`, `]`, or `}` punctuation token whose most
recently opened delimiter is of a different kind (e.g. a `)` closing a
`{`), when the closer is standing in for that opener's own closer: no
opener of the closer's kind is open anywhere, and the next significant
token is not the innermost opener's closer. The diagnostic is an error
ranged on the closing token, with a message naming the expected closer.
The wrong opener is popped from the stack so one mistake does not
cascade.

It must NOT fire when:

- delimiters are balanced and properly nested;
- a closer appears with no delimiter open at all, or is a stray extra
  closer immediately followed by the innermost opener's own closer
  (`DoProc("P", {a, b}});`) — that is `unmatched_delimiter`;
- a matching opener sits deeper in the stack (`Len(aV[1);`) — the openers
  above it are `unclosed_delimiter` and matching resumes at the match;
- an opener is never closed — that is `unclosed_delimiter`.

This code is reserved for the crossed-pair case. It must NOT fire when:

- delimiters are balanced and properly nested, including mixed nesting
  like `{aVals[1], (nA + nB)}`;
- a closer appears with no delimiter open at all — that is
  `unmatched_delimiter`;
- an opener is never closed — that is `unclosed_delimiter`;
- the braces belong to a code block literal (`{|x| x + 1}`), which the
  lexer consumes as a single token, or brackets open a string literal
  (`[text]`) — neither produces punctuation tokens for the scanner.

## Examples

### Flags

```ssl
aValues := {1, 2, 3);
```

### Does not flag

```ssl
nTotal := (nBase + nOffset) * aValues[2];
```

### Does not flag

```ssl
nTotal := nBase + nOffset);
```

### Does not flag

```ssl
DoProc("Calc", {nStart, nEnd});
```

### Does not flag

```ssl
:DECLARE aV, n;
n := Len(aV[1);
```

### Does not flag

```ssl
:DECLARE a, b;
DoProc("P", {a, b}});
```

## Rationale

A crossed delimiter pair can never compile, so error severity is warranted.
The entry's value is the boundary against its sibling codes — this code
means "both ends exist but cross", while `unmatched_delimiter` and
`unclosed_delimiter` cover the missing-end cases; the second Does-not-flag
fence pins that split. Popping the crossed opener (rather than leaving it)
keeps a single typo from producing a mismatch on every subsequent closer.
The last two fences pin the cases that are someone else's (issue #255): a
missing inner closer is the unclosed opener's, and a stray extra closer is
the unmatched closer's — each used to add a mismatch on top.
