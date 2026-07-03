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
tests: []
history:
  - date: 2026-01-10
    ref: "commit 442fa69 (initial commit)"
    note: Present since the first commit as part of the delimiter scanner.
  - date: 2026-04-30
    ref: "PR #3 (v0.4.0, commit d744511)"
    note: Stable diagnostic code assigned; behavior unchanged.
issues: []
---

## Behavior

Part of the delimiter-pairing scanner (`CheckUnmatchedParens`, on by
default). Flags a closing `)`, `]`, or `}` punctuation token whose most
recently opened delimiter is of a different kind (e.g. a `)` closing a
`{`). The diagnostic is an error ranged on the closing token, with a
message naming the expected closer. The wrong opener is popped from the
stack so one mistake does not cascade.

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

## Rationale

A crossed delimiter pair can never compile, so error severity is warranted.
The entry's value is the boundary against its sibling codes — this code
means "both ends exist but cross", while `unmatched_delimiter` and
`unclosed_delimiter` cover the missing-end cases; the second Does-not-flag
fence pins that split. Popping the crossed opener (rather than leaving it)
keeps a single typo from producing a mismatch on every subsequent closer.
