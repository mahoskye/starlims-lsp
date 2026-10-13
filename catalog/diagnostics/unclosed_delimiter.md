---
id: diag.unclosed_delimiter
title: Opening delimiter never closed
kind: diagnostic
status: active
authority: tool
schema_ref: null
default_severity: error
severity_overridable: true
suppressible: true
tests:
  - internal/providers/providers_test.go
history:
  - date: 2026-01-10
    ref: "commit 442fa69 (initial commit)"
    note: >-
      checkUnmatchedDelimiters shipped in the initial commit: stack-based
      matching of () [] {} with three outcomes (unmatched, mismatched,
      unclosed).
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

Flags an opening `(`, `[`, or `{` that is never closed — one diagnostic
per unclosed opener, with the range on the opener itself (where the fix
belongs), not at end-of-file. Two paths emit it:

- **End of file**: every opener still open when the file ends.
- **Recovery**: when a closer does not match the innermost opener but does
  match one deeper in the stack, every opener above the match is reported
  and matching resumes at the match — so `Len(aV[1);` reports the `[`
  missing its `]` once, and the `(` pairs with its `)`. This mirrors the
  block scanner's recovery (`unclosed_block`).

Gated by the `CheckUnmatchedParens` option, which defaults on and has no
user-facing configuration key; per-rule severity/off is available through
`ssl.diagnostics.rules`.

It must NOT flag:

- balanced delimiters, including nested and mixed kinds;
- a stray closer — that is `unmatched_delimiter`;
- a wrong-kind closer with no matching opener deeper in the stack — that
  is `mismatched_delimiter`, and the opener it consumed is popped rather
  than reported again here;
- code-block literals (`{|x| ...}`) — the lexer consumes a balanced code
  block as a single token, so its braces never reach the matcher. Array
  literals (`{1, 2}`) do lex as brace punctuation and are matched.

## Examples

### Flags

```ssl
nResult := Len(("hello");
```

### Flags

```ssl
:DECLARE aV, n;
n := Len(aV[1);
```

### Does not flag

```ssl
nResult := Len(("hello"));
```

### Does not flag

```ssl
:DECLARE s;
/* an opener ( in a comment;
s := "an opener ( in a string";
```

### Does not flag

```ssl
:DECLARE aValues;
aValues := {1, 2, 3};
```

## Rationale

An opener that never closes corrupts everything that parses after it, so
this is an error. Reporting on the opener rather than end-of-file is the
core UX decision: the cursor lands where the missing closer belongs. The
first fence (from `TestGetDiagnostics_UnmatchedParens`) shows exactly one
finding for `Len(("hello");` — the inner pair matches, only the leftover
opener is reported. The recovery fence (issue #255) is the same principle
from the other side: a missing inner closer used to be reported as a
mismatch on the outer closer plus the outer opener left unclosed.

## Known gaps

A code-block literal whose braces never balance (`{|x| x + 1;`) is
consumed by the lexer to end of file and reported by nothing, so the rest
of the file goes unchecked. Making the lexer's code-block reader
string-aware is in flight separately (the formatter audit, issue #255);
marking an unterminated code block and reporting its `{` here is the
follow-up once that lands.
