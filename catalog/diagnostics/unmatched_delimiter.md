---
id: diag.unmatched_delimiter
title: Closing delimiter with nothing open
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

Flags a stray closing `)`, `]`, or `}`:

- one that arrives while **no** delimiter is open — the matching stack is
  empty;
- one of a kind that is open nowhere in the stack, when the next
  significant token is the innermost opener's own closer — an extra closer
  inside a list (`DoProc("P", {a, b}});`, where the second `}` is extra
  and the `)` still closes the call). Nothing is popped, so the `)` pairs
  normally and the mistake is reported once.

The range covers the stray closer.

Gated by the `CheckUnmatchedParens` option, which defaults on and has no
user-facing configuration key; per-rule severity/off is available through
`ssl.diagnostics.rules`.

It must NOT flag:

- balanced delimiters, including nested and mixed kinds;
- a closer of the *wrong kind* standing in for the innermost opener's
  closer — that is `mismatched_delimiter` (the opener is popped, so one
  mistake yields one finding);
- a closer whose matching opener sits deeper in the stack — the openers
  above it are `unclosed_delimiter`;
- an opener that never closes — that is `unclosed_delimiter`;
- braces inside a code-block literal (`{|x| ...}`) — the lexer consumes a
  balanced code block as a single token, so its braces never reach the
  matcher.

## Examples

### Flags

```ssl
nTotal := 1 + 2);
```

### Flags

```ssl
:DECLARE a, b;
DoProc("P", {a, b}});
```

### Does not flag

```ssl
nTotal := (1 + 2) * Len("x");
```

### Does not flag

```ssl
nTotal := (1 + 2];
```

## Rationale

A closer with nothing open can never be valid SSL, so this is an error.
The three-way split (unmatched / mismatched / unclosed) keeps each message
pointing at the token the user must actually fix: this code owns exactly
the empty-stack case, and the third fence pins that a wrong-kind closer is
reported once as `mismatched_delimiter` rather than leaking into this rule.
