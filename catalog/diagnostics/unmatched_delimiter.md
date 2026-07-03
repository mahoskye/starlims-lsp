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
tests: []
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
issues: []
---

## Behavior

Flags a closing `)`, `]`, or `}` that arrives while **no** delimiter is
open — the matching stack is empty. The range covers the stray closer.

Gated by the `CheckUnmatchedParens` option, which defaults on and has no
user-facing configuration key; per-rule severity/off is available through
`ssl.diagnostics.rules`.

It must NOT flag:

- balanced delimiters, including nested and mixed kinds;
- a closer of the *wrong kind* while another delimiter is open — that is
  `mismatched_delimiter` (the opener is popped, so one mistake yields one
  finding);
- an opener that never closes — that is `unclosed_delimiter`;
- braces inside a code-block literal (`{|x| ...}`) — the lexer consumes a
  balanced code block as a single token, so its braces never reach the
  matcher.

## Examples

### Flags

```ssl
nTotal := 1 + 2);
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
