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
issues: []
---

## Behavior

Flags each opening `(`, `[`, or `{` still unmatched when the file ends —
one diagnostic per unclosed opener, with the range on the opener itself
(where the fix belongs), not at end-of-file.

Gated by the `CheckUnmatchedParens` option, which defaults on and has no
user-facing configuration key; per-rule severity/off is available through
`ssl.diagnostics.rules`.

It must NOT flag:

- balanced delimiters, including nested and mixed kinds;
- a stray closer — that is `unmatched_delimiter`;
- a wrong-kind closer — that is `mismatched_delimiter`, and the opener it
  consumed is popped rather than reported again here;
- code-block literals (`{|x| ...}`) — the lexer consumes a balanced code
  block as a single token, so its braces never reach the matcher. Array
  literals (`{1, 2}`) do lex as brace punctuation and are matched.

## Examples

### Flags

```ssl
nResult := Len(("hello");
```

### Does not flag

```ssl
nResult := Len(("hello"));
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
opener is reported.
