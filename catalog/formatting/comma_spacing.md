---
id: fmt.comma_spacing
title: Space after commas
kind: formatter
status: active
authority: style_only
schema_ref: null
config:
  - ssl.format.commaSpacing
tests:
  - internal/providers/formatting_test.go
history:
  - date: 2026-01-10
    ref: "v0.1.0 initial release"
    note: Part of the original document formatter, default on.
  - date: 2026-05-01
    ref: "v0.7.0 (skipped_param_spacing diagnostic removed)"
    note: >-
      Adjacent commas (skipped parameters, e.g. {a,,b}) deliberately get no
      space inserted between them.
issues: ["#35"]
---

## Behavior

With `ssl.format.commaSpacing` on (default), the formatter writes exactly
one space after each comma in argument lists, array literals, and
declaration lists. Between adjacent commas (skipped parameters) no space is
inserted, so the commas themselves stay tight; the last comma of the run
still gets its space before the next value (`{a,, b}`). Commas inside
string literals and comments are untouched.

The rule only governs what follows a comma: a stray space *before* a comma
is currently preserved, not removed (see Known gaps).

## Examples

### Before

```ssl
DoProc("MyProc",{nFirst,nSecond,nThird});
```

### After

```ssl
DoProc("MyProc", {nFirst, nSecond, nThird});
```

Skipped parameters and commas inside strings stay as written:

### Idempotent

```ssl
DoProc("MyProc", {nFirst,, nThird});
sCsv := "a,b,c";
```

A space before a comma survives formatting (actual behavior pinned; the
intended removal is under Known gaps):

### Idempotent

```ssl
aValues := {nFirst , nSecond};
```

## Rationale

Standard readability convention (style_only). The adjacent-comma exception
exists because `{a,,b}` is SSL's skipped-parameter idiom — v0.7.0 removed
the diagnostic that policed its spacing as pure noise, and the formatter
likewise leaves the tight form alone.

## Known gaps

- Space before a comma is not removed: the whitespace suppression list in
  the streaming formatter covers `)`, `]`, `}`, `;` and member `:` but not
  `,`. Intended behavior: a comma is preceded by no space.

### Before

```ssl expect=fail
aValues := {nFirst ,nSecond};
```

### After

```ssl
aValues := {nFirst, nSecond};
```
