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
  - date: 2026-07-02
    ref: "issue #35"
    note: >-
      A stray space before a comma is now removed: "," joined the
      whitespace-suppression set (gated on commaSpacing, like the rest of
      the comma handling).
issues: ["#35"]
---

## Behavior

With `ssl.format.commaSpacing` on (default), the formatter writes exactly
one space after each comma in argument lists, array literals, and
declaration lists, and removes any stray space *before* a comma. Between
adjacent commas (skipped parameters) no space is inserted, so the commas
themselves stay tight; the last comma of the run still gets its space
before the next value (`{a,, b}`). Because before-comma space removal makes
`{a, ,b}` collapse to adjacent commas, it too normalizes to the tight
`{a,, b}` form. Commas inside string literals and comments are untouched.

With the option off, comma spacing is left entirely as written (neither the
after-comma space insertion nor the before-comma space removal runs).

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

A stray space before a comma is removed:

### Before

```ssl
aValues := {nFirst ,nSecond};
```

### After

```ssl
aValues := {nFirst, nSecond};
```

## Rationale

Standard readability convention (style_only). The adjacent-comma exception
exists because `{a,,b}` is SSL's skipped-parameter idiom — v0.7.0 removed
the diagnostic that policed its spacing as pure noise, and the formatter
likewise leaves the tight form alone. Before-comma space removal (issue
#35) completes the rule: a comma binds to what precedes it, so `{nFirst ,
nSecond}` is normalized instead of preserved.
