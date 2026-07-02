---
id: fmt.trim_trailing_whitespace
title: Trim trailing whitespace
kind: formatter
status: active
authority: style_only
schema_ref: null
config:
  - ssl.format.trimTrailingWhitespace
tests:
  - internal/providers/formatting_test.go
history:
  - date: 2026-04-30
    ref: "PR #4 (v0.5.0)"
    note: >-
      Added as a dedicated post-pass, default on; the streaming formatter
      had already been trimming line ends unconditionally since v0.1.0.
issues: []
---

## Behavior

Trailing space and tab characters are removed from every formatted line;
whitespace-only lines become empty lines. This includes the interior lines
of multi-line `/* ... ;` comments — comment content is otherwise preserved
verbatim, but line-end whitespace inside it is trimmed.

`ssl.format.trimTrailingWhitespace` (default on) gates the dedicated
post-pass, but turning it off does not currently preserve trailing
whitespace: the streaming formatter trims line ends unconditionally before
the post-passes run (see Known gaps).

## Examples

Line one ends with a tab, line two is whitespace-only, line three ends with
spaces:

### Before

```ssl
:DECLARE sName;	
   
sName := "x";   
```

### After

```ssl
:DECLARE sName;

sName := "x";
```

Interior lines of a multi-line comment are trimmed too (`first line` ends
with three spaces before formatting):

### Before

```ssl
/* first line   
second line;
nValue := 1;
```

### After

```ssl
/* first line
second line;
nValue := 1;
```

## Rationale

Trailing whitespace is invisible diff noise. Post-pass placement (PR #4,
v0.5.0) means it applies uniformly after all other formatting decisions.

## Known gaps

- Setting `ssl.format.trimTrailingWhitespace: false` has no observable
  effect: `formatTokens` trims trailing whitespace on every line
  unconditionally before `applyPostFormatPasses` runs, so the option
  cannot preserve trailing whitespace. No executable fence can pin this
  here because `spec_options` apply entry-wide; the always-on trim itself
  is pinned by the pairs above.
