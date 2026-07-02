---
id: fmt.trim_trailing_whitespace
title: Trim trailing whitespace
kind: formatter
status: draft
authority: style_only
schema_ref: null
config:
  - ssl.format.trimTrailingWhitespace
tests:
  - internal/providers/formatting_test.go
history:
  - date: 2026-05-01
    ref: "PR #4 (v0.5.0)"
    note: Added as a formatter post-pass, default on.
issues: []
---

## Behavior

With `ssl.format.trimTrailingWhitespace` on (default), trailing space and
tab characters are removed from every formatted line, including blank lines
reduced to zero-width. Trailing whitespace inside multi-line string literals
is preserved (it is literal text).

## Examples

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

## Rationale

Trailing whitespace is invisible diff noise. Post-pass placement (PR #4)
means it applies uniformly after all other formatting decisions.
