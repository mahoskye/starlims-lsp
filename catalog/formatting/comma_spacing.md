---
id: fmt.comma_spacing
title: Space after commas
kind: formatter
status: draft
authority: style_only
schema_ref: null
config:
  - ssl.format.commaSpacing
tests:
  - internal/providers/formatting_test.go
history: []
issues: []
---

## Behavior

With `ssl.format.commaSpacing` on (default), every comma in argument lists,
array literals, and declaration lists is followed by exactly one space and
preceded by none. Commas inside strings and comments are untouched.

## Examples

### Before

```ssl
DoProc("MyProc",{nFirst ,nSecond,nThird});
```

### After

```ssl
DoProc("MyProc", {nFirst, nSecond, nThird});
```

## Rationale

Standard readability convention (style_only).
