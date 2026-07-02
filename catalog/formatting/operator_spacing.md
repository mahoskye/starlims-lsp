---
id: fmt.operator_spacing
title: Spacing around operators
kind: formatter
status: draft
authority: style_only
schema_ref: null
config:
  - ssl.format.operatorSpacing
tests:
  - internal/providers/formatting_test.go
history: []
issues: []
---

## Behavior

With `ssl.format.operatorSpacing` on (default), binary operators —
assignment (`:=`), comparison, arithmetic, and the logical operators
(`.AND.`, `.OR.`) — get exactly one space on each side. Unary operators
(`!`, `.NOT.`, unary minus) take no space before their operand. Member
access `:` is not an operator for spacing purposes and never gains spaces.
Operator characters inside strings and comments are untouched.

## Examples

### Before

```ssl
nTotal:=nBase+nExtra;
```

### After

```ssl
nTotal := nBase + nExtra;
```

## Rationale

Standard readability convention (style_only). The `:` exclusion exists
because spacing member access would change how chains read and collide with
keyword-prefix `:`.
