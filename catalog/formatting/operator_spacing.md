---
id: fmt.operator_spacing
title: Spacing around operators
kind: formatter
status: active
authority: style_only
schema_ref: null
config:
  - ssl.format.operatorSpacing
tests:
  - internal/providers/formatting_test.go
history:
  - date: 2026-01-10
    ref: "v0.1.0 initial release"
    note: Part of the original document formatter, default on.
issues: []
---

## Behavior

With `ssl.format.operatorSpacing` on (default), binary operators —
assignment `:=`, compound assignments, comparison, arithmetic, and the
dot-wrapped logical operators (`.AND.`, `.OR.`) — get exactly one space on
each side. Exceptions:

- `-` / `+` in unary position (after `:=`, an operator, an opening
  delimiter, a comma, a keyword, or at line start) attach directly to their
  operand: `nOffset := -5;`.
- `!`, `++`, `--` attach to their operand and get no spacing treatment.
- The member-access `:` is not an operator: it never gains spaces, and
  spaces around it are removed (`oUser : firstName` → `oUser:firstName`).
- No space is inserted before `;` or a closing delimiter, or after an
  opening delimiter.

Operator characters inside string literals and comments are untouched.

## Examples

### Before

```ssl
nTotal:=nBase+nExtra;
```

### After

```ssl
nTotal := nBase + nExtra;
```

### Before

```ssl
:IF nCount>=10 .AND. bReady;
nCount := 0;
:ENDIF;
```

### After

```ssl
:IF nCount >= 10 .AND. bReady;
	nCount := 0;
:ENDIF;
```

Spaces around member-access `:` are removed; unary minus stays tight:

### Before

```ssl
sName := oUser : firstName;
nOffset := -5;
```

### After

```ssl
sName := oUser:firstName;
nOffset := -5;
```

## Rationale

Standard readability convention (style_only). The `:` exclusion exists
because spacing member access would change how chains read and collide with
the keyword prefix `:` (see fmt.atomic_property_chains for the wrapping
side of the same decision).
