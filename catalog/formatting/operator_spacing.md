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
  - date: 2026-07-22
    ref: "issue #83"
    note: >-
      Lexer fix: a number no longer consumes a trailing dot, so dot
      operators glued to a numeric literal (`10.AND.`) space correctly
      instead of corrupting the rest of the line.
  - date: 2026-07-22
    ref: "issue #88"
    note: >-
      Operators glued to a preceding operator no longer double the space:
      the previous operator's trailing space is reused.
  - date: 2026-10-13
    ref: "issue #326"
    note: >-
      The invalid `===` / `!==` (which lex as `==`/`!=` then `=`) stay
      glued. Spacing them apart (`== =`) split invalid_operator_sequence's
      one operator into two and added an unexpected_token error.
  - date: 2026-10-13
    ref: "issue #326"
    note: >-
      Source spaces after a unary operator (or before a postfix ++/--) are
      removed, as the behavior already required; a known colon keyword
      glued to its neighbors (`1:TO 5`, `:IF(`, `:RETURN-1`) is spaced, so
      step_spacing no longer fires on formatted output.
issues: ["#83", "#88", "#326"]
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
  A source space between a unary operator and its operand is removed
  (`- 5` → `-5`, `! bR` → `!bR`, `nA ++` → `nA++`), except where gluing
  would re-lex as another token (`! =`, `- -5`) (issue #326).
- A known colon keyword glued to the code around it is set apart by one
  space: `1:TO 5:STEP-1` → `1 :TO 5 :STEP -1`, `:IF(j > 1)` →
  `:IF (j > 1)` (schema `spaces.before_step_keyword`, issue #326). A
  colon after `)` or `]` lexes as member access, so `Len(a):STEP` is left
  to `step_spacing`.
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

Operators glued to a preceding operator get exactly one space, and the
dot-wrapped logical operators take their canonical uppercase form
(issues #88/#90):

### Before

```ssl
bFlag:=.not.bFlag;
```

### After

```ssl
bFlag := .NOT. bFlag;
```

A dot operator glued to a numeric literal is recognized and spaced —
before issue #83 the number consumed the dot and later operators on the
line were corrupted:

### Before

```ssl
:IF nCount>=10.AND.bReady;
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

The invalid JavaScript-style `===` and `!==` are left glued, so
`invalid_operator_sequence` reports one operator rather than formatting
creating a second error (issue #326):

### Before

```ssl
:DECLARE bSame;
bSame := 1===1;
```

### After

```ssl
:DECLARE bSame;
bSame := 1 === 1;
```

Unary operators attach to their operand, and a glued keyword is set
apart (issue #326):

### Before

```ssl
:DECLARE nR, bR, i;
nR := - 5;
bR := ! bR;
:FOR i := 1:TO 5:STEP-1;
:NEXT;
```

### After

```ssl
:DECLARE nR, bR, i;
nR := -5;
bR := !bR;
:FOR i := 1 :TO 5 :STEP -1;
:NEXT;
```

## Rationale

Standard readability convention (style_only). The `:` exclusion exists
because spacing member access would change how chains read and collide with
the keyword prefix `:` (see fmt.atomic_property_chains for the wrapping
side of the same decision).
