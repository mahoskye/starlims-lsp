---
id: fmt.semicolon_enforcement
title: Statement semicolon enforcement
kind: formatter
status: draft
authority: authoritative
schema_ref: null
config:
  - ssl.format.semicolonEnforcement
tests:
  - internal/providers/formatting_test.go
history:
  - date: 2025-11-19
    ref: "vs-code-ssl-formatter v1.1.0, issues #3/#26"
    note: >-
      Multi-line expressions continuing with logical operators or inside
      unclosed brackets must NOT get a semicolon appended mid-expression.
issues: []
---

## Behavior

With `ssl.format.semicolonEnforcement` on (default), each complete statement
ends with a semicolon. A line is not a complete statement — and gets no
semicolon — when the expression continues: an unclosed bracket/paren/brace,
or a trailing/leading logical operator joining the next line. Semicolons are
never inserted inside strings or comments.

## Examples

### Before

```ssl
:DECLARE nValue;
nValue := 1
```

### After

```ssl
:DECLARE nValue;
nValue := 1;
```

## Rationale

Semicolon termination is SSL syntax (authoritative), so the formatter may
complete it mechanically — but the v1.1.0 continuation rules (history) bound
the enforcement: inserting a semicolon mid-expression changes program
meaning, which the formatter must never do.
