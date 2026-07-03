---
id: fmt.semicolon_enforcement
title: Statement semicolon enforcement
kind: formatter
status: active
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
  - date: 2026-01-10
    ref: "v0.1.0 initial release"
    note: LSP formatter carries the same continuation-aware enforcement.
  - date: 2026-07-02
    ref: "issue #38"
    note: >-
      Enforcement now also runs at end-of-file, so a final statement with
      no trailing newline gets its semicolon.
issues: ["#38"]
---

## Behavior

With `ssl.format.semicolonEnforcement` on (default), the formatter appends
a `;` at the end of a line that finishes a complete statement but lacks
one. A semicolon is only added when the line ends in statement content (an
identifier, number, string, closing delimiter, or keyword) AND the next
significant token starts a new statement (an identifier or a
statement-starting keyword).

No semicolon is added when the expression continues past the line break:

- the line ends with an operator (including `:=`), a comma, or an opening
  delimiter;
- the line ends inside an unclosed `(` / `{` / `[`;
- the line ends with `:TO` / `:STEP`, or the next line starts with a
  continuation keyword (`:ELSE`, `:CASE`, `:OTHERWISE`, `:CATCH`,
  `:FINALLY`, `:TO`, `:STEP`).

Semicolons are never inserted inside strings or comments. The check runs at
every line break and at end-of-file, so a final statement with no trailing
newline is terminated the same way as one followed by a newline (the
continuation guards above apply identically: a document ending
mid-expression gets no semicolon).

## Examples

### Before

```ssl
:DECLARE nValue;
nValue := 1
:RETURN nValue;
```

### After

```ssl
:DECLARE nValue;
nValue := 1;
:RETURN nValue;
```

An expression continuing across lines (trailing `.AND.`, unclosed brace)
gets no mid-expression semicolon:

### Idempotent

```ssl
bResult := bFirst .AND.
bSecond;
```

### Before

```ssl
aList := {1,
2};
```

### After

```ssl
aList := {1,
	2};
```

A final statement with no trailing newline is terminated:

### Before

```ssl
nValue := 1
```

### After

```ssl
nValue := 1;
```

## Rationale

Semicolon termination is SSL syntax (authoritative), so the formatter may
complete it mechanically — but the v1.1.0 continuation rules (history)
bound the enforcement: inserting a semicolon mid-expression changes program
meaning, which the formatter must never do. That is why enforcement keys on
both what the line ends with and what the next line starts with. The
end-of-file case (issue #38) closes the one hole in the trigger: enforcement
used to fire only on newline tokens, so a document whose last line had no
trailing newline kept its unterminated final statement.
