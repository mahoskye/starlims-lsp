---
id: fmt.sql_in_strings
title: SQL formatting inside string literals
kind: formatter
status: draft
authority: tool
schema_ref: null
config:
  - ssl.format.sql.enabled
  - ssl.format.sql.style
  - ssl.format.sql.keywordCase
  - ssl.format.sql.indentSize
  - ssl.format.sql.maxLineLength
  - ssl.format.sql.detectSQLStrings
tests:
  - internal/providers/sql_formatter_test.go
history:
  - date: 2025-11-19
    ref: "vs-code-ssl-formatter v1.1.0, issues #8/#28"
    note: >-
      Original policy: never modify content inside string literals —
      spacing and indentation in strings are literal text.
  - date: 2025-12-05
    ref: "vs-code-ssl-formatter PRs #50/#51 (v1.2.x)"
    note: >-
      Policy reversed for detected SQL: SQL formatting inside strings
      enabled by default on the canonicalCompact engine.
  - date: 2026-05-06
    ref: "vs-code-ssl-formatter #64 / LSP commits 8129e0e..f387b3e, dc510af"
    note: >-
      Re-guarded after the formatter mangled single-line SQL assignments:
      short SQL that already fits its line is left untouched.
issues: []
---

## Behavior

String literals are literal text and are never reformatted — with one
deliberate exception: a string detected as SQL (per
`ssl.format.sql.detectSQLStrings`) is formatted by the SQL engine when
`ssl.format.sql.enabled` is on (default). Within that exception:

- SQL that already fits on a single line is left exactly as written.
- Multi-line or over-long SQL is reformatted to the configured style and
  keyword case.
- Content of non-SQL strings is never touched, regardless of settings.
- SQL parameter placeholders (`?param?`) are preserved verbatim.

## Examples

### Before

```ssl
sSql := "select * from users where id = ?id?";
```

### After

```ssl
sSql := "select * from users where id = ?id?";
```

## Rationale

This is the formatter's most-reversed decision (see history): "never touch
strings" lost to the value of readable embedded SQL, then over-eager SQL
rewriting broke short assignments (#64). The settled contract is the narrow
middle: only detected SQL, only when it doesn't already fit, everything else
byte-preserved. Any future change to this boundary edits this entry first.
