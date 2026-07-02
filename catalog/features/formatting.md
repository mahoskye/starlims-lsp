---
id: feature.formatting
title: Document and range formatting
kind: feature
status: draft
authority: tool
schema_ref: null
config:
  - ssl.format.indentStyle
  - ssl.format.indentSize
  - ssl.format.maxLineLength
  - ssl.format.operatorSpacing
  - ssl.format.commaSpacing
  - ssl.format.semicolonEnforcement
  - ssl.format.blankLinesBetweenProcs
  - ssl.format.blankLineBetweenBlocks
  - ssl.format.trimTrailingWhitespace
  - ssl.format.maxConsecutiveBlankLines
  - ssl.format.builtinFunctionCase
  - ssl.format.sql.enabled
  - ssl.format.sql.style
  - ssl.format.sql.keywordCase
  - ssl.format.sql.indentSize
  - ssl.format.sql.maxLineLength
  - ssl.format.sql.detectSQLStrings
tests:
  - internal/providers/formatting_test.go
  - internal/providers/sql_formatter_test.go
  - internal/providers/edge_test.go
  - internal/server/handler_test.go
  - internal/server/server_test.go
history:
  - date: 2026-01-10
    ref: "v0.1.0"
    note: Initial token-based formatter with full-document and range formatting and embedded SQL formatting (four SQL styles).
---

## Behavior

This entry is the feature-level contract for `textDocument/formatting` and
`textDocument/rangeFormatting`. Individual formatting decisions (blank-line
rules, operator/comma spacing, casing, indentation specifics, SQL layout
rules) are each specified separately as `fmt.*` entries — this entry does not
restate them.

- The server MUST serve both full-document and range formatting.
- Full-document formatting returns a single edit replacing the whole
  document; it MUST NOT return piecemeal edits.
- Range formatting reformats only the requested lines and MUST leave text
  outside the range untouched, preserving the surrounding indentation
  context.
- The formatter is token-based: comment content and non-SQL string content
  are preserved byte-for-byte; only whitespace, layout, and configured
  canonicalizations change.
- Embedded SQL hand-off: string literals that are the SQL argument of known
  SQL functions — and, when `ssl.format.sql.detectSQLStrings` is on, any
  string structurally recognized as SQL — are delegated to the SQL formatter
  under the `ssl.format.sql.*` options. Strings not recognized as SQL MUST
  pass through unchanged. `ssl.format.sql.enabled: false` disables the
  hand-off entirely.
- Formatting is idempotent as a goal: formatting already-formatted output
  again produces identical text. Deviations are bugs to be recorded as
  `## Known gaps` on the relevant `fmt.*` entry.
- A formatting failure MUST NOT corrupt the document; when the formatter
  cannot proceed it leaves the text unchanged.

## Acceptance

- A1: Given an unformatted SSL document, when `textDocument/formatting` is
  requested, then exactly one TextEdit spanning the full document is
  returned.
- A2: Given a document where only lines N..M are selected, when range
  formatting is requested, then text outside N..M is byte-identical to the
  input.
- A3: Given a document containing block comments and non-SQL string literals,
  when formatted, then the comment text and string contents are unchanged.
- A4: Given `sSQL := "select id from users where active = 1";` with SQL
  formatting enabled, when formatted, then the string is reformatted by the
  SQL formatter per `ssl.format.sql.*`; and given
  `msg := "Update your settings in the configuration";`, the string MUST NOT
  be treated as SQL.
- A5: Given `ssl.format.sql.enabled: false`, when a document with SQL strings
  is formatted, then no string literal content changes.
- A6: Given the output of a previous format run under the same options, when
  formatted again, then the result is byte-identical.

## Rationale

A single full-document edit and a strictly-scoped range edit are the two
shapes editors handle predictably. Token-based reconstruction (v0.1.0) is
what lets the formatter guarantee comment/string preservation while still
rewriting layout, and makes the SQL hand-off a contained delegation rather
than regex surgery. Keeping per-decision behavior in `fmt.*` entries keeps
this contract stable while individual style decisions evolve.
