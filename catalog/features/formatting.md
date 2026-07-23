---
id: feature.formatting
title: Document and range formatting
kind: feature
status: active
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
  - internal/server/server_test.go
  - internal/server/handler_test.go
history:
  - date: 2026-01-10
    ref: "442fa69 (v0.1.0)"
    note: Initial token-based formatter with full-document and range
      formatting and embedded SQL formatting (four SQL styles).
  - date: 2026-02-02
    ref: "ee0abfd (v0.2.0)"
    note: SQL string detection added — standalone SQL-looking strings are
      formatted too when ssl.format.sql.detectSQLStrings is on.
  - date: 2026-04-30
    ref: "49fc459 (PR #4, v0.5.0)"
    note: Post-format passes (trailing-whitespace trim, blank-line cap,
      sibling-block blank lines, built-in casing) layered on the token
      formatter as configurable options.
  - date: 2026-07-22
    ref: "issues #84/#104"
    note: >-
      SQL-mode data sources (plain SQL, or builder directives followed by
      SQL) are excluded from formatting: the SSL formatter injected
      semicolons into SQL and re-cased bind variables. Formatting them
      with the SQL engine is deferred until the SQL lexer understands
      Oracle-style :bind variables.
issues: []
---

## Behavior

This entry is the feature-level contract for `textDocument/formatting` and
`textDocument/rangeFormatting`. Individual formatting decisions (blank-line
rules, operator/comma spacing, casing, indentation specifics, SQL layout
rules) are each specified separately as `fmt.*` entries — this entry does
not restate them.

- The server MUST serve both full-document and range formatting.
- Full-document formatting returns exactly one edit replacing the whole
  document; it MUST NOT return piecemeal edits.
- Range formatting expands the request to whole lines, reformats only those
  lines, and MUST leave text outside the requested lines untouched,
  re-applying the surrounding base indentation so the block stays anchored
  in context.
- Formatting is driven by the server's `ssl.format.*` settings (applied via
  initializationOptions or didChangeConfiguration); the LSP request's
  `options` field (tabSize/insertSpaces) is not consulted.
- The formatter is token-based: comment content and non-SQL string content
  are preserved byte-for-byte; only whitespace, layout, and configured
  canonicalizations change.
- Embedded SQL hand-off: string literals that are the SQL argument of known
  SQL functions — and, when `ssl.format.sql.detectSQLStrings` is on, any
  standalone string structurally recognized as SQL — are delegated to the
  SQL formatter under the `ssl.format.sql.*` options. Strings not
  recognized as SQL MUST pass through unchanged. With `detectSQLStrings`
  off, standalone strings are never touched but SQL-function arguments are
  still formatted. `ssl.format.sql.enabled: false` disables the hand-off
  entirely.
- Formatting is idempotent: formatting already-formatted output again under
  the same options produces identical text. Deviations are bugs to be
  recorded as `## Known gaps` on the relevant `fmt.*` entry.
- A formatting failure MUST NOT corrupt the document; when the formatter
  cannot proceed it leaves the text unchanged.
- Data-source documents whose content is in SQL mode — plain SQL, or a
  builder-directive / inline-defaults `:PARAMETERS` header followed by SQL
  (the classifier shared with feature.diagnostics_pipeline A10/A13) — get
  no edits from either formatting request: the SSL formatter would inject
  statement semicolons into SQL and re-case `:bind` variables. The CLI,
  which has no file-type context, applies the same content classifier to
  every input. SSL-mode data sources format normally.

## Acceptance

- A1: Given an unformatted SSL document, when `textDocument/formatting` is requested, then exactly one TextEdit spanning the full document is returned.
- A2: Given a document where only lines N..M are requested, when range formatting runs, then the returned edit covers only those lines and the block's base indentation is preserved so text outside the range is unaffected.
- A3: Given a document containing comments and non-SQL string literals, when formatted, then the comment text and string contents are byte-identical to the input.
- A4: Given `SQLExecute("select ... from ...")` or an overflowing standalone SQL string with detection on, when formatted, then the string is reformatted by the SQL formatter per `ssl.format.sql.*`; and given a plain-English string, it MUST NOT be treated as SQL.
- A5: Given `ssl.format.sql.enabled: false`, when a document with SQL strings is formatted, then no string literal content changes.
- A6: Given the output of a previous format run under the same options, when formatted again, then the result is byte-identical.
- A7: Given `ssl.format.sql.detectSQLStrings: false`, when formatted, then standalone SQL-looking strings pass through unchanged while the SQL argument of a known SQL function is still formatted.
- A8: Given different `ssl.format.*` option values (e.g. indentStyle tab vs space), when the same document is formatted under each, then the outputs differ accordingly — the configured options are honored.
- A9: Given a data-source document whose content is SQL-mode (plain SQL or directives-then-SQL), when document or range formatting is requested, then no edits are returned; given an SSL-mode data source, formatting proceeds normally.

## Rationale

A single full-document edit and a strictly line-scoped range edit are the
two shapes editors handle predictably; anything else risks partial applies.
Token-based reconstruction (442fa69) is what lets the formatter guarantee
comment/string preservation (A3) while still rewriting layout, and makes
the SQL hand-off a contained delegation rather than regex surgery — the
detection gate (ee0abfd) exists because user strings legitimately contain
words like "select" and must never be rewritten (A4's negative half).
Idempotence (A6) is the contract that lets format-on-save run
unconditionally. Server-side `ssl.format.*` settings take precedence over
the client's generic tabSize because SSL's tab-based style guide indentation
is a project decision, not an editor preference. Keeping per-decision
behavior in `fmt.*` entries keeps this contract stable while individual
style decisions evolve.
