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
  - ssl.format.sql.identifierCase
  - ssl.format.sql.indentSize
  - ssl.format.sql.maxLineLength
  - ssl.format.sql.detectSQLStrings
tests:
  - internal/providers/formatting_test.go
  - internal/providers/sql_formatter_test.go
  - internal/server/server_test.go
  - internal/server/handler_test.go
  - cmd/starlims-lsp/format_test.go
  - internal/server/utf16_output_test.go
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
  - date: 2026-07-23
    ref: "issue #102"
    note: >-
      LF-only output documented (schema files.line_endings); scope
      boundary recorded in DECISIONS.md D11; SQL layout authority
      delegated to sql-canonical-compact-reference.md via
      fmt.sql_in_strings.
  - date: 2026-07-22
    ref: "issue #87"
    note: >-
      Unterminated final string literals make the document unformattable:
      both requests return no edits instead of appending a stray
      semicolon on every pass.
  - date: 2026-07-22
    ref: "issues #84/#104"
    note: >-
      SQL-mode data sources (plain SQL, or builder directives followed by
      SQL) are excluded from formatting: the SSL formatter injected
      semicolons into SQL and re-cased bind variables. Formatting them
      with the SQL engine is deferred until the SQL lexer understands
      Oracle-style :bind variables.
  - date: 2026-10-13
    ref: "issue #255"
    note: >-
      The CLI's SQL-mode passthrough is gated to data sources (.ds/.ds.txt
      paths, or the new --format --ds flag), matching D15: the classifier
      defaults marker-free content to SQL, so an ordinary script with no
      colon keyword and no `:=` used to pass through unformatted (A11).
      ssl.format.sql.identifierCase added to config.
  - date: 2026-10-13
    ref: "issue #307"
    note: >-
      The full-document edit ends at the last line's length in UTF-16
      code units (A12). It used the byte length, an overshoot
      clients clamped to the line end.
  - date: 2026-10-13
    ref: "issues #255/#258"
    note: >-
      Range formatting lexes the whole document and widens the range so
      no multi-line string, comment, code block or region body is cut by
      a boundary (A11): formatting one line from inside a multi-line
      comment appended a ';' that ended the comment early. The pass
      subset range formatting applies is now stated.
  - date: 2026-10-13
    ref: "issue #311"
    note: >-
      A6 is now guaranteed by construction: a document is formatted until
      its output formats to itself (at most three passes; five since bracket
      alignment, which can settle one nesting level per pass), and when no
      fixed point is reached the formatter declines, leaving the text
      unchanged. Fuzzing had found malformed shapes the formatter kept
      rewriting; well-formed input is unaffected (corpus output
      byte-identical).
  - date: 2026-10-13
    ref: "issue #325"
    note: >-
      Range formatting widens the range to whole statements, so one line
      of a multi-line statement no longer gains a ';', loses its
      continuation indent, or has a leading `:Member` re-lexed as a
      keyword; the interior lines of a multi-line string, comment, region
      body or code block keep their text when the base indent is
      re-applied; the width budgets are narrowed by the base indent so
      reflowed SQL packs as in a full format; and a range whose
      formatting never converges is declined like a document (A14). On
      the production corpus, range-formatting each line of formatted
      output changed 5,032 lines in a 577-file sample before, and none
      across all 4,620 scripts after.
issues: ["#325"]
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
  in context. The range is first widened so that no multi-line token — a
  string, comment, code block or `:REGION` body — crosses either boundary
  (issue #258): a line from inside such a token, lexed on its own, reads as
  code, and formatting it rewrites literal text. It is also widened to
  whole statements (issue #325): a range never starts on a line that
  continues the statement before it, or ends on a line the next line
  continues. Formatted alone, such a line reads as a statement of its own
  — it gains a ';', loses its continuation indent, and a leading
  `:Member` lexes as a keyword. A line continues the code before it
  exactly when the token formatter would neither find nor insert a ';'
  there, or when it sits inside an open delimiter, or leads with a binary
  operator or member-access ':'; a line led by any other colon keyword
  than `:TO`/`:STEP` starts a statement. The interior lines of a
  multi-line token the formatter passes through unchanged keep their
  text: they neither set nor take the base indentation. Because the range
  is formatted dedented, its width budgets (`ssl.format.maxLineLength`
  and the SQL engine's) are narrowed by the base indentation, so it packs
  lines as a full format does. Like a document (A6), a range is formatted
  until its output formats to itself, and is declined — no edit — when it
  does not converge. A range runs the token
  formatter (spacing, casing of keywords, semicolons, indentation, SQL
  hand-off, trailing-whitespace trim) and the line-wrap pass only; the
  whole-document post-passes — built-in function casing outside code-block
  bodies (fmt.builtin_function_case), sibling-block blank lines
  (fmt.blank_line_between_blocks) and the blank-line cap
  (fmt.max_consecutive_blank_lines) — are not applied to a range.
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
- Output uses LF line endings; CRLF input is normalized (the style-guide
  schema mandates `files.line_endings: lf`). On Windows-authored files this
  makes the first format a whole-file diff — intended, per the schema.
- Formatting is idempotent: formatting already-formatted output again under
  the same options produces identical text. Deviations are bugs to be
  recorded as `## Known gaps` on the relevant `fmt.*` entry.
- A formatting failure MUST NOT corrupt the document; when the formatter
  cannot proceed it leaves the text unchanged. A document ending in an
  unterminated string literal is one such case: the string has swallowed
  the rest of the file, so both formatting requests return no edits
  (issue #87 — enforcement used to append a semicolon per pass). The
  lexer's own unterminated flag decides this, so a nested bracket string
  such as `[[a]` — which ends in `]` without being closed — is caught too
  (issue #258).
- Data-source documents whose content is in SQL mode — plain SQL, or a
  builder-directive / inline-defaults `:PARAMETERS` header followed by SQL
  (the classifier shared with feature.diagnostics_pipeline A10/A13) — get
  no edits from either formatting request: the SSL formatter would inject
  statement semicolons into SQL and re-case `:bind` variables. The CLI
  applies the same content classifier only to data sources — `.ds`/`.ds.txt`
  paths, or any input under `--format --ds` — because the classifier
  defaults marker-free content to SQL (D15); every other input is
  formatted as SSL. SSL-mode data sources format normally.
- Edit ranges count characters in UTF-16 code units, the encoding the
  server advertises: the full-document edit ends at the last line's
  UTF-16 length, and a range edit ends at the UTF-16 length of its last
  line (issue #307).

## Acceptance

- A1: Given an unformatted SSL document, when `textDocument/formatting` is requested, then exactly one TextEdit spanning the full document is returned.
- A2: Given a document where only lines N..M are requested, when range formatting runs, then the returned edit covers only those lines, widened only to whole statements and as A13 requires, and the block's base indentation is preserved so text outside the edit is unaffected (issue #325).
- A3: Given a document containing comments and non-SQL string literals, when formatted, then the comment text and string contents are byte-identical to the input — and every comment survives: a line carrying several consecutive end-of-line comments keeps all of them in order (issue #215; the pending-EOL-comment slot merged by clobbering, deleting all but the last).
- A4: Given `SQLExecute("select ... from ...")` or an overflowing standalone SQL string with detection on, when formatted, then the string is reformatted by the SQL formatter per `ssl.format.sql.*`; and given a plain-English string, it MUST NOT be treated as SQL.
- A5: Given `ssl.format.sql.enabled: false`, when a document with SQL strings is formatted, then no string literal content changes.
- A6: Given the output of a previous format run under the same options, when formatted again, then the result is byte-identical.
- A7: Given `ssl.format.sql.detectSQLStrings: false`, when formatted, then standalone SQL-looking strings pass through unchanged while the SQL argument of a known SQL function is still formatted.
- A8: Given different `ssl.format.*` option values (e.g. indentStyle tab vs space), when the same document is formatted under each, then the outputs differ accordingly — the configured options are honored.
- A9: Given a data-source document whose content is SQL-mode (plain SQL or directives-then-SQL), when document or range formatting is requested, then no edits are returned; given an SSL-mode data source, formatting proceeds normally.
- A10: Given a document with CRLF line endings, when formatted, then the output uses LF exclusively and a second format is byte-identical.
- A11: Given CLI `--format` input that is not a data source (no `.ds`/`.ds.txt` path, no `--ds`) whose content has no colon keyword and no `:=`, when formatted, then it is formatted as SSL; given SQL-mode content from a data-source path or under `--ds`, then it passes through unchanged.
- A12: Given a document whose last line holds an emoji (one character, two UTF-16 code units), when `textDocument/formatting` is requested, then the full-document edit ends at that line's UTF-16 length (issue #307).
- A13: Given a range whose first or last line lies inside a multi-line comment or string literal, when range formatting runs, then the edit is widened to cover the whole token and the token's text is byte-identical in the result — no `;` or spacing change is ever written inside it (issue #258), and re-applying the base indentation never indents its interior lines (issue #325).
- A14: Given the output of a full format of code that validates without errors, when any one of its lines is range-formatted under the same options, then the result is unchanged — and a range whose formatting does not converge is declined with no edit (issue #325).

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
