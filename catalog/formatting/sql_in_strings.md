---
id: fmt.sql_in_strings
title: SQL formatting inside string literals
kind: formatter
status: active
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
      Original extension policy: never modify content inside string
      literals — spacing and indentation in strings are literal text.
  - date: 2025-12-05
    ref: "vs-code-ssl-formatter PRs #50/#51 (v1.2.x)"
    note: >-
      Policy reversed for detected SQL: SQL formatting inside strings
      enabled by default on the canonicalCompact engine.
  - date: 2026-05-01
    ref: "vs-code-ssl-formatter#64; commits 486e596..f387b3e, dc510af (v0.7.3)"
    note: >-
      LSP re-guarded the same policy after the formatter mangled
      single-line SQL assignments: single-line SQL that fits its line is
      left untouched; rules A–F pin the multi-line layout.
  - date: 2026-07-22
    ref: "issue #81"
    note: >-
      Bracket-quoted strings reflow with a ']' closer. The reflow used to
      write the opening delimiter at both ends, leaving an unterminated
      bracket string that swallowed the rest of the file on the next pass.
  - date: 2026-07-22
    ref: "issue #82"
    note: >-
      Detection hardened against English prose: runs of three or more
      bare words, prose-shaped SELECT lists, and SET/target clauses
      without SQL shape are rejected; only argument 0 of a known SQL
      function is a SQL candidate.
issues: ["#81", "#82"]
---

## Behavior

String literals are literal text and are never reformatted — with one
deliberate exception: a string whose content is detected as a SQL statement
is formatted by the SQL engine when `ssl.format.sql.enabled` is on
(default). Detection applies to any string literal when
`ssl.format.sql.detectSQLStrings` is on (default); with it off, only
strings inside known SQL function calls (SQLExecute, LSearch, RunSQL, …)
are considered. Within the exception:

- Single-line SQL that fits within `ssl.format.maxLineLength` on its
  current line is left exactly as written — including its keyword casing
  and spacing (vs-code-ssl-formatter#64).
- Single-line SQL that overflows the line, and any SQL string already
  containing newlines, is reflowed to the configured
  `ssl.format.sql.style` / `ssl.format.sql.keywordCase` (defaults:
  canonicalCompact, upper). The opening quote stays on the assignment line
  (rule F), the SQL starts on the next line with each clause indented by
  `ssl.format.sql.indentSize` spaces past the statement's base indent, and
  the closing quote lands on its own line at the base indent, glued to the
  trailing punctuation (rule E).
- Strings not detected as SQL are byte-preserved regardless of settings.
  Detection is structural and rejects English prose even when it contains
  SQL trigger words: a run of three or more consecutive bare words, a
  SELECT list that is not `*` / a single expression / comma-separated, a
  SET clause with no `ident =` shape, or a FROM/INTO target followed by
  more prose all disqualify the string (issue #82).
- Within a known SQL function call, only the SQL argument (position 0) is
  ever a candidate — friendly names, LSearch default values, and parameter
  arrays are byte-preserved even when they look like SQL (issue #82).
- SQL parameter placeholders (`?param?`) are preserved verbatim.

## Examples

Short SQL that fits its line is untouched, casing and all; non-SQL strings
are always untouched:

### Idempotent

```ssl
sSql := "select * from users where id = ?id?";
sMsg := "sample not found for the given identifier";
```

Multi-line English strings containing SQL trigger words are byte-preserved
— under the pre-#82 detector these were rewritten as SQL regardless of
length (single-line over-long cases and SQL-function default-value
arguments are pinned by Go tests; their fences wait on the #85/#89 wrap
fixes for stable layout):

### Idempotent

```ssl
sMsg := "Select the samples from the rack
and update the status column";
sBackup := "Delete old records from the archive
after exporting them safely";
```

A single-line SQL string that overflows 90 columns is reflowed by the SQL
engine (canonicalCompact, uppercase keywords), placeholders intact:

### Before

```ssl
sSql := "SELECT sample_id, sample_name, sample_status FROM samples WHERE sample_status = ?status? ORDER BY sample_id";
```

### After

```ssl
sSql := "
    SELECT sample_id, sample_name, sample_status
    FROM samples
    WHERE sample_status = ?status?
    ORDER BY sample_id
";
```

Bracket-quoted strings (the idiomatic style for SQL holding embedded
quotes) reflow the same way; the opening `[` closes with `]` (issue #81):

### Before

```ssl
sSql := [SELECT sample_id, sample_name, sample_status FROM samples WHERE sample_status = ?status? ORDER BY sample_id];
```

### After

```ssl
sSql := [
    SELECT sample_id, sample_name, sample_status
    FROM samples
    WHERE sample_status = ?status?
    ORDER BY sample_id
];
```

SQL that already spans lines is renormalized to the same layout:

### Before

```ssl
sSql := "SELECT sample_id
FROM samples
WHERE sample_status = ?status?";
```

### After

```ssl
sSql := "
    SELECT sample_id
    FROM samples
    WHERE sample_status = ?status?
";
```

## Rationale

This is the formatter's most-reversed decision (see history): "never touch
strings" lost to the value of readable embedded SQL, then over-eager SQL
rewriting broke short assignments (#64). The settled contract is the narrow
middle: only detected SQL, only when it doesn't already fit on one line,
everything else byte-preserved. Any future change to this boundary edits
this entry first.
