---
id: diag.sql_legacy_outer_join
title: Oracle (+) outer-join marker
kind: diagnostic
status: active
authority: tool
schema_ref: null
default_severity: info
config:
  - ssl.diagnostics.infoDiagnostics
severity_overridable: true
suppressible: true
spec_options:
  include_info_diagnostics: true
tests:
  - internal/providers/providers_test.go
history:
  - date: 2026-08-28
    ref: "issue #220 (formatting-review info-tier proposals)"
    note: >-
      Introduced as part of the info-tier SQL advisory batch: seven
      observations the formatter cannot act on, aimed at the
      assistant/LLM consumers the tier serves.
  - date: 2026-10-13
    ref: "issue #255 (catalog audit)"
    note: >-
      Authority corrected style_only -> tool: no style-guide record or
      schema key covers this rule, so it is an LSP product decision. Behavior
      now states that each concatenated piece is judged alone and that
      literal-splice fragments are skipped.
issues: []
---

## Behavior

Flags the Oracle-specific `(+)` outer-join marker — the token sequence
`(`, `+`, `)` — in the first string argument of a recognized
embedded-SQL call. One diagnostic per string; the range covers the
string token.
Each string piece of a concatenated argument is judged on its own. A
piece that opens or closes inside a `'…'` literal continued across
concatenation is a fragment, not a statement, so it is left to
`sql_literal_splice` and skipped here.

It must NOT flag:

- anything when the info tier is off — the default (all seven SQL
  advisories are tier-gated);
- ANSI `LEFT`/`RIGHT JOIN` — the suggested form;
- arithmetic like `(a + b)` — the marker is exactly the three-token
  `(+)` sequence with nothing between;
- `(+)` text inside character literals or SQL comments.

## Examples

### Flags

```ssl
aRows := SQLExecute("select o.ordno from orders o, results r where o.ordno = r.ordno(+)");
```

### Does not flag

```ssl
aRows := SQLExecute("select o.ordno from orders o left join results r on o.ordno = r.ordno");
```

### Does not flag

```ssl
nSum := SQLExecute("select (qty + 1) from orders where ordno = ?nOrd?");
```

## Rationale

`(+)` is Oracle-only (26 corpus statements), reads backwards to anyone
raised on ANSI joins, and dies at the door of a SQL Server environment —
STARLIMS installations are specifically one dialect or the other. Pairs
with `sql_comma_join`: the two rewrites usually happen together.
