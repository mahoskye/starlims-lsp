---
id: diag.sql_comma_join
title: Pre-ANSI comma join in a FROM clause
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
  - internal/providers/datasource_sql_content_test.go
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
  - date: 2026-10-13
    ref: "issue #294"
    note: >-
      Also runs on SQL-mode data-source bodies, which previously got only
      the semicolon and placeholder checks. Corpus (--info, 6,228 files):
      +86 sql_comma_join, +164 sql_select_star, +16
      sql_inconsistent_alias, +1 sql_dialect_mix; no unicode or collate
      findings.
issues: []
---

## Behavior

Flags a `,` at parenthesis depth zero inside a `SELECT` statement's
`FROM` clause — the pre-ANSI comma-join form — in the first string
argument of a recognized embedded-SQL call. Detection runs over the SQL
lexer's token stream, so commas inside character literals, comments, and
subqueries never match. One diagnostic per string; the range covers the
string token.
Each string piece of a concatenated argument is judged on its own. A
piece that opens or closes inside a `'…'` literal continued across
concatenation is a fragment, not a statement, so it is left to
`sql_literal_splice` and skipped here.

It must NOT flag:

- anything when the info tier is off — the default (all seven SQL
  advisories are tier-gated);
- ANSI join syntax (`INNER JOIN … ON`) — the suggested form;
- commas in the SELECT list, in function arguments, in subqueries, in
  `IN (…)` lists, or inside `'a,b'` literals and `-- x, y` comments;
- non-SELECT statements (`DELETE FROM t` has no join surface).

It also runs on the body of a SQL-mode data source (D15), which is one
statement: the finding is ranged on the body's first word (issue #294).

## Examples

### Flags

```ssl
aRows := SQLExecute("select o.ordno, t.testcode from orders o, ordtask t where o.ordno = t.ordno");
```

### Does not flag

```ssl
aRows := SQLExecute("select o.ordno, t.testcode from orders o inner join ordtask t on o.ordno = t.ordno");
```

### Does not flag

```ssl
aRows := SQLExecute("select id, name from orders where kind in ('a', 'b') -- t1, t2
");
```

## Rationale

The dominant legacy pattern in the production corpus (486 statements,
259 files): comma joins scatter join conditions into the WHERE clause,
where they read as filters and go missing without a syntax error. ANSI
JOIN keeps each condition next to its table — the single
highest-leverage readability upgrade the review identified. Info tier:
rewriting joins is a behavior-affecting refactor no formatter should
push, so this observes and leaves the change to the author.
