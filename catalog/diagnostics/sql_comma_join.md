---
id: diag.sql_comma_join
title: Pre-ANSI comma join in a FROM clause
kind: diagnostic
status: active
authority: style_only
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
issues: []
---

## Behavior

Flags a `,` at parenthesis depth zero inside a `SELECT` statement's
`FROM` clause — the pre-ANSI comma-join form — in the first string
argument of a recognized embedded-SQL call. Detection runs over the SQL
lexer's token stream, so commas inside character literals, comments, and
subqueries never match. One diagnostic per string; the range covers the
string token.

It must NOT flag:

- anything when the info tier is off — the default (all seven SQL
  advisories are tier-gated);
- ANSI join syntax (`INNER JOIN … ON`) — the suggested form;
- commas in the SELECT list, in function arguments, in subqueries, in
  `IN (…)` lists, or inside `'a,b'` literals and `-- x, y` comments;
- non-SELECT statements (`DELETE FROM t` has no join surface).

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
