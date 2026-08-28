---
id: diag.sql_select_star
title: SELECT * in embedded SQL
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

Flags `SELECT *` and `SELECT alias.*` at the head of a select list
(after optional `DISTINCT`/`ALL`) in the first string argument of a
recognized embedded-SQL call, including in subqueries and UNION
branches. One diagnostic per string; the range covers the string
token.

It must NOT flag:

- anything when the info tier is off — the default (all seven SQL
  advisories are tier-gated);
- `COUNT(*)` and any `*` inside parentheses — aggregate syntax, not a
  projection;
- explicit column lists — the suggested form;
- `EXISTS (select * …)`? — currently flagged like any other subquery
  star; promote or suppress per taste via `ssl.diagnostics.rules`.

## Examples

### Flags

```ssl
aRows := SQLExecute("select * from orders where ordno = ?nOrd?");
```

### Does not flag

```ssl
nCount := SQLExecute("select count(*) from orders");
```

### Does not flag

```ssl
aRows := SQLExecute("select ordno, folderno from orders");
```

## Rationale

`SELECT *` couples the caller to the table's current column order and
count — schema drift changes result shapes silently, and STARLIMS
arrays index columns by position. An explicit list survives schema
changes and documents what the code consumes. 447 corpus statements;
info tier because plenty are deliberate (EXISTS probes, admin
scripts).
