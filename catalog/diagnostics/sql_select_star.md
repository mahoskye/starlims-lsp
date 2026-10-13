---
id: diag.sql_select_star
title: SELECT * in embedded SQL
kind: diagnostic
status: active
authority: advisory
schema_ref: lints.performance.avoid_select_star
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
      Linked to the schema's lints.performance.avoid_select_star (authority
      style_only -> advisory, mirroring the lints level). EXISTS probes are no
      longer flagged: the old entry listed them under "must NOT flag" while
      the code flagged them. Production corpus: 447 -> 146.
issues: []
---

## Behavior

Flags `SELECT *` and `SELECT alias.*` at the head of a select list
(after optional `DISTINCT`/`ALL`) in the first string argument of a
recognized embedded-SQL call, including in subqueries and UNION
branches. A `SELECT` that opens an `EXISTS (…)` / `NOT EXISTS (…)`
subquery is skipped. One diagnostic per string; the range covers the
string token.
Each string piece of a concatenated argument is judged on its own. A
piece that opens or closes inside a `'…'` literal continued across
concatenation is a fragment, not a statement, so it is left to
`sql_literal_splice` and skipped here.

It must NOT flag:

- anything when the info tier is off — the default (all seven SQL
  advisories are tier-gated);
- `COUNT(*)` and any `*` inside parentheses — aggregate syntax, not a
  projection;
- explicit column lists — the suggested form;
- `select *` directly inside `EXISTS (…)` — a probe, not a projection:
  its columns are never read.

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

### Does not flag

```ssl
aRows := SQLExecute("select o.ordno from orders o where not exists (select * from ordtask t where t.ordno = o.ordno)");
```

## Rationale

`SELECT *` couples the caller to the table's current column order and
count — schema drift changes result shapes silently, and STARLIMS
arrays index columns by position. An explicit list survives schema
changes and documents what the code consumes. The schema states it as
`lints.performance.avoid_select_star` (lints level: advisory). An
`EXISTS` subquery's columns are never read, so the coupling concern does
not apply there: those probes were two thirds of the corpus hits before
the guard (issue #255). Info tier because the remaining hits include
deliberate ones (admin scripts).
