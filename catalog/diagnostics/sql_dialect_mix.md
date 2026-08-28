---
id: diag.sql_dialect_mix
title: Oracle-only and SQL Server-only idioms in one statement
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

Flags a single embedded-SQL string containing markers from both
dialect-exclusive idiom sets: Oracle (`SYSDATE`, `NVL`, `NVL2`,
`DECODE`, `ROWNUM`, `DUAL`, `TO_DATE`, `TO_CHAR`, `TO_NUMBER`,
`LISTAGG`, `(+)`) and SQL Server (`GETDATE`, `ISNULL`, `CHARINDEX`,
`DATEADD`, `DATEDIFF`, `NEWID`, `NOLOCK`). The message names the
markers found on each side. One diagnostic per string; the range covers
the string token.

It must NOT flag:

- anything when the info tier is off — the default (all seven SQL
  advisories are tier-gated);
- statements using only one dialect's idioms — the normal case
  (STARLIMS environments run one dialect; the corpus scan found zero
  mixed statements);
- dialect-neutral ODBC escapes (`{fn …}`, `{d …}`) — portable by
  design, in neither marker set;
- marker names inside character literals or comments.

## Examples

### Flags

```ssl
aRows := SQLExecute("select nvl(a, 0) from t where d > getdate()");
```

### Does not flag

```ssl
aRows := SQLExecute("select nvl(a, 0) from t where d > sysdate");
```

### Does not flag

```ssl
aRows := SQLExecute("select {fn ifnull(a, 0)} from t");
```

## Rationale

A statement mixing `SYSDATE` with `GETDATE()` cannot run anywhere —
each STARLIMS environment is specifically Oracle or specifically MSSQL
(corpus owner). Zero corpus hits confirms the rule's precision: it is a
tripwire for copy-paste across environments, not a cleanup list. The
marker sets are deliberately short and exclusive; expanding them trades
precision for coverage and should be corpus-tested first.
