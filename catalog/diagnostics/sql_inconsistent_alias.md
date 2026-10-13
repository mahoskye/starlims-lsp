---
id: diag.sql_inconsistent_alias
title: SELECT list mixing AS and bare aliases
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
      False positive fixed: the TOP count was left in the first item, so
      `select top 10 col, x as y` read `10 col` as a bare alias and flagged a
      consistent list. The whole TOP clause is now skipped. Authority
      corrected style_only -> tool: the style guide has no record for it.
issues: []
---

## Behavior

Flags a `SELECT` list that mixes explicit `AS alias` and bare
`expr alias` column aliases, in the first string argument of a
recognized embedded-SQL call. Items are split at depth-zero commas
between `SELECT` and `FROM`; an item classifies as bare-aliased when it
ends with an identifier directly following another value token
(identifier, number, string, placeholder, or `)`) with no dot chaining.
A leading `DISTINCT` and a `TOP` clause (the keyword, its count `10` or
`(10)`, and any `PERCENT` / `WITH TIES`) are not part of the first item.
One diagnostic per string when both forms are present; the range covers
the string token.
Each string piece of a concatenated argument is judged on its own. A
piece that opens or closes inside a `'…'` literal continued across
concatenation is a fragment, not a statement, so it is left to
`sql_literal_splice` and skipped here.

It must NOT flag:

- anything when the info tier is off — the default (all seven SQL
  advisories are tier-gated);
- a list using only `AS` aliases, or only bare aliases, or no aliases —
  consistency is the rule, not either form;
- qualified column references (`t.col` is a dot chain, not a bare
  alias);
- `DISTINCT`/`TOP` prefixes and `*` items — in particular a `TOP 10`
  count is never read as an item whose trailing column is a bare alias.

## Examples

### Flags

```ssl
aRows := SQLExecute("select o.ordno as orderno, o.folderno fno from orders o");
```

### Does not flag

```ssl
aRows := SQLExecute("select o.ordno as orderno, o.folderno as fno from orders o");
```

### Does not flag

```ssl
aRows := SQLExecute("select o.ordno, o.folderno from orders o");
```

### Does not flag

```ssl
aRows := SQLExecute("select top 10 o.ordno, o.folderno as fno from orders o");
```

## Rationale

A mixed list makes the reader parse each item twice — once to find the
expression, once to decide whether the trailing word is an alias or a
typo. Uniform explicit `AS` is the researched convention (Holywell et
al.) and what the corpus's cleanest files already do. Rare in practice
(3 corpus statements) — this is a keep-it-clean rule, not a cleanup
campaign.
