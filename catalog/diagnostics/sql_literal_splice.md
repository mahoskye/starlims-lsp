---
id: diag.sql_literal_splice
title: SQL character literal spliced across concatenation
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

Flags a detected embedded-SQL string whose content holds an unbalanced
(odd) number of single quotes — the signature of a `'…'` character
literal continued across SSL string concatenation
(`"… where name = '" + sName + "'"`). Once per call, on the first
odd-quote piece; the range covers that string token.

It must NOT flag:

- anything when the info tier is off — the default (all seven SQL
  advisories are tier-gated);
- balanced literals (`where kind = 'A'`) — complete in one string;
- `?param?` placeholders — the suggested form;
- non-SQL strings (the call-site gate applies as usual).

## Examples

### Flags

```ssl
sSql := SQLExecute("select id from t where name = '" + sName + "'");
```

### Does not flag

```ssl
aRows := SQLExecute("select id from t where name = ?sName?");
```

### Does not flag

```ssl
aRows := SQLExecute("select id from t where kind = 'A'");
```

## Rationale

The splice idiom is triply expensive: it is the formatter's
byte-preserve-only class (issue #216 — no layout help possible), it is
invisible to the parameter APIs, and it is `sql_injection`'s exact
attack surface — this advisory names the idiom before either bites. 343
corpus statements across 193 files. `sql_injection` (warning) fires on
the concatenation risk itself; this info note points at the idiom even
where the spliced value is provably safe, because the placeholder form
is better on all three axes.
