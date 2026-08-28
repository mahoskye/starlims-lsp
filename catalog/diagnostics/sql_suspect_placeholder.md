---
id: diag.sql_suspect_placeholder
title: Template marker stacked inside a placeholder
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

Flags a `?…?` placeholder whose interior contains a `<<…>>` template
marker (`?'<<username>>'?`) in the first string argument of a
recognized embedded-SQL call — two substitution layers stacked, whose
combined resolution is unverified. One diagnostic per string; the range
covers the string token.

It must NOT flag:

- anything when the info tier is off — the default (all seven SQL
  advisories are tier-gated);
- plain named placeholders (`?sName?`) — the supported shape;
- quoted-literal placeholders (`?'Y'?`, `?'N/A'?`) — an established
  production idiom (271 corpus uses across 99 files) that evidently
  substitutes; only the template-marker stacking is suspect;
- `<<…>>` text outside placeholders.

## Examples

### Flags

```ssl
SQLExecute("insert into t (who) values (?'<<username>>'?)");
```

### Does not flag

```ssl
SQLExecute("insert into t (flag) values (?'Y'?)");
```

### Does not flag

```ssl
SQLExecute("insert into t (who) values (?sUser?)");
```

## Rationale

Corpus-owner ruling during the #217 fix: `?'<<username>>'?` "appears
to be bad code" — the formatter byte-preserves it (never legitimizes or
rewrites), and pointing at it is this advisory's job. The initial
detection draft flagged all quoted placeholders and the corpus overruled
it: 271 legitimate `?'literal'?` uses. Only the `<<…>>` stacking
remains suspect; zero corpus hits, so the rule is a pure tripwire.
Runtime verification pairs with the issue #210 LIMS-environment
session.
