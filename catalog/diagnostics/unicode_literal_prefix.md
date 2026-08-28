---
id: diag.unicode_literal_prefix
title: N'...' Unicode literal prefix in embedded SQL
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
  - date: 2026-08-26
    ref: "issue #196"
    note: >-
      Introduced from the runtime-verification batch as an opt-in style
      rule, default off: most schemas don't need N'...' and it creeps in
      via copy-paste.
  - date: 2026-08-27
    ref: "issue #208 discussion (info-tier expansion)"
    note: >-
      Moved hint -> info in the info-tier expansion: the dedicated
      ssl.diagnostics.unicodeLiteralPrefix toggle (never released) is
      subsumed by the tier. Info is the opt-in advisory tier
      (ssl.diagnostics.infoDiagnostics); explicit ssl.diagnostics.rules
      entries still promote or disable per rule.
issues: []
---

## Behavior

Info tier (`ssl.diagnostics.infoDiagnostics`, default off). Flags a
string token containing an `N'` Unicode literal prefix (case-insensitive,
word boundary before the `N`) when the token is part of the first
argument of a recognized embedded-SQL function call (`SQLExecute` plus
the positional family). One diagnostic per string token; the range covers
the token.

It must NOT flag:

- anything when the info tier is off — the default;
- `N'` sequences in strings that are not SQL arguments (the word-boundary
  and call-site guards keep prose strings out);
- words merely ending in N followed by a quote (`COLUMN'...'` does not
  match — the `N` must start its word);
- SQL without the prefix — plain `'...'` literals.

## Examples

### Flags

```ssl
:PROCEDURE Main;
	SQLExecute("UPDATE SAMPLES SET NOTE = N'checked' WHERE ID = ?nId?");
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE Main;
	SQLExecute("UPDATE SAMPLES SET NOTE = 'checked' WHERE ID = ?nId?");
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE Main;
	:DECLARE sMsg;
	sMsg := "PLAN'N'GO";
:ENDPROC;
```

## Rationale

Whether `N'...'` is needed is a schema property (NVARCHAR columns and
genuinely non-ASCII data) the LSP cannot see, so this cannot be an
always-on rule — hence the opt-in info tier, per the issue #196
proposal. Teams whose schemas are plain VARCHAR enable the tier (or
promote this rule) to catch copy-paste imports. Info severity: the
prefix is harmless at worst (minor implicit-conversion cost), so the
rule only observes.
