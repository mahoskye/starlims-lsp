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
  - internal/providers/datasource_sql_content_test.go
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
  - date: 2026-10-13
    ref: "issue #255 (catalog audit)"
    note: >-
      Moved from a raw `\bN'` regex onto the SQL lexer. The regex matched
      the very common 'N' flag literal (the quote before N is a word
      boundary), ?'N'? placeholders, and N' text in SQL comments. Production
      corpus: 691 -> 0; none of the 691 was a real N'…' literal. Data-source
      coverage gap recorded (issue #294).
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

Info tier (`ssl.diagnostics.infoDiagnostics`, default off). Flags a
string token in the first argument of a recognized embedded-SQL function
call (`SQLExecute` plus the positional family) whose SQL holds an `N'…'`
Unicode literal. Detection runs on the SQL lexer's token stream: an `N`
word (either case) directly followed, with no whitespace, by a
single-quoted literal. Literal state carries across concatenated pieces,
so the tail of a spliced literal lexes in step. One diagnostic per string
token; the range covers the token.

It must NOT flag:

- anything when the info tier is off — the default;
- `N'` sequences in strings that are not SQL arguments (the word-boundary
  and call-site guards keep prose strings out);
- words merely ending in N followed by a quote (`COLUMN'...'` does not
  match — the `N` must be a whole word);
- an `'N'` literal, a `?'N'?` placeholder, or `N'` text inside a SQL
  comment — none is an `N` word before a literal;
- SQL without the prefix — plain `'...'` literals.

It also runs on the body of a SQL-mode data source (D15), which is one
statement: the finding is ranged on the body's first word (issue #294).

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
	SQLExecute("UPDATE SAMPLES SET FLAG = 'N' WHERE ACTIVE = ?'N'? -- no N'x' needed
");
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
promote this rule) to catch copy-paste imports. Info severity: an
`N'…'` literal changes the literal's type, which can change how a
comparison against a non-Unicode column is evaluated (the style guide's
`unicode_literal_prefix` record). The rule observes; whether the prefix
is needed is the author's call.
