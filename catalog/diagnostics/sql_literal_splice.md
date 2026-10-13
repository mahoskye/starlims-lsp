---
id: diag.sql_literal_splice
title: SQL character literal spliced across concatenation
kind: diagnostic
status: active
authority: advisory
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
      Quotes are now counted on the SQL lexer's stream, outside SQL
      comments, and only a concatenated piece counts as a splice. An
      apostrophe in a `--` comment used to read as an open literal and also
      hid every other SQL advisory on that string. Authority style_only ->
      advisory (style guide prevent_sql_injection). The deliberate double
      emission with sql_injection is now stated. Corpus count unchanged.
issues: []
---

## Behavior

Flags a string piece of a recognized embedded-SQL call's first argument
that is an operand of SSL `+` and leaves a `'…'` character literal open
for a later piece to close. This is the signature of a literal continued
across concatenation (`"… where name = '" + sName + "'"`). Single quotes
are counted on the SQL lexer's token stream, outside SQL comments and
double-quoted identifiers; a doubled `''` escape counts two. Literal
state carries across the call's pieces, so the closing piece (`"'"`) is
read as the literal's tail. Reported once per call, on the opening
piece; the range covers that string token.

This rule and `sql_injection` usually fire on the same call, and that is
deliberate. `sql_injection` (warning) reports the runtime value
concatenated into SQL text. This rule (info) names the splice idiom
itself, which costs formatting and parameter-API visibility even when
the spliced value is provably safe.

It must NOT flag:

- anything when the info tier is off — the default (all seven SQL
  advisories are tier-gated);
- balanced literals (`where kind = 'A'`) — complete in one string;
- `?param?` placeholders — the suggested form;
- an apostrophe inside a SQL comment (`-- it's`) — not a literal;
- a string with an unbalanced quote that is not concatenated — whatever
  it is, it is not a splice;
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

### Does not flag

```ssl
aRows := SQLExecute("select id from t where kind = 'A' -- it's the default kind
");
```

## Rationale

The splice idiom is triply expensive: it is the formatter's
byte-preserve-only class (issue #216 — no layout help possible), it is
invisible to the parameter APIs, and it is `sql_injection`'s exact
attack surface — this advisory names the idiom before either bites. 343
corpus statements across 193 files. The style guide's
`prevent_sql_injection` record (level advisory) uses this exact idiom as
its Bad example, so the rule mirrors that level. `sql_injection` (warning) fires on
the concatenation risk itself; this info note points at the idiom even
where the spliced value is provably safe, because the placeholder form
is better on all three axes.
