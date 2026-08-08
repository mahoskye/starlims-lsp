---
id: diag.datasource_sql_semicolon
title: Bare statement separator in a SQL data-source body
kind: diagnostic
status: active
authority: advisory
schema_ref: module_structure.data_source_modules.sql_data_source.comments
default_severity: warning
severity_overridable: true
suppressible: false
spec_options:
  is_data_source_file: true
tests:
  - internal/providers/sql_mode_test.go
history:
  - date: 2026-08-08
    ref: "issue #154, ssl-style-guide#50"
    note: >-
      Introduced with the SQL data-source semicolon work: the schema
      settles that semicolons inside SQL comments and quoted literals are
      content (never diagnostics), but stays silent on `;` statement
      separators in the body — the data source runs as a single SQL
      command, so a bare `;` is at best platform-dependent and gets an
      advisory warning rather than an authoritative error.
issues: ["ssl-style-guide#50"]
---

## Behavior

SQL-mode data-source rule (the file must be a data source — URI ending in
`.ds` / `.ds.txt`, or `--ds` on the CLI — whose content classifies as SQL
mode; the spec fences run with `is_data_source_file: true`). Warns on
every `;` that the SQL lexer sees as punctuation in the SQL body: outside
comments, outside quoted string literals, and — in the hybrid
header-then-SQL shape — past the directive header, whose own `;`
terminators belong to the header statements and never flag.

It must NOT flag:

- semicolons inside single-quoted SQL string literals
  (`'all;msoffice->pdf'`) — string content, per the schema's
  sql_data_source comment/literal semantics;
- semicolons inside `--` line comments or `/* ... */` block comments —
  comment content;
- the `;` terminating builder directives or `:PARAMETERS` in the header;
- anything in SSL files or SSL-content data sources — the check runs only
  on SQL-mode bodies.

Suppression comments cannot apply (`@ssl-disable` is an SSL comment form;
a SQL body has no SSL tokens), hence `suppressible: false`; use the
`ssl.diagnostics.rules` severity override (`"datasource_sql_semicolon":
"off"`) to silence it.

## Examples

### Flags

```ssl
update ESIGTYPES set STATUS = 'A';
update ESIGTYPES set STATUS = 'B' where ORIGREC = 2
```

### Does not flag

```ssl
update DOCTYPESCONVERSION
set FORMAT = 'all;msoffice->pdf'
where ORIGREC = 1
```

```ssl
/* examples: 'all;msoffice->pdf' and 'doc->pdf'; keep in sync */
select DOCTYPE from DOCTYPESCONVERSION
where FORMAT = 'all;msoffice->pdf' -- default is 'all;msoffice->pdf'; keep
```

```ssl
:DSN := "LimsDB";
:PARAMETERS pDocType;

select DOCTYPE from DOCTYPESCONVERSION where DOCTYPE = @pDocType
```

## Rationale

The style-guide schema
(module_structure.data_source_modules.sql_data_source.comments, section
level authoritative) settles that SQL data sources use SQL comment syntax
and that semicolons inside comments and quoted literals are content with
no syntactic significance — those cases must be silent, where they
previously drove the whole file into SSL parsing and produced token
errors (issue #154's UpdateDocTypes shape). What the schema deliberately
leaves undefined is a bare `;` between statements: the body is emitted
into a single GetSSLDataset call, so separator support depends on the
database platform. That earns an advisory warning — visible enough to
catch an accidental paste of a multi-statement script, soft enough that a
team whose platform accepts batches can turn it off — rather than an
authoritative error the schema does not back.
