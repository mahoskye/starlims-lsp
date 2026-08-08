---
id: diag.datasource_undeclared_placeholder
title: Undeclared @name placeholder in a SQL data-source body
kind: diagnostic
status: active
authority: authoritative
schema_ref: module_structure.data_source_modules.lint_rules.datasource_undeclared_placeholder
default_severity: warning
severity_overridable: true
suppressible: false
spec_options:
  is_data_source_file: true
tests:
  - internal/providers/sql_mode_test.go
history:
  - date: 2026-08-08
    ref: "ssl-style-guide#51/#53"
    note: >-
      Introduced once the schema documented @name body placeholders for
      SQL data sources (body_parameters) and anchored this rule under
      lint_rules: a @name with no matching :PARAMETERS declaration is not
      substituted and fails when the query executes; a declared parameter
      never referenced is harmless and stays silent.
issues: ["ssl-style-guide#51"]
---

## Behavior

SQL-mode data-source rule (the spec fences run with
`is_data_source_file: true`). Warns on every `@name` placeholder in the
SQL body — `@` immediately followed by a name, outside comments and
string literals — whose name has no case-insensitive match among the
`:PARAMETERS` declarations in the header. In the whole-document SQL shape
there is no header, so every placeholder is undeclared. Ranged over the
full `@name` span, offset past the header in the hybrid shape.

It must NOT flag:

- a placeholder matching a declared parameter in any casing — matching is
  case-insensitive, per the schema's body_parameters note;
- `@@name` — a database system function, not a placeholder;
- `@name` inside string literals or SQL comments — content, per the
  schema's sql_data_source comment/literal semantics;
- a declared parameter that the body never references — harmless per the
  schema; there is no unused-parameter diagnostic;
- anything in a body containing a `DECLARE` keyword outside comments and
  literals — a body that declares its own SQL variables is scripted SQL
  where `@name` may be a local variable, so the check bows out entirely;
- anything in SSL files or SSL-content data sources — `@name` is not part
  of the ssl_data_source form, whose body references parameters as
  ordinary variables.

As with `datasource_sql_semicolon`, suppression comments cannot apply in
a SQL body (`suppressible: false`); use the `ssl.diagnostics.rules`
override to silence it.

## Examples

### Flags

```ssl
:DSN := "LimsDB";
:PARAMETERS pFolderNo;

select SAMPLEID from SAMPLES where FOLDERNO = @pFolder
```

```ssl
select SAMPLEID from SAMPLES where FOLDERNO = @pFolderNo
```

### Does not flag

```ssl
:DSN := "LimsDB";
:PARAMETERS pFolderNo;

select SAMPLEID from SAMPLES where FOLDERNO = @PFOLDERNO
```

```ssl
:PARAMETERS pFolderNo;

select @@ROWCOUNT from SAMPLES where FOLDERNO = @pFolderNo
```

```ssl
select SAMPLEID from SAMPLES where NOTE = '@pX' -- mentions @pY
```

```ssl
:PARAMETERS pFolderNo;

select SAMPLEID from SAMPLES
```

## Rationale

The style-guide schema documents `@name` as the placeholder form SQL
data-source bodies use to reference `:PARAMETERS` values
(sql_data_source.body_parameters, added in ssl-style-guide#53) and
anchors this rule under data_source_modules.lint_rules at warning level:
an undeclared placeholder is not substituted and fails at execute time —
but warning rather than error, because a SQL body may legitimately carry
`@` names that are not data-source parameters (system `@@` functions,
`DECLARE`d locals in scripted bodies). The check excludes both
structurally — `@@` by shape, `DECLARE` bodies by bowing out entirely —
so what remains flagged is the high-confidence case: a placeholder-shaped
name in a declarative body with no declaration behind it, most often a
typo of a declared parameter.
