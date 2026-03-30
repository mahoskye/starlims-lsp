# Data Sources vs Server Scripts: :PARAMETERS Differences

This document describes how `:PARAMETERS` and related directives differ between
server scripts and data source files.

---

## Server Scripts (compiler-handled)

In standard scripts, the SSL compiler parses `:PARAMETERS` and `:DEFAULT` as separate keywords:

```ssl
:PARAMETERS p1, p2, p3;
:DEFAULT p1, 'value1';
:DEFAULT p2, 100;
```

- `:PARAMETERS` lists param names only (no defaults inline)
- `:DEFAULT` is a separate statement: `:DEFAULT <ident>, <expression>;`
- Parsed into AST nodes, compiled to IL

---

## SSL Data Sources (server-preprocessed)

`SSLDataSourceBuilder` handles `:PARAMETERS` via regex preprocessing before compilation:

```ssl
:PARAMETERS p1 := 'value1', p2 := 100;
```

- Parameters use `:=` inline assignment for defaults
- Every parameter must have a default (throws error otherwise)
- `:PARAMETERS;` with no params is an error
- The builder rewrites it into compiler-compatible form: splits into
  `:PARAMETERS p1, p2;` + `:DEFAULT p1, 'value1';` + `:DEFAULT p2, 100;`

### Structure

1. Header comment (optional)
2. `:PARAMETERS` with inline `:=` defaults
3. SSL script body

---

## SQL Data Sources (also server-preprocessed)

`SqlDataSourceBuilder` has additional directives that only exist in SQL data sources:

```ssl
:DSN := connectionName;              /* database connection;
:TABLENAME := name;                  /* dataset table name;
:NULLASBLANK := true;                /* null-to-blank conversion;
:INVARIANTDATECOLUMNS := col1, col2; /* invariant date columns;
```

- `:PARAMETERS` uses the same `:=` syntax as SSL data sources
- The whole thing gets rewritten into an SSL script calling `GetSSLDataset()`

### Structure

1. Header comment (optional)
2. Builder directives (optional): `:DSN`, `:TABLENAME`, `:NULLASBLANK`, `:INVARIANTDATECOLUMNS`
3. `:PARAMETERS` with inline `:=` defaults (optional)
4. SQL query

---

## Lint Rules

| Rule | Severity | Description |
|------|----------|-------------|
| `no_default_statements_in_datasource` | error | Data source files use inline `:=` defaults in `:PARAMETERS`, not separate `:DEFAULT` statements |
| `no_flag_builder_directives` | info | `:DSN`, `:TABLENAME`, `:NULLASBLANK`, `:INVARIANTDATECOLUMNS` are valid builder directives in SQL data source files |
| `datasource_default_required` | error | Every parameter in a data source `:PARAMETERS` declaration must have an inline `:=` default value |

---

## File Extensions

| Extension | Type |
|-----------|------|
| `.srvscr`, `.ssl`, `.ssl.txt` | Server scripts / classes |
| `.ds`, `.ds.txt` | Data source files (SSL or SQL) |
