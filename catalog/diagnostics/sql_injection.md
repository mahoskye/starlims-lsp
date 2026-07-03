---
id: diag.sql_injection
title: String concatenation in a SQL call argument
kind: diagnostic
status: active
authority: advisory
schema_ref: lints.security.prevent_sql_injection
default_severity: warning
severity_overridable: true
suppressible: true
tests:
  - internal/providers/providers_test.go
history:
  - date: 2026-03-28
    ref: "commit be7a174"
    note: >-
      Introduced (checkSQLConcatenationInjection), citing the schema's
      lints.security.prevent_sql_injection.
  - date: 2026-03-30
    ref: "commit 788e5b2"
    note: >-
      False positive fixed: concatenating the return value of
      BuildStringForIn() (safe, escaped IN-clause builder) no longer flags.
  - date: 2026-04-30
    ref: "PR #3 (v0.4.0, commit d744511)"
    note: Stable diagnostic code assigned; rule behavior unchanged.
  - date: 2026-05-01
    ref: "vs-code-ssl-formatter #28 (2025-11) -> #50/#51 (2025-12) -> #64 (2026-05)"
    note: >-
      Tooling policy on SQL-inside-strings reversed twice on the formatter
      side (don't touch string contents -> format SQL in strings by default
      -> mangled call sites, walked back again). Settled stance for the LSP:
      never parse or rewrite the SQL text itself; judge only the visible
      token shape of the call argument, which is what this rule does.
issues: []
---

## Behavior

Flags string concatenation in the **first argument** of a call to a known
SQL function (`SQLExecute` plus the parameterized family: `RunSQL`,
`LSearch`, `LSelect`, `LSelect1`, `LSelectC`, `GetDataSet`, `GetDataSetEx`,
`GetDataSetWithSchemaFromSelect`, `GetDataSetXMLFromSelect`,
`GetNETDataSet`, `GetTables`, `XmlExportSql`). Two shapes fire, both
warning severity with the range on the `+` operator:

- a string literal followed by `+` (e.g. `SQLExecute("... WHERE X = '" +
  sValue ...)`), unless the operand after `+` is a call to a known-safe SQL
  builder (`BuildStringForIn(...)`);
- an identifier followed by `+` followed by a string literal (e.g.
  `SQLExecute(sSql + " WHERE ...")`).

Only the first significant token of the first argument is judged, and at
most one diagnostic is emitted per call. The rule never inspects the SQL
text inside the string — only the concatenation shape at the call site.

It must NOT flag:

- parameterized queries with `?` / `?name?` placeholders and no
  concatenation in the argument;
- a plain variable argument (`SQLExecute(sSql)`), even if that variable was
  built by concatenation elsewhere — chasing dataflow was deliberately left
  out;
- concatenation with `BuildStringForIn(...)` after the leading string
  literal (safe escaped IN-clause values, 788e5b2);
- concatenation outside a SQL function call, e.g. in an ordinary
  assignment;
- identifier `+` identifier in the first argument (no string literal
  involved).

## Examples

### Flags

```ssl
:PROCEDURE Demo;
:DECLARE sName, aRows;
sName := "O'Brien";
aRows := SQLExecute("SELECT ID FROM CUSTOMERS WHERE NAME = '" + sName + "'");
:RETURN aRows;
:ENDPROC;
```

### Flags

```ssl
:PROCEDURE Demo;
:DECLARE sSql, aRows;
sSql := "SELECT ID FROM CUSTOMERS";
aRows := LSearch(sSql + " WHERE STATUS = 'OPEN'");
:RETURN aRows;
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE Demo;
:DECLARE sName, aRows;
sName := "O'Brien";
aRows := SQLExecute("SELECT ID FROM CUSTOMERS WHERE NAME = ?sName?");
:RETURN aRows;
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE Demo;
:DECLARE aIds, aRows;
aIds := {1, 2, 3};
aRows := SQLExecute("SELECT ID FROM CUSTOMERS WHERE ID IN (" + BuildStringForIn(aIds) + ")");
:RETURN aRows;
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE Demo;
:DECLARE sSql, aRows;
sSql := "SELECT ID FROM CUSTOMERS" + " WHERE STATUS = 'OPEN'";
aRows := SQLExecute(sSql);
:RETURN aRows;
:ENDPROC;
```

## Rationale

The schema declares `lints.security.prevent_sql_injection: true` and
`parameterized_queries_required: true` (inheriting the lints section's
advisory level — no per-rule severity is given, so the tool chose warning:
loud enough to read, not blocking, since concatenation of constants is
sometimes legitimate). The rule's precision budget was set by the
SQL-in-strings policy churn (extension #28 → #50/#51 → #64): tools that
tried to understand SQL inside string literals broke real code twice, so
this check judges only the concatenation shape visible in the call
argument. That is also why the plain-variable and outside-the-call cases
are permanent Does-not-flag fences — extending the rule with dataflow
would reopen that class of false positives. `BuildStringForIn` is the one
audited-safe concatenation (788e5b2).
