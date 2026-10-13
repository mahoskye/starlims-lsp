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
  - date: 2026-10-13
    ref: "issue #255 (catalog audit)"
    note: >-
      Rewritten on the expression AST (CollectCalls): the first argument's
      `+` chain is flattened and flagged only when a string-literal operand
      meets a runtime operand. Fixes: constant-only concatenation no longer
      flags; a leading BuildStringForIn(...) no longer hides a later splice;
      a leading call or parenthesized argument is seen through. The
      BuildStringForIn fence now appends the builder's own parenthesized list
      (`IN " + BuildStringForIn(a)`), as the function reference documents.
      Production corpus: 1,702 -> 1,652 (50 constant-only concatenations
      dropped, 2 newly caught, 4 ranges moved to the runtime operand's `+`).
issues: []
---

## Behavior

Flags string concatenation in the **first argument** of a call to a known
SQL function (`SQLExecute` plus the parameterized family: `RunSQL`,
`LSearch`, `LSelect`, `LSelect1`, `LSelectC`, `GetDataSet`, `GetDataSetEx`,
`GetDataSetWithSchemaFromSelect`, `GetDataSetXMLFromSelect`,
`GetNETDataSet`, `GetTables`, `XmlExportSql`). The argument is judged on
its expression tree: the `+` chain is flattened (parentheses unwrapped)
into operands, and the call is flagged when

- at least one operand is a string literal — the concatenation builds
  SQL text; and
- at least one operand is a runtime value — anything other than a string
  or number literal, `.T.`/`.F.`/`NIL`, or a call to a known-safe SQL
  builder (`BuildStringForIn(...)`).

Warning severity, at most one diagnostic per call, with the range on the
`+` next to the first runtime operand (the one before it, or after it
when it leads the chain). The rule never inspects the SQL text inside
the strings, only the shape of the SSL expression.

It must NOT flag:

- parameterized queries with `?` / `?name?` placeholders and no
  concatenation in the argument;
- a plain variable argument (`SQLExecute(sSql)`), even if that variable was
  built by concatenation elsewhere — chasing dataflow was deliberately left
  out;
- concatenation of constants only (`"SELECT …" + " WHERE …"`) — no
  runtime value reaches the SQL text;
- concatenation whose only non-literal operand is `BuildStringForIn(...)`
  (safe escaped IN-clause values, 788e5b2) — but a safe builder does not
  excuse a later runtime operand in the same chain;
- concatenation outside a SQL function call, or in a later argument;
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

### Flags

```ssl
:PROCEDURE Demo;
:DECLARE aIds, sName, aRows;
aIds := {"A1", "B2"};
sName := "O'Brien";
aRows := SQLExecute("SELECT ID FROM CUSTOMERS WHERE ID IN " + BuildStringForIn(aIds) + " AND NAME = '" + sName + "'");
:RETURN aRows;
:ENDPROC;
```

### Flags

```ssl
:PROCEDURE Demo;
:DECLARE sSql, sName, aRows;
sSql := "SELECT ID FROM CUSTOMERS";
sName := "O'Brien";
aRows := SQLExecute(Upper(sSql) + " WHERE NAME = '" + sName + "'");
:RETURN aRows;
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE Demo;
:DECLARE aRows;
aRows := SQLExecute("SELECT ID FROM CUSTOMERS" + " WHERE STATUS = 'OPEN'");
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
aIds := {"A1", "B2"};
aRows := SQLExecute("SELECT ID FROM CUSTOMERS WHERE ID IN " + BuildStringForIn(aIds));
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
advisory level; the style-guide record `prevent_sql_injection` sets
warning). Warning, not error: a concatenated runtime value is a likely
defect, not a certain one, because the record's own exception allows a
fixed fragment chosen by the code itself. Constant-only concatenation is
never flagged, since no runtime value reaches the SQL text. The rule's precision budget was set by the
SQL-in-strings policy churn (extension #28 → #50/#51 → #64): tools that
tried to understand SQL inside string literals broke real code twice, so
this check judges only the concatenation shape visible in the call
argument — since issue #255 on the argument's expression tree rather than
its first token. That is also why the plain-variable and outside-the-call cases
are permanent Does-not-flag fences — extending the rule with dataflow
would reopen that class of false positives. `BuildStringForIn` is the one
audited-safe concatenation (788e5b2).
