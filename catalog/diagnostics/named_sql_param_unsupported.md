---
id: diag.named_sql_param_unsupported
title: Named SQL parameter used with a positional-only function
kind: diagnostic
status: active
authority: tool
schema_ref: null
default_severity: warning
severity_overridable: true
suppressible: true
tests:
  - internal/providers/providers_test.go
history:
  - date: 2026-02-02
    ref: "commit 7261172"
    note: Introduced in the gotcha-detection pass (gotcha #7).
  - date: 2026-03-28
    ref: "commit be7a174"
    note: >-
      Placeholder detection moved to the shared SQL placeholder parser
      (ParseSQLPlaceholders), used by all SQL-string diagnostics.
  - date: 2026-04-30
    ref: "PR #3 (v0.4.0, commit d744511)"
    note: Stable diagnostic code assigned; behavior unchanged.
issues: []
---

## Behavior

Flags a call to a positional-parameter SQL function whose first string
argument contains a named `?name?` placeholder. Only `SQLExecute` supports
named substitution; the positional family — `GetDataSet`, `GetDataSetEx`,
`GetDataSetWithSchemaFromSelect`, `GetDataSetXMLFromSelect`,
`GetNETDataSet`, `GetTables`, `RunSQL`, `LSearch`, `LSelect`, `LSelect1`,
`LSelectC`, `XmlExportSql` — passes `?name?` through to the database
verbatim, where it fails or silently mismatches. Warning severity, ranged
on the offending string token, at most one diagnostic per string. Function
name matching is case-insensitive; only the first top-level string argument
of the call is inspected.

It must NOT flag:

- positional `?` placeholders with a value array
  (`RunSQL("... = ?", "CONN", {sValue})`);
- named placeholders in `SQLExecute` calls — that is the supported form;
- SQL strings that are not the first string argument of a recognized
  positional SQL function (the rule inspects call sites, not free-standing
  strings).

## Examples

### Flags

```ssl
RunSQL("UPDATE SAMPLES SET STATUS = ?sStatus?", "CONN");
```

### Flags

```ssl
aRows := LSearch("SELECT ID FROM SAMPLES WHERE STATUS = ?sStatus?", "", "", {});
```

### Does not flag

```ssl
RunSQL("UPDATE SAMPLES SET STATUS = ?", "CONN", {sStatus});
```

### Does not flag

```ssl
aRows := SQLExecute("SELECT ID FROM SAMPLES WHERE STATUS = ?sStatus?");
```

## Rationale

The style guide's SQL parameters section (support_level: authoritative)
records that only `SQLExecute` supports `?name?` substitution while the
other database functions take positional `?` with parameter arrays; the
function list in `constants.ParameterizedSQLFunctions` mirrors it. Passing
a named placeholder to a positional function is a real bug, but the check
reads only the literal first string argument — dynamically built SQL is
invisible to it — so warning severity is honest about coverage. Severity
and both boundary cases are pinned in providers_test.go
(TestGetDiagnostics_NamedSQLParamsWithWrongFunction*).
