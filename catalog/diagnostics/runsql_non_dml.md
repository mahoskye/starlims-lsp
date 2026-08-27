---
id: diag.runsql_non_dml
title: RunSQL called with a result-returning statement
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
  - date: 2026-08-26
    ref: "issue #195"
    note: >-
      Introduced from the runtime-verification batch: RunSQL is for DML;
      a SELECT/WITH query's result set is silently discarded.
issues: []
---

## Behavior

Flags a `RunSQL` call whose first argument's SQL text begins — after
stripping leading whitespace, `--` line comments, and `/* */` block
comments — with the keyword `SELECT` or `WITH` (case-insensitive). The
first string piece of the argument decides the leading keyword; when the
argument is a concatenation, all its string pieces are consulted for the
write-guards below. The range covers the first string token.

It must NOT flag:

- DML statements (`INSERT`, `UPDATE`, `DELETE`, `MERGE`, `EXEC`, ...) —
  the sanctioned RunSQL use;
- `SELECT ... INTO` — the statement writes, so RunSQL is legitimate
  (guard: the word `INTO` anywhere in the argument text);
- `WITH`-wrapped DML (`WITH cte AS (...) UPDATE ...`) — guard: any of
  `INSERT`/`UPDATE`/`DELETE`/`MERGE` anywhere in the argument text;
- SELECT/WITH strings passed to the result-returning APIs
  (`LSearch`, `LSelect`, `GetDataSet`, ...) — that is the prescribed fix;
- calls whose first argument is not a string literal (a variable-built
  query is not provable).

## Examples

### Flags

```ssl
:PROCEDURE Main;
	RunSQL("SELECT STATUS FROM SAMPLES WHERE ID = ?", "CONN", {1});
:ENDPROC;
```

### Flags

```ssl
:PROCEDURE Main;
	RunSQL("/* fetch; */ WITH recent AS (SELECT ID FROM SAMPLES) SELECT * FROM recent", "CONN");
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE Main;
	RunSQL("UPDATE SAMPLES SET STATUS = ? WHERE ID = ?", "CONN", {"DONE", 1});
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE Main;
	RunSQL("SELECT ID, STATUS INTO ARCHIVE_SAMPLES FROM SAMPLES WHERE DONE = 1", "CONN");
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE Main;
	RunSQL("WITH done AS (SELECT ID FROM SAMPLES WHERE DONE = 1) UPDATE SAMPLES SET ARCHIVED = 1 WHERE ID IN (SELECT ID FROM done)", "CONN");
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE Main;
	:DECLARE aRows;
	aRows := LSelect("SELECT STATUS FROM SAMPLES WHERE ID = ?", "", "CONN", {1});
:ENDPROC;
```

## Rationale

`RunSQL` executes and discards — a SELECT routed through it runs on the
server and returns nothing, which reads like a working query and behaves
like a dropped result (issue #195). Warning severity: the call is not an
error at the API level, but it is nearly always a wrong-API pick. The
`INTO` and WITH-wrapped-DML guards are the precision core — both shapes
genuinely write, and flagging them would train users to ignore the rule.
