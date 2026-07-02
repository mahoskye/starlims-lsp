---
id: diag.complex_sql_placeholder
title: Complex expression inside a SQLExecute named placeholder
kind: diagnostic
status: active
authority: tool
schema_ref: null
default_severity: info
severity_overridable: true
suppressible: true
tests:
  - internal/providers/providers_test.go
history:
  - date: 2026-03-28
    ref: "commit be7a174"
    note: >-
      Introduced during the diagnostics expansion; sourced from
      ssl_agent_instructions.md gotcha #20/#21 — only arithmetic and
      concatenation expressions are "complex"; property access, array
      indexing, and function calls are standard supported placeholder forms.
  - date: 2026-04-30
    ref: "PR #3 (v0.4.0, commit d744511)"
    note: Stable diagnostic code assigned; behavior unchanged.
issues: []
---

## Behavior

Flags a named placeholder `?expr?` in the first string argument of a
`SQLExecute(...)` call (case-insensitive function match) when `expr` is a
complex expression — anything that does not match the standard placeholder
grammar `identifier` optionally followed by `. : [ ] ( ) , '` characters. In
practice that means expressions containing spaces or arithmetic /
concatenation operators (`?sPrefix + sSuffix?`). The range covers just the
placeholder inside the string.

It must NOT flag:

- standard placeholder forms: simple variables `?sVar?`, property access
  `?oObj:Prop?`, array indexing `?aArr[1]?`, and function calls `?Today()?`;
- positional `?` placeholders;
- placeholders in SQL strings passed to functions other than `SQLExecute`
  (other rules cover those);
- strings after the first string argument of the call — only the first
  string is inspected.

## Examples

### Flags

```ssl
:PROCEDURE Demo;
	:DECLARE aRows;
	aRows := SQLExecute("SELECT * FROM T WHERE Code = ?sPrefix + sSuffix?");
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE Demo;
	:DECLARE aRows;
	aRows := SQLExecute("SELECT * FROM T WHERE Code = ?sCode?");
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE Demo;
	:DECLARE aRows;
	aRows := SQLExecute("SELECT * FROM T WHERE Id = ?oUser:ID? AND D = ?Today()? AND N = ?aIds[1]?");
:ENDPROC;
```

## Rationale

Named placeholders are re-evaluated on every query execution, so embedding an
arithmetic/concatenation expression silently costs performance; pre-computing
into a variable is both faster and clearer. Info severity because the code
runs correctly — this is purely a performance/readability nudge, and the
standard forms (property access, indexing, function calls) are deliberately
exempt as supported patterns (be7a174).
