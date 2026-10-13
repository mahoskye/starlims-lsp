---
id: diag.inline_default_outside_datasource
title: Inline := default on :PARAMETERS outside a data source
kind: diagnostic
status: active
authority: authoritative
schema_ref: null
default_severity: error
config:
  - ssl.diagnostics.rules
severity_overridable: true
suppressible: true
tests:
  - internal/providers/parameter_defaults_test.go
history:
  - date: 2026-10-13
    ref: "issue #264"
    note: >-
      Introduced as the mirror of no_default_statements_in_datasource. The
      reference (keywords/PARAMETERS.md) says the inline `:=` form "is only
      valid in data source files"; in a script or procedure the statement
      takes names only. v0.24.0 validated it clean. Zero findings on the
      production corpus (6,228 files).
issues: []
---

## Behavior

Runs only in files that are **not** data sources (data-source mode
detection, DECISIONS.md D15, is the gate: `.ds`/`.ds.txt` files in SSL or
SQL mode are never checked). Flags each `:PARAMETERS` name followed by an
inline `:=` default, in a script-level `:PARAMETERS` or a procedure's, at
error severity. The range runs from the name through the `:=`, so a client
quick fix can delete it and add a `:DEFAULT name, value;` line after the
`:PARAMETERS` statement. A default is read through the next top-level
`,` or `;`, so each name is reported once.

It must NOT flag a plain `:PARAMETERS a, b;`, or a `:DEFAULT` line, or any
`:=` outside a `:PARAMETERS` statement.

## Examples

### Flags

```ssl
:PARAMETERS sName := "a", nCount := 1;
```

### Flags

```ssl
:PROCEDURE Lookup;
	:PARAMETERS sKey, nLimit := 10;
	:RETURN sKey;
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE Lookup;
	:PARAMETERS sKey, nLimit;
	:DEFAULT nLimit, 10;
	:RETURN sKey;
:ENDPROC;
```

## Rationale

The reference documents the inline-default clause as "Data source files
only" and says the inline form "is only valid in data source files". An
ordinary script's `:PARAMETERS` takes names only, so the inline form does
not compile there, and the severity is error. The style guide has no
dedicated lint for this direction, so `schema_ref` is null and the
authority comes from the reference's grammar. Quick fixes are provided by
the client (vs-code-ssl-formatter's code-action provider keys fixes on the
diagnostic code).
