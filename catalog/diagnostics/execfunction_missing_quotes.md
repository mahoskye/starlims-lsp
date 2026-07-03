---
id: diag.execfunction_missing_quotes
title: Unquoted namespace path in ExecFunction/DoProc
kind: diagnostic
status: active
authority: tool
schema_ref: null
default_severity: error
severity_overridable: true
suppressible: true
tests:
  - internal/providers/providers_test.go
history:
  - date: 2026-02-02
    ref: "commit 7261172"
    note: >-
      Introduced with the SSL gotcha checks (Gotcha #8, dot notation):
      ExecFunction(Module.Proc, ...) silently misbehaves because the
      namespace path must be a quoted string.
  - date: 2026-04-30
    ref: "PR #3 (v0.4.0), commit d744511"
    note: Stable code execfunction_missing_quotes assigned.
issues: []
---

## Behavior

Always-on check: flags a call to `ExecFunction` or `DoProc` (matched
case-insensitively) whose **first argument** is a bare dotted namespace path
— an identifier immediately followed by a `.something` fragment — instead of
a quoted string. The reported range spans from the first-argument identifier
through the dotted fragment.

It must NOT flag when:

- the first argument is a quoted string (`ExecFunction("Module.Proc", ...)`)
  — the correct form;
- the first argument is a plain identifier with no dotted fragment after it
  (e.g. a variable holding the path);
- the dotted path appears in a later argument — only the first argument is
  inspected;
- the identifier is neither `ExecFunction` nor `DoProc`.

Note: the same bare `.Proc` fragment also independently triggers the general
`dot_property_access` diagnostic; this rule adds the call-specific fix hint.

## Examples

### Flags

```ssl
nResult := ExecFunction(Utils.Helper, {1});
```

### Flags

```ssl
DoProc(Reports.BuildSummary, {});
```

### Does not flag

```ssl
nResult := ExecFunction("Utils.Helper", {1});
```

### Does not flag

```ssl
:DECLARE sPath;
sPath := "Utils.Helper";
nResult := ExecFunction(sPath, {1});
```

## Rationale

Passing an unquoted namespace path is a classic SSL gotcha (commit 7261172,
Gotcha #8): the code does not do what it reads as doing, because dot notation
is not member access in SSL. Severity is error because the call cannot
succeed as written and the fix is mechanical — quote the path.
