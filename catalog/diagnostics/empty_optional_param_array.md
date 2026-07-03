---
id: diag.empty_optional_param_array
title: Trailing empty array passed for an optional parameter
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
  - date: 2026-03-21
    ref: "commit cdbfee6"
    note: >-
      Introduced during full alignment with ssl-style-guide: the guide
      recommends omitting trailing optional array arguments entirely rather
      than passing '{}'.
  - date: 2026-04-30
    ref: "PR #3 (v0.4.0, commit d744511)"
    note: Stable diagnostic code assigned; behavior unchanged.
issues: []
---

## Behavior

Flags a literal empty array `{}` passed as the second argument of a
`DoProc`, `ExecFunction`, or `GetDataSet` call (case-insensitive function
match) when it is also the last argument provided. The style guide
recommends omitting the trailing optional array instead. The range covers
the `{}` literal. Whitespace or comments inside the braces still count as
empty; anything else does not.

It must NOT flag:

- calls that omit the array entirely (`DoProc("Name")`);
- a non-empty array argument (`DoProc("Name", {1})`);
- an empty array followed by further arguments — `{}` is then a positional
  placeholder that cannot be omitted;
- `{}` passed to functions other than the three targets, or in other
  argument positions.

## Examples

### Flags

```ssl
:PROCEDURE Main;
	:DECLARE result;
	result := DoProc("MyHelper", {});
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE Main;
	:DECLARE result;
	result := DoProc("MyHelper", {1, 2});
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE Main;
	:DECLARE result;
	result := DoProc("MyHelper");
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE Main;
	:DECLARE result;
	result := ExecFunction("Mod.Proc", {}, .T.);
:ENDPROC;
```

## Rationale

Passing `{}` for a trailing optional parameter is harmless noise — the call
works — so this sits at info severity as a pure style nudge (cdbfee6). The
last-argument guard is the rule's precision core: when later arguments
exist, `{}` is load-bearing as a positional placeholder and removing it
would change the call, so the rule stays silent.
