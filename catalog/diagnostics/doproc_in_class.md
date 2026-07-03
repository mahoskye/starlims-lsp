---
id: diag.doproc_in_class
title: DoProc call inside a class method
kind: diagnostic
status: active
authority: authoritative
schema_ref: lints.compile_errors.doproc_in_class
default_severity: error
severity_overridable: true
suppressible: true
tests:
  - internal/providers/providers_test.go
history:
  - date: 2026-03-21
    ref: "commit cdbfee6"
    note: >-
      Introduced during the full style-guide alignment pass, originally
      worded as a Me:/Base: style suggestion (already error severity).
  - date: 2026-03-28
    ref: "commit be7a174"
    note: >-
      Message hardened to state the runtime fact: DoProc inside class
      methods is a compile-time error in STARLIMS — all forms are rejected.
  - date: 2026-04-30
    ref: "PR #3 (v0.4.0)"
    note: Stable diagnostic code assigned when every diagnostic gained a Code.
issues: []
---

## Behavior

Fires an error on a `DoProc` identifier whose next significant token is
`(` when that call sits inside a class method — a `:PROCEDURE` that starts
after the file's `:CLASS` statement. All argument forms are rejected; the
suggested fix is `Me:MethodName()` / `Base:MethodName()`.

It must NOT flag:

- `DoProc(...)` in script files (no `:CLASS` in the file) — there it is
  the sanctioned way to call same-file procedures;
- a bare `DoProc` identifier not followed by `(`;
- tokens outside method bodies in a class file.

## Examples

### Flags

```ssl
:CLASS MyClass;
:PROCEDURE Run;
	DoProc("Helper", {});
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE Run;
	DoProc("Helper", {});
:ENDPROC;

:PROCEDURE Helper;
:ENDPROC;
```

## Rationale

The schema rule (`lints.compile_errors.doproc_in_class`, section level
`authoritative`, severity `error`) mirrors the STARLIMS compiler: DoProc
is rejected inside class methods in every form. Error severity and
authoritative authority match the schema. The be7a174 rewording (history)
replaced a softer style suggestion with the compile-error statement so
users do not assume the call would merely be unidiomatic.
