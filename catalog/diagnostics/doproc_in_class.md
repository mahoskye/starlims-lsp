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
  - date: 2026-08-08
    ref: "issue #151, ssl-style-guide#49"
    note: >-
      Narrowed: qualified string-literal targets
      (`DoProc("Category.Script.Procedure", ...)`) are valid inside class
      methods — confirmed against runtime; the schema's "all forms are
      rejected" wording is being corrected in ssl-style-guide#49. The
      check now flags only string-literal targets without a qualifier
      dot (the provable class-local/base form); non-literal targets are
      left alone as unprovable. Message reworded accordingly.
issues: ["ssl-style-guide#49"]
---

## Behavior

Fires an error on a `DoProc` identifier whose next significant token is
`(` when that call sits inside a class method — a `:PROCEDURE` that starts
after the file's `:CLASS` statement — and the call's first argument is a
string literal containing no `.` qualifier. An unqualified target names a
procedure of the current class or its base, which the compiler rejects;
the suggested fix is `Me:MethodName()` / `Base:MethodName()`, or a fully
qualified `"Category.Script.Procedure"` reference for a deployed external
procedure.

It must NOT flag:

- `DoProc(...)` in script files (no `:CLASS` in the file) — there it is
  the sanctioned way to call same-file procedures;
- a qualified string-literal target
  (`DoProc("Category.Script.Procedure", {...})`) — a deployed external
  script procedure reference, valid inside class methods (issue #151);
- a non-literal first argument (`DoProc(sTarget, {...})`) — the target is
  not provable from the call site, and everything short of provable is
  left alone;
- `ExecFunction(...)` in any form — no class-method restriction applies
  (its own cross-file rule is `execfunction_class_target`);
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

### Does not flag

```ssl
:CLASS ValidationClient;
:PROCEDURE CheckInput;
	:PARAMETERS oInput;
	:DECLARE bResult;
	bResult := DoProc("API_Helper.ValidationHelper.ValidateProperties", {oInput});
	:RETURN bResult;
:ENDPROC;
```

### Does not flag

```ssl
:CLASS ValidationClient;
:PROCEDURE CheckInput;
	:DECLARE sTarget, bResult;
	sTarget := BuildTargetName();
	bResult := DoProc(sTarget, {});
	:RETURN bResult;
:ENDPROC;
```

## Rationale

The schema rule (`lints.compile_errors.doproc_in_class`, section level
`authoritative`, severity `error`) mirrors the STARLIMS compiler's
rejection of class-local DoProc calls; its original "all forms are
rejected" wording proved too broad — qualified deployed-procedure
references are valid inside class methods (issue #151, runtime-confirmed;
schema correction tracked in ssl-style-guide#49). The check keeps error
severity for the provable unqualified-literal form and follows the repo's
provability precedent (zero_based_array_index, execfunction_class_target)
in leaving non-literal targets alone. The be7a174 rewording (history)
replaced a softer style suggestion with the compile-error statement so
users do not assume the call would merely be unidiomatic.
