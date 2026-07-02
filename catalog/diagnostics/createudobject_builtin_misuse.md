---
id: diag.createudobject_builtin_misuse
title: CreateUdObject used to construct a built-in SSL class
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
  - date: 2026-03-21
    ref: "commit cdbfee6"
    note: >-
      Introduced during full alignment with ssl-style-guide: CreateUdObject
      string dispatch is reserved for user-defined :CLASS objects; built-in
      classes must use curly-brace construction.
  - date: 2026-04-30
    ref: "PR #3 (v0.4.0, commit d744511)"
    note: Stable diagnostic code assigned; behavior unchanged.
issues: []
---

## Behavior

Flags `CreateUdObject("Name", ...)` (case-insensitive function match) when
the first argument is a single string literal whose trimmed content matches
(case-insensitively) a built-in SSL class name (e.g. `Email`, `SSLDataset`).
Built-in classes must be constructed with curly braces (`Email{}`). The
range covers the string argument.

It must NOT flag:

- `CreateUdObject` with a string naming a user-defined class or a
  script-path target (`"MyLib.MyClass"` is not a built-in name);
- a non-literal first argument (variable, concatenation, expression) — only
  a single string-literal token is inspected;
- anonymous UDObject construction (`CreateUdObject()` or
  `CreateUdObject({...})`);
- curly-brace construction of built-in classes.

## Examples

### Flags

```ssl
:PROCEDURE Demo;
	:DECLARE oMail;
	oMail := CreateUdObject("Email");
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE Demo;
	:DECLARE oHelper;
	oHelper := CreateUdObject("MyLib.OrderHelper");
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE Demo;
	:DECLARE oMail;
	oMail := Email{};
:ENDPROC;
```

## Rationale

The source guide reserves CreateUdObject string dispatch for user-defined
`:CLASS` objects; passing a built-in class name does not construct the
built-in and fails or misbehaves at runtime, which justifies error severity
(cdbfee6). Precision is kept high by only matching a lone string literal
against the published class inventory — dynamic names are never guessed at.
