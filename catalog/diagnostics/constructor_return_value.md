---
id: diag.constructor_return_value
title: ":RETURN with a value inside a Constructor"
kind: diagnostic
status: active
authority: authoritative
schema_ref: lints.compile_errors.constructor_return_value
default_severity: error
severity_overridable: true
suppressible: true
tests:
  - internal/providers/providers_test.go
history:
  - date: 2026-03-21
    ref: "commit cdbfee6"
    note: Introduced during the full style-guide alignment pass.
  - date: 2026-04-30
    ref: "PR #3 (v0.4.0)"
    note: Stable diagnostic code assigned when every diagnostic gained a Code.
issues: []
---

## Behavior

Inside a class Constructor — a `:PROCEDURE` named `Constructor`
(case-insensitive) that starts after the file's `:CLASS` statement — fires
an error on each `:RETURN` that is followed by any significant token other
than `;` before the statement ends. A Constructor may exit early, but it
cannot return a value.

It must NOT flag:

- a bare `:RETURN;` inside a Constructor;
- `:RETURN <value>;` inside any other method or script procedure;
- a procedure named `Constructor` outside a `:CLASS` definition — that is
  `constructor_outside_class`'s territory, and this rule only inspects
  class methods.

## Examples

### Flags

```ssl
:CLASS MyClass;
:PROCEDURE Constructor;
	:RETURN 1;
:ENDPROC;
```

### Does not flag

```ssl
:CLASS MyClass;
:PROCEDURE Constructor;
	:RETURN;
:ENDPROC;
```

### Does not flag

```ssl
:CLASS MyClass;
:PROCEDURE GetCount;
	:RETURN 1;
:ENDPROC;
```

## Rationale

The schema rule (`lints.compile_errors.constructor_return_value`, section
level `authoritative`) mirrors the STARLIMS compiler: `:RETURN` inside a
Constructor cannot return a value. Error severity and authoritative
authority match the schema. The bare-`:RETURN;` Does-not-flag fence pins
the boundary — early exit is legal, only the returned value is not.
