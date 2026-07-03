---
id: diag.class_or_script
title: ":CLASS must be the file's first significant statement"
kind: diagnostic
status: active
authority: authoritative
schema_ref: lints.compile_errors.class_or_script
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

When a file contains a `:CLASS` statement, fires an error on the first
`:CLASS` token unless `:CLASS` is the first significant statement in the
file — a file is either a class definition or a script, never both.
Comments before `:CLASS` are ignored; any code statement before it (a
declaration, an assignment, a procedure) triggers the error.

It must NOT flag:

- files with no `:CLASS` statement (plain scripts);
- `:CLASS` preceded only by comments (file headers are fine).

Additional `:CLASS` statements beyond the first are the province of
`one_class_per_file`, not this rule; this rule reports at most once, on
the first `:CLASS` token.

## Examples

### Flags

```ssl
:DECLARE nCount;
:CLASS MyClass;
```

### Does not flag

```ssl
/* Utility class for report handling;
:CLASS MyClass;
:DECLARE nCount;
```

### Does not flag

```ssl
:PROCEDURE Demo;
	:DECLARE nCount;
	nCount := 1;
:ENDPROC;
```

## Rationale

The schema rule (`lints.compile_errors.class_or_script`, section level
`authoritative`) mirrors the STARLIMS compiler: a file is one of class or
script, never a mix, so `:CLASS` cannot follow script code. Error severity
and authoritative authority match the schema. The comment-header
Does-not-flag fence pins that documentation before `:CLASS` is not
"script code".
