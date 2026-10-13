---
id: diag.inherit_position
title: ":INHERIT not directly after :CLASS"
kind: diagnostic
status: active
authority: authoritative
schema_ref: null
default_severity: error
severity_overridable: true
suppressible: true
tests:
  - internal/providers/providers_test.go
history:
  - date: 2026-10-13
    ref: "issue #255"
    note: >-
      Split from class_member_order, which reported a misplaced :INHERIT
      as an info-tier ordering note. The reference (keywords/INHERIT)
      makes it a compile error. Zero findings on the private corpus.
issues: []
---

## Behavior

In a `:CLASS` file, fires an error on an `:INHERIT` statement that is not
the first statement after the `:CLASS` statement. Comments between the
two are transparent. Anything else in front of it — a `:DECLARE`, a
method, an executable statement, or an earlier `:INHERIT` — flags. The
range covers the `:INHERIT` keyword.

It must NOT flag:

- `:INHERIT` directly after `:CLASS Name;`, with or without comments
  between them;
- a class without `:INHERIT`;
- files without a `:CLASS` statement (`:INHERIT` outside a class is a
  different defect).

## Examples

### Flags

```ssl
:CLASS Widget;
:DECLARE nCount;
:INHERIT Lab.Control;
```

### Flags

```ssl
:CLASS Widget;
:PROCEDURE Init;
:ENDPROC;
:INHERIT Lab.Control;
```

### Does not flag

```ssl
/* Widget base wiring;
:CLASS Widget;
/* parent supplies Init;
:INHERIT Lab.Control;
:DECLARE nCount;
:PROCEDURE Init;
:ENDPROC;
```

### Does not flag

```ssl
:CLASS Widget;
:DECLARE nCount;
```

## Rationale

The reference (keywords/INHERIT) says SSL accepts `:INHERIT` only
directly after the class declaration, so any other position is a compile
error, hence error severity and authoritative authority. It used to be
one branch of `diag.class_member_order`, an info-tier convention rule
that is hidden by default; a compile error belongs on the default
surface. `diag.inherit_qualified_name` separately checks the base name's
form.
