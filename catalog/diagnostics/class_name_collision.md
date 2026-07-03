---
id: diag.class_name_collision
title: User-defined :CLASS shadows a built-in SSL class
kind: diagnostic
status: active
authority: tool
schema_ref: null
default_severity: warning
severity_overridable: true
suppressible: true
tests:
  - internal/providers/element_reference_test.go
history:
  - date: 2026-04-30
    ref: "PR #1 (commit e628475)"
    note: >-
      Introduced with the ssl-element-reference.json integration, which gave
      the tool the published built-in class inventory to collide against.
  - date: 2026-04-30
    ref: "PR #3 (v0.4.0, commit d744511)"
    note: Stable diagnostic code assigned; behavior unchanged.
issues: []
---

## Behavior

Flags the class name in a `:CLASS Name;` declaration when the name matches
(case-insensitively) one of the published built-in SSL classes — the
generated inventory in `internal/constants/generated_classes.go` (e.g.
`Email`, `SSLRegex`, `SSLDataset`). The range covers the name identifier,
not the `:CLASS` keyword.

It must NOT flag:

- `:CLASS` declarations whose name is not a built-in class name;
- uses of built-in class names anywhere other than directly after `:CLASS`
  (instantiation `Email{}`, string arguments, comments);
- a `:CLASS` keyword followed by something other than an identifier — there
  is no name to check.

## Examples

### Flags

```ssl
:CLASS Email;
:PROCEDURE Send;
:ENDPROC;
```

### Does not flag

```ssl
:CLASS OrderMailer;
:PROCEDURE Send;
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

A user-defined class that reuses a built-in name shadows the built-in only in
the file's local scope, so `Name{}` instantiation may still resolve to the
built-in elsewhere — a confusing trap for readers reaching for the built-in.
The style guide is silent (`authority: tool`); warning rather than error
because the declaration is legal SSL, but new code should pick another name.
