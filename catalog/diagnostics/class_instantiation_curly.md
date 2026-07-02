---
id: diag.class_instantiation_curly
title: Built-in class instantiated with parentheses
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
    note: Introduced with the gotcha-detection batch (gotcha #15 in gotchas.md).
  - date: 2026-04-30
    ref: "PR #3 (v0.4.0)"
    note: Stable diagnostic code assigned; rule behavior unchanged.
  - date: 2026-07-02
    ref: "issue #32"
    note: >-
      False positive fixed: identifiers preceded by the ':' member-access
      punctuation (oSvc:Email(...)) are exempt — that is a member call, not
      an instantiation.
issues: ["#32"]
---

## Behavior

Flags an identifier that names a built-in SSL class (the generated
`constants.SSLClassNames` list, matched case-insensitively — `Email`,
`SSLRegex`, `AzureStorage`, ...) when its next significant token is `(`.
Built-in classes are instantiated with curly braces — `Email{}` — and the
call form `Email()` fails at runtime. The message shows the curly-brace
form; the range covers the class-name identifier.

It must NOT flag:

- curly-brace instantiation `Email{}` / `Email{args}`;
- identifiers that are not built-in class names, including user procedures
  and built-in functions followed by `(`;
- `CreateUdObject("Email")` — string dispatch of a built-in class is the
  separate rule `diag.createudobject_builtin_misuse`;
- a colon-qualified member call whose method shares a built-in class name
  (`oSvc:Email("to")`) — that is a member call on a user object, not an
  instantiation (issue #32);
- the class name in non-call positions (bare mention, string content,
  comments).

## Examples

### Flags

```ssl
:PROCEDURE Demo;
:DECLARE oMail;
oMail := Email();
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE Demo;
:DECLARE oMail;
oMail := Email{};
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE Demo;
:DECLARE oSvc, xResult;
xResult := oSvc:Email("to");
:ENDPROC;
```

## Rationale

`ClassName()` is what every other mainstream language writes, so it is a
high-frequency porting mistake, and it is guaranteed-broken SSL — hence an
error with a message that shows the exact replacement. Matching is a
simple identifier-then-`(` scan with one look-left exemption: an
identifier preceded by the `:` member-access punctuation is a member call,
not an instantiation (issue #32) — the last Does-not-flag fence pins that
permanently. Introduced with the gotcha batch (commit 7261172); the code
slug was stabilized in PR #3 (v0.4.0).
