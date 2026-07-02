---
id: diag.base_outside_class
title: Base member access outside a class definition
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
      Introduced in the full alignment pass with ssl-style-guide: Me/Base
      reference-form rules (checkClassReferenceForms).
  - date: 2026-04-30
    ref: "PR #3 (v0.4.0)"
    note: Stable diagnostic code assigned; rule behavior unchanged.
issues: []
---

## Behavior

Flags a well-formed `Base:MemberName` reference that appears outside a
`:CLASS` definition — either the file contains no `:CLASS` statement at
all, or the reference sits on a line before the first `:CLASS` keyword.
`Base` refers to the parent of the current class, so it is meaningless in
plain scripts and in file-header code. The range covers the `Base` token.

Class membership is tracked at line granularity against the file's first
`:CLASS` keyword: every token on or after that line counts as inside the
class. SSL files define at most one class, so no end-of-class boundary is
tracked.

It must NOT flag:

- `Base:Member` references on or after the `:CLASS` line (a missing
  `:INHERIT` is reported by `diag.base_requires_inherit`, not this rule);
- standalone `Base` without `:` — that form is owned by
  `diag.base_standalone` and must produce exactly one of the two codes.

## Examples

### Flags

```ssl
:PROCEDURE Demo;
Base:Init();
:ENDPROC;
```

### Does not flag

```ssl
:CLASS Widget;
:INHERIT Control;
:PROCEDURE Init;
Base:Init();
:ENDPROC;
```

## Rationale

Outside a class there is no parent to delegate to, so `Base:Member` is
guaranteed-broken code and merits an error. Splitting the Base rules into
three codes (standalone form, outside-class placement, missing `:INHERIT`)
lets each message state the one fix the author needs instead of a generic
"invalid Base usage". Introduced in the style-guide alignment pass (commit
cdbfee6); the code slug was stabilized in PR #3 (v0.4.0).
