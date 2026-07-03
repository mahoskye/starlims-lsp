---
id: diag.base_requires_inherit
title: Base member access in a class without :INHERIT
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

Flags a well-formed `Base:MemberName` reference inside a `:CLASS`
definition when the class declares no `:INHERIT`. Without a parent class,
`Base` has nothing to resolve against. The range covers the `Base` token,
and every such reference in the class flags individually.

The `:INHERIT` is recognized anywhere after the `:CLASS` keyword in token
order; the rule does not require it to precede the `Base` reference or to
be the first class member (member ordering is a separate rule).

It must NOT flag:

- `Base:Member` references in a class that has an `:INHERIT` statement;
- `Base` references outside a class or standalone `Base` — those forms are
  owned by `diag.base_outside_class` and `diag.base_standalone`
  respectively, and exactly one code fires per bad reference.

## Examples

### Flags

```ssl
:CLASS Widget;
:PROCEDURE Init;
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

`Base` in a parentless class is guaranteed-broken code — the runtime has
no parent member table to search — so this is an error. Keeping it separate
from `diag.base_outside_class` means the message can state the precise fix
(add `:INHERIT`) rather than a generic complaint. Introduced in the
style-guide alignment pass (commit cdbfee6); the code slug was stabilized
in PR #3 (v0.4.0).
