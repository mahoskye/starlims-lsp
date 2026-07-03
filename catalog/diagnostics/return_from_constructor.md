---
id: diag.return_from_constructor
title: Return from a constructor (never implemented)
kind: diagnostic
status: removed
authority: tool
schema_ref: null
default_severity: error
severity_overridable: true
suppressible: true
tests: []
history:
  - date: 2026-04-30
    ref: "PR #3 (v0.4.0, commit d744511)"
    note: >-
      Constant CodeReturnFromConstructor reserved alongside the stable code
      batch, but no check was ever implemented.
  - date: 2026-07-02
    ref: "issue #31"
    note: >-
      Constant deleted. Constructor return-value misuse is already covered
      by the schema-backed constructor_return_value, and a bare ':RETURN;'
      in a constructor is legal SSL — a rule flagging any return would be
      wrong, and no distinct behavior remains for this code to express.
issues: ["#31"]
---

## Behavior

Removed without ever being implemented. The semantics this code's name
suggests are either wrong (a bare `:RETURN;` in a constructor is legal) or
already owned by `constructor_return_value`
(lints.compile_errors, active). No emit site was ever written.
