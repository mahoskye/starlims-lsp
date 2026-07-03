---
id: diag.identifier_too_short
title: Identifier shorter than a minimum length (never implemented)
kind: diagnostic
status: removed
authority: tool
schema_ref: null
default_severity: info
severity_overridable: true
suppressible: true
tests: []
history:
  - date: 2026-04-30
    ref: "PR #3 (v0.4.0, commit d744511)"
    note: >-
      Constant CodeIdentifierTooShort reserved alongside the stable code
      batch, but no check was ever implemented — checkNameLengths only
      enforces maxima.
  - date: 2026-07-02
    ref: "issue #31"
    note: >-
      Constant deleted. A minimum-length rule would contradict the style
      guide, which blesses one-character loop counters (i/j/k); no flag
      condition exists that would not be noise.
issues: ["#31"]
---

## Behavior

Removed without ever being implemented. The constant was reserved in PR #3
but no emit site was written, and none should be: the style guide
explicitly allows one-character loop counters, so any minimum-length rule
would flag idiomatic code. `identifier_too_long` (the maxima) remains
active and is the only name-length rule.
