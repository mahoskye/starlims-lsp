---
id: diag.inherit_qualified_name
title: Inherit base-name form (never implemented)
kind: diagnostic
status: removed
authority: style_only
schema_ref: lints.class_rules.inherit_qualified_name
default_severity: warning
severity_overridable: true
suppressible: true
tests: []
history:
  - date: 2026-04-30
    ref: "PR #3 (v0.4.0, commit d744511)"
    note: >-
      Constant CodeInheritQualifiedName reserved alongside the stable code
      batch, but no check was ever implemented.
  - date: 2026-07-02
    ref: "issue #31"
    note: >-
      Constant deleted. The schema's own message says both bare and
      qualified base names are accepted, which leaves no flag condition —
      there is nothing for a lint to reject. If the style guide later
      settles on one form, spec a new planned entry first.
issues: ["#31"]
---

## Behavior

Removed without ever being implemented. The schema lint this slug points
at documents that both `:INHERIT Base;` and `:INHERIT Category.Base;` are
accepted forms — an acceptance note, not a rule with a violation. No emit
site was ever written and no flag condition exists.
