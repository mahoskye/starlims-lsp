---
id: diag.inherit_qualified_name
title: INHERIT base-name form advice
kind: diagnostic
status: draft
authority: style_only
schema_ref: lints.class_rules.inherit_qualified_name
default_severity: warning
config:
  - ssl.diagnostics.rules
severity_overridable: true
suppressible: true
tests: []
history:
  - date: 2026-04-30
    ref: "PR #3 (v0.4.0)"
    note: >-
      Code constant CodeInheritQualifiedName was added with the stable
      diagnostic codes, but no check has ever emitted it — the rule is
      unimplemented.
issues: []
---

## Behavior

Unimplemented. The constant `CodeInheritQualifiedName` exists in
`internal/providers/diagnostic_codes.go`, but nothing in
`internal/providers/` ever emits it: no `:INHERIT` name-form check runs,
so this diagnostic can never appear. The entry stays `draft` (not
`planned`) because the slug-bijection test requires `planned` entries to
have no code constant, and the constant is already defined.

The schema rule it reserves a slot for is advice about `:INHERIT` name
forms: qualified names (`:INHERIT Category.ScriptName;`) are common, but
bare base names (`:INHERIT BaseName;`) are equally accepted — so the
eventual check would be informational about the name form, not a
correctness rule. What exactly it should flag (bare names? unresolvable
qualified names?) has not been decided.

## Examples

### Does not flag

```ssl
:CLASS Derived;
:INHERIT Framework.BaseClass;
:PROCEDURE DoWork;
:ENDPROC;
```

### Does not flag

```ssl
:CLASS Derived;
:INHERIT BaseClass;
:PROCEDURE DoWork;
:ENDPROC;
```

## Rationale

The schema lists `inherit_qualified_name` under `lints.class_rules` with
`severity: warning` (style_only): "Qualified :INHERIT names are common,
but bare or qualified base names are accepted." The schema message
states that both forms are valid, which leaves no obvious condition to
flag; the code constant was reserved in PR #3 (history) but no check was
written. Promotion requires either implementing a check (and adding a
Flags fence) or removing the constant and marking this entry `removed`.

## Known gaps

- The rule is entirely unimplemented: `CodeInheritQualifiedName` is dead
  code with no emit site. Because the conformance lints require an
  active diagnostic to have at least one firing Flags fence, this entry
  cannot be promoted until a check lands (or the constant is dropped and
  the entry marked `removed`).
