---
id: diag.base_standalone
title: Base used without a member access
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

Flags the identifier `Base` (case-insensitive) whenever the next
significant token is not the `:` member-access punctuation. In SSL, `Base`
exists only as the head of a parent-member reference — `Base:MemberName` —
and cannot be read, assigned, passed, or otherwise stand alone. The range
covers the `Base` token.

This form check fires regardless of class context: a standalone `Base`
inside a `:CLASS` is just as invalid as one outside. (Whether a well-formed
`Base:Member` is allowed where it appears is owned by the companion rules
`diag.base_outside_class` and `diag.base_requires_inherit`.)

It must NOT flag:

- well-formed `Base:MemberName` references;
- `Base` in declaration positions — an identifier immediately following
  `:DECLARE`, `:PARAMETERS`, `:DEFAULT`, `:PUBLIC`, `:PROCEDURE`, `:CLASS`,
  or `:INHERIT` is a declared name, not a reference.

## Examples

### Flags

```ssl
:CLASS Widget;
:INHERIT Control;
:PROCEDURE Init;
:DECLARE oParent;
oParent := Base;
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

`Base` standing alone has no value in SSL — the runtime resolves it only
as a member-access qualifier, so any other use is guaranteed-broken code
and merits an error. The check is purely token-shape based (`Base` not
followed by `:`), which keeps it precise: the only exempted positions are
declarations, where the word is a name rather than a reference. Introduced
in the style-guide alignment pass (commit cdbfee6); the code slug was
stabilized in PR #3 (v0.4.0).
