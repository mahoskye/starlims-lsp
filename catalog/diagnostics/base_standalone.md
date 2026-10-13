---
id: diag.base_standalone
title: Base used without a member access
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
  - date: 2026-03-21
    ref: "commit cdbfee6"
    note: >-
      Introduced in the full alignment pass with ssl-style-guide: Me/Base
      reference-form rules (checkClassReferenceForms).
  - date: 2026-04-30
    ref: "PR #3 (v0.4.0)"
    note: Stable diagnostic code assigned; rule behavior unchanged.
  - date: 2026-10-13
    ref: "issue #255"
    note: >-
      authority corrected to authoritative: the style-guide record
      base_member_access (level authoritative) names this rule in
      enforced_by, so the guide is not silent. A member named Base on
      some receiver (`oUri:Base`) no longer flags (D10, issue #258), and
      every name in a :DECLARE / :PARAMETERS / :PUBLIC list is exempt,
      not only the first.
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
  or `:INHERIT`, or any later name in a `:DECLARE` / `:PARAMETERS` /
  `:PUBLIC` list, is a declared name, not a reference;
- a member named `Base` on some receiver (`oUri:Base`) — the preceding
  `:` makes it member access, which never warns (DECISIONS.md D10,
  issue #258).

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

### Does not flag

```ssl
:PROCEDURE Demo;
:PARAMETERS oUri;
:DECLARE sHost, sRoot;
sRoot := oUri:Base;
sHost := oUri:Base:Host;
:ENDPROC;
```

## Rationale

`Base` standing alone has no value in SSL — the runtime resolves it only
as a member-access qualifier, so any other use is guaranteed-broken code
and merits an error. The check is purely token-shape based (`Base` not
followed by `:`), which keeps it precise: the exempted positions are declarations, where
the word is a name rather than a reference, and member access, where it
names a member of something else (D10). Introduced
in the style-guide alignment pass (commit cdbfee6); the code slug was
stabilized in PR #3 (v0.4.0).
