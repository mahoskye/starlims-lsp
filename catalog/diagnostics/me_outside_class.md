---
id: diag.me_outside_class
title: Me used outside a class definition
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
      Introduced in checkClassReferenceForms alongside the Base rules
      (base_standalone / base_outside_class / base_requires_inherit),
      aligning with the style guide's class-context special forms.
  - date: 2026-04-30
    ref: "PR #3 (v0.4.0, commit d744511)"
    note: Stable diagnostic code assigned; behavior unchanged.
  - date: 2026-07-02
    ref: "issue #32"
    note: >-
      False positive fixed: identifiers preceded by the ':' member-access
      punctuation (oObj:Me) are exempt — that Me is a member name, not the
      self-reference.
  - date: 2026-08-12
    ref: "issue #171"
    note: >-
      Include-library downgrade added (option c from the issue): a file
      consisting solely of :PROCEDURE blocks with no top-level statements
      may be an :INCLUDE target of a class file, where its Me references
      compile inside the :CLASS and are valid — single-file analysis
      cannot tell, so such files warn instead of error. A workspace
      include-graph reverse lookup remains a possible future upgrade.
issues: ["#32"]
---

## Behavior

Flags the identifier `Me` (any casing) when the file contains no `:CLASS`
statement, or when the `Me` appears on a line before the first `:CLASS`
statement. `Me` is the class self-reference; outside a class definition it
has no meaning and fails at runtime, hence error severity. The diagnostic
ranges over the `Me` token.

The class-context guard is deliberately coarse to avoid false positives:
any `Me` on or after the line of the file's first `:CLASS` statement is
accepted, without checking that it sits inside a method body (classes
extend to end of file — there is no `:ENDCLASS`).

Severity is contextual (issue #171): in a classless file consisting solely
of `:PROCEDURE` blocks — comments and paste-time `:INCLUDE` directives
allowed, no top-level statements — the diagnostic is a warning, because
that is the shape of an include library whose procedures compile inside a
`:CLASS` via `:INCLUDE` (stock ENTERPRISE_DB_OBJECTS pattern:
`:CLASS TablesComparer; ... :INCLUDE Enterprise_DB_Objects.DTORelationsComparer;`).
Any top-level statement (a `:DECLARE`, an assignment) makes the file a
script and restores the error. The diagnostic code is the same in both
tiers.

It must NOT flag:

- `Me` anywhere on or after the first `:CLASS` line, whether standalone
  (`Me`), field access (`Me:sName`), or method call (`Me:Load()`);
- `Me` appearing as a declaration name (immediately preceded by a
  declaration keyword such as `:DECLARE`, `:PARAMETERS`, `:PROCEDURE`,
  `:DEFAULT`, `:PUBLIC`, `:CLASS`, `:INHERIT`);
- a member access whose member is named `Me` (`oObj:Me`) — that is a
  member name, not the self-reference (issue #32);
- the word "me" inside strings or comments, or as part of a longer
  identifier (`sMessage`).

## Examples

### Flags

```ssl
vResult := Me;
```

### Flags

(warning tier — the file is an include-library shape, issue #171)

```ssl
:PROCEDURE GetName;
	:RETURN Me:sName;
:ENDPROC;
```

### Flags

(warning tier — include-library shape with parameters, issue #171)

```ssl
:PROCEDURE Compare;
:PARAMETERS oOther;
:RETURN Me:Name == oOther:Name;
:ENDPROC;
```

### Does not flag

```ssl
:CLASS UserRecord;
:DECLARE sName;

:PROCEDURE GetName;
	:RETURN Me:sName;
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE Notify;
	:DECLARE sMessage;
	sMessage := "call me back";
:ENDPROC;
```

### Does not flag

```ssl
vValue := oConfig:Me;
```

## Rationale

The style guide lists `Me` among the class-context special forms
(special_literals.class_context_forms): it is only meaningful inside a
`:CLASS` definition. Because a wrong `Me` is a guaranteed runtime failure,
this is an error, matching the sibling Base rules introduced in the same
commit (cdbfee6). The line-based class-range guard trades precision for
safety: it can never false-positive inside a real class file, at the cost
of accepting a stray top-level `Me` below the `:CLASS` line. Identifiers
preceded by the `:` member-access punctuation are exempt (issue #32), the
same guard checkUnqualifiedFieldAssignment uses — the last Does-not-flag
fence pins it.
