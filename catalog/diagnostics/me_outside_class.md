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
issues: []
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

It must NOT flag:

- `Me` anywhere on or after the first `:CLASS` line, whether standalone
  (`Me`), field access (`Me:sName`), or method call (`Me:Load()`);
- `Me` appearing as a declaration name (immediately preceded by a
  declaration keyword such as `:DECLARE`, `:PARAMETERS`, `:PROCEDURE`,
  `:DEFAULT`, `:PUBLIC`, `:CLASS`, `:INHERIT`);
- the word "me" inside strings or comments, or as part of a longer
  identifier (`sMessage`).

## Examples

### Flags

```ssl
vResult := Me;
```

### Flags

```ssl
:PROCEDURE GetName;
	:RETURN Me:sName;
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

## Rationale

The style guide lists `Me` among the class-context special forms
(special_literals.class_context_forms): it is only meaningful inside a
`:CLASS` definition. Because a wrong `Me` is a guaranteed runtime failure,
this is an error, matching the sibling Base rules introduced in the same
commit (cdbfee6). The line-based class-range guard trades precision for
safety: it can never false-positive inside a real class file, at the cost
of accepting a stray top-level `Me` below the `:CLASS` line.

## Known gaps

- A member access whose member happens to be named `Me` (`oObj:Me`) is
  flagged in non-class files, even though that `Me` is a member name, not
  the self-reference. The sibling unqualified_field_assignment check skips
  identifiers preceded by `:`; this check should apply the same guard.
  Covered by the expect=fail fence below; fix in a follow-up PR citing this
  entry.

### Does not flag

```ssl expect=fail
vValue := oConfig:Me;
```
