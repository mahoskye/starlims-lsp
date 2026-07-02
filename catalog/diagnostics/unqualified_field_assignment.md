---
id: diag.unqualified_field_assignment
title: Bare assignment to a class field inside a method
kind: diagnostic
status: active
authority: tool
schema_ref: null
default_severity: warning
severity_overridable: true
suppressible: true
tests:
  - internal/providers/providers_test.go
history:
  - date: 2026-05-13
    ref: "starlims-ssl-reference #4/#5 -> PR #6; ssl-style-guide 386d57e"
    note: >-
      Runtime correction upstream: inside a class method a bare identifier
      resolves to a local, not the :DECLAREd field — unqualified assignment
      silently creates a local and leaves the field untouched.
  - date: 2026-05-14
    ref: "PR #23 (v0.7.7, commit 54a32c5)"
    note: >-
      Rule added: flag bare assignment to a declared class field in a
      method; suppressed by a local shadow or Me:/Base: qualification.
issues: []
---

## Behavior

In a `:CLASS` file only: flags a bare identifier on the left of an
assignment operator (`:=`, `+=`, `-=`, `*=`, `/=`, `^=`, `%=`) inside a
class method when the name matches a class field. Fields are the
identifiers on `:DECLARE` lines between the `:CLASS` statement and the
first method. At runtime the bare assignment creates a method-local and
leaves the field unchanged (ssl-style-guide `classes.fields.access_rule`),
so the message prescribes `Me:fieldName`. The range covers the identifier.

It must NOT flag:

- qualified assignment — any `:`-qualified form (`Me:nTotal`,
  `Base:nTotal`, `oOther:nTotal`) is object member access, not a bare
  local;
- a field name shadowed by a method-local `:DECLARE` or `:PARAMETERS`
  entry — the local is then the author's evident intent;
- the declaration lines themselves, or names matching SSL built-ins;
- reads of the field name — only assignment targets are checked;
- anything in a non-class file: without `:CLASS` there are no fields and
  the rule is inert.

## Examples

### Flags

```ssl
:CLASS Counter;
:DECLARE nTotal;

:PROCEDURE Increment;
	nTotal := nTotal + 1;
:ENDPROC;
```

### Flags

```ssl
:CLASS Counter;
:DECLARE nTotal;

:PROCEDURE Add;
	:PARAMETERS nDelta;
	nTotal += nDelta;
:ENDPROC;
```

### Does not flag

```ssl
:CLASS Counter;
:DECLARE nTotal;

:PROCEDURE Increment;
	Me:nTotal := Me:nTotal + 1;
:ENDPROC;
```

### Does not flag

```ssl
:CLASS Counter;
:DECLARE nTotal;

:PROCEDURE Compute;
	:DECLARE nTotal;
	nTotal := 5;
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE Run;
:DECLARE sName;
	sName := "ok";
:ENDPROC;
```

## Rationale

This is the silent-footgun class of bug: the code runs, no error is
raised, and the field simply never changes. The rule exists because the
upstream reference corrected its own documentation on this exact point
(starlims-ssl-reference #4/#5), so the check encodes verified runtime
resolution order, not style. Warning rather than error because a
deliberate method-local of the same name is legal — which is also why an
explicit local shadow suppresses the finding (fourth fence). Note the
severity of the trap: only assignments are flagged; bare *reads* do
resolve to the field when no local exists, so flagging reads would be
noise.
