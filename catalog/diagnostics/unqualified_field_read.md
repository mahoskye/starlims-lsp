---
id: diag.unqualified_field_read
title: Bare read of a class field inside a method
kind: diagnostic
status: active
authority: authoritative
schema_ref: null
default_severity: warning
severity_overridable: true
suppressible: true
tests:
  - internal/providers/audit255_test.go
history:
  - date: 2026-10-13
    ref: "issue #260 (catalog audit #255)"
    note: >-
      Added beside unqualified_field_assignment. The reference's
      variable-scope guide states that inside a method a bare identifier
      never refers to a class field: a bare read with nothing else of that
      name in reach raises `Variable [name] is undefined!`, and otherwise
      silently reads a caller's or a public variable. The assignment
      rule's rationale had claimed the opposite for reads. Measured over
      6,228 production files: 26 findings in 5 files, each a bare read of
      a :DECLAREd field with no local of that name in the method.
issues: ["#260"]
---

## Behavior

In a `:CLASS` file only: flags a bare identifier *read* inside a class
method when the name matches a class field. Fields are the identifiers on
`:DECLARE` lines between the `:CLASS` statement and the first method —
the same field set `unqualified_field_assignment` uses. A bare name inside
a method follows ordinary variable lookup (local, then the callers'
scopes, then publics) and never reaches the field, so the read either
raises `Variable [name] is undefined!` or reads somebody else's variable.
The message prescribes `Me:fieldName`. The range covers the identifier.

Reads are every bare identifier the expression tree places in value
position: right-hand sides, conditions (`:IF bReady;`), `:RETURN` values,
call arguments, subscripts, receivers of `:` member access
(`oOwner:GetName()` with `oOwner` a field).

It must NOT flag:

- qualified access — `Me:nTotal`, `Base:nTotal`, `oOther:nTotal` are member
  access, not a bare lookup;
- a name the method declares itself — a method-local `:DECLARE`,
  `:PARAMETERS`, or `:PUBLIC` entry shadows the field, and the bare name is
  then evidently the author's own variable;
- a name followed by `(` — a call's name is a function, not a variable;
- the declaration lines themselves, or names matching SSL built-ins;
- a read in a statement that also bare-writes the same name
  (`nTotal := nTotal + 1`) — that statement is reported once, by
  `unqualified_field_assignment`;
- a read after an earlier bare write of the same name in the same method —
  the write is the reported mistake, and after a plain `:=` the name is a
  method-local anyway;
- anything in a non-class file, or inside a code-block literal (opaque to
  the expression tree).

## Examples

### Flags

```ssl
:CLASS Totaler;
:DECLARE nTotal;
:PROCEDURE Peek;
:RETURN nTotal;
:ENDPROC;
```

### Flags

```ssl
:CLASS Gate;
:DECLARE bReady;
:PROCEDURE Check;
	:IF bReady;
		UsrMes("ready");
	:ENDIF;
:ENDPROC;
```

### Does not flag

```ssl
:CLASS Totaler;
:DECLARE nTotal;
:PROCEDURE Peek;
:RETURN Me:nTotal;
:ENDPROC;
```

### Does not flag

```ssl
:CLASS Totaler;
:DECLARE nTotal;
:PROCEDURE Peek;
	:DECLARE nTotal;
	nTotal := 5;
	:RETURN nTotal;
:ENDPROC;
```

### Does not flag

```ssl
:CLASS Counter;
:DECLARE nTotal;
:PROCEDURE Increment;
	nTotal := nTotal + 1;
	:RETURN nTotal;
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE Peek;
	:DECLARE nTotal;
	nTotal := 1;
	:RETURN nTotal;
:ENDPROC;
```

## Rationale

The style-guide rule `field_access_via_me` (authoritative, warning) says to
access fields through `Me:` or `Base:` inside methods, and the reference's
variable-scope guide gives the runtime reason: a bare name never reaches
the field. A bare read is the louder half of the mistake when nothing else
of that name is in reach (`Variable [name] is undefined!`) and the quieter,
worse half when something is: the method reads a caller's local or a
public of the same name and carries on. Warning rather than error because
the code compiles, and a caller-scope variable of the same name is legal
SSL — the rule reports the likely bug, not a certain failure.

The two skip rules keep one mistake to one finding: a statement that
writes the name is the assignment rule's (the fifth fence), and once a
method has bare-written a name the reads that follow are the same mistake
(or, after `:=`, a genuine local). Measured over 6,228 production files
before shipping: 26 findings in 5 files, every one a field read with no
local of that name in its method.
