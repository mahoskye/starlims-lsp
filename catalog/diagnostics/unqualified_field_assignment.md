---
id: diag.unqualified_field_assignment
title: Bare assignment to a class field inside a method
kind: diagnostic
status: active
authority: authoritative
schema_ref: null
default_severity: warning
severity_overridable: true
suppressible: true
tests:
  - internal/providers/providers_test.go
  - internal/providers/audit255_test.go
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
  - date: 2026-10-13
    ref: "issues #255, #260"
    note: >-
      Catalog audit. Targets now come from the expression tree instead of
      a token scan, which missed every write that is not `name :=`:
      `nTotal++` / `++nTotal`, element writes (`aItems[1] := x`), and
      chained assignment inner targets; a method-local `:PUBLIC` now
      shadows the field like `:DECLARE`. The rationale's claim that a bare
      read reaches the field was wrong (the reference's variable-scope
      guide: it never does), and reads got their own rule,
      unqualified_field_read. Authority corrected to authoritative to
      mirror the style-guide rule field_access_via_me. Production: 14 ->
      15 findings (one element write).
issues: ["#260"]
---

## Behavior

In a `:CLASS` file only: flags a bare identifier *written* inside a class
method when the name matches a class field. Fields are the identifiers on
`:DECLARE` lines between the `:CLASS` statement and the first method.
Writes are read from the expression tree, so every form counts: the target
of `:=` or a compound assignment (`+=`, `-=`, `*=`, `/=`, `^=`, `%=`),
including the inner targets of a chained assignment; the operand of
`++`/`--` in either position; the root of an element write
(`aItems[1] := x`); and a `:FOR` loop variable. A bare name inside a method
never reaches the field (ssl-style-guide `field_access_via_me`): a plain
`:=` creates a method-local and leaves the field unchanged, and the other
forms act on whatever the bare lookup finds. The message prescribes
`Me:fieldName` and names the form. The range covers the identifier.

Bare *reads* are `unqualified_field_read`. A statement that both reads and
writes the name (`nTotal := nTotal + 1`) is reported once, here, and later
reads of a name already bare-written in the method are not reported.

It must NOT flag:

- qualified assignment — any `:`-qualified form (`Me:nTotal`,
  `Base:nTotal`, `oOther:nTotal`) is object member access, not a bare
  local;
- a field name shadowed by a method-local `:DECLARE`, `:PARAMETERS`, or
  `:PUBLIC` entry — the local is then the author's evident intent;
- the declaration lines themselves, or names matching SSL built-ins;
- reads of the field name — those are `unqualified_field_read`;
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

### Flags

```ssl
:CLASS Counter;
:DECLARE nTotal, aItems;

:PROCEDURE Bump;
	nTotal++;
	aItems[1] := 5;
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

:PROCEDURE Share;
	:PUBLIC nTotal;
	nTotal := 5;
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
explicit local shadow suppresses the finding. Bare *reads* do not reach
the field either (the reference's variable-scope guide: a bare name in a
method never refers to a field); they are `unqualified_field_read`, a
separate code so a team can tune the two independently. Reading targets
from the expression tree (issue #255) is what catches the increment and
element-write forms in the third fence, which a scan for `name :=` could
not see.
