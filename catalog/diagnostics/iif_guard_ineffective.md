---
id: diag.iif_guard_ineffective
title: IIf value argument reads what its condition tests for
kind: diagnostic
status: active
authority: tool
schema_ref: null
default_severity: warning
config:
  - ssl.diagnostics.rules
severity_overridable: true
suppressible: true
tests:
  - internal/providers/iif_guard_test.go
history:
  - date: 2026-11-27
    ref: "issue #372"
    note: >-
      Introduced. Issue #372 asked for a rule on `IIf` arguments, and three
      candidates were measured on the production corpus; this is the one
      aimed at a run-time error. Verified on STARLIMS 12: `IIf` runs both
      value arguments whatever the condition is;
      `IIf(Empty(aRows), 0, aRows[1, 1])` raises an index error when
      `aRows` is empty, and so does the same read behind a `Len` test; a
      property read of `NIL` behind an `Empty` test raises too; and
      `SQLExecute`, `LSelect` and `LSelect1` return an empty array for a
      query that matches no rows, so the array case is an ordinary one.
      Production corpus: 15 findings in 11 scripts, 14 element reads and
      one member read. Four of the 14 read the result of an aggregate
      query, which returns a row even when nothing matches, so they never
      raise and their test never fires. A new default-on warning: a
      behavior change under D16.
  - date: 2026-11-27
    ref: "PR #381 follow-up"
    note: >-
      The two forms the rule reported on reasoning alone were run on
      STARLIMS 12, and both raise: an element read of NIL behind a NIL
      test (`IIf(aRows != NIL, aRows[1], "")`) and a method call on NIL
      behind one (`IIf(oOwner == NIL, "", oOwner:GetName())`). The silence
      for `Empty` of a string was confirmed as well:
      `IIf(Empty(sName), "", sName:Trim())` returns for an empty string and
      for a declared name never assigned, and raises only when the name
      holds NIL. No behavior change.
issues: []
---

## Behavior

Flags a read inside a value argument of a three-argument `IIf(...)` call
when the call's condition is what would make that read safe. `IIf` is a
function: both value arguments are evaluated before one is returned, so in
`IIf(Empty(aRows), 0, aRows[1, 1])` the element is read when `aRows` is
empty too, and the call raises. The test in the condition guards nothing.
Warning severity; the range covers the read.

The rule works out what the condition proves about a **subject** when it
is true and when it is false, and looks for a read of that subject in the
value argument selected by the outcome that proves it is there.

What a condition proves:

- `Empty(x)`: when false, `x` has an element;
- `Len(x)` or `ALen(x)` compared with a whole-number literal: whichever
  outcome means the length is at least 1. `Len(x) > 0`, `Len(x) >= 1`,
  `Len(x) != 0` and `Len(x) == 2` prove it when true; `Len(x) < 1`,
  `Len(x) == 0` and `Len(x) != 1` prove it when false. Either operand
  order is read;
- `x == NIL` or `x != NIL` (also `=`, `<>`, `#`, and NIL on the left):
  whichever outcome means `x` is not NIL;
- `Empty(x)` where `x` is named as an object by its Hungarian prefix
  (`oOwner`, `Me:oClient`): when false, `x` is not NIL as well;
- `.NOT.` and `!` swap the outcomes, parentheses change nothing, and
  `.AND.` and `.OR.` combine: a conjunction proves when true what either
  operand proves when true, and a disjunction proves when false what
  either operand proves when false.

The subject is a variable, a member of one (`Me:aRows`), or an element of
one at a fixed position (`aParts[3]`). Names compare without case, and
`x[1, 2]` is the same subject as `x[1][2]`.

What is flagged in the selected argument:

- an element read, `x[...]`, of a subject proved to have an element or
  proved not NIL;
- a member read, `x:name` or `x:Name(...)`, of a subject proved not NIL.

The read may sit anywhere in the argument: inside a call, a
concatenation, another subscript or a nested `IIf`. One finding per value
argument, on the first such read in source order.

The rule reads the call on its own. It does not look at an enclosing
`:IF`, or at where the subject's value came from.

It must NOT flag:

- a condition that tests the element itself
  (`IIf(Empty(aRow[1]), "", aRow[1])`): the read is of `aRow`, the test is
  of `aRow[1]`, and reading an element that exists is safe;
- a read in the argument the condition does not protect
  (`IIf(Len(aRows) > 1, "Several", aRows[1])`): an `:IF` would read it on
  the same path, so `IIf` adds no failure;
- a member read behind an `Empty` test of a name that does not declare an
  object (`IIf(Empty(sName), "", sName:Trim())`): the test may be for an
  empty string, whose members are still there to call (D10);
- a member read behind a length test (`IIf(Len(aRows) > 0, aRows:Count, 0)`);
- a condition joined so that neither outcome proves anything
  (`IIf(Len(aRows) > 0 .OR. bAny, aRows[1], NIL)`);
- a read that the argument guards itself: `.AND.` and `.OR.` short-circuit,
  so in `Len(aRows) > 0 .AND. aRows[1] == 1` the element is read only when
  it is there;
- a subject that holds a call (`IIf(Empty(GetRows()), 0, GetRows()[1])`):
  two calls are not known to return the same value;
- a read inside a code block literal, which is not evaluated where it is
  written;
- a member call `oObject:IIf(...)`, which is not the built-in (D10), or a
  call with other than three arguments;
- the same logic written with `:IF`.

## Examples

### Flags

```ssl
:PROCEDURE GetOwnerName;
	:PARAMETERS sSampleId;
	:DECLARE aRows;

	aRows := SQLExecute("SELECT OwnerName FROM Samples WHERE SampleID = ?sSampleId?");

	:RETURN IIf(Empty(aRows), "", aRows[1, 1]);
:ENDPROC;
```

### Flags

```ssl
:PROCEDURE FirstItem;
	:PARAMETERS aItems;

	:RETURN IIf(Len(aItems) > 0, aItems[1], NIL);
:ENDPROC;
```

### Flags

The read may be anywhere in the argument.

```ssl
:PROCEDURE BatchLabel;
	:PARAMETERS aBatch;

	:RETURN IIf(Len(aBatch) == 0, "No batch", LimsString(aBatch[1, 1]) + " / " + aBatch[1, 2]);
:ENDPROC;
```

### Flags

A member read behind a NIL test fails the same way.

```ssl
:PROCEDURE OwnerLabel;
	:PARAMETERS oOwner;

	:RETURN IIf(oOwner == NIL, "Unassigned", oOwner:displayName);
:ENDPROC;
```

### Flags

`Empty` of a name that declares an object is a NIL test.

```ssl
:PROCEDURE OwnerLabel;
	:PARAMETERS oOwner;

	:RETURN IIf(Empty(oOwner), "Unassigned", oOwner:displayName);
:ENDPROC;
```

### Does not flag

The fix: the test is an `:IF`, and the read is inside it.

```ssl
:PROCEDURE GetOwnerName;
	:PARAMETERS sSampleId;
	:DECLARE aRows, sOwner;

	aRows := SQLExecute("SELECT OwnerName FROM Samples WHERE SampleID = ?sSampleId?");
	sOwner := "";

	:IF !Empty(aRows);
		sOwner := aRows[1, 1];
	:ENDIF;

	:RETURN sOwner;
:ENDPROC;
```

### Does not flag

The condition tests the element, and reading an element that exists is
safe.

```ssl
:PROCEDURE CleanName;
	:PARAMETERS aRow;

	:RETURN IIf(Empty(aRow[1]), "Unknown", aRow[1]);
:ENDPROC;
```

### Does not flag

The read is in the argument the condition does not protect, so an `:IF`
would read it on the same path.

```ssl
:PROCEDURE Summary;
	:PARAMETERS aRows;

	:RETURN IIf(Len(aRows) > 1, "Several", aRows[1]);
:ENDPROC;
```

### Does not flag

An `Empty` test of a string does not make its members unsafe.

```ssl
:PROCEDURE Tidy;
	:PARAMETERS sName;

	:RETURN IIf(Empty(sName), "", sName:Trim());
:ENDPROC;
```

### Does not flag

The argument guards its own read: `.AND.` stops at a false left operand.

```ssl
:PROCEDURE StartsWithOne;
	:PARAMETERS aItems;

	:RETURN IIf(Len(aItems) > 0, Len(aItems) > 0 .AND. aItems[1] == 1, .F.);
:ENDPROC;
```

## Rationale

`IIf(Empty(aRows), 0, aRows[1, 1])` reads like a guarded access and is
not one. The author wrote the test, so the author expects the array to be
empty sometimes, and each time it is the call raises instead of returning
the default. The style guide states the cause (`IIf` evaluates both
branches) and asks for a length check before an index
(`check_alen_before_index`); this is the shape where the check is present
and does nothing. The behavior was verified on STARLIMS 12 before the rule
was written (history).

Warning, not error: the code compiles, and it runs correctly whenever the
subject is there. Nothing weaker either, because when it fails it fails
with a run-time error on the path the author tried to protect.

The rule is narrow on purpose. Issue #372 measured two wider candidates
on the production corpus: any value argument that is not a literal or a
name (238 of 924 calls, mostly harmless expressions), and a dispatch or
SQL call in a value argument (4 calls). Both describe code that works.
This one reports only a read that an `:IF` would have protected, which is
why a read in the unprotected argument and a test of the element itself
stay silent: `IIf` changes nothing there.

A member read is reported only behind a test that says NIL. `Empty(x)` is
true for NIL and for an empty string alike, and a string's members can be
called on an empty string, so that test is taken as a NIL test only when
the name declares an object. A subject compared with NIL is the author's
own statement that it can be NIL, and member access on NIL raises, so
this does not judge what kind of value the receiver is (D10).

A finding does not mean the call fails today. Four of the 15 corpus
findings read the result of an aggregate query, which returns a row even
when nothing matches, so the array is never empty. The test there is dead
code, and the finding is still the right pointer to it.
