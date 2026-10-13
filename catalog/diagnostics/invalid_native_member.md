---
id: diag.invalid_native_member
title: Colon member that raises on an SSL value
kind: diagnostic
status: active
authority: authoritative
schema_ref: null
default_severity: warning
config:
  - ssl.diagnostics.rules
severity_overridable: true
suppressible: true
tests:
  - internal/providers/native_members_test.go
history:
  - date: 2026-10-13
    ref: "issue #267"
    note: >-
      Introduced, narrow by design. The reference (guides/native-members.md,
      types/array.md, verified on STARLIMS 12) states that strings,
      numbers, logicals, dates and arrays have no SSL-defined members,
      lists which colon members raise, and says native member names are
      case-sensitive. Scope decision: a diagnostic only where both the
      receiver's type and the failure are certain; completion and hover
      offer no members on these values (feature.completion A11). Zero
      findings on the production corpus. The two `a`-prefixed `:Count`
      receivers there are .NET collections where `:Count` is valid, which
      is why the receiver's type comes from structure, never from its
      Hungarian prefix.
  - date: 2026-10-13
    ref: "issue #260"
    note: >-
      Also reports ++/-- on a value that is certainly a string, which the
      reference (operators/increment.md, decrement.md) documents as a
      run-time invalid-operand error.
issues: []
---

## Behavior

Flags a colon member on an SSL value when the reference documents that
member as raising at run time, at warning severity, ranged on the member
name. Two conditions must both hold.

**The receiver's type is certain.** The receiver is a literal (array,
string, number, logical, date-producing call, or code block), or a
variable whose every plain `:=` assignment in its scope (its procedure, or
the script level) infers the same definite type from structure alone:
literals, operators and builtin return types. Hungarian prefixes are never
used. A variable is not judged at all when it is a `:PARAMETERS` or
`:PUBLIC` name, has a compound assignment, is a `:FOR` counter, has an
assignment of unknown type or `NIL`, or is passed to `DoProc`,
`ExecFunction` or `ExecUdf` (the callee may rebind it).

**The member is a documented failure** for that type, matched exactly
(native names are case-sensitive):

- array: `:Count` (property or method), `:Append()`, `:length`, `:Length()`
  called as a method;
- string: `:Split()`, `:length`, `:Length()`;
- string, number, logical, date: a verified native method spelled in
  another case (`sText:toupper()`, `nNum:tostring()`), and for dates a
  verified property in another case (`dToday:year`);
- any of those value types: the SSL-object members `:IsEmpty()`,
  `:ToJson()`, `:clone()`, `:value`;
- code block: any member;
- string: the `++` and `--` operators (`sText--;`), which the reference
  documents as raising an invalid-operand error on a string.

It must NOT flag:

- a member not documented as failing: an unmatched member on a value is
  usually a legitimate .NET passthrough (DECISIONS.md D10), such as
  `aItems:Length`, `aItems:Clone()`, `sText:PadLeft(5)`;
- a receiver whose type is uncertain, including an `a`-prefixed variable
  holding a .NET collection (`:Count` is valid there);
- objects (`CreateUdObject`, classes, returns objects), whose member names
  ignore case.

## Examples

### Flags

```ssl
:PROCEDURE Demo;
	:DECLARE aItems, nCount;
	aItems := {1, 2, 3};
	nCount := aItems:Count;
	:RETURN nCount;
:ENDPROC;
```

### Flags

```ssl
:PROCEDURE Demo;
	:DECLARE dToday, nYear;
	dToday := Today();
	nYear := dToday:year;
	:RETURN nYear;
:ENDPROC;
```

### Flags

```ssl
:PROCEDURE Demo;
	:DECLARE sText;
	sText := "abc";
	sText--;
	:RETURN sText;
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE Demo;
	:DECLARE aItems, aCopy, nCount;
	aItems := {1, 2, 3};
	nCount := aItems:Length;
	aCopy := aItems:Clone();
	:RETURN nCount;
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE Demo;
	:PARAMETERS aRows;
	:RETURN aRows:Count;
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE Demo;
	:DECLARE aNames;
	aNames := LimsNETConnect(, "System.Collections.Generic.List<String>");
	:RETURN aNames:Count;
:ENDPROC;
```

## Rationale

The reference says a colon member on a string, number, logical, date or
array "either reaches a native member or raises an error", documents the
raising cases, and gives `aValues:Count` (`Invalid property: Count`) as the
common mistake. It is a run-time failure, not a compile error, so the
severity is warning. Because a passthrough to .NET is legal and its member
set is not enumerated, the rule reasons only from documented failures, and
only when the receiver's type is proven from structure; a Hungarian
prefix is author intent, and on real code it labels .NET collections as
arrays.
