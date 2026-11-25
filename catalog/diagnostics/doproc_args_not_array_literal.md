---
id: diag.doproc_args_not_array_literal
title: DoProc with a literal name and arguments not written as an array literal
kind: diagnostic
status: active
authority: authoritative
schema_ref: null
default_severity: error
severity_overridable: true
suppressible: true
tests:
  - internal/providers/expr_call_checks_test.go
history:
  - date: 2026-11-25
    ref: "issue #372"
    note: >-
      Introduced. A project that validated clean did not compile: it
      passed `DoProc("CheckEqual", aCheck)` an argument array held in a
      variable. A one-part string-literal target is bound when the script
      compiles (reference functions/DoProc), and the compiler accepts that
      form's arguments only as an array literal. Verified on STARLIMS 12:
      a script holding `DoProc("Name", aArgs)` stops with `Compile-time
      error: Invalid arguments for: doproc`. On the production corpus
      all 5,797 one-part literal `DoProc` calls that pass arguments write
      them as `{...}`, so the rule reports nothing there. A new default-on
      error code, so a breaking change under D16.
issues: []
---

## Behavior

Flags the second argument of a `DoProc(...)` call when the first argument
is a string literal with no `.` in it and the second is anything but an
array literal: a variable, a parameter, a call, a subscript, a member, a
literal value or `NIL`, or an expression that builds an array. A one-part
literal name is bound to the procedure when the script compiles, and that
form takes its arguments only as `{...}`. Written any other way the
script does not compile (`Invalid arguments for: doproc`), so no `:TRY`
can catch it. The range covers the second argument. Error severity: a
compile-time error.

The fix is to write the array out, `DoProc("Name", {aArgs[1], aArgs[2]})`,
or to call by a name held in a variable when the argument list really is
built at run time.

It must NOT flag:

- arguments written as an array literal, empty or not, on one line or
  several;
- a call with no second argument;
- a target that is not a string literal (`DoProc(sName, aArgs)`): the name
  is resolved at run time, and that path takes any array (reference
  functions/DoProc shows this form);
- a dotted target (`DoProc("Category.Script.Procedure", aArgs)`): whether
  it is bound when the script compiles depends on how the server compiles,
  so the call is left alone (D17);
- `ExecFunction(...)` in any form, for the same reason;
- a call in a class file: a one-part literal target there is already
  `doproc_in_class`, and one finding per call is enough;
- a member call `oObject:DoProc(...)`, which is not the built-in (D10);
- a second argument in parentheses, a skipped one (`DoProc("Name",)`), or
  one the parser could not read: what the compiler makes of these is not
  known, and unknown is silence.

## Examples

### Flags

```ssl
:PROCEDURE RunCheck;
	:PARAMETERS sLabel, vExpected, vActual;
	:DECLARE aCheck, aResult;

	aCheck := {sLabel, vExpected, vActual};
	aResult := DoProc("CheckEqual", aCheck);

	:RETURN aResult;
:ENDPROC;

:PROCEDURE CheckEqual;
	:PARAMETERS sLabel, vExpected, vActual;

	:RETURN {sLabel, vExpected == vActual};
:ENDPROC;
```

### Flags

An expression that yields an array is not an array literal either.

```ssl
:PROCEDURE RunAll;
	:PARAMETERS aChecks;
	:DECLARE aResult;

	aResult := DoProc("CheckEqual", aChecks[1]);

	:RETURN aResult;
:ENDPROC;

:PROCEDURE CheckEqual;
	:PARAMETERS sLabel, vExpected, vActual;

	:RETURN {sLabel, vExpected == vActual};
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE RunCheck;
	:PARAMETERS sLabel, vExpected, vActual;
	:DECLARE aCheck, aResult;

	aCheck := {sLabel, vExpected, vActual};
	aResult := DoProc("CheckEqual", {aCheck[1], aCheck[2], aCheck[3]});

	:RETURN aResult;
:ENDPROC;

:PROCEDURE CheckEqual;
	:PARAMETERS sLabel, vExpected, vActual;

	:RETURN {sLabel, vExpected == vActual};
:ENDPROC;
```

### Does not flag

A name held in a variable is resolved at run time and takes any array.

```ssl
:PROCEDURE CallByName;
	:DECLARE sProcName, aArgs;

	sProcName := "LogMessage";
	aArgs := {"Sample analysis complete", "INFO"};

	DoProc(sProcName, aArgs);
:ENDPROC;

:PROCEDURE LogMessage;
	:PARAMETERS sMessage, sLevel;

	UsrMes("[" + sLevel + "] " + sMessage);
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE Forward;
	:PARAMETERS aArgs;

	:RETURN DoProc("Common.TextRules.Normalize", aArgs);
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE Forward;
	:PARAMETERS aArgs;
	:DECLARE oRunner;

	oRunner := CreateUdObject("Common.Runner");

	:RETURN oRunner:DoProc("Normalize", aArgs);
:ENDPROC;
```

## Rationale

The call reads like the form the reference shows for a run-time name,
`DoProc(sProcName, aArgs)`, and differs only in that the name is a
literal. That difference decides when the call is bound: a one-part
literal is bound at compile time (the reference's `DoProc("NoSuchProc")`
caveat is the same binding), and the bound form reads its arguments out
of an array literal. So code that looks ordinary stops its whole script
from compiling, and a validator that says nothing leaves the author to
find that out in STARLIMS (issue #372). The behavior was verified on
STARLIMS 12 before the rule was written.

Error severity because it is a compile error, with no legitimate use to
weigh against it: the corpus measurement in the history note found no
call of this shape among 5,797. The rule reports only what is certain.
A dotted target and `ExecFunction` are bound at compile time only under a
server setting the file does not show, and the shapes of argument the
parser or the compiler may read differently are left alone, following
D17: where the behavior is not confirmed, the server stays silent.
