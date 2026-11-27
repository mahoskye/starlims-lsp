---
id: diag.doproc_args_not_array_literal
title: DoProc call without the argument array it needs
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
  - date: 2026-11-27
    ref: "issue #372"
    note: >-
      Widened to a comma with no argument after it. The argument forms the
      rule had left alone as unknown were run on STARLIMS 12.
      `DoProc("Name",)` stops with the same compile-time error, so it is
      reported now, on the comma. An array literal in parentheses compiles
      and runs. So does the array in a variable when the name is built by
      an expression, when it is a dotted literal, and with `ExecFunction`,
      so those stay unreported. The same run confirmed the shapes already
      reported: a bracket-quoted name, `NIL`, an element of an array, and
      a procedure that comes from an `:INCLUDE`. The production corpus has
      no call with the trailing comma, so the rule still reports nothing
      there.
  - date: 2026-11-27
    ref: "PR #381 follow-up"
    note: >-
      The last argument forms left as unknown were run on STARLIMS 12, and
      the rule follows the result. With a literal one-part name, a
      variable in parentheses (`DoProc("Name", (aArgs))`) and an empty
      array slot followed by another argument (`DoProc("Name",, vExtra)`)
      stop with the same compile-time error, so both are reported now.
      With a name in a variable, `DoProc(sName,)` raises when the call
      runs, so it is reported too, and the title no longer says "literal
      name". What runs, and stays unreported: commas after the array, a
      third argument after the array (it is ignored), an empty array
      literal, and `DoProc(sName, aArgs,)`. None of the newly reported
      shapes occurs in the production corpus.
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

Parentheses change nothing. `DoProc("Name", (aArgs))` is still a variable
and is flagged, parentheses included in the range; `DoProc("Name",
({1, 2}))` is still an array literal and is not.

The fix is to write the array out, `DoProc("Name", {aArgs[1], aArgs[2]})`,
or to call by a name held in a variable when the argument list really is
built at run time.

An empty array slot is flagged too, on the comma that opens it:

- `DoProc("Name",)`, a comma with no argument after it, and
  `DoProc("Name",, vExtra)`, an empty second position with something
  after it. With a literal one-part name both stop with the same compile
  error. The message gives both fixes: drop the comma, or write the array.
- `DoProc(sName,)`, the same comma when the name is not a literal. That
  name is resolved at run time, so the script compiles, and the call
  raises when it runs. This is the only shape reported for a name that is
  not a literal.

It must NOT flag:

- arguments written as an array literal, empty or not, on one line or
  several;
- a call with no second argument;
- a target that is not a string literal (`DoProc(sName, aArgs)`): the name
  is resolved at run time, and that path takes any array (reference
  functions/DoProc shows this form), in parentheses or with a comma after
  it. Only the empty slot above fails for it;
- a target built by an expression (`DoProc("Check" + sKind, aArgs)`): it
  is resolved at run time like a name in a variable;
- a dotted target (`DoProc("Category.Script.Procedure", aArgs)`): it
  compiled and ran on STARLIMS 12, and whether it is bound when the script
  compiles depends on how the server compiles, so the call is left alone
  (D17);
- `ExecFunction(...)` in any form, for the same reason;
- a call in a class file: a one-part literal target there is already
  `doproc_in_class`, and one finding per call is enough;
- a member call `oObject:DoProc(...)`, which is not the built-in (D10);
- an array literal in parentheses (`DoProc("Name", ({1, 2}))`), which
  compiles and runs;
- commas after the array (`DoProc("Name", {1, 2},)`), which compile and
  run and are `trailing_skip_commas`'s to note, and a third argument after
  the array (`DoProc("Name", {1, 2}, vExtra)`), which is ignored and is
  `builtin_excess_arguments`'s;
- an empty array literal (`DoProc("Name", {})`): the procedure's
  parameters are NIL;
- a dotted target with an empty slot or a parenthesized argument: those
  shapes were not run for it (D17);
- an argument the parser could not read: unknown is silence.

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

### Flags

A comma with nothing after it fails the same way.

```ssl
:PROCEDURE RunAll;
	:DECLARE aResult;

	aResult := DoProc("CheckAll",);

	:RETURN aResult;
:ENDPROC;

:PROCEDURE CheckAll;
	:RETURN {};
:ENDPROC;
```

### Flags

A variable in parentheses is still a variable.

```ssl
:PROCEDURE RunCheck;
	:PARAMETERS aCheck;

	:RETURN DoProc("CheckEqual", (aCheck));
:ENDPROC;

:PROCEDURE CheckEqual;
	:PARAMETERS sLabel, vExpected, vActual;

	:RETURN {sLabel, vExpected == vActual};
:ENDPROC;
```

### Flags

An empty second position fails whatever follows it.

```ssl
:PROCEDURE RunAll;
	:PARAMETERS vExtra;

	:RETURN DoProc("CheckAll",, vExtra);
:ENDPROC;

:PROCEDURE CheckAll;
	:RETURN {};
:ENDPROC;
```

### Flags

With a name in a variable the script compiles, and this call raises when
it runs.

```ssl
:PROCEDURE CallByName;
	:PARAMETERS sProcName;

	:RETURN DoProc(sProcName,);
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

An array literal in parentheses is still an array literal, and it compiles.

```ssl
:PROCEDURE RunCheck;
	:PARAMETERS sLabel, vExpected, vActual;

	:RETURN DoProc("CheckEqual", ({sLabel, vExpected, vActual}));
:ENDPROC;

:PROCEDURE CheckEqual;
	:PARAMETERS sLabel, vExpected, vActual;

	:RETURN {sLabel, vExpected == vActual};
:ENDPROC;
```

### Does not flag

A name built by an expression is resolved at run time, like a name in a
variable.

```ssl
:PROCEDURE CallKind;
	:PARAMETERS sKind, aArgs;

	:RETURN DoProc("Check" + sKind, aArgs);
:ENDPROC;

:PROCEDURE CheckEqual;
	:PARAMETERS sLabel, vExpected, vActual;

	:RETURN {sLabel, vExpected == vActual};
:ENDPROC;
```

### Does not flag

Commas after the array, and a third argument after it, compile and run.

```ssl
:PROCEDURE RunCheck;
	:PARAMETERS sLabel, vExpected, vActual, vExtra;
	:DECLARE aResult;

	aResult := DoProc("CheckEqual", {sLabel, vExpected, vActual},);
	aResult := DoProc("CheckEqual", {sLabel, vExpected, vActual}, vExtra);

	:RETURN aResult;
:ENDPROC;

:PROCEDURE CheckEqual;
	:PARAMETERS sLabel, vExpected, vActual;

	:RETURN {sLabel, vExpected == vActual};
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
D17: where the behavior is not confirmed, the server stays silent. Each
shape the rule reports, and each it deliberately passes, was run on
STARLIMS 12; the 2026-11-27 history notes list them.

`DoProc(sName,)` is the one finding that is not a compile error. It is
here because it is the same mistake, an empty slot where the array
belongs, and because it fails every time the call runs: there is no input
for which it works. Error severity for that reason, as with a non-integer
array index.
