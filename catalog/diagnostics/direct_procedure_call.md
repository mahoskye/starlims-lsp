---
id: diag.direct_procedure_call
title: Procedure called directly instead of via DoProc/ExecFunction
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
  - date: 2026-02-02
    ref: "commit 7261172"
    note: >-
      Introduced with the gotcha checks (gotcha #1): SSL procedures cannot
      be invoked C-style; they require DoProc/ExecFunction dispatch.
  - date: 2026-04-30
    ref: "PR #3 (v0.4.0, commit d744511)"
    note: Stable diagnostic code assigned; behavior unchanged.
  - date: 2026-05-01
    ref: "commit d134334"
    note: >-
      Malformed declarations ("PROCEDURE Name(" / ":PROCEDURE Name(") now
      report procedure_declaration_syntax instead of double-firing this rule.
  - date: 2026-08-12
    ref: "issue #167"
    note: >-
      Severity tiered: stock vendor scripts call built-ins absent from the
      published inventory (SetLocationSQLServer, LimsCleanUp, SetAMPM —
      the largest post-region corpus failure bucket, 90 files), and the
      flat error misdiagnosed the inventory gap as a syntax error. Calling
      a procedure declared in this file keeps the error (definite misuse);
      an unknown bare callable warns with wording that names both
      possibilities.
issues: []
---

## Behavior

Flags an identifier followed by `(` when the name is not a built-in SSL
function or class — the shape of a C-style direct call, which SSL does not
support. Custom procedures must be dispatched via `DoProc("Name", {args})`,
`ExecFunction("Module.Name", {args})`, or `Me:`/`Base:` inside classes. The
range covers the called identifier.

Severity is tiered (issue #167):

- **Error** when the called name matches a `:PROCEDURE` declared in this
  file — dispatch bypass is then provable, and the message prescribes the
  DoProc/ExecFunction/Me: alternatives.
- **Warning** otherwise: an unknown bare callable cannot be distinguished
  from a vendor built-in missing from the published function inventory
  (stock SYSTEMINIT/IMPEXP_FRAMEWORK-era scripts call
  `SetLocationSQLServer`, `LimsCleanUp`, `SetAMPM`), so the message names
  both readings instead of asserting a syntax error. The diagnostic code
  is the same in both tiers; per-rule overrides and suppression apply as
  usual for teams that want the strict or silent behavior.

It must NOT flag:

- built-in SSL function calls (`Len(...)`, `Today()`) and built-in class
  names (those get their own instantiation rule);
- the dispatch functions themselves: `DoProc`, `ExecFunction`, `ExecUDF`,
  `Eval`;
- method calls through a colon (`Me:Helper()`, `oObj:Method()`) — the
  preceding `:` marks member dispatch;
- procedure declarations: `:PROCEDURE Name(...)` and the colonless
  `PROCEDURE Name(...)` typo are reported by
  `procedure_declaration_syntax` instead (d134334);
- a bare reference without parentheses (`x := MyHelper;`).

## Examples

### Flags

```ssl
:PROCEDURE MyHelper;
:ENDPROC;

:PROCEDURE Main;
	:DECLARE result;
	result := MyHelper();
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE MyHelper;
:ENDPROC;

:PROCEDURE Main;
	:DECLARE result;
	result := DoProc("MyHelper", {1});
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE Main;
	:DECLARE nLen;
	nLen := Len("abc");
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE Demo;
	:DECLARE sName;
	sName := "abc";
	sName:SomeRandomDotNetMethod();
:ENDPROC;
```

### Flags

```ssl
:DECLARE s;
s := LimsCleanUp();
```

## Rationale

Direct calls are gotcha #1 for developers arriving from C-style languages:
the code looks obviously right and simply does not work in SSL, so error
severity is warranted (7261172). Precision comes from subtraction — every
legitimate identifier-paren shape (built-ins, dispatchers, member calls,
declarations) is carved out, and d134334 moved the malformed-declaration
shape to its own rule so users see a syntax message rather than a misleading
dispatch message. The colon-member fence pins DECISIONS.md D10 (issue
#22): built-in value types forward unmatched `:` members to .NET at
runtime, so member calls are legitimate dispatch, never C-style calls.
The warning tier (issue #167) applies the same positive-knowledge
principle as D10 to bare calls: an error is only asserted where the
misuse is provable (the target procedure is visible in this file); an
unknown name gets a nudge, not a rejection, because the published
inventory is known to be incomplete for legacy vendor built-ins.
