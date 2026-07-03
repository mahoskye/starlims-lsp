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
issues: []
---

## Behavior

Flags an identifier followed by `(` when the name is not a built-in SSL
function or class — the shape of a C-style direct call, which SSL does not
support. Custom procedures must be dispatched via `DoProc("Name", {args})`,
`ExecFunction("Module.Name", {args})`, or `Me:`/`Base:` inside classes. The
range covers the called identifier. The rule is shape-based: it does not
require the name to match a declared procedure in the file.

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
