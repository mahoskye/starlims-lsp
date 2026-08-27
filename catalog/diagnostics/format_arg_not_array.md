---
id: diag.format_arg_not_array
title: sFmt:Format called with a scalar instead of the values array
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
  - date: 2026-08-26
    ref: "issue #194"
    note: >-
      Introduced from the runtime-verification batch: Format takes ONE
      array holding every replacement value, even for a single
      placeholder; a scalar second argument is the single most common
      misuse. Hungarian-heuristic detection until expression typing
      exists (issue #184).
issues: []
---

## Behavior

Flags the second argument of a `Format` method call (`<recv>:Format(...)`,
method name case-insensitive) when the receiver has the full Hungarian
string shape — leading underscores ignored, `s`/`S` followed by an
uppercase letter (`sFmt`, `sMsg`) — and the argument is provably not an
array:

- with exactly two arguments, a **single-token** second argument that is
  a string literal, a numeric literal, or an identifier whose Hungarian
  prefix is not `a` (and is not `NIL`);
- with **more than two** arguments, the call flags unconditionally on the
  second argument — Format takes exactly a template plus one array.

The range covers the offending argument's first token.

It must NOT flag:

- a `{...}` array-literal second argument — the correct form, even with
  one element;
- an `a`-prefixed identifier (`aValues`) — Hungarian says array;
- a multi-token second argument (`oReq:aIds`, `BuildArgs()`, `1 + 2`) —
  not provable without expression typing (issue #184);
- `NIL` — explicitly passing no values;
- calls on receivers without the Hungarian string shape (`oDoc:Format`,
  `Me:Format`) — other objects may define scalar-accepting Format
  methods;
- `String:Format(...)` — the .NET String class, whose Format is
  legitimately variadic (corpus-observed); `String` fails the shape test
  because its second letter is lowercase;
- `Format` with fewer than two arguments.

## Examples

### Flags

```ssl
:PROCEDURE Greet;
	:PARAMETERS sName;
	:DECLARE sFmt, sMsg;
	sFmt := "Hello {0}";
	sMsg := sFmt:Format(sName);
	sMsg := sFmt:Format("Hello {0}", sName);
:ENDPROC;
```

### Flags

```ssl
:PROCEDURE Greet;
	:PARAMETERS sFirst, sLast;
	:DECLARE sFmt, sMsg;
	sFmt := "{0} {1}";
	sMsg := sFmt:Format("{0} {1}", sFirst, sLast);
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE Greet;
	:PARAMETERS sName;
	:DECLARE sFmt, sMsg, aArgs;
	sFmt := "Hello {0}";
	aArgs := {sName};
	sMsg := sFmt:Format("Hello {0}", {sName});
	sMsg := sFmt:Format("Hello {0}", aArgs);
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE Render;
	:DECLARE oDoc, sOut;
	sOut := oDoc:Format("plain", "utf-8");
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE Report;
	:PARAMETERS nMillis, sCode;
	UsrMes(String:Format("Completed in {0:N3} seconds - Code: {1}", nMillis / 1000, sCode));
:ENDPROC;
```

## Rationale

`:Format(tpl, sVal)` with a scalar is the single most common Format
misuse (issue #194) — the call misbehaves instead of interpolating. The
rule is deliberately Hungarian-bound on both sides (s-prefixed receiver,
non-a-prefixed argument) because without expression typing that naming
convention is the only type evidence available; everything unprovable
stays silent. #184's expression AST is the upgrade path to real
receiver/argument typing.

Note the flagged two-argument example: `sFmt:Format(sName)` (line 5) does
NOT flag — one argument only — while line 6's scalar `sName` does.
