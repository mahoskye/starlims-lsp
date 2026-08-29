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
  - internal/providers/expr_call_checks_test.go
  - internal/providers/expr_types_test.go
history:
  - date: 2026-08-26
    ref: "issue #194"
    note: >-
      Introduced from the runtime-verification batch: Format takes ONE
      array holding every replacement value, even for a single
      placeholder; a scalar second argument is the single most common
      misuse. Hungarian-heuristic detection until expression typing
      exists (issue #184).
  - date: 2026-08-28
    ref: "issue #184 (expression AST consumers)"
    note: >-
      Rebuilt on the expression AST: both sides are now typed rather than
      token-matched. The receiver may be any expression that infers to a
      string (`AllTrim(sTpl):Format`, `Me:sTemplate:Format`), and the
      second argument is judged by inferred type instead of having to be
      a single token — so `sA + sB`, `AllTrim(sA)`, and `nCount > 3` flag
      where they previously stayed silent. Two deliberate narrowings: an
      identifier now needs a documented Hungarian prefix to claim a type,
      so `xThing`/`vThing`/loop counters are unknown rather than
      presumed-scalar; and `String:Format` is excluded because `String`
      claims no type under the prefix shape, not because of a
      second-letter test. Corpus differential over 6,228 production
      files: identical output to the token implementation (0 hits both
      before and after).
issues: []
---

## Behavior

Flags the second argument of a `Format` method call (`<recv>:Format(...)`,
method name case-insensitive) found in the expression AST's call-site
index, when the receiver infers to a **string** and the second argument
infers to a definite **non-array** type:

- with exactly two arguments, an argument inferring to string, number,
  boolean, date, object, or code block — of any expression shape, so
  `sA + sB`, `AllTrim(sA)`, `Len(sA)`, and `nCount > 3` all qualify;
- with **more than two** arguments, the call flags unconditionally on the
  second argument — Format takes exactly a template plus one array.

The range covers the offending argument's full span. The message names
the receiver when it has a name of its own (a variable or a member); a
composed receiver (`AllTrim(sTpl):Format`) drops the prefix rather than
naming a token that is not the receiver.

Typing is the coarse inference described in `internal/providers/expr_types.go`:
literals, operator results, and builtin return types from the element
inventory, plus — for this rule specifically — Hungarian prefixes on bare
identifiers and member names, since for `sFmt` the naming convention *is*
the declared contract. A prefix claims a type only in its documented
shape: a lowercase prefix from the style guide's table followed by an
uppercase body letter.

It must NOT flag:

- a `{...}` array-literal second argument — the correct form, even with
  one element;
- anything inferring to an array (`aValues`, `SelectList(...)`);
- `NIL` — explicitly passing no values;
- an argument whose type cannot be resolved — a `v`-prefixed or
  loop-counter name, a user procedure's return, an array element
  (`aList[1]`), an `any`-returning builtin (`Eval(...)`). Unknown is no
  claim, never evidence (issue #184);
- receivers that do not infer to a string (`oDoc:Format`, `Me:Format`,
  `aTemplates[1]:Format`) — other objects may define scalar-accepting
  Format methods;
- `String:Format(...)` — the .NET String class, whose Format is
  legitimately variadic (corpus-observed); `String` claims no type
  because its body letter is lowercase;
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

### Flags

```ssl
:PROCEDURE Greet;
	:PARAMETERS sA, sB, nCount;
	:DECLARE sFmt, sMsg;
	sMsg := sFmt:Format("{0}", sA + sB);
	sMsg := sFmt:Format("{0}", AllTrim(sA));
	sMsg := sFmt:Format("{0}", nCount > 3);
:ENDPROC;
```

### Flags

```ssl
:PROCEDURE Greet;
	:PARAMETERS sA;
	:DECLARE sMsg;
	sMsg := AllTrim(sA):Format("{0}", sA);
	sMsg := Me:sTemplate:Format("{0}", sA);
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
:PROCEDURE Greet;
	:PARAMETERS xThing, vThing;
	:DECLARE sFmt, sMsg, aList, aTemplates, fnBuild;
	sMsg := sFmt:Format("{0}", xThing);
	sMsg := sFmt:Format("{0}", vThing);
	sMsg := sFmt:Format("{0}", MyHelper(1));
	sMsg := sFmt:Format("{0}", aList[1]);
	sMsg := sFmt:Format("{0}", Eval(fnBuild));
	sMsg := aTemplates[1]:Format("{0}", xThing);
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
rule was deliberately Hungarian-bound on both sides while token scanning
was the only tool available; #184's expression AST replaced that with
inference over the real tree, so receiver and argument are now judged by
type rather than by token shape, and the "multi-token argument" blind
spot is closed. Everything unprovable still stays silent: inference
returns unknown for names outside the documented prefix table, for
user-procedure returns, and for array elements, and unknown never flags.
Hungarian prefixes remain evidence *here* — for a variable named `sFmt`
the convention is the declared contract, and it is what separates a
string variable from `String` (the .NET class) — but they are opt-in
evidence the structural inference does not use.

Note the flagged two-argument example: `sFmt:Format(sName)` (line 5) does
NOT flag — one argument only — while line 6's scalar `sName` does.
