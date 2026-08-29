---
id: diag.hungarian_type_mismatch
title: Hungarian prefix disagrees with the assigned expression's type
kind: diagnostic
status: active
authority: advisory
schema_ref: lints.hungarian_notation
default_severity: warning
severity_overridable: true
suppressible: true
config:
  - ssl.diagnostics.hungarianNotation
  - ssl.diagnostics.rules
spec_options:
  check_hungarian_notation: true
tests:
  - internal/providers/expr_hungarian_test.go
  - internal/providers/expr_types_test.go
history:
  - date: 2026-08-28
    ref: "issue #184 (expression AST consumers)"
    note: >-
      Introduced as the second expression-AST consumer. #184 proposed
      exactly this as the upgrade for CheckHungarianNotation: SSL's naming
      convention encodes a type annotation, so with expression typing the
      annotation can be cross-checked against what the assigned expression
      actually produces, rather than only validating name shape. Shares
      the existing opt-in setting; separately silenceable through
      ssl.diagnostics.rules. Corpus measurement over 4,620 production
      files: 459 hits in 272 files (5.9%), the largest classes being
      number-named variables assigned string-returning builtins (SubStr,
      Right, At) and array-named variables assigned strings.
issues: []
---

## Behavior

Opt-in check (`ssl.diagnostics.hungarianNotation`, default off, shared
with `hungarian_notation`): flags an assignment whose target's Hungarian
prefix promises one type while the assigned expression infers to a
different one.

It applies to plain `:=` assignments and to `:DEFAULT ident, value`, and
requires definite evidence at both ends:

- the target is a bare identifier whose prefix claims a type in its
  documented shape — a lowercase prefix from the style guide's table
  (`s`, `n`, `b`, `d`, `a`, `o`, `fn`) followed by an uppercase body
  letter;
- the expression infers to a definite type under
  `inferExprTypeNamed` — literals, operator results from the inventory's
  documented type matrix, builtin return types, and Hungarian prefixes on
  identifiers and member names.

The range spans the target through the end of the expression; the message
names both types.

It must NOT flag:

- an expression whose type cannot be resolved — a user procedure's
  return, an `any`-returning builtin, an array element, a `v`-prefixed or
  loop-counter name, or an operand combination the language documents no
  result for (`aList + sText`, `nCount * sText`);
- `NIL` — any variable may be cleared;
- a target with no type-claiming prefix (`vThing`, `Total`, `MAX_ROWS`,
  `i`) — `hungarian_notation` owns the missing-prefix complaint;
- compound assignments (`nTotal += ...`) — the result combines the old
  value with the new one, so the right-hand type is not the result type;
- `:FOR` headers — loop counters carry no prefix by convention;
- a member or indexed target (`Me:sName := 1`, `aRows[1] := "x"`) — the
  prefix on a member does not declare the container's element type;
- anything when the check is disabled, which is the default.

## Examples

### Flags

```ssl
:PROCEDURE Demo;
	:DECLARE nCode, sText, sTotal, aNames, bReady;
	nCode := SubStr(sText, 1, 4);
	sTotal := 0;
	aNames := "Ann,Bob";
	bReady := "Y";
:ENDPROC;
```

### Flags

```ssl
:PROCEDURE Demo;
	:PARAMETERS nCount;
	:DEFAULT nCount, "10";
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE Demo;
	:DECLARE nCode, sText, dStart, aNames, oRec;
	nCode := Len(sText);
	sText := AllTrim(sText);
	dStart := Today();
	aNames := {"Ann", "Bob"};
	oRec := CreateUdObject();
	sText := NIL;
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE Demo;
	:DECLARE nTotal, vThing, aList, sText, i;
	nTotal := MyHelper(1);
	nTotal := aList[1];
	vThing := "anything";
	nTotal := aList + sText;
	i := "counter";
:ENDPROC;
```

## Rationale

The style guide requires Hungarian prefixes and assigns each one a type
(`lints.hungarian_notation.prefixes`), which makes every prefixed name a
type annotation the author wrote down. Before the expression AST there was
no way to check it, so `hungarian_notation` could only ask whether a
prefix was present; #184 named this cross-check as the payoff of typing.
The corpus classes it finds are the ones the convention exists to prevent:
`nCode := SubStr(...)` and `nMax := Right(...)` store strings in
number-named variables, `aTests := BuildString(aTests)` collapses an array
name onto a string, and `:DEFAULT bFlag, ""` gives a boolean a string
default.

Both ends demand definite evidence, so the rule stays quiet wherever
inference is partial — which is most of a dynamically typed codebase. In
particular the operator matrix is taken from the element inventory rather
than hand-written, so a combination the language documents no result for
(`aList + sText`) makes no claim; assuming string concatenation there cost
94 false positives in the first corpus run.

One class deserves naming: `dBug := .F.` flags because `dBug` reads as a
date-prefixed name under the guide's shape rule while holding a boolean.
That is not a miss — the name is "debug" written so that its first letter
impersonates a prefix, and the fix the message proposes (rename it) is the
right one. Opt-in and warning-severity, per the noisy-checks policy: the
code runs, but a name that lies about its type outlives whoever wrote it.
