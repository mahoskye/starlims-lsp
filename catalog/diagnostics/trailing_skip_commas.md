---
id: diag.trailing_skip_commas
title: Trailing skip-commas before a call's closing parenthesis
kind: diagnostic
status: active
authority: tool
schema_ref: null
default_severity: hint
severity_overridable: true
suppressible: true
tests:
  - internal/providers/providers_test.go
history:
  - date: 2026-08-26
    ref: "issue #193"
    note: >-
      Introduced from the runtime-verification batch: the runtime pads
      missing trailing arguments with NIL, so trailing skip-commas add
      nothing. Companion to empty_optional_param_array (the '{}' shape).
issues: []
---

## Behavior

Flags a run of one or more `,` tokens immediately preceding a call's `)`
(only whitespace/comments between the last comma and the parenthesis).
Calls are `(` groups directly following an identifier. Trailing skipped
arguments are unnecessary: the runtime pads missing trailing arguments
with NIL, so `Foo(a,,)` is exactly `Foo(a)`. One diagnostic per call; the
range covers the whole comma run.

It must NOT flag:

- interior skip-commas (`Foo(a,,c)`) — there the comma is a positional
  placeholder that cannot be removed;
- empty call parens (`Foo()`);
- commas inside array literals (`{1,2,}` changes the array's length —
  behavior, not style — and is out of this rule's scope);
- grouping parentheses (`(a + b)`) — no identifier precedes a
  non-call paren with commas in valid SSL anyway.

## Examples

### Flags

```ssl
:PROCEDURE Main;
	:DECLARE x;
	x := DoProc("Helper", {1},,);
:ENDPROC;
```

### Flags

```ssl
:PROCEDURE Main;
	CallMe(1,);
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE Main;
	:DECLARE x;
	x := DoProc("Helper", {1});
	CallMe(1,, 3);
:ENDPROC;
```

## Rationale

Skip-commas exist to hold interior positions; at the tail they hold
nothing — the runtime NIL-pads either way (issue #193). Hint severity
like its sibling `empty_optional_param_array`: the call works, the fix is
pure deletion. The interior-comma carve-out is the precision core, since
removing an interior skip changes argument positions.
