---
id: diag.trailing_skip_commas
title: Trailing skip-commas before a call's closing parenthesis
kind: diagnostic
status: active
authority: style_only
schema_ref: lints.parameter_skipping_style
default_severity: info
config:
  - ssl.diagnostics.infoDiagnostics
severity_overridable: true
suppressible: true
spec_options:
  include_info_diagnostics: true
tests:
  - internal/providers/providers_test.go
history:
  - date: 2026-08-26
    ref: "issue #193"
    note: >-
      Introduced from the runtime-verification batch: the runtime pads
      missing trailing arguments with NIL, so trailing skip-commas add
      nothing. Companion to empty_optional_param_array (the '{}' shape).
  - date: 2026-10-13
    ref: "issue #255 (diagnostics audit, batch B)"
    note: >-
      Severity hint -> info, matching empty_optional_param_array (the
      Rationale's "like its sibling" was wrong: the sibling was already info).
      Authority tool -> style_only and schema_ref set: the style guide's
      parameter_skipping_style.trailing_params clause covers it. Call sites now
      come from the expression AST, and qualified member calls no longer flag
      (D10).
issues: []
---

## Behavior

Info tier (`ssl.diagnostics.infoDiagnostics`, default off). Flags the run
of `,` tokens after a call's last real argument when the argument list
ends in skipped slots (only whitespace/comments between the last comma and
the parenthesis). Calls and their argument slots come from the expression
AST. Trailing skipped
arguments are unnecessary: the runtime pads missing trailing arguments
with NIL, so `Foo(a,,)` is exactly `Foo(a)`. One diagnostic per call; the
range covers the whole comma run.

It must NOT flag:

- anything when the info tier is off — the default;

- interior skip-commas (`Foo(a,,c)`) — there the comma is a positional
  placeholder that cannot be removed;
- empty call parens (`Foo()`);
- commas inside array literals (`{1,2,}` changes the array's length —
  behavior, not style — and is out of this rule's scope);
- grouping parentheses (`(a + b)`) — not a call;
- a qualified member call (`sValue:Substring(1,)`) — the NIL padding of
  trailing arguments is a property of SSL calls, not of whatever a member
  call dispatches to (D10).

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

### Does not flag

```ssl
:PROCEDURE Main;
	:DECLARE sValue;
	sValue := "abc";
	sValue := sValue:Substring(1,);
:ENDPROC;
```

## Rationale

Skip-commas exist to hold interior positions; at the tail they hold
nothing — the runtime NIL-pads either way (issue #193). It transcribes
the style guide's `lints.parameter_skipping_style.trailing_params` clause
("Omit trailing optional parameters rather than passing empty values"),
so it is `style_only`, at info severity like its sibling
`empty_optional_param_array`, which covers the `{}` form of the same
clause: the call works, and the fix is pure deletion. The interior-comma carve-out is the precision core, since
removing an interior skip changes argument positions.
