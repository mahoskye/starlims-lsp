---
id: diag.mixed_error_handling_families
title: Legacy :ERROR/:RESUME mixed with :TRY/:CATCH in one procedure
kind: diagnostic
status: active
authority: style_only
schema_ref: null
default_severity: warning
severity_overridable: true
suppressible: true
tests:
  - internal/providers/providers_test.go
  - internal/providers/diagnostics_audit_test.go
history:
  - date: 2026-08-26
    ref: "issue #191"
    note: >-
      Introduced from the runtime-verification batch: the legacy handler
      can intercept a raised error before the :CATCH sees it — very
      confusing control flow, rarely intentional.
  - date: 2026-10-13
    ref: "issue #255"
    note: >-
      Top-level script code became one span. Every :PROCEDURE/:ENDPROC
      boundary had closed the top-level span too, so a script's :TRY
      before its procedures and its legacy handler after them (the
      grammar's place for it) never met. Authority corrected tool ->
      style_only to mirror the style guide record
      (no_mixed_error_handling_families), and a Flags fence added for the
      shape that actually intercepts: the handler as the procedure's last
      section, after the :TRY.
  - date: 2026-10-13
    ref: "issue #328"
    note: >-
      Corrected: `:ERROR` in expression position is not a corpus-observed
      form; the grammar admits only the statement marker and
      unexpected_token reports the operand. This rule still ignores it.
  - date: 2026-10-13
    ref: "issue #328"
    note: >-
      For the top-level span the message names the top-level script code
      and asks for one family in the script body; it said "the same
      procedure".
issues: []
---

## Behavior

Flags a procedure that contains both error-handling families: a legacy
marker statement — `:ERROR` or `:RESUME` whose next significant token is
`;` — together with any `:TRY` or `:CATCH` keyword (structured family) in
the same span. Each `:PROCEDURE` ... `:ENDPROC` is one span; all
top-level script code is one further span, however many procedures sit
between its pieces — a script's legacy handler comes after the
statements it protects, so a top-level `:TRY` before the procedures and
a top-level `:ERROR;` after them still meet. One diagnostic per span,
ranged on the first token of whichever family appears later — the token
that introduced the mix. In the shape that intercepts, that is the
`:ERROR`.

It must NOT flag:

- a procedure using only one family, whichever it is;
- the two families in *different* procedures of the same file — each
  procedure's handler chain is independent;
- `:ERROR` written as an operand (`LimsString(:ERROR)`) — only the
  marker statement `:ERROR;` starts a legacy handler; the grammar has no
  expression form, so `unexpected_token` reports the operand (issue
  #328);
- any severity duplication with `deprecated_keyword`: every `:ERROR` /
  `:RESUME` use already carries that separate warning; this rule adds the
  interception hazard only when the families actually meet.

## Examples

### Flags

```ssl
:PROCEDURE CountRows;
	:DECLARE nCount;
	:TRY;
		nCount := 1;
	:CATCH;
		nCount := 0;
	:ENDTRY;
	:RETURN nCount;
	:ERROR;
		nCount := -1;
	:RESUME;
:ENDPROC;
```

### Flags

```ssl
:DECLARE nCount;
:TRY;
	nCount := 1;
:CATCH;
	nCount := 0;
:ENDTRY;

:PROCEDURE Helper;
	nCount := 2;
:ENDPROC;

:ERROR;
	nCount := -1;
:RESUME;
```

### Flags

```ssl
:PROCEDURE Process;
	:ERROR;
		:RESUME;
	:TRY;
		DoWork();
	:CATCH;
		LogError();
	:ENDTRY;
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE Process;
	:TRY;
		DoWork();
	:CATCH;
		LogError();
	:ENDTRY;
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE Process;
	:DECLARE sResult;
	:TRY;
		DoWork();
	:CATCH;
		sResult := "Error: " + LimsString(:ERROR);
	:ENDTRY;
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE LegacyOnly;
	:ERROR;
		:RESUME;
:ENDPROC;

:PROCEDURE ModernOnly;
	:TRY;
		DoWork();
	:CATCH;
		LogError();
	:ENDTRY;
:ENDPROC;
```

## Rationale

A legacy `:ERROR` handler is the last section of its procedure (or
script) and protects every statement before it, including a `:TRY` body,
so in resume mode it takes a raised error before the `:CATCH` sees it —
control flow that reads as structured but is not (style guide record
`no_mixed_error_handling_families`, style_only; grammar `ErrorHandler`;
issue #191). A handler placed before the `:TRY` protects nothing, but the
rule flags any mix: the families are not to be combined at all. Warning
severity: the code compiles and may even behave as intended, but the mix
is rarely deliberate and always costly to reason about. Per-procedure
scoping keeps gradual migrations legal — converting one procedure at a
time to `:TRY`/`:CATCH` never flags the file's remaining legacy
procedures.
