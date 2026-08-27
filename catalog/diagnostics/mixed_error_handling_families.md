---
id: diag.mixed_error_handling_families
title: Legacy :ERROR/:RESUME mixed with :TRY/:CATCH in one procedure
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
    ref: "issue #191"
    note: >-
      Introduced from the runtime-verification batch: the legacy handler
      can intercept a raised error before the :CATCH sees it — very
      confusing control flow, rarely intentional.
issues: []
---

## Behavior

Flags a procedure that contains both error-handling families: a legacy
marker statement — `:ERROR` or `:RESUME` whose next significant token is
`;` — together with any `:TRY` or `:CATCH` keyword (structured family) in
the same procedure span. Spans
are delimited by `:PROCEDURE`/`:ENDPROC`; tokens outside any procedure
(top-level script code) form one span of their own. One diagnostic per
span, ranged on the first token of whichever family appears later — the
token that introduced the mix.

It must NOT flag:

- a procedure using only one family, whichever it is;
- the two families in *different* procedures of the same file — each
  procedure's handler chain is independent;
- `:ERROR` in expression position (`LimsString(:ERROR)` inside a
  `:CATCH`, a corpus-observed pattern) — only the marker-statement form
  `:ERROR;` starts a legacy handler;
- any severity duplication with `deprecated_keyword`: every `:ERROR` /
  `:RESUME` use already carries that separate warning; this rule adds the
  interception hazard only when the families actually meet.

## Examples

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

When both families coexist in one procedure the legacy `:ERROR` handler
can intercept a raised error before the `:CATCH` sees it, producing
control flow that reads as structured but is not (issue #191). Warning
severity: the code compiles and may even behave as intended, but the mix
is rarely deliberate and always costly to reason about. Per-procedure
scoping keeps gradual migrations legal — converting one procedure at a
time to `:TRY`/`:CATCH` never flags the file's remaining legacy
procedures.
