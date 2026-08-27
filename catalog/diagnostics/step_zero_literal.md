---
id: diag.step_zero_literal
title: ":FOR loop with a literal zero :STEP"
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
    ref: "issue #199"
    note: >-
      Introduced from the runtime-verification batch: a literal zero step
      never advances the loop variable, so the loop cannot terminate once
      entered.
issues: []
---

## Behavior

Flags the step value of a `:FOR` loop whose `:STEP` is a provable literal
zero: the next significant token after `:STEP` — allowing one leading `+`
or `-` sign — is a numeric literal whose digits are all zero (`0`, `0.0`,
`00`, `-0`). A zero step never advances the loop variable, so the loop
cannot terminate once entered. The range covers the numeric literal.

It must NOT flag:

- any non-zero literal step, including fractional (`:STEP 0.5`) and
  negative (`:STEP -1`) steps;
- a variable or expression step (`:STEP nStep`, `:STEP nA - nB`) — the
  value is not provable from the call site, even when it may be zero at
  runtime;
- the `:STEP` spacing concerns of `step_spacing` — separate rule.

## Examples

### Flags

```ssl
:PROCEDURE Main;
	:DECLARE i;
	:FOR i := 1 :TO 10 :STEP 0;
	:NEXT;
:ENDPROC;
```

### Flags

```ssl
:PROCEDURE Main;
	:DECLARE i;
	:FOR i := 1 :TO 10 :STEP 0.0;
	:NEXT;
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE Main;
	:DECLARE i;
	:FOR i := 10 :TO 1 :STEP -1;
	:NEXT;
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE Main;
	:DECLARE i, nStep;
	nStep := 0;
	:FOR i := 1 :TO 10 :STEP nStep;
	:NEXT;
:ENDPROC;
```

## Rationale

`:STEP 0` compiles and runs; the loop simply spins forever once entered
(issue #199). Warning rather than error because the loop body may exit by
other means (`:EXITFOR`, `:RETURN`), so the code is not provably broken —
but a literal zero step is never what the author wanted. Only literals
flag: a variable step that happens to be zero is a runtime property this
token-level check cannot prove, and guessing would violate the
false-positive bar.
