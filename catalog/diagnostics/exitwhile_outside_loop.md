---
id: diag.exitwhile_outside_loop
title: ":EXITWHILE outside a :WHILE loop"
kind: diagnostic
status: active
authority: authoritative
schema_ref: lints.compile_errors.exitwhile_outside_loop
default_severity: error
severity_overridable: true
suppressible: true
tests:
  - internal/providers/providers_test.go
history:
  - date: 2026-03-21
    ref: "commit cdbfee6"
    note: Introduced during the full style-guide alignment pass.
  - date: 2026-04-30
    ref: "PR #3 (v0.4.0)"
    note: Stable diagnostic code assigned when every diagnostic gained a Code.
issues: []
---

## Behavior

Fires an error on each `:EXITWHILE` with no lexically enclosing open
`:WHILE` at that point in the token stream (`:WHILE` pushes, `:ENDWHILE`
pops). An enclosing `:FOR` does not satisfy `:EXITWHILE` — it fires inside
a `:FOR`-only loop.

It must NOT flag:

- `:EXITWHILE` anywhere inside a `:WHILE` ... `:ENDWHILE` body, including
  when nested deeper inside a `:FOR` within the `:WHILE` (any open
  `:WHILE` on the stack satisfies it, since `:EXITWHILE` exits the
  enclosing while).

## Examples

### Flags

```ssl
:PROCEDURE Demo;
	:EXITWHILE;
:ENDPROC;
```

### Flags

```ssl
:PROCEDURE Demo;
	:DECLARE nIdx;
	:FOR nIdx := 1 :TO 10;
		:EXITWHILE;
	:NEXT;
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE Demo;
	:DECLARE nIdx;
	nIdx := 1;
	:WHILE nIdx < 10;
		nIdx := nIdx + 1;
		:EXITWHILE;
	:ENDWHILE;
:ENDPROC;
```

## Rationale

The schema rule (`lints.compile_errors.exitwhile_outside_loop`, section
level `authoritative`) mirrors the STARLIMS compiler: `:EXITWHILE` must be
inside a `:WHILE` loop. Error severity and authoritative authority match
the schema. The second Flags fence pins the loop-kind distinction — a
`:FOR` loop never legitimizes `:EXITWHILE` (that is `:EXITFOR`'s job).
