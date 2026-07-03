---
id: diag.loop_outside_loop
title: ":LOOP outside a :WHILE or :FOR loop"
kind: diagnostic
status: active
authority: authoritative
schema_ref: lints.compile_errors.loop_outside_loop
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

Fires an error on each `:LOOP` (continue statement) with no lexically
enclosing open loop of either kind at that point in the token stream
(`:WHILE`/`:FOR` push, `:ENDWHILE`/`:NEXT` pop).

It must NOT flag:

- `:LOOP` inside a `:WHILE` ... `:ENDWHILE` body;
- `:LOOP` inside a `:FOR` ... `:NEXT` body — unlike `:EXITWHILE` /
  `:EXITFOR`, either loop kind satisfies `:LOOP`.

## Examples

### Flags

```ssl
:PROCEDURE Demo;
	:LOOP;
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE Demo;
	:DECLARE nIdx;
	nIdx := 1;
	:WHILE nIdx < 10;
		nIdx := nIdx + 1;
		:LOOP;
	:ENDWHILE;
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE Demo;
	:DECLARE nIdx;
	:FOR nIdx := 1 :TO 10;
		:LOOP;
	:NEXT;
:ENDPROC;
```

## Rationale

The schema rule (`lints.compile_errors.loop_outside_loop`, section level
`authoritative`) mirrors the STARLIMS compiler: `:LOOP` must be inside a
`:WHILE` or `:FOR` loop. Error severity and authoritative authority match
the schema. The two Does-not-flag fences pin that both loop kinds count —
the rule must never regress into requiring specifically a `:WHILE`.
