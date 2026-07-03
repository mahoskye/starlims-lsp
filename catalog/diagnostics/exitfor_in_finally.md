---
id: diag.exitfor_in_finally
title: ":EXITFOR inside a :FINALLY block"
kind: diagnostic
status: active
authority: authoritative
schema_ref: lints.compile_errors.exitfor_in_finally
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

Fires an error on each `:EXITFOR` that is lexically inside the `:FINALLY`
section of any enclosing `:TRY` block. The check walks the whole try
stack, so an `:EXITFOR` inside a nested `:TRY` whose outer frame is in
`:FINALLY` still fires; a `:CATCH` ends the current frame's finally
section. This rule fires independently of loop context — an `:EXITFOR` in
a `:FINALLY` with no enclosing `:FOR` additionally gets
`exitfor_outside_loop`.

It must NOT flag:

- `:EXITFOR` in the `:TRY` body or in a `:CATCH` section;
- `:EXITFOR` in loop code outside any `:TRY` block.

## Examples

### Flags

```ssl
:PROCEDURE Demo;
	:DECLARE nIdx;
	:FOR nIdx := 1 :TO 10;
		:TRY;
			nIdx := nIdx + 1;
		:FINALLY;
			:EXITFOR;
		:ENDTRY;
	:NEXT;
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE Demo;
	:DECLARE nIdx;
	:FOR nIdx := 1 :TO 10;
		:TRY;
			:EXITFOR;
		:CATCH;
			nIdx := 10;
		:ENDTRY;
	:NEXT;
:ENDPROC;
```

## Rationale

The schema rule (`lints.compile_errors.exitfor_in_finally`, section level
`authoritative`) mirrors the STARLIMS compiler: `:EXITFOR` inside a
`:FINALLY` block is rejected, because finally blocks must run to
completion and cannot transfer control out. Error severity and
authoritative authority match the schema. The Does-not-flag fence pins
that `:TRY`/`:CATCH` sections remain legal homes for `:EXITFOR`.
