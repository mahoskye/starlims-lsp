---
id: diag.exitwhile_in_finally
title: EXITWHILE inside a FINALLY block
kind: diagnostic
status: active
authority: authoritative
schema_ref: lints.compile_errors.exitwhile_in_finally
default_severity: error
config:
  - ssl.diagnostics.rules
severity_overridable: true
suppressible: true
tests:
  - internal/providers/providers_test.go
history:
  - date: 2026-03-21
    ref: "commit cdbfee6"
    note: >-
      Introduced (checkLoopAndFinallyControl) in the style-guide alignment
      pass that added schema-backed rule enforcement.
  - date: 2026-04-30
    ref: "PR #3 (v0.4.0)"
    note: Stable diagnostic code assigned when Code was populated on every diagnostic.
issues: []
---

## Behavior

Flags every `:EXITWHILE` token that sits lexically inside a `:FINALLY`
region — from the `:FINALLY` keyword up to the matching `:ENDTRY` — of any
enclosing `:TRY` block, mirroring the SSL compiler's rejection. The check
is token-based and blanket: an `:EXITWHILE` fires even when a `:WHILE`
loop is opened *inside* the `:FINALLY` body, and it fires in addition to
`exitwhile_outside_loop` when no `:WHILE` encloses it.

It must NOT flag `:EXITWHILE`:

- in the `:TRY` body or in a `:CATCH` handler (`:CATCH` ends the FINALLY
  region of that TRY level);
- after `:ENDTRY`, or anywhere with no enclosing `:TRY` at all;
- in an outer loop when the `:FINALLY` block belongs to a TRY that has
  already been closed by its `:ENDTRY`.

## Examples

### Flags

```ssl
:WHILE .T.;
:TRY;
    x := 1;
:FINALLY;
    :EXITWHILE;
:ENDTRY;
:ENDWHILE;
```

### Does not flag

```ssl
:WHILE .T.;
:TRY;
    x := 1;
:CATCH;
    :EXITWHILE;
:ENDTRY;
:ENDWHILE;
```

### Does not flag

```ssl
:WHILE .T.;
:TRY;
    x := 1;
:FINALLY;
    x := 2;
:ENDTRY;
:EXITWHILE;
:ENDWHILE;
```

## Rationale

The schema lists `exitwhile_in_finally` under `lints.compile_errors`
(`level: authoritative`): the STARLIMS compiler rejects `:EXITWHILE`
inside `:FINALLY` outright, so the LSP reports it as an error to surface
the compile failure before check-in. Introduced in the 2026-03-21
alignment pass (history) verbatim from the schema rule, message
":EXITWHILE inside a :FINALLY block is rejected."
