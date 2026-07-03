---
id: diag.loop_in_finally
title: LOOP inside a FINALLY block
kind: diagnostic
status: active
authority: authoritative
schema_ref: lints.compile_errors.loop_in_finally
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

Flags every `:LOOP` token that sits lexically inside a `:FINALLY` region —
from the `:FINALLY` keyword up to the matching `:ENDTRY` — of any enclosing
`:TRY` block, mirroring the SSL compiler's rejection. The check is
token-based and blanket: a `:LOOP` fires even when a `:WHILE`/`:FOR` loop
is opened *inside* the `:FINALLY` body, and it fires in addition to
`loop_outside_loop` when no loop encloses it.

It must NOT flag `:LOOP`:

- in the `:TRY` body or in a `:CATCH` handler (`:CATCH` ends the FINALLY
  region of that TRY level);
- after `:ENDTRY`, or anywhere with no enclosing `:TRY` at all;
- in ordinary loop bodies with no `:TRY`/`:FINALLY` in sight.

## Examples

### Flags

```ssl
:WHILE .T.;
:TRY;
    x := 1;
:FINALLY;
    :LOOP;
:ENDTRY;
:ENDWHILE;
```

### Does not flag

```ssl
:WHILE .T.;
:TRY;
    x := 1;
:CATCH;
    :LOOP;
:ENDTRY;
:ENDWHILE;
```

### Does not flag

```ssl
:WHILE .T.;
    :LOOP;
:ENDWHILE;
```

## Rationale

The schema lists `loop_in_finally` under `lints.compile_errors`
(`level: authoritative`): the STARLIMS compiler rejects `:LOOP` inside
`:FINALLY` outright, so the LSP reports it as an error to surface the
compile failure before check-in. Introduced in the 2026-03-21 alignment
pass (history) verbatim from the schema rule, message ":LOOP inside a
:FINALLY block is rejected."
