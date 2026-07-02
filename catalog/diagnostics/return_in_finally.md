---
id: diag.return_in_finally
title: RETURN inside a FINALLY block
kind: diagnostic
status: active
authority: authoritative
schema_ref: lints.compile_errors.return_in_finally
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

Flags every `:RETURN` token that sits lexically inside a `:FINALLY`
region — from the `:FINALLY` keyword up to the matching `:ENDTRY` — of any
enclosing `:TRY` block, mirroring the SSL compiler's rejection. The check
is token-based: nesting a procedure-like construct inside the `:FINALLY`
body does not exempt the `:RETURN`.

It must NOT flag `:RETURN`:

- in the `:TRY` body or in a `:CATCH` handler (`:CATCH` ends the FINALLY
  region of that TRY level);
- after `:ENDTRY`, or in code with no `:TRY` at all — ordinary procedure
  returns never fire this rule.

## Examples

### Flags

```ssl
:TRY;
    x := 1;
:FINALLY;
    :RETURN NIL;
:ENDTRY;
```

### Does not flag

```ssl
:TRY;
    x := 1;
:CATCH;
    :RETURN NIL;
:ENDTRY;
```

### Does not flag

```ssl
:PROCEDURE GetValue;
    :RETURN 1;
:ENDPROC;
```

## Rationale

The schema lists `return_in_finally` under `lints.compile_errors`
(`level: authoritative`): the STARLIMS compiler rejects `:RETURN` inside
`:FINALLY` outright, so the LSP reports it as an error to surface the
compile failure before check-in. Introduced in the 2026-03-21 alignment
pass (history) verbatim from the schema rule, message ":RETURN inside a
:FINALLY block is rejected."
