---
id: diag.exitfor_outside_loop
title: EXITFOR outside a FOR loop
kind: diagnostic
status: active
authority: authoritative
schema_ref: lints.compile_errors.exitfor_outside_loop
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

Flags every `:EXITFOR` token that has no open `:FOR` loop on the lexical
loop stack (a `:FOR` opens an entry, its matching `:NEXT` closes it). An
enclosing `:WHILE` does not count — `:EXITFOR` inside a `:WHILE` but
outside any `:FOR` still fires, matching the compiler rule that
`:EXITFOR` exits FOR loops only. The loop stack is tracked per file, not
per procedure: the check follows token order across the whole document.

It must NOT flag `:EXITFOR`:

- anywhere between a `:FOR` and its matching `:NEXT`, at any nesting
  depth (including inside `:IF`/`:BEGINCASE`/`:TRY` bodies within the
  loop);
- inside a `:FOR` loop that is itself nested in a `:WHILE` loop.

This rule is independent of `exitfor_in_finally`: an `:EXITFOR` inside a
`:FOR` loop's `:FINALLY` block fires that rule, not this one.

## Examples

### Flags

```ssl
:EXITFOR;
```

### Flags

```ssl
:WHILE .T.;
    :EXITFOR;
:ENDWHILE;
```

### Does not flag

```ssl
:FOR i := 1 :TO 10;
    :IF i > 5;
        :EXITFOR;
    :ENDIF;
:NEXT;
```

## Rationale

The schema lists `exitfor_outside_loop` under `lints.compile_errors`
(`level: authoritative`): the STARLIMS compiler rejects `:EXITFOR`
outside a `:FOR` loop, so the LSP reports it as an error to surface the
compile failure before check-in. Introduced in the 2026-03-21 alignment
pass (history) verbatim from the schema rule, message ":EXITFOR must be
inside a :FOR loop."
