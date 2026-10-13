---
id: diag.endclass_invalid
title: ":ENDCLASS used to close a class"
kind: diagnostic
status: active
authority: authoritative
schema_ref: blocks.class.end
default_severity: error
config:
  - ssl.diagnostics.rules
severity_overridable: true
suppressible: true
tests:
  - internal/providers/providers_test.go
history:
  - date: 2026-10-13
    ref: "issue #269"
    note: >-
      Split out of unknown_keyword (warning). :ENDCLASS is a known invalid
      form, not a possible gap in the keyword list: a class extends to the
      end of its file and has no closing keyword. The style guide's
      class_ends_at_eof rule is an authoritative error; reporting it as an
      unknown keyword at warning disagreed with that. Same pattern as
      endfor_invalid. Zero findings on the production corpus.
issues: []
---

## Behavior

Flags a `:ENDCLASS` keyword token, in any case, at error severity, in
scripts and in SSL-mode data sources alike. The range covers the token,
and the message says a class runs to the end of its file. `unknown_keyword`
does not also fire on it.

It must NOT flag member access such as `oObj:EndClass()` (the lexer forms
no keyword token after a receiver).

## Examples

### Flags

```ssl
:CLASS Counter;
:DECLARE nCount;
:ENDCLASS;
```

### Does not flag

```ssl
:CLASS Counter;
:DECLARE nCount;
```

## Rationale

SSL has no `:ENDCLASS`. A `:CLASS` file is one class, and the class extends
to the end of the file (style guide `class_ends_at_eof`, authoritative,
error; schema `blocks.class.end` is null). Writing `:ENDCLASS` does not
compile. Like `:ENDFOR`, it is a form people bring from other languages, so
it gets a targeted message instead of the generic unknown-keyword one.
