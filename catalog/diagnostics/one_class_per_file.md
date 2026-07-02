---
id: diag.one_class_per_file
title: More than one CLASS definition in a file
kind: diagnostic
status: active
authority: authoritative
schema_ref: lints.compile_errors.one_class_per_file
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
      Introduced (checkClassContextRules) in the style-guide alignment
      pass that added schema-backed rule enforcement.
  - date: 2026-04-30
    ref: "PR #3 (v0.4.0)"
    note: Stable diagnostic code assigned when Code was populated on every diagnostic.
issues: []
---

## Behavior

Flags the second and every subsequent `:CLASS` keyword in a file, one
diagnostic per extra `:CLASS` token, at that token's range. The first
`:CLASS` is never flagged by this rule (a first `:CLASS` that is not the
file's first significant statement is `class_or_script`'s business, not
this rule's).

It must NOT flag:

- a file with exactly one `:CLASS` definition, regardless of how many
  methods it contains;
- plain script files with no `:CLASS` at all;
- identifiers that merely mention a class name — only the `:CLASS`
  keyword token counts.

## Examples

### Flags

```ssl
:CLASS Alpha;
:PROCEDURE DoWork;
:ENDPROC;
:CLASS Beta;
```

### Does not flag

```ssl
:CLASS Alpha;
:DECLARE nCount;
:PROCEDURE DoWork;
:ENDPROC;
```

## Rationale

The schema lists `one_class_per_file` under `lints.compile_errors`
(`level: authoritative`, message "Only one :CLASS definition allowed per
file"), and `module_structure.class_file_constraint` restates it: a file
is one of class, script, or data source — never a mix, with the class
ending at end-of-file (there is no `:ENDCLASS`). A second `:CLASS` can
therefore never be valid, so the LSP reports it as an error. Introduced
in the 2026-03-21 alignment pass (history).
