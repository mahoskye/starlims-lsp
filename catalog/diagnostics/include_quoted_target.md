---
id: diag.include_quoted_target
title: ":INCLUDE target written as a string"
kind: diagnostic
status: active
authority: authoritative
schema_ref: declarations.include_invalid_forms
default_severity: error
config:
  - ssl.diagnostics.rules
severity_overridable: true
suppressible: true
tests:
  - internal/providers/include_quoted_target_test.go
history:
  - date: 2026-10-13
    ref: "issue #266"
    note: >-
      Introduced. The reference (keywords/INCLUDE.md) and the style guide
      (declarations.include_invalid_forms) both say a quoted :INCLUDE
      target is not accepted; v0.24.0 validated it clean. Zero findings
      on the production corpus (6,228 files).
issues: []
---

## Behavior

Flags an `:INCLUDE` whose target is a string literal in any quote style
(`"..."`, `'...'`, `[...]`). The target must be an identifier or a
qualified identifier (`:INCLUDE Common.Helpers;`); a quoted target does
not compile. The range covers the whole string token, delimiters
included, and the message gives the unquoted statement, so a client quick
fix can replace the range with the text between the delimiters.

Cross-file resolution still reads the quoted form (it strips the quotes),
so go-to-definition and references keep working on the target while it is
being fixed (`feature.definition` A10).

It must NOT flag the identifier forms `:INCLUDE Name;` and
`:INCLUDE Category.Name;`.

## Examples

### Flags

```ssl
:INCLUDE "Common.Helpers";
```

### Flags

```ssl
:INCLUDE [Common.Helpers];
```

### Does not flag

```ssl
:INCLUDE Common.Helpers;
:INCLUDE Helpers;
```

## Rationale

The reference states the target "is written as an identifier or qualified
identifier" and that a quoted-string target "is not accepted". The style
guide lists `:INCLUDE "ScriptName";` under
`declarations.include_invalid_forms`. It is a compile failure, so the
severity is error. Quick fixes are provided by the client
(vs-code-ssl-formatter's code-action provider keys fixes on the
diagnostic code); the LSP supplies the exact range to replace.
