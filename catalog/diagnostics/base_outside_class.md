---
id: diag.base_outside_class
title: Base member access outside a class definition
kind: diagnostic
status: active
authority: authoritative
schema_ref: null
default_severity: error
severity_overridable: true
suppressible: true
tests:
  - internal/providers/providers_test.go
history:
  - date: 2026-03-21
    ref: "commit cdbfee6"
    note: >-
      Introduced in the full alignment pass with ssl-style-guide: Me/Base
      reference-form rules (checkClassReferenceForms).
  - date: 2026-04-30
    ref: "PR #3 (v0.4.0)"
    note: Stable diagnostic code assigned; rule behavior unchanged.
  - date: 2026-10-13
    ref: "issue #255"
    note: >-
      A classless file of nothing but :PROCEDURE blocks reports at
      warning, the include-library tier me_outside_class has had since
      issue #171: such a file can compile inside an inheriting class via
      :INCLUDE. authority set to authoritative (reference special-
      forms/base: Base outside a class method is a compile-time error).
issues: []
---

## Behavior

Flags a well-formed `Base:MemberName` reference that appears outside a
`:CLASS` definition — either the file contains no `:CLASS` statement at
all, or the reference sits on a line before the first `:CLASS` keyword.
`Base` refers to the parent of the current class, so it is meaningless in
plain scripts and in file-header code. The range covers the `Base` token.

Severity is contextual, the same tier `diag.me_outside_class` uses (issue
#171): in a classless file consisting solely of `:PROCEDURE` blocks —
comments and paste-time `:INCLUDE` directives allowed, no top-level
statements — the diagnostic is a warning, because that is the shape of an
include library whose procedures compile inside an inheriting `:CLASS`
via `:INCLUDE`, where `Base:Member` is valid. Any top-level statement
makes the file a script and restores the error (issue #255). The
diagnostic code is the same in both tiers.

Class membership is tracked at line granularity against the file's first
`:CLASS` keyword: every token on or after that line counts as inside the
class. SSL files define at most one class, so no end-of-class boundary is
tracked.

It must NOT flag:

- `Base:Member` references on or after the `:CLASS` line (a missing
  `:INHERIT` is reported by `diag.base_requires_inherit`, not this rule);
- standalone `Base` without `:` — that form is owned by
  `diag.base_standalone` and must produce exactly one of the two codes.

## Examples

### Flags

A procedure-only file: reported at warning, since it may be an include
library.

```ssl
:PROCEDURE Demo;
Base:Init();
:ENDPROC;
```

### Flags

A script with top-level code: reported at error.

```ssl
:DECLARE nCount;
nCount := 1;
Base:Init();
```

### Does not flag

```ssl
:CLASS Widget;
:INHERIT Control;
:PROCEDURE Init;
Base:Init();
:ENDPROC;
```

## Rationale

Outside a class there is no parent to delegate to, so `Base:Member` is
guaranteed-broken code and merits an error (reference special-forms/base:
a compile-time error outside a class method). The include-library tier
exists because single-file analysis cannot see the class that will
`:INCLUDE` a procedure library; it mirrors `me_outside_class`. Splitting the Base rules into
three codes (standalone form, outside-class placement, missing `:INHERIT`)
lets each message state the one fix the author needs instead of a generic
"invalid Base usage". Introduced in the style-guide alignment pass (commit
cdbfee6); the code slug was stabilized in PR #3 (v0.4.0).
