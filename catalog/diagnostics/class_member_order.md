---
id: diag.class_member_order
title: Class members ordered :INHERIT, :DECLARE, then methods
kind: diagnostic
status: active
authority: advisory
schema_ref: lints.class_rules.class_member_order
default_severity: info
config:
  - ssl.diagnostics.infoDiagnostics
severity_overridable: true
suppressible: true
spec_options:
  include_info_diagnostics: true
tests:
  - internal/providers/providers_test.go
history:
  - date: 2026-03-21
    ref: "commit cdbfee6"
    note: Introduced during the full style-guide alignment pass.
  - date: 2026-04-30
    ref: "PR #3 (v0.4.0)"
    note: Stable diagnostic code assigned when every diagnostic gained a Code.
  - date: 2026-05-01
    ref: "commit d134334"
    note: >-
      Dropped the Constructor-must-be-last special case — Constructor
      placement among methods is stylistic and legacy classes routinely put
      it first. Only :INHERIT before :DECLARE before methods is enforced.
issues: []
---

## Behavior

In a `:CLASS` file, class-level member statements rank `:INHERIT` (1),
`:DECLARE` (2), `:PROCEDURE` (3). After the `:CLASS` statement, an info
diagnostic fires on each statement-initial member keyword whose rank is
lower than the highest rank already seen: a class-level `:DECLARE` after
the first method, or an `:INHERIT` after a `:DECLARE` or a method.

It must NOT flag:

- files without a `:CLASS` statement (the check only runs on class files);
- members in the correct order, including repeated members of the same
  rank (several `:DECLARE` lines in a row);
- anything inside a method body — the scan skips from `:PROCEDURE` to
  `:ENDPROC`, so method-local `:DECLARE` statements never count;
- `Constructor` placement anywhere among the methods (commit d134334 —
  the earlier Constructor-must-be-last enforcement was deliberately
  removed as stylistic noise).

## Examples

### Flags

```ssl
:CLASS MyClass;
:PROCEDURE DoWork;
:ENDPROC;
:DECLARE nCount;
```

### Flags

```ssl
:CLASS MyClass;
:DECLARE nCount;
:INHERIT BaseClass;
```

### Does not flag

```ssl
:CLASS MyClass;
:INHERIT BaseClass;
:DECLARE nCount;
:PROCEDURE Constructor;
:ENDPROC;
:PROCEDURE DoWork;
:ENDPROC;
```

### Does not flag

```ssl
:CLASS MyClass;
:PROCEDURE DoWork;
	:DECLARE nLocal;
	nLocal := 1;
:ENDPROC;
:PROCEDURE Cleanup;
:ENDPROC;
```

## Rationale

The schema rule (`lints.class_rules.class_member_order`, severity `info`)
describes the successful member order this guide uses, not a compiler
requirement — hence advisory authority and info severity. History shows the
rule was over-enforced once: d134334 removed the Constructor-position check
after legacy classes that place Constructor first kept tripping it. The
Constructor-first Does-not-flag fence pins that rollback permanently.
