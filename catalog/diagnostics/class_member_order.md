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
  - date: 2026-10-13
    ref: "issue #255"
    note: >-
      Split: a misplaced :INHERIT is a compile error (reference
      keywords/INHERIT: SSL only accepts :INHERIT directly after the
      class declaration) and now reports as diag.inherit_position at
      error severity. This rule keeps the convention, a class-level
      :DECLARE after the first method, at info. The style-guide record's
      claim that Constructor must be last to compile contradicts
      reference keywords/CLASS, which calls it convention; the d134334
      rollback stands.
issues: []
---

## Behavior

In a `:CLASS` file, fires an info diagnostic on a class-level `:DECLARE`
that appears after the first method (`:PROCEDURE`): fields come before
the methods that use them. The range covers the `:DECLARE` keyword.

A misplaced `:INHERIT` is not this rule's finding. `:INHERIT` must
directly follow the `:CLASS` statement, and anything else is a compile
error, reported as `diag.inherit_position` at error severity (issue
#255).

It must NOT flag:

- files without a `:CLASS` statement (the check only runs on class files);
- members in the correct order, including repeated members of the same
  rank (several `:DECLARE` lines in a row);
- anything inside a method body — the scan skips from `:PROCEDURE` to
  `:ENDPROC`, so method-local `:DECLARE` statements never count;
- `:INHERIT` in any position (`diag.inherit_position` owns it);
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

### Does not flag

A misplaced `:INHERIT` reports as `inherit_position` instead.

```ssl
:CLASS MyClass;
:DECLARE nCount;
:INHERIT BaseClass;
```

## Rationale

The fields-before-methods order is the guide's convention (the
style-guide record class_member_order, severity info), not a compiler
requirement — the reference (keywords/CLASS) lists fields and methods
after the class declaration without ordering them, and calls
Constructor-last a convention that "compiles and runs the same way".
Hence advisory authority and info severity. The `:INHERIT` position is
different in kind: the reference (keywords/INHERIT) says SSL accepts
`:INHERIT` only directly after the class declaration, so issue #255
split it into `diag.inherit_position` at error rather than leave a
compile error in the opt-in info tier. History shows the rule was
over-enforced once: d134334 removed the Constructor-position check after
legacy classes that place Constructor first kept tripping it. The
Constructor-first Does-not-flag fence pins that rollback permanently.
