---
id: diag.constructor_outside_class
title: Constructor procedure outside a :CLASS definition
kind: diagnostic
status: active
authority: style_only
schema_ref: lints.coding_standards.constructor_outside_class
default_severity: warning
severity_overridable: true
suppressible: true
tests:
  - internal/providers/providers_test.go
history:
  - date: 2026-03-21
    ref: "commit cdbfee6"
    note: Introduced during the full style-guide alignment pass.
  - date: 2026-04-30
    ref: "PR #3 (v0.4.0)"
    note: Stable diagnostic code assigned when every diagnostic gained a Code.
issues: []
---

## Behavior

Fires a warning on each `:PROCEDURE` named `Constructor`
(case-insensitive) that is not a class method: either the file contains no
`:CLASS` statement at all, or the procedure begins on or before the
`:CLASS` line. Constructor syntax has no meaning outside a class — the
procedure is just an oddly named script procedure there.

It must NOT flag:

- a `Constructor` method defined after the `:CLASS` statement (whatever
  its position among the other methods);
- procedures with any other name, in scripts or classes.

## Examples

### Flags

```ssl
:PROCEDURE Constructor;
	:DECLARE nCount;
:ENDPROC;
```

### Does not flag

```ssl
:CLASS MyClass;
:PROCEDURE Constructor;
:ENDPROC;
```

## Rationale

The schema rule (`lints.coding_standards.constructor_outside_class`,
severity `warning`) records that Constructor syntax is only meaningful
inside a `:CLASS` definition; the catalog maps schema warnings to
`style_only` authority and the emit site uses warning severity to match.
It is a warning rather than an error because the code still runs — the
procedure simply never fires as a constructor, which is exactly the silent
surprise worth flagging.
