---
id: diag.include_early
title: ":INCLUDE placed late in the file"
kind: diagnostic
status: active
authority: advisory
schema_ref: lints.coding_standards.include_early
default_severity: info
severity_overridable: true
suppressible: true
tests:
  - internal/providers/providers_test.go
history:
  - date: 2026-03-28
    ref: "commit be7a174"
    note: checkIncludePlacement introduced (late placement + in-procedure paths).
  - date: 2026-03-30
    ref: "commit 9dc171f"
    note: >-
      Aligned with declaration ordering: :PARAMETERS and :DEFAULT are
      required to precede :INCLUDE, so they no longer count as prior
      statements for the late-placement path.
  - date: 2026-04-30
    ref: "PR #3 (v0.4.0), commit d744511"
    note: >-
      Stable code include_early assigned — to BOTH placement paths,
      although a separate include_in_procedure constant was defined at the
      same time.
  - date: 2026-07-02
    ref: "issue #30"
    note: >-
      Narrowed to the late-placement path only; the in-procedure path now
      emits its intended code, include_in_procedure.
issues: ["#30"]
---

## Behavior

Always-on placement check: flags a top-level `:INCLUDE` that appears after
some other significant statement. `:PARAMETERS` and `:DEFAULT` statements
do not count as "other statements" (they are required to precede
`:INCLUDE`), and comments never count. Recommended order: `:PARAMETERS`,
`:DEFAULT`, `:INCLUDE`, `:PUBLIC`, `:DECLARE`.

It must NOT flag when the `:INCLUDE` is preceded only by comments,
`:PARAMETERS`, `:DEFAULT`, or other `:INCLUDE` statements at the top level.
An `:INCLUDE` inside a procedure body is not this rule's concern — it emits
`include_in_procedure` (warning) instead.

## Examples

### Flags

```ssl
:DECLARE nCount;
:INCLUDE "MyLibrary";
```

### Does not flag

```ssl
/* module header;
:PARAMETERS sName;
:DEFAULT sName, "";
:INCLUDE "MyLibrary";
:DECLARE nCount;
```

### Does not flag

```ssl
:PROCEDURE Setup;
	:INCLUDE "MyLibrary";
:ENDPROC;
```

## Rationale

The schema rates late `:INCLUDE` as info-level advice
(`lints.coding_standards.include_early`): the directive is a textual paste
resolved before the file is read, so position is technically flexible but
early placement keeps expanded content visibly available. The first tuning
(9dc171f) fixed the false positive where a preceding `:PARAMETERS`/`:DEFAULT`
— which *must* come first — triggered the "late" hint. The in-procedure
situation is a stronger claim (unsupported, not just unconventional) and was
split out to its own code, `include_in_procedure`, in the issue #30 fix; the
last Does-not-flag fence pins that this code no longer fires there.
