---
id: diag.include_early
title: ":INCLUDE placed late or inside a procedure"
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
      Stable code include_early assigned — to BOTH paths, although a
      separate include_in_procedure constant was defined at the same time
      (see Known gaps).
issues: []
---

## Behavior

Always-on placement check on `:INCLUDE` directives, with two paths sharing
this code:

- **Late placement** (info, the `default_severity` path): a top-level
  `:INCLUDE` appears after some other significant statement. `:PARAMETERS`
  and `:DEFAULT` statements do not count as "other statements" (they are
  required to precede `:INCLUDE`), and comments never count. Recommended
  order: `:PARAMETERS`, `:DEFAULT`, `:INCLUDE`, `:PUBLIC`, `:DECLARE`.
- **Inside a procedure** (warning): an `:INCLUDE` between `:PROCEDURE` and
  `:ENDPROC` is not supported at all, and escalates above the style-level
  hint.

A severity override via `ssl.diagnostics.rules` remaps both paths.

It must NOT flag when the `:INCLUDE` is preceded only by comments,
`:PARAMETERS`, `:DEFAULT`, or other `:INCLUDE` statements at the top level.

## Examples

### Flags

```ssl
:DECLARE nCount;
:INCLUDE "MyLibrary";
```

### Flags

```ssl
:PROCEDURE Setup;
	:INCLUDE "MyLibrary";
:ENDPROC;
```

### Does not flag

```ssl
/* module header;
:PARAMETERS sName;
:DEFAULT sName, "";
:INCLUDE "MyLibrary";
:DECLARE nCount;
```

## Rationale

The schema rates late `:INCLUDE` as info-level advice
(`lints.coding_standards.include_early`): the directive is a textual paste
resolved before the file is read, so position is technically flexible but
early placement keeps expanded content visibly available. The first tuning
(9dc171f) fixed the false positive where a preceding `:PARAMETERS`/`:DEFAULT`
— which *must* come first — triggered the "late" hint. The in-procedure path
is a stronger claim (unsupported, not just unconventional) and warns.

## Known gaps

- The in-procedure path emits code `include_early` even though a dedicated
  `CodeIncludeInProcedure` constant (`include_in_procedure`) exists and was
  clearly intended for it (both landed in PR #3). Consequence: the two
  situations cannot be configured apart in `ssl.diagnostics.rules`, and the
  `include_in_procedure` entry is unimplementable dead code. Splitting the
  code would be a behavior change to this entry and to
  `include_in_procedure`; until then this entry specifies the shared-code
  reality.
