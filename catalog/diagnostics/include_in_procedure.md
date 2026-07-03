---
id: diag.include_in_procedure
title: ":INCLUDE inside a procedure body"
kind: diagnostic
status: active
authority: tool
schema_ref: null
default_severity: warning
severity_overridable: true
suppressible: true
tests:
  - internal/providers/providers_test.go
history:
  - date: 2026-03-28
    ref: "commit be7a174"
    note: >-
      In-procedure detection introduced in checkIncludePlacement, but
      emitted under code include_early.
  - date: 2026-04-30
    ref: "PR #3 (v0.4.0), commit d744511"
    note: >-
      Constant CodeIncludeInProcedure defined, but the in-procedure emit
      site in checkIncludePlacement stayed tagged CodeIncludeEarly, leaving
      this code dead.
  - date: 2026-07-02
    ref: "issue #30"
    note: >-
      Emit site retagged to CodeIncludeInProcedure; include_early narrowed
      to the late-placement path. The two situations are now configurable
      apart in ssl.diagnostics.rules.
issues: ["#30"]
---

## Behavior

Flags an `:INCLUDE` directive that appears between `:PROCEDURE` and
`:ENDPROC`. Unlike late top-level placement (`include_early`, a style
hint), `:INCLUDE` inside a procedure body is not supported at all, so this
escalates to warning severity.

It must NOT flag top-level `:INCLUDE` directives, wherever they appear in
the file — top-level placement is `include_early`'s concern.

## Examples

### Flags

```ssl
:PROCEDURE Setup;
	:INCLUDE "MyLibrary";
:ENDPROC;
```

### Does not flag

```ssl
:INCLUDE "MyLibrary";
:PROCEDURE Setup;
	:DECLARE nCount;
:ENDPROC;
```

## Rationale

The in-procedure situation is a stronger claim than late placement
(unsupported, not just unconventional), which is why it carries its own
code and warning severity rather than sharing `include_early`'s info-level
default. The split (issue #30) lets `ssl.diagnostics.rules` target the two
situations independently — both constants were defined together in PR #3,
but the emit site was mis-tagged until 2026-07-02.
