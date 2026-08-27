---
id: diag.spaced_skip_commas
title: Skip-comma pair written with whitespace between the commas
kind: diagnostic
status: active
authority: style_only
schema_ref: null
default_severity: warning
config:
  - ssl.diagnostics.spacedSkipCommas
severity_overridable: true
suppressible: true
spec_options:
  check_spaced_skip_commas: true
tests:
  - internal/providers/providers_test.go
history:
  - date: 2026-08-26
    ref: "issue #193"
    note: >-
      Issue first proposed `, ,` as a syntax error; runtime verification
      (issue comment, 2026-08-27) corrected that — the spaced form is
      valid, and the adjacent form is a stylistic preference. Landed as an
      opt-in warning, default off.
issues: []
---

## Behavior

Opt-in (`ssl.diagnostics.spacedSkipCommas`, default off). Flags a run of
consecutive `,` tokens where at least one adjacent pair has whitespace
(and nothing else) between the commas — `, ,` is valid syntax, but the
adjacent form `,,` makes the skipped argument read as deliberate rather
than as a typo. One diagnostic per run; the range spans the run.

It must NOT flag:

- anything when the setting is off — the default;
- adjacent skip-commas (`Foo(a,,c)`) — the preferred form;
- ordinary argument separators — a comma pair with any non-whitespace
  token between the commas is two separators, not a skip.

## Examples

### Flags

```ssl
:PROCEDURE Main;
	CallMe(1, , 3);
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE Main;
	CallMe(1,, 3);
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE Main;
	CallMe(1, 2, 3);
:ENDPROC;
```

## Rationale

Runtime verification on issue #193 established that spaced skip-commas
parse and behave identically to adjacent ones, so this cannot be an
error — it is a legibility preference (a lone ` , ` reads like an
accidentally deleted argument), hence opt-in and default off per the
issue's correction comment. Warning severity when enabled, as the
comment specified.
