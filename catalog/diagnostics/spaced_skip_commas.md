---
id: diag.spaced_skip_commas
title: Skip-comma pair written with whitespace between the commas
kind: diagnostic
status: active
authority: style_only
schema_ref: lints.parameter_skipping_style
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
  - date: 2026-08-26
    ref: "issue #193"
    note: >-
      Issue first proposed `, ,` as a syntax error; runtime verification
      (issue comment, 2026-08-27) corrected that — the spaced form is
      valid, and the adjacent form is a stylistic preference. Landed as an
      opt-in warning, default off.
  - date: 2026-08-27
    ref: "issue #208 discussion (info-tier expansion)"
    note: >-
      Moved warning -> info in the info-tier expansion: the dedicated
      ssl.diagnostics.spacedSkipCommas toggle (never released) is
      subsumed by the tier; promote via ssl.diagnostics.rules to restore
      warning severity. Info is the opt-in advisory tier
      (ssl.diagnostics.infoDiagnostics); explicit ssl.diagnostics.rules
      entries still promote or disable per rule.
  - date: 2026-10-13
    ref: "issue #255 (diagnostics audit, batch B)"
    note: >-
      schema_ref set to lints.parameter_skipping_style (its `rule` clause).
      A spaced run that ends a call's argument list is now left to
      diag.trailing_skip_commas, which used to report the same commas, and the
      Behavior text now says array literals are in scope (they always were).
issues: []
---

## Behavior

Info tier (`ssl.diagnostics.infoDiagnostics`, default off). Flags a run of
consecutive `,` tokens where at least one adjacent pair has whitespace
(and nothing else) between the commas — `, ,` is valid syntax, but the
adjacent form `,,` makes the skipped argument read as deliberate rather
than as a typo. One diagnostic per run; the range spans the run. Any comma
run qualifies — in a call's argument list or in an array literal such as
the argument array of `DoProc`/`ExecFunction`, the form the style guide's
own example uses. The formatter rewrites `, ,` to `,,`.

It must NOT flag:

- anything when the info tier is off — the default;
- adjacent skip-commas (`Foo(a,,c)`) — the preferred form;
- ordinary argument separators — a comma pair with any non-whitespace
  token between the commas is two separators, not a skip;
- a run that ends a call's argument list (`Foo(a, , )`) — that is
  `diag.trailing_skip_commas`, whose fix (delete the run) settles both, so
  one finding per cause.

## Examples

### Flags

```ssl
:PROCEDURE Main;
	CallMe(1, , 3);
:ENDPROC;
```

### Flags

```ssl
:PROCEDURE Main;
	:DECLARE x;
	x := DoProc("Helper", {1, , 3});
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
	CallMe(1, , );
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
accidentally deleted argument), hence the opt-in info tier per the
issue's correction comment (which asked for a configurable,
default-off rule — the tier is that mechanism). It stays separate from
`diag.trailing_skip_commas` rather than merging: the two transcribe
different clauses of the schema's `lints.parameter_skipping_style`
(`rule` and `trailing_params`) and have different fixes (close the gap vs.
delete the run); they meet only on a spaced trailing run, which the
trailing rule owns. Promote via
`ssl.diagnostics.rules: {"spaced_skip_commas": "warn"}` to restore
the originally proposed warning severity.
