---
id: diag.procedure_length_review
title: Long procedure review trigger
kind: diagnostic
status: active
authority: advisory
schema_ref: lints.style_rules.procedure_length_review
default_severity: info
config:
  - ssl.diagnostics.infoDiagnostics
  - ssl.diagnostics.maxProcedureCodeLines
severity_overridable: true
suppressible: true
spec_options:
  include_info_diagnostics: true
  max_procedure_code_lines: 3
tests:
  - internal/providers/size_review_test.go
history:
  - date: 2026-10-14
    ref: "issue #344"
    note: >-
      Introduced from ssl-style-guide's procedure_length_review (advisory,
      info): a prompt to look, not a limit. The default threshold of 80
      code lines is final; STARLIMS 12's base layer puts procedures' 95th
      percentile at 83 code lines and class methods' at 61, identical on
      two systems. The threshold is a setting, as with max_block_depth.
issues: []
---

## Behavior

Reports a procedure or class method with more than
`ssl.diagnostics.maxProcedureCodeLines` code lines (default 80). A code
line is a line that is not blank and not comment-only, where a comment
runs from `/*` to its first `;` and may span several lines; a line inside
a multi-line string or region body counts. The count runs from the
`:PROCEDURE` line through the `:ENDPROC` line; a `:PROCEDURE` with no
`:ENDPROC` ends where the next one starts, or at end of file. The range
covers the `:PROCEDURE` keyword and the procedure name, and the message
gives the count and the threshold.

Info tier: hidden unless `ssl.diagnostics.infoDiagnostics` (or
`--validate --info`) is on or the rule is listed in
`ssl.diagnostics.rules`. Data-source files are not measured. Setting the
threshold to `0` disables the rule. The spec fences here run with the
threshold at 3.

It must NOT flag:

- a procedure at or under the threshold;
- blank lines and comment-only lines, which do not count toward it.

## Examples

### Flags

```ssl
:PROCEDURE Totals;
	:DECLARE nA, nB;
	nA := 1;
	nB := 2;
	:RETURN nA + nB;
:ENDPROC;
```

### Does not flag

Comments and blank lines are not code lines, so this procedure has 3.

```ssl
:PROCEDURE Totals;

	/* Sum the two
	parts, then return them;
	:RETURN 1 + 2;
:ENDPROC;
```

## Rationale

Length alone is not a defect, but a long procedure often does several
things, and each is easier to read, test, and change as its own
procedure. The style guide makes this an advisory review trigger, so it
sits in the info tier with `max_block_depth` and `max_params_warning`; the
threshold is a setting because it is a team's call, and its default is
the style guide's, measured on STARLIMS 12's own base layer (issue #344).
