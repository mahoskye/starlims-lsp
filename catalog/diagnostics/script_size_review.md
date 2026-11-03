---
id: diag.script_size_review
title: Large server script review trigger
kind: diagnostic
status: active
authority: advisory
schema_ref: lints.style_rules.script_size_review
default_severity: info
config:
  - ssl.diagnostics.infoDiagnostics
  - ssl.diagnostics.maxScriptCodeLines
  - ssl.diagnostics.maxScriptProcedures
severity_overridable: true
suppressible: true
spec_options:
  include_info_diagnostics: true
  max_script_code_lines: 4
  max_script_procedures: 2
tests:
  - internal/providers/size_review_test.go
history:
  - date: 2026-10-14
    ref: "issue #344"
    note: >-
      Introduced from ssl-style-guide's script_size_review (advisory,
      info). The defaults (400 code lines, 12 local procedures) are final:
      the maintainer chose generous triggers over STARLIMS 12's base-layer
      95th/99th percentiles (198/551 code lines, 5/15 procedures). The
      style guide's ideal of about 200 lines and 0-1 procedures is
      guidance for new code and has no diagnostic.
  - date: 2026-11-03
    ref: "issue #358"
    note: >-
      Message updated to the style guide's new advice: past about 400 code
      lines, move each responsibility that stands alone into its own
      script. The old message put regions first. Thresholds and severity
      are unchanged. The size limit in bytes is
      diag.script_size_ceiling.
issues: []
---

## Behavior

Reports a server script with more than `ssl.diagnostics.maxScriptCodeLines`
code lines (default 400) or more than `ssl.diagnostics.maxScriptProcedures`
local procedures (default 12), once per file, ranged on line 1. Code lines
are counted as in `procedure_length_review`, over the whole file; local
procedures are the file's `:PROCEDURE` statements. The message gives both
counts and both thresholds and suggests moving each responsibility that
stands alone into its own script.

Info tier, like `procedure_length_review`. Class files (a file with a
`:CLASS` statement) and data-source files are not measured; the style
guide scopes the rule to server scripts. Setting a threshold to `0`
disables that trigger. The spec fences here run with the thresholds at 4
code lines and 2 procedures.

It must NOT flag:

- a script at or under both thresholds;
- a class file, however large.

## Examples

### Flags

```ssl
:DECLARE nA, nB;
nA := 1;
nB := 2;
nA := nA + nB;
:RETURN nA;
```

### Flags

Three procedures is over the threshold of 2.

```ssl
:PROCEDURE One;
:ENDPROC;
:PROCEDURE Two;
:ENDPROC;
:PROCEDURE Three;
:ENDPROC;
```

### Does not flag

```ssl
/* Short script;
:DECLARE nA;
nA := 1;
:RETURN nA;
```

### Does not flag

```ssl
:CLASS Totals;
:DECLARE nA;
:PROCEDURE One;
:ENDPROC;
:PROCEDURE Two;
:ENDPROC;
:PROCEDURE Three;
:ENDPROC;
```

## Rationale

A script past the triggers is well beyond the product's own top of normal
and likely holds more than one responsibility. STARLIMS compiles a
script as one unit, with its `:INCLUDE`d code, even when a caller needs a
single procedure, so a responsibility that stands alone costs every
caller a load until it moves into its own script. Procedures that other
scripts call belong in small scripts. Regions make a script easier to
navigate but do not change that cost (issue #358). Like
`procedure_length_review` this is an advisory review trigger in the info
tier, with configurable thresholds (issue #344). The size limit itself is
`script_size_ceiling`.
