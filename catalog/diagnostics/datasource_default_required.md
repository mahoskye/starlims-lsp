---
id: diag.datasource_default_required
title: Data source parameter missing its inline default
kind: diagnostic
status: active
authority: authoritative
schema_ref: module_structure.data_source_modules.lint_rules.datasource_default_required
default_severity: error
config:
  - ssl.diagnostics.rules
severity_overridable: true
suppressible: true
spec_options:
  is_data_source_file: true
tests:
  - internal/providers/providers_test.go
  - internal/providers/parameter_defaults_test.go
history:
  - date: 2026-03-30
    ref: "commit f6e78ef"
    note: >-
      Introduced with data source file support (.ds/.ds.txt): data sources
      declare parameter defaults inline in :PARAMETERS rather than via
      separate :DEFAULT statements.
  - date: 2026-04-30
    ref: "PR #3 (v0.4.0, commit d744511)"
    note: Stable diagnostic code assigned; behavior unchanged.
  - date: 2026-08-07
    ref: "issue #147, ssl-style-guide#48"
    note: >-
      Removed. The rule's premise was judged wrong: production .ds files
      appeared to use `:PARAMETERS p1;` with no inline default, so the
      rule was taken to false-flag valid files, and the style-guide schema
      was changed to `default_required: false` (ssl-style-guide#48).
      Constant, check, and tests deleted.
  - date: 2026-10-13
    ref: "issues #263, #269; ssl-style-guide#82, PR ssl-style-guide#104"
    note: >-
      Reinstated, narrowed to the script-level :PARAMETERS. On STARLIMS 12
      a data source in SSL or SQL mode whose :PARAMETERS has a name
      without an inline default fails when it runs ("No default value for
      one or more parameters."), whatever the caller passes; the reference
      (keywords/PARAMETERS.md) and the style guide (default_required: true,
      lint at error) now both say so. The 2026-08 removal is explained by
      scope, not by the runtime: the old check read every :PARAMETERS
      statement in the file, including the signatures of procedures
      defined inside a data source, which are ordinary procedure
      parameters and never take inline defaults. Those were the
      "defaultless" production cases. On the production corpus (1,608
      .ds files) the narrowed rule reports nothing: every script-level
      data-source parameter there has a default.
issues: []
---

## Behavior

Data-source-only rule (the file must be detected as a data source — URI
ending in `.ds` or `.ds.txt`; the spec fences run with
`is_data_source_file: true`). Applies in both data-source modes: in an SSL
data source to the whole file, in a SQL data source to the
`:PARAMETERS` header (after any masked header comment).

Flags each name in the script-level `:PARAMETERS` statement that is not
followed by an inline `:=` default, at error severity, ranged on the
name. A default value is read through the next top-level `,` or `;`, so a
default that contains commas inside parentheses, braces or brackets
(`aIds := {1, 2}`, `sX := Upper("a,b")`) is one entry, and identifiers
inside it are never read as parameter names. The keyword is matched in
any case: a lowercase `:parameters` is already a keyword-case error, and
the stricter reading applies to its names too.

It must NOT flag:

- a name that has an inline default;
- the `:PARAMETERS` of a `:PROCEDURE` defined inside the data source: that
  is an ordinary procedure signature, not the data source's interface;
- any `:PARAMETERS` in a file that is not a data source (there,
  `inline_default_outside_datasource` reports the opposite form).

## Examples

### Flags

```ssl
:PARAMETERS sStatus := "A", nMaxRows;
:RETURN SQLExecute("SELECT sample_id FROM sample WHERE status = ?sStatus?");
```

### Flags

```ssl
/* Orders by status */
:PARAMETERS sStatus;
SELECT order_id FROM orders WHERE status = @sStatus
```

### Flags

```ssl
:parameters sStatus;
:RETURN sStatus;
```

### Does not flag

```ssl
:PARAMETERS sStatus := "A", aIds := {1, 2}, sMode := Upper("a,b");
:RETURN SQLExecute("SELECT sample_id FROM sample WHERE status = ?sStatus?");
```

### Does not flag

```ssl
:PARAMETERS sStatus := "A";
:PROCEDURE Helper;
	:PARAMETERS sInner;
	:RETURN sInner;
:ENDPROC;
:RETURN DoProc("Helper", {sStatus});
```

## Rationale

The data source builder needs a default for each parameter it rewrites;
without one the data source fails at run time with "No default value for
one or more parameters.", even when the caller passes that parameter.
That is a hard failure, so the severity is error, matching the style
guide's `datasource_default_required` (level authoritative, severity
error). The 2026-08 removal record above is kept: its lesson is that the
false positives came from checking procedure signatures, and the rule is
now scoped so they cannot recur.
