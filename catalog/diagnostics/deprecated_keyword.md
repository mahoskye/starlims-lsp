---
id: diag.deprecated_keyword
title: Legacy keyword discouraged in new code
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
  - date: 2026-03-21
    ref: "commit cdbfee6"
    note: >-
      Introduced during full alignment with ssl-style-guide: :ERROR/:RESUME
      superseded by :TRY/:CATCH/:FINALLY, :LABEL by structured control flow.
  - date: 2026-04-30
    ref: "PR #3 (v0.4.0, commit d744511)"
    note: Stable diagnostic code assigned; behavior unchanged.
issues: []
---

## Behavior

Flags each occurrence of a legacy keyword token, with a message naming the
modern replacement:

- `:ERROR` and `:RESUME` — legacy error handling; prefer
  `:TRY` / `:CATCH` / `:FINALLY`;
- `:LABEL` (including the glued `:LABELName` form) — legacy flow control
  used with `Branch()`; prefer structured control flow.

Matching is case-insensitive on the keyword token; every occurrence flags,
so a full `:ERROR ... :RESUME` handler yields two diagnostics.

It must NOT flag:

- the modern constructs themselves (`:TRY`, `:CATCH`, `:FINALLY`);
- identifiers that merely contain these words (`nErrorCount`, a procedure
  named `ResumeJob`) — only keyword tokens are inspected;
- the words inside strings or comments.

## Examples

### Flags

```ssl
:PROCEDURE Demo;
	:ERROR;
	LogMessage("failed");
	:RESUME;
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE Demo;
	:DECLARE nErrorCount;
	:TRY;
		nErrorCount := 0;
	:CATCH;
		nErrorCount := 1;
	:ENDTRY;
:ENDPROC;
```

## Rationale

These keywords still execute, so error severity would be wrong — but the
style guide alignment (cdbfee6) treats them as legacy constructs that new
code should not add. Warning is the standard severity for
"valid-but-discouraged" in this catalog. Structural problems *inside* a
legacy handler are a separate concern owned by `error_handler_structure`.
