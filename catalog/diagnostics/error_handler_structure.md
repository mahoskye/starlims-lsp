---
id: diag.error_handler_structure
title: Empty :ERROR handler
kind: diagnostic
status: active
authority: tool
schema_ref: null
default_severity: error
severity_overridable: true
suppressible: true
tests:
  - internal/providers/providers_test.go
history:
  - date: 2026-03-21
    ref: "commit cdbfee6"
    note: >-
      Introduced during full alignment with ssl-style-guide: an :ERROR
      handler must contain at least one statement.
  - date: 2026-04-30
    ref: "PR #3 (v0.4.0, commit d744511)"
    note: Stable diagnostic code assigned; behavior unchanged.
issues: []
---

## Behavior

Flags an `:ERROR` handler that contains no statements: after the `:ERROR`
keyword (skipping whitespace, comments, and semicolons), the next
significant token is either end-of-file or one of the scope/handler
terminators `:RESUME`, `:ENDPROC`, `:ENDTRY`, `:ENDWHILE`, `:NEXT`,
`:ENDCASE`, `:ENDINLINECODE`, `:ENDREGION`. Two emit sites (empty-at-EOF
and terminator-next) share the same error severity and message; the range
covers the `:ERROR` keyword.

It must NOT flag:

- an `:ERROR` handler with at least one statement before `:RESUME` or the
  end of the enclosing scope;
- code using `:TRY`/`:CATCH` instead of `:ERROR` (no `:ERROR` token, nothing
  to check). Note that any `:ERROR`/`:RESUME` use additionally raises the
  separate `deprecated_keyword` warning — that is not this rule.

## Examples

### Flags

```ssl
:PROCEDURE Demo;
	:ERROR;
:ENDPROC;
```

### Flags

```ssl
:PROCEDURE Demo;
	:ERROR;
	:RESUME;
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE Demo;
	:DECLARE nCode;
	:ERROR;
	nCode := 1;
	:RESUME;
:ENDPROC;
```

## Rationale

An empty `:ERROR` handler silently swallows runtime errors — the most
dangerous possible no-op — so this is an error (cdbfee6). Comments do not
count as statements: a handler containing only an explanatory comment still
swallows the error at runtime. Deprecation of the `:ERROR` construct itself
is deliberately kept in `deprecated_keyword` so that users who must maintain
legacy handlers still get structural checking here at full severity.
