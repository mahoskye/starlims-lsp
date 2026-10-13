---
id: diag.error_handler_structure
title: Empty :ERROR handler
kind: diagnostic
status: active
authority: authoritative
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
  - date: 2026-10-13
    ref: "issue #255"
    note: >-
      Added the placement checks from reference keywords/ERROR and
      RESUME: an `:ERROR;` with no executable statement before it in its
      procedure protects nothing, and a `:RESUME;` in a procedure
      without `:ERROR;` does not compile. Any block boundary after
      :ERROR now counts as an empty handler. The Does-not-flag fence no
      longer models a handler placed first. Zero new findings on the
      private corpus. authority corrected to authoritative: the style-
      guide record error_handler_last_section (level authoritative)
      names this rule in enforced_by, so the guide is not silent.
issues: []
---

## Behavior

Checks the shape of the legacy `:ERROR` handler (reference keywords/ERROR
and RESUME). Three findings share this code, all at error severity:

- **Empty handler.** After an `:ERROR` keyword (skipping whitespace,
  comments, and semicolons) the next significant token is end-of-file or
  a block boundary — `:RESUME`, `:ENDPROC`, `:ENDTRY`, `:ENDWHILE`,
  `:NEXT`, `:ENDCASE`, `:ENDINLINECODE`, `:ENDREGION`, `:ENDIF`, `:ELSE`,
  `:CASE`, `:OTHERWISE`, `:CATCH`, or `:FINALLY`. The handler body must
  contain at least one statement. The range covers `:ERROR`.
- **Handler placed first.** An `:ERROR;` with no executable statement
  before it in its procedure (or, outside procedures, at script level).
  `:ERROR` protects only the statements before it, so placed first it
  protects nothing and the code after it never runs on the success path.
  `:PARAMETERS`, `:DEFAULT`, `:DECLARE`, `:PUBLIC`, `:INCLUDE`, and
  comments do not count as protected statements. The range covers
  `:ERROR`.
- **`:RESUME` without a handler.** A `:RESUME;` in a procedure (or the
  script level) that has no `:ERROR;` does not compile. The range covers
  `:RESUME`.

Only the marker-statement forms `:ERROR;` and `:RESUME;` count for the
placement findings: `:ERROR` also appears in expression position
(`LimsString(:ERROR)`).

It must NOT flag:

- an `:ERROR` handler at the end of its procedure, after the statements
  it protects, with at least one handler statement;
- code using `:TRY`/`:CATCH` instead of `:ERROR`. Note that any
  `:ERROR`/`:RESUME` use additionally raises the separate
  `deprecated_keyword` warning — that is not this rule.

## Examples

### Flags

```ssl
:PROCEDURE Demo;
	:DECLARE nCode;
	nCode := 1;
	:ERROR;
:ENDPROC;
```

### Flags

```ssl
:PROCEDURE Demo;
	:DECLARE nCode;
	nCode := 1;
	:ERROR;
	:RESUME;
:ENDPROC;
```

### Flags

The handler is first, so it protects nothing.

```ssl
:PROCEDURE Demo;
	:DECLARE nCode;
	:ERROR;
	nCode := 1;
:ENDPROC;
```

### Flags

```ssl
:PROCEDURE Demo;
	:DECLARE nCode;
	nCode := 1;
	:RESUME;
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE Demo;
	:DECLARE nCode;
	nCode := DoProc("Load");
	:ERROR;
	nCode := -1;
	:RESUME;
:ENDPROC;
```

## Rationale

An empty `:ERROR` handler silently swallows runtime errors — the most
dangerous possible no-op — so this is an error (cdbfee6). Comments do not
count as statements: a handler containing only an explanatory comment
still swallows the error at runtime. Issue #255 added the two placement
defects the reference documents (style-guide record
error_handler_last_section): a handler placed first protects nothing,
and `:RESUME` without `:ERROR` fails compilation. Neither appears in the
private corpus. Deprecation of the `:ERROR` construct itself is
deliberately kept in `deprecated_keyword` so that users who must maintain
legacy handlers still get structural checking here at full severity.
