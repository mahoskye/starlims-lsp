---
id: diag.raiseerror_in_catch
title: RaiseError called inside :CATCH
kind: diagnostic
status: active
authority: style_only
schema_ref: error_handling.raise_error_doctrine
default_severity: warning
severity_overridable: true
suppressible: true
tests:
  - internal/providers/providers_test.go
history:
  - date: 2026-08-07
    ref: "issue #142"
    note: >-
      Introduced from the RaiseError placement doctrine adopted in
      ssl-style-guide#36 (schema error_handling.raise_error_doctrine):
      never call RaiseError inside :CATCH — the error handler must not
      become the thing that crashes.
issues: []
---

## Behavior

Flags a `RaiseError(` call whose nearest enclosing `:TRY` section is a
`:CATCH` block — i.e. the call sits between `:CATCH` and the matching
`:FINALLY`/`:ENDTRY`, not inside some deeper `:TRY` body nested within
that handler. The range covers the `RaiseError` identifier. Detection is
lexical: an identifier `RaiseError` (case-insensitive) followed by `(`.

It must NOT flag:

- `RaiseError` inside a `:TRY` body — the doctrine-blessed placement;
- `RaiseError` inside `:FINALLY`;
- `RaiseError` in ordinary code outside any `:TRY` structure (raise-only
  `/*@private;` helpers are legitimate);
- `RaiseError` inside the `:TRY` body of a nested `:TRY`/`:CATCH` that
  itself sits inside an outer `:CATCH` — the nested handler catches it;
- the word RaiseError in comments or strings, or without a call.

## Examples

### Flags

```ssl
:PROCEDURE Demo;
	:TRY;
		DoWork();
	:CATCH;
		RaiseError("failed");
	:ENDTRY;
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE Demo;
	:TRY;
		RaiseError("failed");
	:CATCH;
		LogError(GetLastSSLError());
	:ENDTRY;
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE Demo;
	RaiseError("raise-only helper");
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE Demo;
	:TRY;
		DoWork();
	:CATCH;
		:TRY;
			RaiseError("nested, caught below");
		:CATCH;
			LogError(GetLastSSLError());
		:ENDTRY;
	:ENDTRY;
:ENDPROC;
```

### Flags

```ssl
:PROCEDURE Demo;
	:TRY;
		DoWork();
	:CATCH;
		:TRY;
			DoCleanup();
		:CATCH;
			RaiseError("handler of the nested handler raises");
		:ENDTRY;
	:ENDTRY;
:ENDPROC;
```

## Rationale

The doctrine (ssl-style-guide#36, schema `error_handling.raise_error_doctrine`)
is explicit: the error handler must not become the thing that crashes — a
raise from `:CATCH` escapes the very structure that was supposed to contain
the failure, and if nothing above catches it the invocation surfaces to the
end user as a server error. Warning rather than error because the schema
carries this as doctrine (guidance), not a compile error; `severity_overridable`
lets teams promote it. Sibling doctrine items ("raise only inside `:TRY`",
"ClearLastSSLError after handling") were considered and rejected as
diagnostics — too noisy / unknowable cross-file (issue #142) — and live in
hover best-practices via the element metadata instead.
