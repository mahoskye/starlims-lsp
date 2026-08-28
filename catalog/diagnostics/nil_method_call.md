---
id: diag.nil_method_call
title: Method call on NIL
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
      Introduced in the diagnostics expansion pass with both detection
      paths (NIL literal and NIL-tracked variable).
  - date: 2026-04-30
    ref: "PR #3 (v0.4.0, commit d744511)"
    note: Stable diagnostic code assigned; behavior unchanged.
  - date: 2026-08-27
    ref: "issue #207"
    note: >-
      Production-corpus FP class (1,391 hits, 7.3% of the whole run):
      qualified member assignment `Me:oClient := NIL` in a teardown
      registered the bare name and every later `Me:oClient:...` chain in
      the file flagged. Qualification now excluded on both the tracking
      and matching sides, and tracking resets per procedure.
issues: []
---

## Behavior

Flags member access on something known to be NIL, in two forms (both
warning severity, ranged on the NIL expression):

- **NIL literal**: `NIL:Member` in any casing — including when the lexer
  glues the colon onto the member as a keyword-like token (`NIL:ToString`),
  which is recognized as member access as long as the member is not a real
  SSL keyword.
- **NIL-tracked variable**: a variable whose most recent `:=` assignment
  (in source order) was the NIL literal, later used as `var:Member`. Any
  intervening non-NIL assignment to the variable clears the tracking.

Tracking is lexical, not flow-aware: assignments are replayed in token
order regardless of branches, and only direct `var := NIL` /
`var := other` statements participate. Tracking is scoped per procedure
(reset at every `:PROCEDURE`/`:ENDPROC`) and covers bare locals only: a
`:`-qualified target (`Me:oClient := NIL;`) is object state, not a
local, and registers nothing; a `:`-qualified occurrence of a tracked
name (`Me:oClient:Send(...)`) is a member in a chain, not the local,
and never matches (issue #207).

It must NOT flag:

- a variable that was reassigned a non-NIL value after the NIL assignment
  and before the member access;
- variables never assigned NIL, even if they might be NIL at runtime
  (e.g. uninitialized parameters) — the rule only trusts explicit NIL
  assignments;
- comparisons or assignments involving NIL without member access
  (`:IF oRec = NIL;`, `oRec := NIL;`);
- member calls whose receiver is `:`-qualified even when the member name
  matches a NIL-tracked local (`Me:oClient:Send()` after a bare
  `oClient := NIL;`) — the member and the local are different storage;
- uses in a different procedure than the NIL assignment — a teardown
  procedure assigning NIL cannot poison its siblings (issue #207).

## Examples

### Flags

```ssl
sText := NIL:ToString();
```

### Flags

```ssl
:DECLARE oRecord;
oRecord := NIL;
oRecord:Refresh();
```

### Does not flag

```ssl
:DECLARE oRecord;
oRecord := NIL;
oRecord := CreateUdObject("SampleRecord");
oRecord:Refresh();
```

### Does not flag

```ssl
:DECLARE oRecord;
oRecord := NIL;
:IF oRecord = NIL;
	oRecord := CreateUdObject("SampleRecord");
:ENDIF;
oRecord:Refresh();
```

### Does not flag

```ssl
:CLASS Service;
:DECLARE oClient;

:PROCEDURE Cleanup;
	Me:oClient := NIL;
:ENDPROC;

:PROCEDURE DoWork;
	Me:oClient:Send(1);
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE Teardown;
	:DECLARE oConn;
	oConn := NIL;
:ENDPROC;

:PROCEDURE Use;
	:DECLARE oConn;
	oConn := OpenConnection();
	oConn:Execute("cmd");
:ENDPROC;
```

## Rationale

Calling a method on NIL raises a runtime error in SSL, and `x := NIL;`
followed shortly by `x:Method()` is almost always a refactoring leftover.
Warning rather than error because the variable path is lexical: it cannot
see control flow, so it deliberately errs toward silence — any later
non-NIL assignment clears the flag even if it sits in a branch that may
not execute (the fourth fence pins that unsound-but-quiet choice). The
literal path (`NIL:Member`) has no false positives and simply is a bug.
Both paths and the reassignment fence are pinned in providers_test.go
(TestGetDiagnostics_NilMethodCall*).
