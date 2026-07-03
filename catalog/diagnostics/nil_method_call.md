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
`var := other` statements participate.

It must NOT flag:

- a variable that was reassigned a non-NIL value after the NIL assignment
  and before the member access;
- variables never assigned NIL, even if they might be NIL at runtime
  (e.g. uninitialized parameters) — the rule only trusts explicit NIL
  assignments;
- comparisons or assignments involving NIL without member access
  (`:IF oRec = NIL;`, `oRec := NIL;`).

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
