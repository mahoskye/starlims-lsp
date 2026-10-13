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
  - internal/providers/fp_triage_304_test.go
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
  - date: 2026-10-13
    ref: "issue #304"
    note: >-
      Two flow guards for false positives in shipped product code. A use
      guarded by a non-emptiness test of the same variable (`!Empty(v)`,
      `.NOT. Empty(v)`, `v != NIL`) in an enclosing :IF/:WHILE condition,
      or in an earlier `.AND.` operand of the same condition (which
      short-circuits), is not flagged. A NIL assignment inside an :IF
      branch or loop body no longer reaches the :ELSE branch, nor code
      after the block when that branch ends in :RETURN. Corpus: 5 -> 1
      finding on unmodified product code.
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

Tracking replays assignments in token order, and only direct
`var := NIL` / `var := other` statements participate. It follows block
structure in two ways only (issue #304):

- **Emptiness guards.** A use is not flagged when the same variable was
  tested non-empty — `!Empty(v)`, `.NOT. Empty(v)`, or `v != NIL` (also
  `<>`, `#`, and NIL on the left) — either in an enclosing `:IF`/`:WHILE`
  condition that is a top-level `.AND.` chain with no top-level `.OR.`,
  or in an earlier `.AND.` operand of the same condition, since `.AND.`
  short-circuits (`!Empty(v) .AND. v:Prop > 0`). A NIL assignment to the
  variable inside the guarded branch ends the guard, and an `:ELSE`
  branch is not guarded.
- **Branches.** A NIL assignment inside an `:IF` branch does not reach
  that `:IF`'s `:ELSE` branch, and one inside a branch or loop body that
  ends in a `:RETURN` at the branch's own level does not reach the code
  after the block. An `:EXITWHILE`, `:EXITFOR` or `:LOOP` inside can
  leave the branch before its `:RETURN`, so it cancels the second rule.
  `:BEGINCASE` and `:TRY` sections are not separate paths (a CASE can fall
  through; `:CATCH` runs after part of the `:TRY` body), so neither rule
  applies across them. Tracking is scoped per procedure
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
  procedure assigning NIL cannot poison its siblings (issue #207);
- a use guarded by a non-emptiness test of the variable, in an enclosing
  `:IF`/`:WHILE` condition or an earlier `.AND.` operand (issue #304);
- a use the NIL assignment cannot reach: in the `:ELSE` branch of the
  `:IF` whose other branch assigned it, or after a branch that ends in
  `:RETURN` (issue #304).

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

### Does not flag

```ssl
:PROCEDURE Release;
	:PARAMETERS bAbort;
	:DECLARE oStream;
	oStream := LimsNetConnect("mscorlib", "System.IO.MemoryStream");
	:IF bAbort;
		:IF !Empty(oStream);
			oStream:Close();
			oStream := NIL;
		:ENDIF;
		:RETURN;
	:ENDIF;
	oStream:Close();
	oStream := NIL;
	:IF !Empty(oStream);
		oStream:Dispose();
	:ENDIF;
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE Compare;
	:PARAMETERS bChanged;
	:DECLARE oPrev;
	oPrev := NIL;
	:IF bChanged .AND. !Empty(oPrev) .AND. oPrev:Name != "";
		oPrev:Reset();
	:ENDIF;
:ENDPROC;
```

### Flags

```ssl
:PROCEDURE Compare;
	:PARAMETERS bChanged;
	:DECLARE oPrev;
	oPrev := NIL;
	:IF bChanged .OR. !Empty(oPrev);
		oPrev:Reset();
	:ENDIF;
:ENDPROC;
```

## Rationale

Calling a method on NIL raises a runtime error in SSL, and `x := NIL;`
followed shortly by `x:Method()` is almost always a refactoring leftover.
Warning rather than error because the variable path is mostly lexical:
apart from the two block-structure guards it cannot see control flow, so
it deliberately errs toward silence — any later
non-NIL assignment clears the flag even if it sits in a branch that may
not execute (the fourth fence pins that unsound-but-quiet choice). The
literal path (`NIL:Member`) has no false positives and simply is a bug.
Both paths and the reassignment fence are pinned in providers_test.go
(TestGetDiagnostics_NilMethodCall*). The guards (issue #304) come from
shipped product code that tests `!Empty(v)` before a member access and
releases a resource in an early-exit branch; both are idioms the
reference teaches (`.AND.` short-circuits, so `!Empty(v) .AND. v:Prop`
never reads a member of an empty value).
