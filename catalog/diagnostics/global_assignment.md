---
id: diag.global_assignment
title: Assignment to a global variable
kind: diagnostic
status: active
authority: tool
schema_ref: null
default_severity: warning
severity_overridable: true
suppressible: true
config:
  - ssl.diagnostics.globals
spec_options:
  global_variables: ["gAppContext"]
tests:
  - internal/providers/predefined_globals_test.go
  - internal/providers/providers_test.go
history:
  - date: 2026-01-14
    ref: "commit 567b287"
    note: Introduced for user-configured globals via server settings.
  - date: 2026-03-28
    ref: "commit be7a174"
    note: >-
      Built-in predefined globals (MYUSERNAME) always checked, independent
      of configuration.
  - date: 2026-04-03
    ref: "commit d23fca8"
    note: STARLIMS status keywords (Pending, Approved, ...) added to the always-checked set.
  - date: 2026-04-30
    ref: "PR #3 (v0.4.0), commit d744511"
    note: Stable code global_assignment assigned.
  - date: 2026-08-12
    ref: "issue #169"
    note: >-
      In-file declarations now suppress the check: the token-stream match
      ignored :DECLARE/:PARAMETERS/:PUBLIC, so a declared local that
      case-insensitively collides with a status keyword (loop variable iS
      vs IS) flagged, and a system-init script assigning the :PUBLIC
      global it just created flagged — the read-only premise holds for
      consumers, not the declarer.
  - date: 2026-10-13
    ref: "issue #270"
    note: >-
      The predefined-globals list grew to the reference's
      base-installation set for undeclared_variable, but the always-checked
      set here stays MYUSERNAME. Nothing makes a predefined global
      read-only, and the application's own login and session-start
      scripts assign the user and site globals; widening this error to
      the whole list added 57 errors on the corpus, mostly in that
      shipped code.
  - date: 2026-10-13
    ref: "issue #260"
    note: >-
      Severity error -> warning. The reference says nothing makes a
      predefined global read-only: the assignment succeeds and changes
      the value for every routine that reads it. A likely bug, not a
      failure; severity encodes the kind of finding.
issues: []
---

## Behavior

Always-on check: flags an identifier that names a global variable when it is
immediately followed (across whitespace only) by the `:=` assignment
operator. Nothing makes a predefined global read-only: the assignment
succeeds and every routine that reads the global afterwards sees the new
value, which is almost never intended.
The checked set is the case-insensitive union of:

- the built-in predefined global `MYUSERNAME` (only that one: the other
  base-installation globals, such as `STARLIMSDEPT` or `CRLF`, are
  written by the application's own session-start code, so they are not
  checked unless listed in `ssl.diagnostics.globals`);
- STARLIMS status keywords (`Pending`, `Approved`, `Rejected`, ... — each a
  read-only constant whose value is its own name);
- names configured in `ssl.diagnostics.globals` (surfaced to the spec runner
  via `spec_options.global_variables`).

It must NOT flag:

- reads of a global (`sUser := MYUSERNAME;`);
- assignment to any identifier not in the set — ordinary locals, even
  undeclared ones (that is `undeclared_variable`'s job);
- comparisons (`==`, `=`) against a global — only `:=` triggers;
- assignment to a name declared in this file via `:DECLARE`,
  `:PARAMETERS`, or `:PUBLIC` (issue #169): a declared local colliding
  with a status keyword is the author's own variable (`iS` vs `IS`), and
  a file declaring `:PUBLIC MYUSERNAME;` is the initializer that creates
  the global — read-only applies to consumers, not the declarer (the
  `:PUBLIC` line still gets its own `limit_public_vars` warning).

## Examples

### Flags

```ssl
MYUSERNAME := "someone";
```

### Flags

```ssl
gAppContext := "override";
```

### Does not flag

```ssl
:DECLARE sUser;
sUser := MYUSERNAME;
```

### Does not flag

```ssl
:DECLARE sStatus, bMatch;
bMatch := sStatus == Pending;
```

### Does not flag

```ssl
:DECLARE iS, aSeg;
aSeg := {1,2};
:FOR iS := 1 :TO Len(aSeg);
:NEXT;
```

### Does not flag

```ssl
:PUBLIC MYUSERNAME;
MYUSERNAME := "system";
```

### Does not flag

```ssl
:PROCEDURE SetSite;
	:PARAMETERS sDept;
	STARLIMSDEPT := sDept;
:ENDPROC;
```

## Rationale

Writing to a pre-declared global succeeds and silently changes shared
state for every routine that reads it afterwards (reference
guides/predefined-globals.md). That is a likely bug, not a compile or
run-time failure, so the severity is warning (it was error until
2026-10-13, when the "fails at runtime" premise was checked against the
reference). The rule
started as an opt-in for user-configured globals (commit 567b287) but the
built-in sets were promoted to always-on (be7a174, d23fca8) because
assigning to `MYUSERNAME` or a status keyword is never intentional. The
`gAppContext` fence pins the configured-globals path.
