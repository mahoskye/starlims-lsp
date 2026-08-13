---
id: diag.global_assignment
title: Assignment to a global variable
kind: diagnostic
status: active
authority: tool
schema_ref: null
default_severity: error
severity_overridable: true
suppressible: true
config:
  - ssl.diagnostics.globals
spec_options:
  global_variables: ["gAppContext"]
tests:
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
issues: []
---

## Behavior

Always-on check: flags an identifier that names a global variable when it is
immediately followed (across whitespace only) by the `:=` assignment
operator. Globals are pre-declared by the runtime and must not be written.
The checked set is the case-insensitive union of:

- built-in predefined globals (`MYUSERNAME`);
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

## Rationale

Writing to a pre-declared global either fails at runtime or silently
corrupts shared state; both are real defects, hence error severity. The rule
started as an opt-in for user-configured globals (commit 567b287) but the
built-in sets were promoted to always-on (be7a174, d23fca8) because
assigning to `MYUSERNAME` or a status keyword is never intentional. The
`gAppContext` fence pins the configured-globals path.
