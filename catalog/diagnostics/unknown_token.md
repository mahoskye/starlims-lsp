---
id: diag.unknown_token
title: Unknown token
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
  - date: 2026-01-10
    ref: "commit 442fa69 (initial commit)"
    note: checkTokenErrors reports every lexer token classified TokenUnknown.
  - date: 2026-02-02
    ref: "commit 7261172"
    note: >-
      False-positive guard: `.identifier` unknown tokens preceded by an
      identifier are skipped and handed to the new dot_property_access rule,
      which explains the actual mistake (dot instead of colon).
  - date: 2026-04-30
    ref: "PR #3 (v0.4.0, commit d744511)"
    note: Stable diagnostic code assigned; behavior unchanged.
issues: []
---

## Behavior

Flags every token the lexer classified as unknown — a character or sequence
that fits no SSL token class (e.g. a backtick). The range covers the token
and the message quotes its text.

It must NOT flag:

- an unknown token of the shape `.identifier` whose previous significant
  token is an identifier — that is dot-style property access
  (`oObj.Value`), which has its own targeted rule (`dot_property_access`)
  and would otherwise be double-reported;
- dotted boolean literals (`.T.`, `.F.`) and dotted logical operators
  (`.AND.`, `.OR.`, `.NOT.`) — the lexer classifies those as literals and
  operators, so they never reach this rule.

## Examples

### Flags

```ssl
nResult := `nCount`;
```

### Does not flag

```ssl
:DECLARE oObj;
nTotal := oObj.Value;
```

### Does not flag

```ssl
:DECLARE bDone;
bDone := .T. .AND. .F.;
```

## Rationale

An unlexable character is nearly always foreign syntax pasted into SSL, so
the finding is always on. It stays a warning rather than an error because
the lexer's token classes — not the runtime — are the authority, and the
one recurring legitimate shape (`.identifier` after an identifier) is
carved out to a rule that names the real fix (commit 7261172). The
Does-not-flag fences pin that carve-out and the dotted literal/operator
forms permanently.
