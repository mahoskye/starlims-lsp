---
id: diag.unknown_token
title: Unknown token
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
  - date: 2026-10-13
    ref: "issue #291 (catalog audit #255)"
    note: >-
      Raised to error. A character no SSL token class accepts cannot
      compile (the grammar's Identifier and Letter productions are ASCII),
      and unexpected_token defers to this rule, so as a warning it let
      unlexable code validate as `valid: true`. Production cost: 70
      findings in 7 of 6,228 files, all genuinely broken code. Breaking
      for --validate consumers. In the same audit the dot-access carve-out
      widened to a receiver ending in `)` or `]` (`oObj:Get().Value`,
      `aRows[1].Value`), which dot_property_access now names, and a
      leading UTF-8 byte-order mark is no longer read as an unknown
      character.
  - date: 2026-10-14
    ref: "issue #343"
    note: >-
      Defers inside a statement of comment text stranded by a `;` (a
      banner's `..` leaders, say): comment_text_after_terminator reports
      the cause once, at error severity.
issues: ["#291"]
---

## Behavior

Flags every token the lexer classified as unknown — a character or sequence
that fits no SSL token class (e.g. a backtick, `?`, `\`, or a non-ASCII
letter). The range covers the token and the message quotes its text.
Error severity: such code cannot compile.

It must NOT flag:

- an unknown token of the shape `.identifier` whose previous significant
  token ends a receiver — an identifier, a call's `)`, or a subscript's
  `]` — which is dot-style property access (`oObj.Value`,
  `oObj:Get().Value`, `aRows[1].Value`) with its own targeted rule
  (`dot_property_access`) and would otherwise be double-reported. The
  same guard covers qualified names (`:INHERIT MyCat.BaseClass;`), whose
  dotted segments follow an identifier;
- a UTF-8 byte-order mark at the start of the file — an encoding marker,
  which the lexer keeps as zero-width whitespace;
- dotted boolean literals (`.T.`, `.F.`) and dotted logical operators
  (`.AND.`, `.OR.`, `.NOT.`) — the lexer classifies those as literals and
  operators, so they never reach this rule;
- a token inside comment text stranded as code by a `;`, which
  `comment_text_after_terminator` reports once at that `;` (issue #343).

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

### Does not flag

```ssl
:DECLARE oObj, aRows, nTotal;
nTotal := oObj:Get().Value;
nTotal := aRows[1].Value;
```

### Does not flag

```ssl
:CLASS Widget;
:INHERIT MyCat.BaseClass;
```

## Rationale

An unlexable character is nearly always foreign syntax pasted into SSL, so
the finding is always on. It is an error because no grammar production
accepts the character — the code cannot compile — and because
`unexpected_token` defers to this rule for exactly that token: as a
warning it let a file that cannot compile validate as `valid: true`
(issue #291). Over 6,228 production files it fires 70 times in 7 files,
every one broken code. The one recurring dotted shape (`.identifier` after
a receiver) is carved out to a rule that names the real fix (commit
7261172; widened to `)`/`]` receivers in issue #255). The Does-not-flag
fences pin that carve-out, qualified names, and the dotted literal and
operator forms permanently.
