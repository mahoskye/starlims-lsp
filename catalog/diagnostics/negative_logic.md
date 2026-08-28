---
id: diag.negative_logic
title: Negated IF condition with an ELSE branch
kind: diagnostic
status: active
authority: advisory
schema_ref: lints.style_rules.prefer_positive_logic
default_severity: info
config:
  - ssl.diagnostics.infoDiagnostics
severity_overridable: true
suppressible: true
spec_options:
  include_info_diagnostics: true
tests:
  - internal/providers/providers_test.go
history:
  - date: 2026-03-28
    ref: "commit be7a174"
    note: >-
      Introduced in the diagnostics expansion pass, implementing the style
      guide's prefer_positive_logic lint; restricted to leading negations
      on IF blocks that actually have an ELSE.
  - date: 2026-04-30
    ref: "PR #3 (v0.4.0, commit d744511)"
    note: Stable diagnostic code assigned; behavior unchanged.
  - date: 2026-08-27
    ref: "issue #208 discussion (info-tier expansion)"
    note: >-
      Moved hint -> info in the info-tier expansion: pure readability
      preference. Info is the opt-in advisory tier
      (ssl.diagnostics.infoDiagnostics); explicit ssl.diagnostics.rules
      entries still promote or disable per rule.
issues: []
---

## Behavior

Flags an `:IF` whose condition begins with a negation operator (`.NOT.` or
`!` as the first significant token after `:IF`) when the block also has an
`:ELSE` branch at its own nesting level — the branches can simply be
swapped and the negation dropped. Hint severity, ranged on the negation
operator token. Nested `:IF`/`:ENDIF` pairs are depth-tracked so an
`:ELSE` belonging to an inner block does not count.

It must NOT flag:

- a negated `:IF` without an `:ELSE` — there is nothing to swap, and
  negation is often the natural guard form;
- conditions where the negation is not the leading token
  (`:IF bDone .AND. .NOT. bFailed;`) — inverting those changes meaning;
- `:ELSE` branches belonging to a nested `:IF` rather than the negated
  one.

## Examples

### Flags

```ssl
:IF .NOT. bReady;
	DoProc("HandleNotReady");
:ELSE;
	DoProc("HandleReady");
:ENDIF;
```

### Flags

```ssl
:IF !bReady;
	DoProc("HandleNotReady");
:ELSE;
	DoProc("HandleReady");
:ENDIF;
```

### Does not flag

```ssl
:IF .NOT. bReady;
	:RETURN .F.;
:ENDIF;
```

### Does not flag

```ssl
:IF bDone .AND. .NOT. bFailed;
	nCount := 1;
:ELSE;
	nCount := 2;
:ENDIF;
```

## Rationale

The style guide's lints set `prefer_positive_logic: true` at advisory
level: "if not-X then A else B" reads harder than "if X then B else A".
Because the transformation is only mechanical when the negation governs
the whole condition and an `:ELSE` exists, the check restricts itself to
exactly that shape (be7a174) — anything broader would second-guess
legitimate guard clauses. Hint severity matches the advisory schema level:
this is a readability suggestion, never a correctness issue. Both boundary
cases are pinned in providers_test.go (TestGetDiagnostics_NegativeLogic*).
