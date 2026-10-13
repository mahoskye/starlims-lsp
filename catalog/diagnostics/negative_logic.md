---
id: diag.negative_logic
title: Negated IF condition with an ELSE branch
kind: diagnostic
status: active
authority: style_only
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
  - internal/providers/diagnostics_audit_test.go
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
  - date: 2026-10-13
    ref: "issue #255"
    note: >-
      The negation must now govern the whole condition, read from the
      expression tree: `.NOT. bA .AND. bB` is `(.NOT. bA) .AND. bB`, and
      swapping its branches cannot remove the negation (75 of 895 reports
      on the production corpus). Authority corrected advisory ->
      style_only to mirror the style guide record (prefer_positive_logic).
issues: []
---

## Behavior

Flags an `:IF` whose whole condition is a negation (`.NOT. x` or `!x`,
optionally parenthesized) when the block also has an `:ELSE` branch at
its own nesting level — the branches can simply be swapped and the
negation dropped. Info severity, ranged on the negation operator token. Nested `:IF`/`:ENDIF` pairs are depth-tracked so an
`:ELSE` belonging to an inner block does not count.

It must NOT flag:

- a negated `:IF` without an `:ELSE` — there is nothing to swap, and
  negation is often the natural guard form;
- conditions where the negation is not the leading token
  (`:IF bDone .AND. .NOT. bFailed;`) — inverting those changes meaning;
- conditions that start with a negation it does not govern
  (`:IF .NOT. bDone .AND. bReady;` is `(.NOT. bDone) .AND. bReady`,
  because unary operators bind tighter than every binary one) — swapping
  the branches cannot remove that negation;
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

### Does not flag

```ssl
:IF .NOT. bDone .AND. bReady;
	nCount := 1;
:ELSE;
	nCount := 2;
:ENDIF;
```

### Flags

```ssl
:IF .NOT. (bDone .AND. bReady);
	nCount := 1;
:ELSE;
	nCount := 2;
:ENDIF;
```

## Rationale

The style guide's `prefer_positive_logic` record (style_only): "if not-X
then A else B" reads harder than "if X then B else A".
Because the transformation is only mechanical when the negation governs
the whole condition and an `:ELSE` exists, the check restricts itself to
exactly that shape (be7a174) — anything broader would second-guess
legitimate guard clauses; the tree check (issue #255) closes the case a
leading-token test cannot see, a leading negation that binds only the
first operand. Info severity (D13's opt-in advisory tier): this is a
readability suggestion, never a correctness issue. The fences pin each
boundary; the no-`:ELSE` case is also pinned in providers_test.go
(TestGetDiagnostics_NegativeLogic*).
