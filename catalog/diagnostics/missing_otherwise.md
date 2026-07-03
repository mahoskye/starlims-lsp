---
id: diag.missing_otherwise
title: BEGINCASE without an OTHERWISE clause
kind: diagnostic
status: active
authority: tool
schema_ref: null
default_severity: hint
severity_overridable: true
suppressible: true
tests:
  - internal/providers/providers_test.go
history:
  - date: 2026-03-28
    ref: "commit be7a174"
    note: >-
      Introduced in the diagnostics expansion pass as an advisory
      default-handling nudge; scoped to outermost blocks only
      (TestGetDiagnostics_MissingOtherwise_NestedIgnoresInner).
  - date: 2026-04-30
    ref: "PR #3 (v0.4.0, commit d744511)"
    note: Stable diagnostic code assigned; behavior unchanged.
issues: []
---

## Behavior

Flags an outermost `:BEGINCASE` block that reaches its `:ENDCASE` without
any `:OTHERWISE` clause at its own nesting level. The diagnostic is a hint
ranged on the `:BEGINCASE` keyword — omitting `:OTHERWISE` is legal and
often intentional, so this is the lowest severity in the pipeline.

Only outermost `:BEGINCASE` blocks are evaluated (a deliberate scope
limit pinned by tests): a `:BEGINCASE` nested inside another is never
itself flagged, and an `:OTHERWISE` belonging to a nested block does not
satisfy the outer block.

It must NOT flag:

- a `:BEGINCASE` block containing an `:OTHERWISE` at its own level;
- a nested `:BEGINCASE` without `:OTHERWISE`, when the outer block has
  one;
- `:IF` blocks without `:ELSE` — the style guide's
  `require_else_for_if` is false and no rule covers that.

## Examples

### Flags

```ssl
:BEGINCASE;
:CASE nStatus = 1;
	sLabel := "Open";
	:EXITCASE;
:ENDCASE;
```

### Does not flag

```ssl
:BEGINCASE;
:CASE nStatus = 1;
	sLabel := "Open";
	:EXITCASE;
:OTHERWISE;
	sLabel := "Unknown";
	:EXITCASE;
:ENDCASE;
```

### Does not flag

```ssl
:BEGINCASE;
:CASE nStatus = 1;
	:BEGINCASE;
	:CASE nSubStatus = 1;
		sLabel := "Open/A";
		:EXITCASE;
	:ENDCASE;
	:EXITCASE;
:OTHERWISE;
	sLabel := "Unknown";
	:EXITCASE;
:ENDCASE;
```

## Rationale

In SSL's `:BEGINCASE`, `:OTHERWISE` only executes when no earlier `:CASE`
body ran, making it the natural place for default handling; the style guide
documents this semantics without mandating the clause, so the tool nudges
at hint severity rather than warning (be7a174). The outermost-only scope is
intentional and test-pinned (`..._NestedIgnoresInner` asserts exactly one
hint when an inner block has `:OTHERWISE` and the outer does not): nested
case blocks are usually exhaustive dispatch helpers, and flagging every
level proved noisy relative to the rule's advisory weight.
