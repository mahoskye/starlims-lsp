---
id: diag.prefer_exitcase
title: CASE block without EXITCASE
kind: diagnostic
status: active
authority: style_only
schema_ref: lints.coding_standards.prefer_exitcase
default_severity: warning
config:
  - ssl.diagnostics.rules
severity_overridable: true
suppressible: true
tests:
  - internal/providers/providers_test.go
history:
  - date: 2026-01-14
    ref: "commit 567b287"
    note: >-
      Introduced (checkMissingExitCase) with the first batch of
      SSL-specific rule checks.
  - date: 2026-04-30
    ref: "PR #3 (v0.4.0)"
    note: Stable diagnostic code assigned when Code was populated on every diagnostic.
issues: []
---

## Behavior

Flags each `:CASE` and `:OTHERWISE` clause inside a
`:BEGINCASE`...`:ENDCASE` block that does not contain an `:EXITCASE`
before the next `:CASE`/`:OTHERWISE`/`:ENDCASE`, at the clause keyword's
range — one diagnostic per unterminated clause. Nested `:BEGINCASE`
blocks are tracked on a stack, so an `:EXITCASE` inside an inner block
does not satisfy the enclosing clause. `:OTHERWISE` is held to the same
requirement as `:CASE`, even though nothing follows it, because the
style guide says to always include `:EXITCASE`.

It must NOT flag:

- clauses that end with `:EXITCASE` (anywhere in the clause body,
  including inside nested `:IF`s at the same BEGINCASE depth);
- code outside any `:BEGINCASE` block — stray `:CASE` tokens without a
  `:BEGINCASE` are ignored by this rule.

There is no exemption for intentional multi-match fall-through; authors
who rely on it must suppress the diagnostic.

## Examples

### Flags

```ssl
:BEGINCASE;
:CASE nVal == 1;
    x := 1;
:CASE nVal == 2;
    x := 2;
    :EXITCASE;
:ENDCASE;
```

### Does not flag

```ssl
:BEGINCASE;
:CASE nVal == 1;
    x := 1;
    :EXITCASE;
:OTHERWISE;
    x := 0;
    :EXITCASE;
:ENDCASE;
```

## Rationale

The schema lists `prefer_exitcase` under `lints.coding_standards` with
`severity: warning` (style_only): ":EXITCASE is syntactically optional,
but without it later :CASE expressions are still evaluated and
additional matching bodies may execute... Always include :EXITCASE
unless multi-match behavior is intentional." Because a missing
`:EXITCASE` silently changes control flow (multiple case bodies can
run), this earns a warning rather than a hint, while staying overridable
for codebases that use multi-match deliberately. One of the oldest rules
in the pipeline (history, 2026-01-14).
