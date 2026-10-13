---
id: diag.equals_vs_strict_equals
title: String comparison with = is prefix matching
kind: diagnostic
status: active
authority: advisory
schema_ref: lints.type_safety.equals_vs_strict_equals
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
  - date: 2026-05-01
    ref: "PR #3 (v0.4.0)"
    note: Introduced firing on = and != with string operands (schema lint).
  - date: 2026-05-06
    ref: "vs-code-ssl-formatter #63 / commit 5820d9f"
    note: Message reworded for clarity after user confusion.
  - date: 2026-05-14
    ref: "PR #20 (v0.7.6), issue #14"
    note: >-
      REMOVED for standalone != with a string operand — != is exact
      inequality, there is no prefix-match asymmetry to warn about. The
      companion warning on = (prefix-match vs == exact) was deliberately
      kept. Any future re-broadening must revisit issue #14 first.
  - date: 2026-10-13
    ref: "issue #260"
    note: >-
      The reference verified on STARLIMS 12 corrected `#` (operators/hash.md):
      it negates the strict `==` comparison, like `!=` and `<>`, rather than
      the prefix-matching `=`, so `"Logged" # "Log"` is .T. The rule
      already left `#` and `<>` alone; a fence now pins that, alongside
      the issue #14 `!=` fence.
  - date: 2026-10-13
    ref: "issue #255"
    note: >-
      Behavior corrected to what the code does: it fires when an operand
      is a string literal, not any string-typed expression. The always-
      true `x = ""` comparison is split out as diag.equals_empty_string
      at warning.
issues: []
---

## Behavior

Flags a `=` comparison where either adjacent operand is a string
literal: in SSL, `=` on strings performs prefix matching (`"abc" = "ab"` is
true), which is rarely what the author means. The suggestion is `==` for
exact comparison. Only a literal operand is evidence here; `sA = sB`
between two string-typed names does not flag.

The sharpest case, `x = ""`, is true for every string `x`, so it is also
reported at warning by `diag.equals_empty_string`.

It must NOT flag:

- `!=` with string operands — `!=` is exact inequality (issue #14);
- `#` or `<>` with string operands — both are aliases of `!=` and negate
  the exact `==` comparison, not the prefix-matching `=` (issue #260);
- `==` comparisons — already exact;
- `=` between non-string operands;
- `=` between two operands neither of which is a string literal.

## Examples

### Flags

```ssl
:DECLARE sName;
sName := "abcdef";
:IF sName = "abc";
	nCount := 1;
:ENDIF;
```

### Does not flag

```ssl
:DECLARE sName;
sName := "abcdef";
:IF sName != "abc";
	nCount := 1;
:ENDIF;
```

### Does not flag

```ssl
:DECLARE sName;
sName := "abcdef";
:IF sName # "abc" .AND. sName <> "abc";
	nCount := 1;
:ENDIF;
```

### Does not flag

```ssl
:DECLARE sName;
sName := "abcdef";
:IF sName == "abc";
	nCount := 1;
:ENDIF;
```

## Rationale

Prefix-matching `=` is one of SSL's most surprising behaviors, but the rule
is advisory (info): `=` is sometimes intentional. The `!=` exclusion is the
permanent record of issue #14 — the rule originally fired on every `!=`
with a string operand, and the first Does-not-flag fence exists so that
regression can never return silently.
