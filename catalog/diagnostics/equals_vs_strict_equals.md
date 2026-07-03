---
id: diag.equals_vs_strict_equals
title: String comparison with = is prefix matching
kind: diagnostic
status: active
authority: advisory
schema_ref: lints.type_safety.equals_vs_strict_equals
default_severity: info
severity_overridable: true
suppressible: true
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
issues: []
---

## Behavior

Flags a `=` comparison where either operand is a string: in SSL, `=` on
strings performs prefix matching (`"abc" = "ab"` is true), which is rarely
what the author means. The suggestion is `==` for exact comparison.

It must NOT flag:

- `!=` with string operands — `!=` is exact inequality (issue #14);
- `==` comparisons — already exact;
- `=` between non-string operands.

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
