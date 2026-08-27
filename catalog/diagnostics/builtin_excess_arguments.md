---
id: diag.builtin_excess_arguments
title: Surplus builtin arguments silently dropped at compile time
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
  - date: 2026-08-26
    ref: "issue #200"
    note: >-
      Introduced from the runtime-verification batch: the SSL compiler
      silently drops arguments beyond a builtin's accepted count — they
      are never evaluated and produce no warning (only IIf enforces its
      count). Max-arity data derived from the ssl-style-guide element
      inventory.
issues: []
---

## Behavior

Flags a builtin call that passes more arguments than the builtin's
published signature accepts. Maximum arity comes from the generated
element inventory (`GeneratedFunctionSummaries` signature strings,
counting optional `[x]` parameters) combined with the curated signature
list, taking the larger count. Functions whose signature is variadic
(`...`), unparseable, or absent are never flagged — unknown arity stays
silent. Arguments are counted top-level (nesting respected); skipped
arguments (`,,`) count as arguments. The range spans the surplus
arguments; the message names the accepted and surplus counts.

It must NOT flag:

- calls at or under the accepted count, including with optional
  arguments present;
- variadic builtins (`Eval(fn, a, b, c, d, e)`);
- `:`-qualified method calls (`oDoc:Left(...)`) — that is the object's
  method, not the builtin;
- unknown identifiers — not in the inventory, no arity to enforce
  (`direct_procedure_call` owns that territory);
- trailing skip-commas alone (`Left(s, 10,)`) — the trailing comma adds
  no argument (and `trailing_skip_commas` already covers it).

## Examples

### Flags

```ssl
:PROCEDURE Main;
	:DECLARE sPrefix, sText, nExtra;
	sPrefix := Left(sText, 10, nExtra);
:ENDPROC;
```

### Flags

```ssl
:PROCEDURE Main;
	:DECLARE sNow;
	sNow := LimsTime(Today());
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE Main;
	:DECLARE sPrefix, sText;
	sPrefix := Left(sText, 10);
	sPrefix := AllTrim(Upper(Left(sText, 10)));
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE Main;
	:DECLARE oDoc, sOut;
	sOut := oDoc:Left("custom", "method", "args");
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE Main;
	:DECLARE fnAdd, nSum;
	fnAdd := {|a, b, c, d, e| a + b + c + d + e};
	nSum := Eval(fnAdd, 1, 2, 3, 4, 5);
:ENDPROC;
```

## Rationale

Surplus arguments are never evaluated and produce no compile-time signal
(issue #200), so wrong arity survives indefinitely — the corpus scan for
this rule surfaced 31 standing examples in stock and documentation code
(`GetUserData("NAME")`, `SetUserPassword(u, p, .F.)`,
`LimsTime(Now())` — each silently ignoring an argument the author
believed did something). Warning severity: the call runs, but an ignored
argument almost always encodes a wrong belief about the call. The rule
is exactly as good as the element inventory's arity data; a signature
found to under-report optional parameters is an upstream inventory fix
(ssl-style-guide), which propagates here through the generated data.
