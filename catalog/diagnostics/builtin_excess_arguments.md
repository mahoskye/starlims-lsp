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
  - internal/providers/expr_call_checks_test.go
history:
  - date: 2026-08-26
    ref: "issue #200"
    note: >-
      Introduced from the runtime-verification batch: the SSL compiler
      silently drops arguments beyond a builtin's accepted count — they
      are never evaluated and produce no warning (only IIf enforces its
      count). Max-arity data derived from the ssl-style-guide element
      inventory.
  - date: 2026-08-28
    ref: "issue #210 (runtime verification)"
    note: >-
      Runtime-confirmed (issue #210, ArityProbe.ssl run 2026-08-28):
      surplus arguments are dropped at COMPILE time — a side-effecting
      surplus expression (DoProc call) never executed — and no
      undocumented argument forms exist among the 15 corpus-contested
      builtins (At has no start-position form, Time no date form, etc.;
      SetUserPassword's 3-arg call drops its surplus too). Cross-checked
      first against the runtime-validated signatures in the archived
      documentation corpus's ssl-element-list.json validation blocks,
      which matched the inventory on all 15. The 97 production hits are
      genuine silent-drop bugs; the message's 'never evaluated' claim is
      literal.
  - date: 2026-08-28
    ref: "issue #184 (expression AST consumers)"
    note: >-
      Rebuilt on the expression AST's call-site index: the callee and the
      argument boundaries come from the tree instead of a token scan, so
      a member call is excluded by being a member call (any receiver
      shape — `aDocs[1]:Left(...)`, `GetDoc():Left(...)`) rather than by
      a preceding-token test, and each surplus argument carries its own
      range. Fixes a latent crash: a surplus run ending in skipped slots
      (`Left(s, a, b,,)`) indexed the argument list at -1 and panicked,
      which the pipeline's recovery turned into an internal_error
      diagnostic that replaced the file's real output. Corpus
      differential over 6,228 production files: identical output to the
      token implementation (97 hits before and after).
issues: []
---

## Behavior

Flags a builtin call that passes more arguments than the builtin's
published signature accepts. Calls come from the expression AST's
call-site index, so a bare `Name(...)` call is distinguished from a
member call structurally and nesting is respected by construction.
Maximum arity comes from the generated element inventory
(`GeneratedFunctionSummaries` signature strings, counting optional `[x]`
parameters) combined with the curated signature list, taking the larger
count. Functions whose signature is variadic (`...`), unparseable, or
absent are never flagged — unknown arity stays silent. Skipped arguments
(`,,`) count as arguments; a single trailing comma does not. The range
spans the surplus arguments, taken from their own subtrees; when the
first surplus slot is a skipped argument there is no expression to point
at and the call name carries the range. The message names the accepted
and surplus counts.

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

### Flags

```ssl
:PROCEDURE Main;
	:DECLARE sPrefix, sText, nA, nB;
	sPrefix := Left(sText, nA, nB,,);
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
	:DECLARE aDocs, sOut;
	sOut := aDocs[1]:Left("a", "b", "c");
	sOut := GetDoc():Left("a", "b", "c");
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
