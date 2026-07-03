---
id: diag.assignment_in_condition
title: Assignment operator used in a condition
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
  - date: 2026-02-02
    ref: "commit 7261172"
    note: Introduced with the gotcha-detection batch (gotcha #9 in gotchas.md).
  - date: 2026-04-30
    ref: "PR #3 (v0.4.0)"
    note: Stable diagnostic code assigned; rule behavior unchanged.
issues: []
---

## Behavior

Flags the assignment operator `:=` when it appears inside the condition of
an `:IF`, `:WHILE`, or `:CASE` statement — the token span between the
condition keyword and the terminating `;`. In SSL, `:=` in that position is
almost always a typo for `=` or `==` comparison, and it silently assigns
instead of comparing. The message names the enclosing keyword
(IF/WHILE/CASE) and the range covers the `:=` token itself; a condition
containing two assignments produces two diagnostics.

It must NOT flag:

- `:=` in ordinary assignment statements outside a condition;
- comparison operators (`=`, `==`) inside conditions;
- the loop-variable initializer of `:FOR i := 1 :TO n;` — `:FOR` is not a
  tracked condition keyword and its `:=` is the only legal way to write
  the loop.

## Examples

### Flags

```ssl
:PROCEDURE Demo;
:DECLARE nCount;
:IF nCount := 5;
:ENDIF;
:ENDPROC;
```

### Flags

```ssl
:PROCEDURE Demo;
:DECLARE nMode, nOut;
:BEGINCASE;
:CASE nMode := 1;
nOut := 1;
:EXITCASE;
:OTHERWISE;
nOut := 0;
:EXITCASE;
:ENDCASE;
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE Demo;
:DECLARE nCount;
:IF nCount = 5;
nCount := 6;
:ENDIF;
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE Demo;
:DECLARE i, nTotal;
nTotal := 0;
:FOR i := 1 :TO 10;
nTotal := nTotal + i;
:NEXT;
:ENDPROC;
```

## Rationale

Assignment-in-condition is a classic silent-corruption bug carried over
from C-family habits, and SSL makes it easier to hit because `=`
(comparison) and `:=` (assignment) differ by one keystroke. Severity is
warning rather than error because the construct is syntactically valid SSL
and could conceivably be intentional — the rule points at the suspicious
token and asks. Introduced with the gotcha batch (commit 7261172); the code
slug was stabilized in PR #3 (v0.4.0).
