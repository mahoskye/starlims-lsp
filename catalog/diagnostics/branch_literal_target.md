---
id: diag.branch_literal_target
title: Branch() target that is not a string literal
kind: diagnostic
status: active
authority: authoritative
schema_ref: null
default_severity: error
severity_overridable: true
suppressible: true
tests:
  - internal/providers/literal_semantics_test.go
history:
  - date: 2026-10-14
    ref: "issue #321"
    note: >-
      Introduced. Verified on STARLIMS 12 (12.3.2): Branch() jumps only for
      a string-literal target. A computed target never matches a label,
      even with the exact label text, and falls through to the next
      statement with no error — at script level and inside procedures, for
      a variable or parameter, other casings, the joined form, and the
      constant expression `"LABEL " + "SKIP"`. Follow-up to #274, which
      left computed targets unchecked while their behavior was unknown.
      Matches ssl-style-guide's branch_literal_target (authoritative,
      error).
issues: []
---

## Behavior

Flags a `Branch(...)` call whose first argument is not a single string
literal: a variable, a parameter, a call, a concatenation, or any other
expression. Only a literal target is resolved against the labels; a
computed target never jumps, and the call falls through to the next
statement without an error, even when the computed text equals a label's
text exactly (reference functions/Branch). The range covers the first
argument. Error severity: the call can never do what it says, and the
failure is silent at run time.

A literal target that misses its label is `branch_target_label`'s finding;
the two rules never judge the same argument.

It must NOT flag:

- a string-literal target, whichever quote style it uses;
- a member call `oObj:Branch(...)`, which is not the built-in;
- `Branch()` with no first argument.

## Examples

### Flags

```ssl
:PROCEDURE Demo;
	:PARAMETERS sTarget;
	Branch(sTarget);
	:RETURN "fell";
	:LABEL SKIP;
	:RETURN "reached";
:ENDPROC;
```

### Flags

A constant expression is computed too.

```ssl
Branch("LABEL " + "SKIP");
:LABEL SKIP;
```

### Does not flag

```ssl
:PROCEDURE Demo;
	Branch("LABEL SKIP");
	:RETURN "fell";
	:LABEL SKIP;
	:RETURN "reached";
:ENDPROC;
```

### Does not flag

```ssl
:DECLARE oFlow, sTarget;
oFlow:Branch(sTarget);
```

## Rationale

A computed `Branch()` target looks like a jump and silently is not one, so
it is an error by the same reasoning as `branch_target_label`: code that
compiles, runs, and quietly skips the flow it describes. The behavior was
verified on STARLIMS 12 before the rule was written (issue #321). Where the
target depends on run-time data, the fix is to choose among literal
`Branch()` calls with `:IF` or `:BEGINCASE`, which the message says.
