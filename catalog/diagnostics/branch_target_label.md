---
id: diag.branch_target_label
title: Branch() target string without the LABEL token
kind: diagnostic
status: active
authority: tool
schema_ref: null
default_severity: error
severity_overridable: true
suppressible: true
tests:
  - internal/providers/providers_test.go
history:
  - date: 2026-03-21
    ref: "commit cdbfee6"
    note: >-
      Introduced in the full alignment pass with ssl-style-guide
      (checkBranchTargetLabels).
  - date: 2026-04-30
    ref: "PR #3 (v0.4.0)"
    note: Stable diagnostic code assigned; rule behavior unchanged.
issues: []
---

## Behavior

Flags a `Branch(...)` call whose first argument is a single string literal
that does not begin (case-insensitively) with `LABEL`. The SSL runtime
matches Branch targets against the full label statement text, so the
string must include the label keyword itself — `"LABEL SKIP"` or the
compact `"LABELSKIP"` — and a bare `"SKIP"` never matches anything. The
range covers the offending string literal.

The rule is deliberately conservative and must NOT flag when it cannot
prove the target is wrong:

- targets that start with `LABEL` in any case (`"LABEL SKIP"`,
  `"LABELSKIP"`, `"labelskip"`);
- non-literal targets — variables, concatenations, or any first argument
  that is not exactly one string-literal token;
- empty string literals;
- calls to anything other than `Branch` (case-insensitive), or `Branch`
  without a call's parentheses.

Whether the named label actually exists is a separate check; this rule
only polices the `LABEL` prefix convention.

## Examples

### Flags

```ssl
:PROCEDURE Demo;
Branch("SKIP");
:LABEL SKIP;
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE Demo;
Branch("LABEL SKIP");
:LABEL SKIP;
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE Demo;
:DECLARE sTarget;
sTarget := "LABEL SKIP";
Branch(sTarget);
:LABEL SKIP;
:ENDPROC;
```

## Rationale

`Branch("SKIP")` looks correct, runs, and silently falls through because
the runtime compares against the text including the `LABEL` keyword — a
classic silent-misbehavior trap, hence an error. Restricting the check to
single string literals keeps it free of false positives on computed
targets, at the cost of missing them; that trade is intentional for legacy
flow control the style guide already discourages
(`diag.deprecated_keyword` separately warns on every `:LABEL`). Introduced in the style-guide alignment pass
(commit cdbfee6); the code slug was stabilized in PR #3 (v0.4.0).
