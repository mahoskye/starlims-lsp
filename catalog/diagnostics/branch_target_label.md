---
id: diag.branch_target_label
title: Branch() target string without the LABEL token
kind: diagnostic
status: active
authority: authoritative
schema_ref: lints.label_behavior.token_text
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
  - date: 2026-10-13
    ref: "issue #255"
    note: >-
      The LABEL prefix is now case-sensitive (keywords are uppercase and
      the match is exact), and a literal target that starts with LABEL
      is checked against the label texts declared in the same procedure
      or at script level. A typo (`"LABEL SKP"`) now flags. A scope that
      pulls code in through :INCLUDE is not judged. authority corrected
      to authoritative: the style-guide record branch_target_label
      (level authoritative) names this rule in enforced_by, so the guide
      is not silent.
issues: []
---

## Behavior

Flags a `Branch(...)` call whose first argument is a single string literal
that cannot reach a label. The runtime matches a target against the
stored label text exactly — the keyword included, so `:LABEL SKIP;` is
reached by `"LABEL SKIP"` and the compact `:LABELSKIP;` by `"LABELSKIP"` —
and resolves labels within the current procedure or script (reference
functions/Branch, keywords/LABEL). Two cases, both under this code and
ranged on the string literal:

- **Missing keyword.** The target does not begin with the uppercase
  `LABEL` (`"SKIP"`, `"labelskip"`). Keywords are uppercase and the match
  is exact, so no label can have that text.
- **No such label.** The target begins with `LABEL` but equals none of the
  label texts declared in the same scope: the enclosing procedure, or —
  for a call outside every procedure — the script-level code outside
  procedures. A label's text is taken from its statement (`:LABEL` through
  the terminating `;`); runs of whitespace are folded to one space on
  both sides before comparing, so spacing differences never flag.

The rule is deliberately conservative and must NOT flag when it cannot
prove the target misses:

- a target equal to a label text in its scope;
- non-literal targets — variables, concatenations, or any first argument
  that is not exactly one string-literal token;
- empty string literals;
- a scope that pulls code in with `:INCLUDE` — a label may arrive from
  the included file;
- calls to anything other than `Branch` (case-insensitive), or `Branch`
  without a call's parentheses.

## Examples

### Flags

```ssl
:PROCEDURE Demo;
Branch("SKIP");
:LABEL SKIP;
:ENDPROC;
```

### Flags

The keyword is uppercase, so a lowercase target never matches.

```ssl
:PROCEDURE Demo;
Branch("labelskip");
:LABELSKIP;
:ENDPROC;
```

### Flags

A typo of the label name, and a label that lives in another procedure.

```ssl
:PROCEDURE Demo;
Branch("LABEL SKP");
Branch("LABEL ELSEWHERE");
:LABEL SKIP;
:ENDPROC;

:PROCEDURE Other;
:LABEL ELSEWHERE;
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE Demo;
Branch("LABEL SKIP");
Branch("LABEL  SKIP");
:LABEL SKIP;
Branch("LABELRETRY");
:LABELRETRY;
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

### Does not flag

```ssl
:PROCEDURE Demo;
:INCLUDE Shared.Labels;
Branch("LABEL FROM_INCLUDE");
:ENDPROC;
```

## Rationale

`Branch("SKIP")` looks correct, runs, and silently falls through because
the runtime compares against the text including the `LABEL` keyword — a
classic silent-misbehavior trap, hence an error. Issue #255 extended the
check from the prefix convention to the label set itself: a literal
target is static and labels resolve within the procedure, so a target no
label in scope carries is provably dead, typo or not. Restricting the
check to single string literals keeps it free of false positives on
computed targets, at the cost of missing them; that trade is intentional
for legacy flow control the style guide already discourages
(`diag.deprecated_keyword` separately warns on every `:LABEL`). Introduced
in the style-guide alignment pass (commit cdbfee6); the code slug was
stabilized in PR #3 (v0.4.0).
