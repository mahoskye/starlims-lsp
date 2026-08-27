---
id: diag.exitcase_after_return
title: Unreachable :EXITCASE after a branch-level :RETURN
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
  - date: 2026-08-26
    ref: "issue #190"
    note: >-
      Introduced from the runtime-verification batch: the guidance to end
      every :CASE with :EXITCASE makes the redundant :RETURN + :EXITCASE
      pair a common generated/refactored pattern.
issues: []
---

## Behavior

Flags an `:EXITCASE` that is the next statement after a `:RETURN`
statement inside a `:BEGINCASE` structure: from the `:RETURN` keyword, the
statement runs to its terminating `;` (paren/brace/bracket nesting
respected), and if the next significant token is `:EXITCASE`, that
`:EXITCASE` is unreachable — the `:RETURN` already leaves the procedure.
The range covers the `:EXITCASE` keyword. Comments between the two
statements do not break the pairing.

It must NOT flag:

- `:EXITCASE` after ordinary statements — only a directly preceding
  `:RETURN` proves unreachability;
- a `:RETURN` / `:EXITCASE` pair outside any `:BEGINCASE` structure
  (broken structure is other rules' business);
- `:EXITCASE` separated from the `:RETURN` by another statement — the
  intervening statement is the unreachable one, and flagging `:EXITCASE`
  there would mislabel the problem;
- a `:RETURN` at the end of a branch with no `:EXITCASE` following — that
  is the idiomatic minimal form.

## Examples

### Flags

```ssl
:PROCEDURE Grade;
	:PARAMETERS nScore;
	:BEGINCASE;
	:CASE nScore > 90;
		:RETURN "A";
		:EXITCASE;
	:OTHERWISE;
		:RETURN "B";
	:ENDCASE;
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE Grade;
	:PARAMETERS nScore;
	:DECLARE sGrade;
	:BEGINCASE;
	:CASE nScore > 90;
		sGrade := "A";
		:EXITCASE;
	:OTHERWISE;
		sGrade := "B";
		:EXITCASE;
	:ENDCASE;
	:RETURN sGrade;
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE Grade;
	:PARAMETERS nScore;
	:BEGINCASE;
	:CASE nScore > 90;
		:RETURN "A";
	:OTHERWISE;
		:RETURN "B";
	:ENDCASE;
:ENDPROC;
```

## Rationale

The style guidance to end every `:CASE` with `:EXITCASE` collides with
early-return branches, so generators and refactors routinely emit the
dead pair (issue #190). Hint severity because the code is correct — the
`:EXITCASE` just never runs — and the fix is a pure deletion. The rule
anchors on the `:EXITCASE` (the token to delete), not the `:RETURN`.
