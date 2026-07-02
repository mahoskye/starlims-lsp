---
id: diag.comment_text_after_terminator
title: Comment terminated earlier than its layout suggests
kind: diagnostic
status: active
authority: tool
schema_ref: null
default_severity: warning
severity_overridable: true
suppressible: true
tests:
  - internal/providers/comment_termination_test.go
history:
  - date: 2026-03-30
    ref: "commits 764a8de -> fe7a3aa -> 0d46f55"
    note: >-
      Tuning reversed three times in one day, settling on: same-line
      continuation warns; multi-line fires only on the bare-keyword signal.
  - date: 2026-04-XX
    ref: "issue #6, commits bccfe51/a501293/a99938d"
    note: >-
      False positive on benign comment-after-comment chains; suppressed when
      a paragraph break (blank line or standalone comment) separates the
      comment from the next code token.
  - date: 2026-06-XX
    ref: "issue #25 (open)"
    note: >-
      Dispute: the bare-keyword-only tuning misses a multi-line comment
      whose interior line ends in `;` when the orphaned lines are plain
      prose. Target semantics recorded under Known gaps; fix is a follow-up
      PR citing this entry.
issues: ["#25"]
---

## Behavior

Flags a terminated comment whose layout suggests the `;` landed earlier than
the author intended, in exactly two situations:

- **Same-line continuation** (warning): code follows the comment's `;` on
  the same line. That text is executable and easy to misread as comment.
- **Bare-keyword break-out** (error): a `/*` comment spanning multiple lines
  terminates, and the next significant token is an identifier that matches
  an SSL keyword name (e.g. `Parameters`, `Default`) — the signature of a
  doc-header comment cut in half.

This rule deliberately emits two severities: `default_severity: warning`
records the same-line path; the break-out path escalates to error because
it almost always means real code is being swallowed. A severity override
via `ssl.diagnostics.rules` remaps both paths.

It must NOT flag when:

- a paragraph break (blank line or another standalone comment) separates the
  comment from the following code — the comment ended deliberately
  (issue #6);
- the comment is a `/* region` / `/* endregion` marker, whose `;` is
  intentional;
- an ordinary terminated comment is simply followed by code on later lines.

## Examples

### Flags

```ssl
/* note; nCount := 1;
```

### Flags

```ssl
/* Documentation for this script
Parameters are described here;
Default values apply to each one
;
```

### Does not flag

```ssl
/* explains the assignment below;
nCount := 1;
```

### Does not flag

```ssl
/* first comment block;

/* second comment block;
:PROCEDURE Foo;
:ENDPROC;
```

### Does not flag

```ssl
/* region Helpers;
:PROCEDURE Helper;
:ENDPROC;
/* endregion;
```

## Rationale

Premature termination detection is inherently heuristic — the `;` is valid
syntax either way — which is why this rule churned three times in one day
(history) before settling on high-precision signals only: same-line
continuation and bare-keyword break-out. Every suppression above encodes a
real false positive that must never regress. Issue #25 shows the tuning now
has a real false negative; the target below extends the multi-line signal
without reopening the #6 false-positive class.

## Known gaps

- Issue #25: a multi-line `/*` comment whose interior line ends in `;`
  strands its remaining prose lines as code, but when that prose contains no
  bare keyword, nothing flags. Target: the multi-line signal must also fire
  when the next significant line (no paragraph break) begins with two or
  more consecutive bare identifiers that form no valid statement — the
  signature of orphaned prose. Covered by the expect=fail fence below; fix
  in a follow-up PR citing this entry.

### Flags

```ssl expect=fail
/* This header explains the module
and this line accidentally ends with one;
so these words are now parsed as code
;
```
