---
id: diag.comment_termination
title: Comment missing its semicolon terminator
kind: diagnostic
status: active
authority: authoritative
schema_ref: null
default_severity: error
severity_overridable: true
suppressible: true
tests:
  - internal/providers/comment_termination_test.go
history:
  - date: 2026-03-30
    ref: "commits 764a8de -> fe7a3aa -> 0d46f55"
    note: >-
      Tuning reversed three times in one day: detect early-terminated
      multi-line comments -> downgrade same-line termination to warning ->
      tighten to fire only on a bare-keyword signal. Most-churned rule in
      the codebase. The early-termination heuristics now live in
      diag.comment_text_after_terminator; this code covers only the
      missing-terminator case.
  - date: 2026-04-XX
    ref: "issue #6"
    note: >-
      False positive: fired on a comment-only line following a terminated
      comment; fixed by the paragraph-break suppression (see
      diag.comment_text_after_terminator).
issues: []
---

## Behavior

Flags a comment that never terminates: SSL comments open with `/*` and end
at the first `;`, so a comment token that reaches end-of-file without a `;`
is a structural error — everything after the `/*` is swallowed as comment
text. Terminated comments never flag here, whatever their content;
premature-termination suspicion is diag.comment_text_after_terminator's
job.

## Examples

### Flags

```ssl
/* this comment never terminates and no semicolon follows anywhere
```

Note the comment swallows everything to end-of-file: if any `;` appears in
the swallowed text — even inside code — the comment terminates there and
this rule does not fire (the damage surfaces as other diagnostics instead).

### Does not flag

```ssl
/* properly terminated comment;
:PROCEDURE Foo;
:ENDPROC;
```

### Does not flag

```ssl
/* a multi-line comment
   is fine as long as it
   eventually terminates
;
:PROCEDURE Foo;
:ENDPROC;
```

## Rationale

An unterminated comment silently comments out the rest of the file — one of
the most destructive SSL mistakes — so this is an error, not a warning. The
2026-03-30 churn (history) came from this rule and the early-termination
heuristics sharing one identity; splitting the missing-terminator case (this
rule, structural, always correct) from the terminated-too-early heuristics
(diag.comment_text_after_terminator, probabilistic) is what let the severity
stay at error here.
