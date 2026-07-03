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
  - date: 2026-07-02
    ref: "issue #25"
    note: >-
      Gap closed: fires (warning) when the next significant line — no
      paragraph break in between — begins with two consecutive bare
      identifiers, the signature of orphaned prose. Applies to multi-line
      comments AND to a comment whose ';' lands on its first line (the
      issue's original shape). Paragraph-break suppression (issue #6)
      still applies unchanged.
issues: ["#25"]
---

## Behavior

Flags a terminated comment whose layout suggests the `;` landed earlier than
the author intended, in exactly three situations:

- **Same-line continuation** (warning): code follows the comment's `;` on
  the same line. That text is executable and easy to misread as comment.
- **Bare-keyword break-out** (error): a `/*` comment spanning multiple lines
  terminates, and the next significant token is an identifier that matches
  an SSL keyword name (e.g. `Parameters`, `Default`) — the signature of a
  doc-header comment cut in half.
- **Orphaned-prose break-out** (warning): a terminated `/*` comment —
  whether it spans multiple lines or its `;` lands on the first line — is
  followed, with no paragraph break in between, by a significant line that
  begins with two or more consecutive bare identifiers with nothing between
  them. No valid SSL statement starts with two adjacent bare identifiers
  (assignments, calls, and keyword statements all place an operator,
  parenthesis, or keyword between/before names), so the line is comment
  prose stranded as code (issue #25).

This rule deliberately emits two severities: `default_severity: warning`
records the same-line path; the bare-keyword break-out path escalates to
error because it almost always means real code is being swallowed. The
orphaned-prose break-out stays at warning: it is the same comment-cut-in-half
failure, but keyword-less prose is a weaker signal than a keyword match, so
it takes the default severity rather than the error escalation. A severity
override via `ssl.diagnostics.rules` remaps all paths.

It must NOT flag when:

- a paragraph break (blank line or another standalone comment) separates the
  comment from the following code — the comment ended deliberately
  (issue #6); this suppression applies to the orphaned-prose signal exactly
  as it does to the bare-keyword signal;
- the comment is a `/* region` / `/* endregion` marker, whose `;` is
  intentional;
- an ordinary terminated comment is simply followed by code on later lines;
- the next line is a single bare identifier followed by valid code — one
  identifier can legitimately start a statement continued on the next line,
  so the prose signal requires the second identifier to share the first
  one's line.

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

### Flags

```ssl
/* This header explains the module
and this line accidentally ends with one;
so these words are now parsed as code
;
```

### Flags

```ssl
/* Client address: treat as a single composite object;
   if any component changes, the whole address is flagged;
```

### Does not flag

```ssl
/* explains the assignment below;
nCount := 1;
```

### Does not flag

```ssl
/* a multi-line note
that spans two lines;
nCount := 1;
DoProc("X");
```

### Does not flag

```ssl
/* a multi-line note
that spans two lines;
orphan
nCount := 1;
```

### Does not flag

```ssl
/* a multi-line note
that spans two lines;

these words follow a paragraph break
;
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
(history) before settling on high-precision signals only. Every suppression
above encodes a real false positive that must never regress. Issue #25
showed the bare-keyword-only tuning had a real false negative on keyword-less
prose; the orphaned-prose signal (2026-07-02, history) extends the multi-line
detection to two-adjacent-bare-identifiers, a pattern no valid SSL statement
can produce, while sitting behind the same paragraph-break suppression so the
#6 false-positive class stays closed. The two-identifier minimum (on one
line) keeps single stranded words from flagging: precision over recall, per
this rule's history.
