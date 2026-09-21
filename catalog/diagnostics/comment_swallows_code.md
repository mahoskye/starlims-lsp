---
id: diag.comment_swallows_code
title: Comment runs past its '*/' and swallows the next statement
kind: diagnostic
status: active
authority: authoritative
schema_ref: comments
default_severity: warning
severity_overridable: true
suppressible: true
tests:
  - internal/providers/diagnostics_comment_test.go
history:
  - date: 2026-09-21
    ref: "issue #249"
    note: >-
      Added. c_style_comment_closer covered only the inert `*/;` form;
      the form that actually hides code drew nothing.
issues:
  - 249
---

## Behavior

An SSL comment ends at the first `;` — never at `*/`. The grammar is
`CommentStatement ::= "/*" {Character} ";"`, and the style guide states
it outright: comments "end at the first semicolon `;`, rather than using
traditional comment delimiters like `*/`".

So a comment closed C-style does not end where its author thinks. It
runs on to the next `;` and takes whatever lies between with it. That
code is inside the comment and never executes, while still reading as
live code to anyone looking at the file.

Fires at warning severity when a comment token contains `*/` followed by
a line that looks like a statement — an assignment, a colon keyword, or
a call. The message names the swallowed fragment.

It must NOT flag:

- `/* ... */;` — the `;` immediately after the `*/` really does end the
  comment, so nothing is hidden. That inert-but-stylistic form stays
  with `diag.c_style_comment_closer` at info severity;
- a `*/` followed only by banner decoration (`*`, `=`, `-`, `_`, `+`,
  `#`, `~`) before the terminator;
- a comment correctly ended with `;`;
- data-source documents in SQL mode, where `/* ... */` is a genuine SQL
  comment and these rules do not apply.

## Examples

### Flags

```ssl
:PROCEDURE Demo;
:DECLARE nA, nB;
/* banner
*/
nA := 1;
nB := nA;
:ENDPROC;
```

`nA := 1;` is inside the comment. `nB` reads an unassigned `nA`.

### Does not flag

```ssl
:PROCEDURE Demo;
:DECLARE nA;
/* banner
*/;
nA := 1;
:ENDPROC;
```

## Rationale

This is the dangerous half of the C-style comment confusion, and it was
the half nothing reported. A swallowed `:PROCEDURE` surfaced only as a
puzzling `unmatched_block_end` pointing at a later `:ENDPROC`; a
swallowed `:DECLARE` or assignment surfaced as nothing at all, and the
file validated clean while behaving differently than it reads.

Severity is warning rather than error because the construct is valid
SSL — the author is allowed to write a comment that spans those lines.
It matches `diag.comment_text_after_terminator`, the mirror case where a
`;` inside comment text ends a comment early and turns the remainder
into code; the two together cover both directions of the same mistake.
