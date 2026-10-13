---
id: diag.step_spacing
title: STEP keyword with no space before it
kind: diagnostic
status: active
authority: style_only
schema_ref: null
default_severity: warning
severity_overridable: true
suppressible: true
tests:
  - internal/providers/providers_test.go
history:
  - date: 2026-03-28
    ref: "commit be7a174"
    note: >-
      Introduced (checkStepSpacing), citing ssl_agent_instructions.md
      gotcha #16 and the schema's spacing rule before_step_keyword.
  - date: 2026-04-30
    ref: "PR #3 (v0.4.0, commit d744511)"
    note: Stable diagnostic code assigned; rule behavior unchanged.
  - date: 2026-10-13
    ref: "issue #255 (diagnostics audit, batch B)"
    note: >-
      Authority tool -> style_only to mirror the style-guide record. The
      glued form with an identifier, call, or subscript limit never reached
      this rule — it parses as member access and was reported as a generic
      unexpected_token — so that diagnostic now carries a hint naming the
      missing space, and this entry says which rule owns which shape.
issues: []
---

## Behavior

Flags a `:STEP` keyword token whose immediately preceding token is not
whitespace — the pattern `... :TO 10:STEP 2` where the limit expression
runs straight into `:STEP`. The range covers the `:STEP` token; the message
shows the corrected form (`:FOR i := 1 :TO 10 :STEP 2;`).

The check is keyed purely on the `:STEP` keyword in the token stream: it
does not verify the surrounding `:FOR` header (a `:STEP` outside a loop is
someone else's diagnostic — this rule still only inspects the spacing).

Only a limit that cannot continue into member access — a numeric literal,
as in `10:STEP` — leaves `:STEP` lexed as the keyword. With an identifier,
call, or subscript limit (`:TO nMax:STEP 2`), the glued `:STEP` parses as
a member of the limit, the header then fails to parse, and
`diag.unexpected_token` reports it at error severity with the hint
`(missing space before ':STEP'?)`. That shape is not this rule's.

It must NOT flag:

- `:STEP` with whitespace before it (space, tab, or start of line);
- a `:STEP` glued to an identifier, call, or subscript limit — reported
  as `diag.unexpected_token` (above);
- a `:STEP` that is the very first token of the file;
- the letters "STEP" inside identifiers, strings, or comments — only
  keyword tokens are inspected.

## Examples

### Flags

```ssl
:PROCEDURE Demo;
:DECLARE i, nTotal;
nTotal := 0;
:FOR i := 1 :TO 10:STEP 2;
nTotal := nTotal + i;
:NEXT;
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE Demo;
:DECLARE i, nMax;
nMax := 10;
:FOR i := 1 :TO nMax:STEP 2;
:NEXT;
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE Demo;
:DECLARE i, nTotal;
nTotal := 0;
:FOR i := 1 :TO 10 :STEP 2;
nTotal := nTotal + i;
:NEXT;
:ENDPROC;
```

## Rationale

Without a space, `10:STEP` reads as one blob and has historically confused
both readers and tooling — the style-guide record `step_spacing` (level
style_only, from the schema's `formatting.spaces.before_step_keyword:
true`) requires the space, and agent gotcha #16 records it as a
real-world failure mode. Warning severity: the construct may still parse, but the layout is
misleading enough to surface prominently rather than as a hint.
