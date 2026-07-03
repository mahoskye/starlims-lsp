---
id: diag.step_spacing
title: STEP keyword with no space before it
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
  - date: 2026-03-28
    ref: "commit be7a174"
    note: >-
      Introduced (checkStepSpacing), citing ssl_agent_instructions.md
      gotcha #16 and the schema's spacing rule before_step_keyword.
  - date: 2026-04-30
    ref: "PR #3 (v0.4.0, commit d744511)"
    note: Stable diagnostic code assigned; rule behavior unchanged.
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

It must NOT flag:

- `:STEP` with whitespace before it (space, tab, or start of line);
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
:DECLARE i, nTotal;
nTotal := 0;
:FOR i := 1 :TO 10 :STEP 2;
nTotal := nTotal + i;
:NEXT;
:ENDPROC;
```

## Rationale

Without a space, `10:STEP` reads as one blob and has historically confused
both readers and tooling — the style guide encodes the required space as
`formatting.spacing.before_step_keyword: true` (style-level formatting
guidance, no lints slug, hence `authority: tool` for the diagnostic
transcription) and agent gotcha #16 records it as a real-world failure
mode. Warning severity: the construct may still parse, but the layout is
misleading enough to surface prominently rather than as a hint.
