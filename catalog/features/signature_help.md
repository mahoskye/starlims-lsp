---
id: feature.signature_help
title: Signature help
kind: feature
status: draft
authority: tool
schema_ref: null
config:
  - ssl.intellisense.signatureHelp.autoTrigger
tests:
  - internal/providers/providers_test.go
  - internal/providers/element_reference_test.go
history:
  - date: 2026-05-01
    ref: "v0.3.0"
    note: Constructor signature help added for built-in class instantiation
      (`ClassName{...}`), one signature per documented constructor form.
  - date: 2026-05-06
    ref: "#10"
    note: "v0.7.4, issues #8/#9: stopped aggressive signature-help re-trigger
      that obscured the current line; no trigger characters advertised by
      default. Opt back in via ssl.intellisense.signatureHelp.autoTrigger."
issues: []
---

## Behavior

- Signature help MUST return the full signature label, Markdown description,
  and per-parameter labels/documentation for the 330 canonical built-in
  functions and the dispatch helpers `DoProc` / `ExecFunction`.
- `activeParameter` MUST reflect the argument under the cursor, counting
  top-level commas only: commas inside nested parentheses or string literals
  MUST NOT advance it.
- With the cursor inside nested calls, the innermost enclosing call's
  signature MUST be shown; after the inner call closes, the outer one.
- Inside built-in class instantiation `ClassName{...}`, constructor signature
  help MUST be shown, one signature per documented constructor form.
- Function lookup MUST be case-insensitive; optional parameters MUST be
  rendered bracketed (e.g. `[friendlyName: variant]`).
- By default the server MUST NOT advertise any trigger characters — help
  appears only on explicit invocation. When
  `ssl.intellisense.signatureHelp.autoTrigger` is `true`, `(` and `,` MUST be
  advertised as trigger characters (`,` as retrigger).
- Signature help MUST return null for: unrecognized function names, direct
  user-procedure calls (`MyProc(...)` — invalid SSL; dispatch goes through
  DoProc/ExecFunction), and positions outside any call's argument list.

## Acceptance

- A1: Given the cursor immediately after `SQLExecute(`, when signature help is invoked, then the SQLExecute signature is returned with its parameter list and `activeParameter` 0.
- A2: Given `SQLExecute(query, ` with the cursor after the comma, when signature help is invoked, then `activeParameter` is 1, and it remains 1 while typing the second argument.
- A3: Given `Upper(AllTrim(` with the cursor inside the inner call, when signature help is invoked, then the AllTrim signature is shown, not Upper's; after the inner `)` closes, Upper's is shown.
- A4: Given a same-file `:PROCEDURE Calculate;` and the cursor inside `Calculate(`, when signature help is invoked, then the response is null — direct user-procedure calls must not produce signature help.
- A5: Given `UnknownFunc(` for a name not in the inventory, when signature help is invoked, then the response is null.
- A6: Given default configuration, when the server capabilities are advertised, then no signature-help trigger characters are declared; given `ssl.intellisense.signatureHelp.autoTrigger: true`, then `(` and `,` are declared.
- A7: Given the cursor on a statement outside any function call (e.g. after `x := 5;`), when signature help is invoked, then the response is null.

## Rationale

Auto-trigger on `(`/`,` made the popup reappear on every keystroke inside a
call and physically obscured the line being written, so PR #10 (v0.7.4,
issues #8/#9) made no-trigger the default and gated the old behavior behind
`ssl.intellisense.signatureHelp.autoTrigger`; hover and explicit invocation
work regardless. The null response for `MyProc(...)` is deliberate: direct
procedure calls are invalid SSL (the `direct_procedure_call` diagnostic flags
them), and offering a signature there would legitimize the pattern. Comma
counting ignores nested parens and strings because SQL text and nested calls
are the dominant argument shapes in real SSL.
