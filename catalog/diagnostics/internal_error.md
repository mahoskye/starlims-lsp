---
id: diag.internal_error
title: Internal error during diagnostic collection
kind: diagnostic
status: active
authority: tool
schema_ref: null
default_severity: error
severity_overridable: true
suppressible: true
tests:
  - internal/providers/pipeline_robustness_test.go
history:
  - date: 2026-05-01
    ref: "5814391 (v0.7.1)"
    note: >-
      Introduced with the panic recovery around collectDiagnostics: a check
      that panicked used to take the whole server down, so the recovery
      reports it as one error diagnostic instead.
  - date: 2026-10-13
    ref: "issue #258"
    note: >-
      Given a code constant (CodeInternalError) and this entry; the slug was
      a string literal at its one emit site, outside the catalog's
      code-to-entry bijection. The recovery now keeps the diagnostics
      collected before the panic, and also covers the data-source SQL-mode
      path, lexing, and parsing.
issues: []
---

## Behavior

Reports a panic recovered inside the diagnostics pipeline — a defect in
this tool, never in the document. One `error` diagnostic with Code
`internal_error` and Source `ssl-lsp` is emitted at line 1, column 1
(range `0:0`–`0:1`), with a message naming the panic value and asking for
a bug report; the stack trace goes to stderr, which an editor shows in
its LSP output channel.

When the panic happens inside rule collection, the diagnostics the
earlier rules collected are kept and go through suppression, rule
overrides, and the info gate as usual; only the panicking rule and the
rules after it are missing. When it happens elsewhere in the pipeline
(data-source SQL-mode classification, lexing, parsing), the result is the
internal_error alone. Either way the panic never propagates.

It must NOT appear for any document the pipeline handles without
panicking — which is every document, absent a bug.

Like every coded diagnostic it honors `ssl.diagnostics.rules` (D2) and
`@ssl-disable` suppression (D3) when it comes from rule collection.

## Examples

No input produces this diagnostic on a correct build, so it has no
`### Flags` fence; the recovery is driven by a real panic in
`internal/providers/pipeline_robustness_test.go`
(`TestCollectDiagnostics_PanicKeepsCollectedDiagnostics`, citing
feature.diagnostics_pipeline A7).

### Does not flag

```ssl
:PROCEDURE Demo;
:PARAMETERS nValue;
:RETURN nValue + 1;
:ENDPROC;
```

## Rationale

A panicking rule previously killed the server process — on the client,
every feature for every open document stopped at once. Recovering and
reporting keeps the editor usable and makes the bug visible and
reportable, which is why the diagnostic is loud (`error`) rather than
silent. It is `authority: tool`: the style guide has nothing to say about
this tool's own defects. Keeping the earlier rules' findings (issue #258)
matters because one defective rule should cost only its own findings, not
every other rule's.
