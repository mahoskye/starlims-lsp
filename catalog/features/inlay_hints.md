---
id: feature.inlay_hints
title: Parameter-name inlay hints
kind: feature
status: draft
authority: tool
schema_ref: null
config:
  - ssl.inlayHints.enabled
  - ssl.inlayHints.minParameterCount
tests:
  - internal/providers/inlayhints_test.go
history:
  - date: 2026-02-02
    ref: "b44eaf9"
    note: >-
      Inlay hints added (LSP 3.17 via wrapper handler). Hints are suppressed
      inside string and comment contexts — fixes extension issue #39, where
      hints rendered inside SQL string literals.
  - date: 2026-03-21
    ref: "cdbfee6"
    note: ssl.inlayHints.minParameterCount setting wired (default 2) to cut visual noise on single-argument calls.
---

## Behavior

Serves `textDocument/inlayHint` with parameter-name hints:

- Hints are of kind Parameter (2), rendered as `paramName:` positioned before
  the argument, with right padding.
- Built-in function calls get hints from the canonical signature database.
  Unknown functions get no hints.
- `DoProc(...)` and `ExecFunction(...)` calls get `sProcName:` / `aParams:`
  hints; when the first argument is a string literal naming a `:PROCEDURE` in
  the current document and the second is an array literal `{...}`, the
  procedure's own parameter names are additionally hinted inside the array.
  When the procedure name is a variable, only the outer hints are shown.
- Hints are emitted only for arguments that are actually present, and for all
  calls in nested expressions.
- No hints may be produced for function-like text inside string literals
  (including SQL strings) or comments.
- `ssl.inlayHints.enabled: false` disables the feature entirely;
  `ssl.inlayHints.minParameterCount` (default 2) suppresses hints for
  functions whose signature has fewer parameters than the threshold.
- Only the client-requested line range is processed.

## Acceptance

- A1: Given `x := SubStr("Hello", 1, 5);`, when hints are requested, then
  parameter-name hints appear before each of the three arguments.
- A2: Given `x := Len(sValue);` with `minParameterCount` at its default of 2,
  when hints are requested, then no hint is produced for the call.
- A3: Given a document defining `:PROCEDURE Calc; :PARAMETERS nQty, nPrice;`
  and a call `DoProc("Calc", {100, 25.50});`, when hints are requested, then
  `sProcName:`, `aParams:`, `nQty:`, and `nPrice:` hints are all produced.
- A4: Given `DoProc("UnknownProc", {x, y});`, when hints are requested, then
  outer `sProcName:` / `aParams:` hints appear but no hints are produced for
  the array elements.
- A5: Given `s := "SubStr(s, 1, 5)";`, when hints are requested, then no hint
  is produced inside the string literal.
- A6: Given `ssl.inlayHints.enabled: false`, when hints are requested, then
  the result is empty.
- A7: Given calls on lines outside the requested range, when hints are
  requested for a narrower range, then hints are produced only for calls
  inside that range.

## Rationale

SSL calls procedures through `DoProc`/`ExecFunction` with positional array
arguments, so parameter names are otherwise invisible at call sites — hints
recover that signal (b44eaf9). The string/comment exclusion exists because
SQL literals are full of `name(...)` shapes that are not SSL calls
(extension issue #39). The `minParameterCount` default of 2 (cdbfee6) trades
completeness for noise reduction on trivial one-argument calls.
