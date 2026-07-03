---
id: feature.inlay_hints
title: Parameter-name inlay hints
kind: feature
status: active
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
      Inlay hints added (LSP 3.17 via wrapper handler), including the
      ssl.inlayHints.enabled and minParameterCount (default 2) settings.
      Hints are suppressed inside string and comment contexts — fixes
      extension issue #39, where hints rendered inside SQL string literals.
  - date: 2026-07-02
    ref: "issue #46"
    note: >-
      DoProc/ExecFunction argument-array hints implemented: a lone
      string-literal first argument resolves to a same-file procedure and
      the array elements are hinted with its parameter names. Dynamic
      targets stay unhinted.
issues: ["#46"]
---

## Behavior

Serves `textDocument/inlayHint` with parameter-name hints:

- Hints are of kind Parameter (2), rendered as `paramName:` positioned
  before the argument, with right padding (the `:` suffix and padding are
  applied by the server handler).
- Built-in function calls get hints from the canonical signature database.
  Functions not in that database get no hints — the server MUST NOT guess.
- `DoProc(...)` and `ExecFunction(...)` calls (case-insensitive) get
  `sProcName:` / `aParams:` hints on their own two arguments.
- When the first DoProc/ExecFunction argument is a string literal naming a
  `:PROCEDURE` in the current document and the second is an array literal
  `{...}`, the procedure's own parameter names are additionally hinted
  inside the array; when the name is a variable, only the outer hints are
  shown. (Inner-array hints are not implemented yet — see Known gaps / A4.)
- Hints are emitted only for arguments that are actually present, and for
  every call in nested expressions independently.
- No hints may be produced for function-like text inside string literals
  (including SQL strings) or comments.
- `ssl.inlayHints.enabled: false` disables the feature entirely;
  `ssl.inlayHints.minParameterCount` (default 2) suppresses hints for
  functions whose signature has fewer parameters than the threshold.
- Only calls within the client-requested line range are processed.

## Acceptance

- A1: Given `x := Substr("Hello", 1, 5);`, when hints are requested, then parameter-name hints of kind Parameter appear before each of the three arguments.
- A2: Given `x := Len(sValue);` with `minParameterCount` at its default of 2, when hints are requested, then no hint is produced for the call; with `minParameterCount: 1` the `source` hint appears.
- A3: Given `DoProc("MyProc", {100, 200});` or `ExecFunction(...)` in any casing, when hints are requested, then `sProcName:` and `aParams:` hints are produced for the two outer arguments.
- A4: Given a document defining `:PROCEDURE Calc;` with `:PARAMETERS nQty, nPrice;` and a call `DoProc("Calc", {100, 25.50});`, when hints are requested, then `nQty:` and `nPrice:` hints are additionally produced inside the array literal.
- A5: Given `s := "Substr(a, 1, 2)";` or the same text inside a comment, when hints are requested, then no hint is produced for the call-shaped text in string or comment context.
- A6: Given `ssl.inlayHints.enabled: false`, when hints are requested, then the result is empty.
- A7: Given calls on several lines, when hints are requested for a narrower line range, then hints are produced only for calls inside that range.
- A8: Given `x := UnknownFunc(a, b, c);` where the callee is neither a built-in nor DoProc/ExecFunction, when hints are requested, then no hints are produced.

## Rationale

SSL calls procedures through `DoProc`/`ExecFunction` with positional array
arguments, so parameter names are otherwise invisible at call sites — hints
recover that signal (b44eaf9). The string/comment exclusion exists because
SQL literals are full of `name(...)` shapes that are not SSL calls
(extension issue #39). The `minParameterCount` default of 2 trades
completeness for noise reduction on trivial one-argument calls — `Len(x):`
hints teach nothing. Unknown callees get nothing rather than a guess (A8)
for the same reason hover returns null when it has no information.

## Known gaps

- The `paramName:` label suffix and right padding live in the server
  handler (handleInlayHint), outside the provider tests' reach; the wire
  format has no direct test.
