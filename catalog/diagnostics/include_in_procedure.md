---
id: diag.include_in_procedure
title: ":INCLUDE inside a procedure body"
kind: diagnostic
status: draft
authority: tool
schema_ref: null
default_severity: warning
severity_overridable: true
suppressible: true
tests: []
history:
  - date: 2026-04-30
    ref: "PR #3 (v0.4.0), commit d744511"
    note: >-
      Constant CodeIncludeInProcedure defined, but the in-procedure emit
      site in checkIncludePlacement was tagged CodeIncludeEarly instead.
      The constant has never been emitted.
issues: []
---

## Behavior

NOT PROMOTABLE — dead code. `include_in_procedure` has a code constant
(`CodeIncludeInProcedure`, internal/providers/diagnostic_codes.go) but no
emit site: the ":INCLUDE inside a procedure body is not supported"
diagnostic in `checkIncludePlacement` (internal/providers/diagnostics.go)
emits code `include_early` (warning severity) instead. See the
`include_early` entry, whose Behavior and Known gaps document the shared
code.

A `### Flags` fence for this slug cannot fire, so this entry stays `draft`
until either (a) the in-procedure emit site is switched to
`CodeIncludeInProcedure` — at which point this entry is promoted and
`include_early` narrows to the late-placement path — or (b) the constant is
removed and this entry becomes `status: removed`.

## Examples

### Flags

```ssl
/* unreachable: no emit site uses code include_in_procedure — see Behavior; */
```

### Does not flag

```ssl
:INCLUDE "MyLibrary";
```

## Rationale

Keeping the draft (rather than deleting the entry) preserves the catalog's
bijection with diagnostic_codes.go: the constant exists in code, so the
catalog must carry its entry. The severity recorded here (warning) matches
what the in-procedure situation actually emits today under the
`include_early` code.
