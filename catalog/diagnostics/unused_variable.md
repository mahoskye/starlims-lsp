---
id: diag.unused_variable
title: Variable declared but never used
kind: diagnostic
status: active
authority: tool
schema_ref: null
default_severity: hint
severity_overridable: true
suppressible: true
config:
  - ssl.diagnostics.unusedVariables
spec_options:
  check_unused_vars: true
tests:
  - internal/providers/providers_test.go
history:
  - date: 2026-02-02
    ref: "commit d56cbfe"
    note: >-
      Introduced opt-in (default off) alongside the other noisy checks
      (DECISIONS.md D5): flags :DECLARE/:PARAMETERS/:PUBLIC names with zero
      uses, scoped per procedure for locals and parameters.
  - date: 2026-04-30
    ref: "PR #3 (v0.4.0, commit d744511)"
    note: Stable diagnostic code assigned; behavior unchanged.
  - date: 2026-07-02
    ref: "issue #48"
    note: >-
      Opt-in exposed to clients as ssl.diagnostics.unusedVariables; the
      toggle was previously only reachable programmatically.
  - date: 2026-08-28
    ref: "issue #184 (expression AST consumers)"
    note: >-
      Declared names now come from statement-based declaration resolution
      (parser.CollectDeclarations). A declaration written as a bare
      `:DECLARE` / `:PARAMETERS` with its names on the following lines
      previously produced no names at all, so every name it declared was
      invisible to this behavior. No change on the production corpus's
      default-on output; the names it recovers were simply missing before.
issues: ["#48"]
---

## Behavior

Opt-in check (default off, per DECISIONS.md D5): flags each name declared
via `:DECLARE`, `:PARAMETERS`, or `:PUBLIC` that has zero uses. A use is
any other identifier token with the same name (case-insensitive); for
locals and parameters only tokens inside the declaring procedure count,
while `:PUBLIC` names are counted file-wide. The range covers the name on
its declaration line. Severity is hint so clients render the fade-out
"unused" style rather than a squiggle.

Deliberately generous notion of "use" — each of these suppresses the
finding:

- an assignment alone (`nCount := 1;`): write-only variables are *used*
  for this rule; dead stores are out of scope;
- an identifier that merely matches the name in a member access on some
  other object (`oOrder:nValue` counts as a use of a declared `nValue`) —
  a documented simplification in `countVariableUsages`, trading false
  negatives for zero false positives;
- any read, argument use, or placeholder-free mention inside the scope.

## Examples

### Flags

```ssl
:PROCEDURE Demo;
	:DECLARE nUnused;
	:DECLARE nCount;
	nCount := 1;
:ENDPROC;
```

### Flags

```ssl
:PROCEDURE Demo;
	:PARAMETERS nSeed;
	:DECLARE nCount;
	nCount := 1;
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE Demo;
	:DECLARE nCount;
	nCount := 1;
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE Demo;
	:PARAMETERS nSeed;
	:DECLARE nResult;
	nResult := nSeed + 1;
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE Demo;
	:DECLARE nValue;
	:DECLARE nTotal, oOrder;
	nTotal := oOrder:nValue;
:ENDPROC;
```

## Rationale

Unused-variable detection is inherently noisy in a language where publics
may be consumed by other files and identifiers double as member names, so
it follows the D5 policy: off by default, hint severity when on. Every
suppression above is a deliberate precision trade — counting writes and
member-name coincidences as uses (fences three and five) means a finding
is only raised when the name appears exactly once in its scope, which is
as close to provably-dead as a token scan gets. Enable it with
`ssl.diagnostics.unusedVariables` (issue #48 — the toggle was previously
only reachable programmatically via `DiagnosticOptions.CheckUnusedVars`,
as the spec runner uses it here).
