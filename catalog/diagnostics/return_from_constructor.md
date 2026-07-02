---
id: diag.return_from_constructor
title: Return from constructor (dead code — never emitted)
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
    ref: "PR #3 (v0.4.0, commit d744511)"
    note: >-
      Constant CodeReturnFromConstructor added to diagnostic_codes.go in the
      code-assignment pass, but no emit site was added then or since.
  - date: 2026-07-02
    ref: "catalog review (feat/behavior-catalog)"
    note: >-
      Promotion blocked: repo-wide search finds no code path that emits
      "return_from_constructor". Constructor :RETURN-with-value is covered
      by diag.constructor_return_value (checkClassContextRules); this slug
      appears to be a stillborn duplicate. Needs either an implementation
      or removal of the constant plus status: removed here.
issues: []
---

## Behavior

Left as draft: the code constant `CodeReturnFromConstructor`
(`internal/providers/diagnostic_codes.go`) exists, but no check in
`internal/providers/` (or anywhere else in the repo) ever emits a
diagnostic with this code, so no Flags fence can fire and the entry cannot
be promoted truthfully.

The semantics this slug names — a `:RETURN` returning a value inside a
`Constructor` — are already enforced by the schema-backed rule
`diag.constructor_return_value` (`lints.compile_errors.
constructor_return_value`, emitted from checkClassContextRules). A bare
`:RETURN;` in a constructor is legal SSL, so a separate rule flagging any
return from a constructor would contradict the schema.

Intended resolution (not done here — spec-first): delete the unused
constant and set this entry to `status: removed`, or implement a distinct
behavior if one is actually wanted. Tracked in the review report for this
batch.

## Examples

Illustration only (not executable while the rule is unimplemented):

```text
:CLASS Widget;
:PROCEDURE Constructor;
:RETURN Me;   /* constructor_return_value fires here today,
                 return_from_constructor never does; */
:ENDPROC;
```

## Rationale

Recording the dead slug keeps the catalog's bijection with
diagnostic_codes.go honest: the constant exists, so an entry must exist,
but claiming active behavior for a rule that never fires would make the
catalog lie. Draft status plus this note is the truthful state.
