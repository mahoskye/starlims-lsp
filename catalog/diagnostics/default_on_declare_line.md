---
id: diag.default_on_declare_line
title: ":DEFAULT on a name that is not a parameter"
kind: diagnostic
status: active
authority: authoritative
schema_ref: null
default_severity: error
severity_overridable: true
suppressible: true
tests:
  - internal/providers/providers_test.go
history:
  - date: 2026-01-14
    ref: "commit 567b287"
    note: >-
      Introduced with the declare/default usage checks: :DEFAULT pairs with
      :PARAMETERS, not :DECLARE (ssl_agent_instructions.md gotcha #3).
  - date: 2026-04-30
    ref: "PR #3 (v0.4.0, commit d744511)"
    note: Stable diagnostic code assigned; behavior unchanged.
  - date: 2026-10-13
    ref: "issue #255"
    note: >-
      Re-scoped. The rule now flags a :DEFAULT whose target is not a
      :PARAMETERS name of the enclosing procedure (or of the script),
      ranged on the target, on any line (reference keywords/DEFAULT: the
      name must already be in the :PARAMETERS list). The old same-line
      heuristic always co-fired with default_after_parameters,
      misreported a :DEFAULT on a parameter that merely shared a line
      with :DECLARE, and missed the defect on any other line. The slug
      is kept for client compatibility. authority corrected to
      authoritative: the style-guide record no_default_on_declare_line
      (level authoritative) names this rule in enforced_by, so the guide
      is not silent.
  - date: 2026-10-14
    ref: "issue #323"
    note: >-
      Regression from the #255 re-scope fixed. `:DECLARE nCount :DEFAULT
      0;` (no `;` between the two) validated clean: the "target" is the
      value `0`, not a name, so the target check skipped it. A :DEFAULT
      inside an unterminated :DECLARE list is now flagged on the
      :DEFAULT keyword; it does not compile (verified on STARLIMS 12:
      the compiler expects `;` before `:DEFAULT`).
issues: []
---

## Behavior

Flags a `:DEFAULT` whose target is not a `:PARAMETERS` name of its scope:
the enclosing `:PROCEDURE`, or — outside every procedure — the
script-level `:PARAMETERS`. `:DEFAULT` only supplies a fallback for a
parameter (reference keywords/DEFAULT: the name must already be in the
current `:PARAMETERS` list), so defaulting a `:DECLARE`d local, a
misspelled parameter, or a name from another procedure is a defect. The
range covers the target name; names compare case-insensitively. The
check runs in non-data-source files; data source files route to their
own `:DEFAULT` rules.

The slug predates the re-scope. The rule used to flag only a `:DECLARE`
and a `:DEFAULT` sharing one line, which co-fired with
`default_after_parameters`, misreported a `:DEFAULT` on a real parameter
that happened to share a line with a `:DECLARE`, and missed the defect on
any other line (issue #255). Placement — `:DEFAULT` not directly after
`:PARAMETERS` — remains `default_after_parameters`' finding; the two
report different defects and can both fire on one statement.

It also flags a `:DEFAULT` inside a `:DECLARE` list that no `;` has
ended (`:DECLARE nCount :DEFAULT 0;`), ranged on the `:DEFAULT` keyword.
That is the classic attempt to initialize a declaration; it does not
compile, and its "target" is the value rather than a name, so the
target check alone cannot see it (issue #323). `unexpected_token` defers
this one keyword to this rule; any other keyword inside a declaration
list is `unexpected_token`'s finding.

It must NOT flag:

- a `:DEFAULT` on a name in the scope's `:PARAMETERS` list, whatever else
  shares its line;
- a script-level `:DEFAULT` on a script-level parameter;
- data source files, where this check does not run.

## Examples

### Flags

```ssl
:PROCEDURE Demo;
	:DECLARE sName; :DEFAULT sName, "";
:ENDPROC;
```

### Flags

A `:DECLARE`d local on its own line, after the parameter defaults.

```ssl
:PROCEDURE Demo;
	:PARAMETERS sIn;
	:DEFAULT sIn, "";
	:DECLARE nCount;
	:DEFAULT nCount, 0;
:ENDPROC;
```

### Flags

`:DEFAULT` inside an unterminated `:DECLARE` list (issue #323).

```ssl
:DECLARE nCount :DEFAULT 0;
:RETURN nCount;
```

### Does not flag

```ssl
:PROCEDURE Demo;
	:PARAMETERS sName;
	:DEFAULT sName, "";
	:DECLARE sLocal;
:ENDPROC;
```

### Does not flag

The target is a parameter, so sharing a line with `:DECLARE` is not this
rule's concern.

```ssl
:PARAMETERS sTop;
:DEFAULT sTop, "";
:PROCEDURE Demo;
	:PARAMETERS sName; :DEFAULT sName, ""; :DECLARE sLocal;
:ENDPROC;
```

## Rationale

Defaulting a name that is not a parameter is a language constraint
violation, not a style preference, so this is an error (567b287; reference
keywords/DEFAULT). The re-scope keys the rule on the defect — the target —
rather than on a line layout, so it needs no data flow: the parameter
list of the enclosing scope is all it reads. On the private corpus it
found 35 such defaults in one script, which `default_after_parameters`
already rejected for placement.
