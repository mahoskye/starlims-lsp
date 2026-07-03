---
id: diag.undeclared_variable
title: Use of an undeclared variable
kind: diagnostic
status: active
authority: tool
schema_ref: null
default_severity: warning
severity_overridable: true
suppressible: true
config:
  - ssl.diagnostics.globals
  - ssl.diagnostics.endpointPatterns
spec_options:
  check_undeclared_vars: true
  is_endpoint_file: true
  global_variables: ["gSystemContext"]
tests:
  - internal/providers/providers_test.go
history:
  - date: 2025-11-XX
    ref: "commit a4df25b"
    note: Introduced opt-in (default off) per the noisy-checks policy (DECISIONS.md D5).
  - date: 2025-11-19
    ref: "vs-code-ssl-formatter #2/#53 (v1.1.0+)"
    note: "False positives fixed: Me access and built-in function names are not undeclared variables."
  - date: 2025-12-XX
    ref: "vs-code-ssl-formatter #55/#56"
    note: >-
      ssl.diagnostics.globals entries count as declared; identifiers inside
      :INCLUDE paths are not variable references.
  - date: 2026-05-14
    ref: "PR #23 (v0.7.7)"
    note: >-
      Endpoint scripts (ssl.diagnostics.endpointPatterns match or Endpoint:
      docblock) treat Request/Response as pre-injected ambients — declared
      for reads, never flagged. In non-endpoint files they still flag:
      using them there is a real bug (DECISIONS.md D4).
  - date: 2026-07-03
    ref: "include-aware diagnostics PR"
    note: >-
      Names declared by resolved :INCLUDE targets count as declared
      (feature.cross_file_resolution A18-A19) — :INCLUDE is a full-splice
      textual paste, so the included script's declarations belong to the
      including file. Single-file behavior (and every fence) unchanged.
issues: []
---

## Behavior

Opt-in check (default off, per DECISIONS.md D5): flags an identifier used as
a variable that has no `:DECLARE`/`:PUBLIC`/`:PARAMETERS` declaration in
scope, reported once per scope.

Treated as declared, never flagged: built-in function and class names,
`Me`/`Base` forms, identifiers inside `:INCLUDE` paths, names listed in
`ssl.diagnostics.globals`, names declared by the file's resolved
`:INCLUDE` targets (the include declaration closure,
`feature.cross_file_resolution` A18-A19 — `:INCLUDE` splices the included
script's text, so its declarations are in scope), and — in endpoint
scripts only — the runtime ambients `Request` and `Response`. In
non-endpoint files `Request` and `Response` flag like any other
undeclared name. Without a workspace index (or when includes don't
resolve) the check is single-file, exactly as before.

## Examples

### Flags

```ssl
:PROCEDURE Demo;
	nTotal := nMissing + 1;
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
:PROCEDURE Handler;
	Response:Write(Request:QueryString("id"));
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE Demo;
	gSystemContext := "ready";
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE Demo;
	:DECLARE sName;
	sName := "abc";
	sName:SomeRandomDotNetMethod();
:ENDPROC;
```

## Rationale

Undeclared-variable detection is the highest-value and highest-noise check
in the pipeline — its history is a list of contexts that had to be exempted
one false positive at a time. Each Does-not-flag fence pins one of those
exemptions permanently. The endpoint-ambient fence runs with
`is_endpoint_file: true` (spec_options); the inverse case — `Request`
flagging in a non-endpoint file — is covered by PR #23's tests in
providers_test.go, since spec fences share one option set per entry.
The member-access fence pins DECISIONS.md D10 (issue #22): the member of
a `:` access is never a variable reference, and built-in value types
forward unmatched members to .NET at runtime, so flagging them would
false-positive every legitimate passthrough.
