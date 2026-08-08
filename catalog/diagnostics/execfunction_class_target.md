---
id: diag.execfunction_class_target
title: ExecFunction targets a class file
kind: diagnostic
status: active
authority: authoritative
schema_ref: null
default_severity: error
severity_overridable: true
suppressible: true
spec_options:
  class_file_dispatch_targets: ["LIMS.SampleTools", "LIMS.SampleTools.Recalculate"]
tests:
  - internal/providers/providers_test.go
  - internal/server/workspace_index_test.go
history:
  - date: 2026-08-07
    ref: "issue #143 (ssl-style-guide#42, ssl-docs#52)"
    note: >-
      Introduced: a class file has no script entry point, so
      ExecFunction("Cat.ClassFile") fails at runtime and
      ExecFunction("Cat.ClassFile.Method") does not invoke the method.
      The correct pattern is CreateUdObject("Cat.ClassName") and calling
      methods on the instance.
issues: []
---

## Behavior

Flags the string target of an `ExecFunction(...)` call when it resolves
through the workspace index to class files only — conservatively: at least
one candidate, and **every** candidate is a class file (the #125
conservative-write lesson; a target that also matches an ordinary script
never flags). Both the two-segment entry-point form and the
three-plus-segment method form flag. The range covers the dispatch string
content; the message points to `CreateUdObject` and instance calls.

The cross-file resolution is supplied by the server per document
(`DiagnosticOptions.ClassFileDispatchTargets`, filled from the workspace
index before collection so suppression and severity overrides apply
normally). Consumers without a workspace — the `--validate` CLI, stdin —
leave the list empty and the check is silent.

It must NOT flag:

- `DoProc(...)` sites, whatever their target (DoProc has its own rules);
- `ExecFunction` targets that resolve to ordinary scripts, to nothing at
  all, or ambiguously to a mix of scripts and classes;
- `CreateUdObject("Cat.ClassName")` — the correct pattern;
- data-source documents (the server does not resolve dispatch targets
  there).

Class files deliberately remain in dispatch *completion* and
definition/references navigation — only this diagnostic marks the call
invalid; hiding the target from navigation would make the mistake harder
to investigate, not easier (recorded per issue #143).

## Examples

### Flags

```ssl
:PROCEDURE Demo;
	ExecFunction("LIMS.SampleTools", {1});
:ENDPROC;
```

### Flags

```ssl
:PROCEDURE Demo;
	ExecFunction("LIMS.SampleTools.Recalculate", {1});
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE Demo;
	:DECLARE oTools;
	oTools := CreateUdObject("LIMS.SampleTools");
	oTools:Recalculate(1);
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE Demo;
	DoProc("LIMS.SampleTools", {1});
	ExecFunction("LIMS.OrdinaryScript", {1});
:ENDPROC;
```

## Rationale

Authoritative runtime behavior documented in ssl-style-guide#42 /
ssl-docs#52: class files are not runnable targets — the two-segment call
fails at runtime and the three-segment call silently does nothing useful,
which makes this an error, and one worth catching before runtime. The
all-candidates-must-be-classes gate keeps the false-positive risk at the
level the cross-file write side already accepts (#125): when resolution is
ambiguous the tool stays quiet rather than second-guessing.
