---
id: diag.dot_property_access
title: Dot notation used for property access
kind: diagnostic
status: active
authority: tool
schema_ref: null
default_severity: error
severity_overridable: true
suppressible: true
tests:
  - internal/providers/providers_test.go
history:
  - date: 2026-02-02
    ref: "commit 7261172"
    note: >-
      Introduced with the gotcha checks (gotcha #8): SSL uses colon
      notation for member access, not dots.
  - date: 2026-04-30
    ref: "PR #3 (v0.4.0, commit d744511)"
    note: Stable diagnostic code assigned; behavior unchanged.
  - date: 2026-05-01
    ref: "issue #56, commit 967c264"
    note: >-
      False positive fixed: dots inside :INCLUDE module paths (which can be
      deep, e.g. :INCLUDE A.B.C.D;) are path separators, not property access.
  - date: 2026-07-22
    ref: "issue #77"
    note: >-
      No longer runs in SQL-mode data-source documents: `table.column`
      qualified names in plain-SQL .ds files were the reported false
      positive. Suppression happens at the pipeline level
      (feature.diagnostics_pipeline A10-A12), not in this check.
  - date: 2026-08-08
    ref: "issue #149"
    note: >-
      False positive fixed: qualified base names in :INHERIT declarations
      (`:INHERIT Category.ScriptName;`) are required syntax per the schema
      (classes.signature.inherit), not property access. Exempted through
      the terminating `;`, same mechanism as :INCLUDE (issue #56).
issues: []
---

## Behavior

Flags `identifier.identifier` patterns — property access using dot notation,
familiar from other languages but wrong in SSL, which uses colon notation
(`object:property`). Concretely: a `.name` fragment (lexed as an unknown
token starting with `.` followed by identifier characters) whose preceding
significant token is an identifier. The range covers the `.name` fragment
and the message names the property and the colon replacement.

It must NOT flag:

- colon member access (`oEmail:Subject`);
- dots inside `:INCLUDE` module paths, however deep (`:INCLUDE A.B.C.D;`) —
  the whole statement through its terminating `;` is exempt (issue #56);
- dots inside `:INHERIT` qualified base names
  (`:INHERIT Category.ScriptName;`) — the schema documents both bare and
  qualified forms as accepted (classes.signature.inherit), so the whole
  statement through its terminating `;` is exempt (issue #149);
- dotted logical operators and boolean literals (`.AND.`, `.OR.`, `.T.`,
  `.F.`), which lex as operators, not unknown fragments;
- numeric literals with decimal points;
- a leading `.name` with no preceding identifier.

Like every SSL check, it does not run at all in SQL-mode data-source
documents — plain-SQL `.ds` files are suppressed at the pipeline level
(feature.diagnostics_pipeline, issue #77), so `table.column` qualified
names there never reach this check.

## Examples

### Flags

```ssl
:PROCEDURE Demo;
	:DECLARE x, oEmail;
	oEmail := Email{};
	x := oEmail.Subject;
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE Demo;
	:DECLARE x, oEmail;
	oEmail := Email{};
	x := oEmail:Subject;
:ENDPROC;
```

### Does not flag

```ssl
:INCLUDE Framework.Core.Utils.Strings;
:PROCEDURE Demo;
:ENDPROC;
```

### Does not flag

```ssl
:CLASS RestApiUsers;
:INHERIT RestApi.RestApiBase;
:PROCEDURE GetUsers;
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE Demo;
	:DECLARE bOk, a, b;
	:IF a .AND. b;
		bOk := .T.;
	:ENDIF;
:ENDPROC;
```

## Rationale

`object.property` is gotcha #8: it parses as garbage at runtime while looking
perfectly natural to most developers, so this is an error (7261172). The rule
leans on the lexer — a dot-fragment only becomes an unknown token when it is
not a valid SSL construct — and its one recorded false positive class,
`:INCLUDE` paths, is permanently pinned by the fence above (issue #56,
967c264).
