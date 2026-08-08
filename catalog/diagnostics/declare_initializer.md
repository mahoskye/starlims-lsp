---
id: diag.declare_initializer
title: ":DECLARE with inline initializer"
kind: diagnostic
status: active
authority: authoritative
schema_ref: declarations.declare_rule
default_severity: error
severity_overridable: true
suppressible: true
tests:
  - internal/providers/providers_test.go
history:
  - date: 2026-08-07
    ref: "issue #138"
    note: >-
      Introduced: :DECLARE statements carrying inline initializers
      (:DECLARE x := 1;) were silently accepted as valid. Authoritative SSL
      permits only a comma-separated identifier list; class-level fields
      "initialized" this way are silently never assigned at runtime.
issues: []
---

## Behavior

Flags each `:=` operator appearing inside a `:DECLARE` statement (from the
`:DECLARE` keyword through its terminating `;`). Authoritative SSL accepts
only `:DECLARE ident(, ident)*;` — in every context: procedure locals,
script level, class fields, and data-source files alike. Initialization must
be a separate assignment statement (for class fields, in the Constructor).
One diagnostic per initializer, ranged on the `:=` operator.

The check runs in ordinary SSL files and in SSL-mode data-source files. (In
data-source files `:PARAMETERS` takes inline `:=` defaults — that form is
required there, see `datasource_default_required` — but `:DECLARE` never
does; data-source coverage is pinned by a Go test since this entry's spec
fences run without `is_data_source_file`.)

It must NOT flag:

- `:DECLARE` with a plain identifier list, single or comma-separated;
- assignment statements on lines after the `:DECLARE`;
- `:=` in any other statement, including `:PARAMETERS p := v` data-source
  headers;
- `:=` in comments or string literals.

## Examples

### Flags

```ssl
:PROCEDURE Demo;
	:DECLARE nCount := 1;
:ENDPROC;
```

### Flags

```ssl
:CLASS Demo;
:DECLARE sName := "X";
```

### Flags

```ssl
:PROCEDURE Demo;
	:DECLARE nOne := 1, nTwo := 2;
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
:CLASS Demo;
:DECLARE sName, nCount;
:PROCEDURE Constructor;
	sName := "X";
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE Demo;
	:DECLARE sText;
	sText := "a := b";
	/* comment with x := 1;
:ENDPROC;
```

## Rationale

Authoritative language behavior (issue #138): the runtime accepts only an
identifier list after `:DECLARE`, so an inline initializer is a syntax error
— hence error severity. The missing diagnostic was actively dangerous:
generated code containing `:DECLARE CONST_X := ...;` lines passed validation
cleanly while the class-level "constants" were silently never assigned at
runtime. The tree-sitter-ssl grammar (`declaration_statement_declare` =
identifier list only) already agreed with this rule; the LSP was the
outlier.
