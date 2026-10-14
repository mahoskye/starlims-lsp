---
id: diag.non_logical_condition
title: Literal non-logical condition or .AND./.OR. left operand
kind: diagnostic
status: active
authority: authoritative
schema_ref: null
default_severity: error
severity_overridable: true
suppressible: true
tests:
  - internal/providers/literal_semantics_test.go
history:
  - date: 2026-10-14
    ref: "issue #322"
    note: >-
      Introduced. Verified on STARLIMS 12 (12.3.2): SSL has no truthy/falsy
      coercion. A non-logical :IF, :WHILE, or :CASE condition raises
      ("invalid operand ... for operator: asBool"; `:IF NIL` raises a
      generic invocation error), and so does a non-logical left operand of
      .AND. or .OR. Only literals are judged, since a variable's or call's
      type is not known statically. Matches ssl-style-guide's
      no_truthy_falsy_coercion (authoritative, error).
issues: []
---

## Behavior

Flags a literal of a non-logical type where SSL tests a logical value:

- the condition of `:IF`, `:WHILE`, or `:CASE`;
- the left operand of `.AND.` or `.OR.`, anywhere in an expression.

A literal of a non-logical type is a string, number (including a negated
number such as `-1`), `NIL`, array, or code-block literal, looking through
parentheses. SSL has no truthy/falsy coercion, so each of these raises
when it runs (reference keywords/IF, operators/and). The range covers the
literal, and the message names its type. Error severity: the statement
cannot run.

It must NOT flag:

- a logical literal (`.T.`, `.F.`);
- a variable, property, call, or any computed expression: their type is
  not known statically, and this rule makes no guess;
- the right operand of `.AND.` / `.OR.`, which is returned unchanged
  rather than tested (`.T. .AND. 5` is `5`). Such a result raises later,
  when a condition tests it, but proving that needs data flow this rule
  does not do;
- a condition built from a literal with an operator that yields a logical
  value (`:IF "x" == sCode;`).

## Examples

### Flags

```ssl
:DECLARE s, b;
:IF "x";
	s := "a";
:ENDIF;
b := "x" .AND. .T.;
```

### Flags

```ssl
:WHILE 1;
:ENDWHILE;
:IF NIL;
:ENDIF;
```

### Does not flag

```ssl
:DECLARE s, b;
:IF .T.;
	s := "a";
:ENDIF;
:IF s;
:ENDIF;
b := .T. .AND. "x";
```

## Rationale

A literal condition of the wrong type is a defect the source proves: the
statement raises every time it runs, so it is an error. Variables are left
alone because SSL is dynamically typed and the inference here makes no
claim without evidence (issue #184's constraint). A later warning-level
check could use inferred types where a variable was clearly last assigned
a non-logical literal (issue #322).
