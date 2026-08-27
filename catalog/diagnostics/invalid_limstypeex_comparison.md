---
id: diag.invalid_limstypeex_comparison
title: LimsTypeEx compared against a string outside its result set
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
  - date: 2026-08-26
    ref: "issue #187"
    note: >-
      Introduced from the runtime-verification batch: LimsTypeEx returns
      exactly one of nine fixed strings, so a guard against any other
      literal (chronically "NUMBER") can never pass.
issues: []
---

## Behavior

Flags a comparison between a `LimsTypeEx(...)` call and a string literal
that is not one of the function's nine possible results: `NIL`, `STRING`,
`NUMERIC`, `LOGIC`, `DATE`, `ARRAY`, `CODEBLOCK`, `OBJECT`, `SSLVALUE`
(matched case-insensitively, surrounding whitespace in the literal
ignored). Comparison operators are `=`, `==`, and `!=`; both operand
orders are checked (`LimsTypeEx(x) == "NUMBER"` and
`"NUMBER" == LimsTypeEx(x)`). The range covers the string literal. Error
severity: the guard can never pass (or, for `!=`, never fail), so the
comparison is provably dead.

It must NOT flag:

- comparison against a valid result string in any casing
  (`limstypeex(x) = "numeric"`);
- comparison against a non-literal (`LimsTypeEx(x) == sExpected`) — the
  value is not provable;
- `LimsTypeEx` results used in other operations (assignment, `IIf`,
  concatenation) — only direct comparisons are checked;
- other functions compared against arbitrary strings — the fixed result
  set is specific to `LimsTypeEx`.

## Examples

### Flags

```ssl
:PROCEDURE Check;
	:PARAMETERS uValue;
	:IF LimsTypeEx(uValue) == "NUMBER";
		:RETURN .T.;
	:ENDIF;
	:RETURN .F.;
:ENDPROC;
```

### Flags

```ssl
:PROCEDURE Check;
	:PARAMETERS uValue;
	:IF "INTEGER" = LimsTypeEx(uValue);
		:RETURN .T.;
	:ENDIF;
	:RETURN .F.;
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE Check;
	:PARAMETERS uValue;
	:IF LimsTypeEx(uValue) == "NUMERIC";
		:RETURN .T.;
	:ENDIF;
	:RETURN .F.;
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE Check;
	:PARAMETERS uValue, sExpected;
	:RETURN LimsTypeEx(uValue) == sExpected;
:ENDPROC;
```

## Rationale

`LimsTypeEx` has a closed result set, so a literal outside it makes the
comparison statically dead — the classic bug is `"NUMBER"` for
`"NUMERIC"`, a type guard that silently never fires (issue #187). Error
severity because unlike most style rules this is a provable logic bug:
no runtime state can make the guard behave. Only direct
literal-vs-call comparisons flag, keeping the rule immune to the
expression-typing limits tracked in #184.
