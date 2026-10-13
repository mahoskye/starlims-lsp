---
id: diag.invalid_limstypeex_comparison
title: LimsTypeEx compared against a string outside its result set
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
  - date: 2026-08-26
    ref: "issue #187"
    note: >-
      Introduced from the runtime-verification batch: LimsTypeEx returns
      exactly one of nine fixed strings, so a guard against any other
      literal (chronically "NUMBER") can never pass.
  - date: 2026-10-13
    ref: "issue #255"
    note: >-
      String comparison is case-sensitive and `=` is a prefix match
      (reference operators/equals), so literals are now judged exactly
      as written: `== "numeric"` flags, while `= "NUM"`, `= ""` and
      `"STRINGS" = LimsTypeEx(x)` (all can be true) no longer do. `<>`
      and `#` are checked, and the inequality message says 'always
      true'. authority corrected to authoritative: the style-guide
      record limstypeex_numeric_not_number (level authoritative) names
      this rule in enforced_by, so the guide is not silent.
issues: []
---

## Behavior

Flags a comparison between a `LimsTypeEx(...)` call and a string literal
that can never come out the way the guard expects. `LimsTypeEx` returns
one of nine uppercase names: `NIL`, `STRING`, `NUMERIC`, `LOGIC`, `DATE`,
`ARRAY`, `CODEBLOCK`, `OBJECT`, `SSLVALUE`. String comparison is
case-sensitive and `=` is a prefix match (reference operators/equals), so
the literal is judged exactly as written — no case folding, no trimming —
and each operator by its own semantics. Both operand orders are checked;
the range covers the string literal; error severity.

- `==` with a literal that is not one of the nine names: never true.
- `!=`, `<>`, `#` with such a literal: always true. The message says so.
- `=` with the call on the left (`LimsTypeEx(x) = "NUM"`): true when the
  literal is empty or some result starts with it, so it flags only a
  non-empty literal that no result starts with.
- `=` with the literal on the left (`"NUMBERS" = LimsTypeEx(x)`): true
  when the literal starts with some result, so it flags only a literal
  that starts with none.

It must NOT flag:

- comparison against a valid result string (`LimsTypeEx(x) == "NUMERIC"`);
- an `=` that can be true by prefix (`LimsTypeEx(x) = "NUM"`,
  `"STRINGS" = LimsTypeEx(x)`). `LimsTypeEx(x) = ""` is always true and is
  `diag.equals_empty_string`'s finding;
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

### Flags

The result names are uppercase and the comparison is case-sensitive.

```ssl
:PROCEDURE Check;
	:PARAMETERS uValue;
	:IF LimsTypeEx(uValue) = "String";
		:RETURN .T.;
	:ENDIF;
	:RETURN LimsTypeEx(uValue) <> "NUMBER";
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

`=` is a prefix match, so these can be true.

```ssl
:PROCEDURE Check;
	:PARAMETERS uValue;
	:DECLARE bHit;
	bHit := LimsTypeEx(uValue) = "NUM";
	bHit := "STRINGS" = LimsTypeEx(uValue);
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
no runtime state can make the guard behave. The original check folded
case, trimmed the literal, and treated `=` like `==`; issue #255 aligned
it with the documented comparison semantics, which both adds findings
(`= "String"` never matches the uppercase `STRING`) and removes false
ones (`= "NUM"` is a working prefix test). Only direct literal-vs-call
comparisons flag.
