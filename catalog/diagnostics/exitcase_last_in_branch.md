---
id: diag.exitcase_last_in_branch
title: EXITCASE not the last statement directly in its branch
kind: diagnostic
status: active
authority: authoritative
schema_ref: blocks.case.exitcase_placement
default_severity: error
config:
  - ssl.diagnostics.rules
severity_overridable: true
suppressible: true
tests:
  - internal/providers/exitcase_placement_test.go
history:
  - date: 2026-10-13
    ref: "issues #265, #273"
    note: >-
      Introduced. The reference (keywords/EXITCASE.md, verified on
      STARLIMS 12) and the style guide's exitcase_last_in_branch rule
      state that :EXITCASE is valid only as the last statement directly
      in a :CASE/:OTHERWISE branch; v0.24.0 reported neither a nested
      :EXITCASE nor a statement after one. Zero findings on the
      production corpus (6,228 files), as expected for code that
      compiles.
issues: []
---

## Behavior

Flags an `:EXITCASE` that is not the last statement directly in the body
of its `:CASE` or `:OTHERWISE` branch. Each form is a compile error
(`unexpected token`), so the severity is error. Three cases:

- **Nested.** An `:EXITCASE` inside a block (`:IF`, `:WHILE`, `:FOR`,
  `:TRY`, including their `:ELSE`/`:CATCH`/`:FINALLY` parts) within the
  branch. The range covers that `:EXITCASE`.
- **Not last.** A statement after a direct `:EXITCASE`, before the
  branch's next `:CASE`, `:OTHERWISE` or `:ENDCASE`. The range covers the
  first such statement's first token, once per branch. A second
  `:EXITCASE` counts as a statement.
- **Outside.** An `:EXITCASE` with no enclosing `:BEGINCASE`.

Blocks are tracked on a stack, so a nested `:BEGINCASE` inside a branch
has its own branches: an `:EXITCASE` that ends an inner branch is direct
there, and the outer branch can still end with its own `:EXITCASE` after
the inner `:ENDCASE`.

It must NOT flag:

- an `:EXITCASE` that is the last statement of its branch, including
  after a `:RETURN` (`exitcase_after_return` reports that pair as
  unreachable, at warning);
- comments after the `:EXITCASE`: they are not statements;
- a branch with no `:EXITCASE` (that is `prefer_exitcase`).

`prefer_exitcase` still counts a nested `:EXITCASE` as ending its branch,
so the clause is reported once, here, as the compile error, not twice.

## Examples

### Flags

```ssl
:PROCEDURE Demo;
	:PARAMETERS nX, bY;
	:BEGINCASE;
	:CASE nX == 1;
		:IF bY;
			:EXITCASE;
		:ENDIF;
	:OTHERWISE;
		:EXITCASE;
	:ENDCASE;
:ENDPROC;
```

### Flags

```ssl
:PROCEDURE Demo;
	:PARAMETERS nX;
	:DECLARE nZ;
	:BEGINCASE;
	:CASE nX == 2;
		:EXITCASE;
		nZ := 1;
	:OTHERWISE;
		:EXITCASE;
	:ENDCASE;
:ENDPROC;
```

### Flags

```ssl
:PROCEDURE Demo;
	:EXITCASE;
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE Demo;
	:PARAMETERS nX, bY;
	:DECLARE nZ;
	:BEGINCASE;
	:CASE nX == 1;
		:BEGINCASE;
		:CASE bY;
			nZ := 1;
			:EXITCASE;
		:ENDCASE;
		:EXITCASE;
	:OTHERWISE;
		nZ := 2;
		:EXITCASE;
		/* trailing comment;
	:ENDCASE;
:ENDPROC;
```

## Rationale

The reference says `:EXITCASE` "is valid only as the last statement of a
`:CASE` or `:OTHERWISE` branch ... directly in the branch body", and lists
both a statement after it and an `:EXITCASE` inside an `:IF` as
compile-time errors. The style guide records the same rule as
`exitcase_last_in_branch` (`level: authoritative`, severity error), so
this entry uses that slug. The LSP reports it so the compile failure is
seen before check-in. The outside-`:BEGINCASE` form follows from the same
"valid only" statement.
