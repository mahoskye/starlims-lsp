---
id: diag.unmatched_block_end
title: Block-end keyword with no open block
kind: diagnostic
status: active
authority: tool
schema_ref: null
default_severity: error
severity_overridable: true
suppressible: true
tests:
  - internal/providers/audit255_test.go
history:
  - date: 2026-01-10
    ref: "commit 442fa69 (initial commit)"
    note: >-
      checkUnclosedBlocks shipped in the initial commit: stack-based
      matching of block keyword pairs with unmatched / mismatched /
      unclosed outcomes.
  - date: 2026-04-30
    ref: "PR #3 (v0.4.0, commit d744511)"
    note: Stable diagnostic code assigned; behavior unchanged.
  - date: 2026-10-13
    ref: "issue #255"
    note: >-
      Catalog audit: the empty-stack condition made this code unreachable
      inside any procedure — a stray `:ENDIF` there was reported as
      mismatched_block_end, "':ENDIF' does not match ':PROCEDURE'". The
      innermost :PROCEDURE frame now counts as an empty stack, so the same
      defect gets the same code at top level and in a procedure.
issues: []
---

## Behavior

Flags a block-end keyword (`:ENDIF`, `:ENDWHILE`, `:NEXT`, `:ENDCASE`,
`:ENDINLINECODE`, `:ENDTRY`, `:ENDPROC`, `:ENDREGION`) that arrives while
**no** block is open — the block stack is empty, or the only thing open is
the enclosing `:PROCEDURE` and no block inside it is open. A procedure is
a frame, not a block the end keyword could be crossing: `:ENDIF` with
nothing open inside a procedure is stray exactly as it is at top level.
The range covers the end keyword. `:ENDFOR` is never reported here — it
is `endfor_invalid`'s.

Gated by the `CheckUnclosedBlocks` option, which defaults on and has no
user-facing configuration key; per-rule severity/off is available through
`ssl.diagnostics.rules`.

It must NOT flag:

- a correctly closed block of any kind;
- an end keyword that does not match the innermost open block inside the
  procedure: if a matching opener exists deeper in the stack the
  intervening openers are reported as `unclosed_block` (recovery),
  otherwise the end keyword is reported as `mismatched_block_end` — never
  as this code;
- an opener left open at end-of-file — that is `unclosed_block`.

## Examples

### Flags

```ssl
nCount := 1;
:ENDIF;
```

### Flags

```ssl
:PROCEDURE Demo;
	nCount := 1;
:ENDIF;
:ENDPROC;
```

### Does not flag

```ssl
:IF nCount > 0;
	nCount := 1;
:ENDIF;
```

### Does not flag

```ssl
:WHILE nCount > 0;
	nCount := 1;
:ENDIF;
```

## Rationale

A block terminator with nothing open is unambiguous dead syntax — usually
the debris of a deleted opener — so it is an error on the terminator
itself. The empty-stack case is deliberately separated from the
wrong-block case (`mismatched_block_end`) and the never-closed case
(`unclosed_block`) so each message points at the token the user must fix;
the third fence pins that an `:ENDIF` against an open `:WHILE` does not
leak into this rule. The second Flags fence pins the procedure frame
(issue #255): a stray closer means the same thing inside a procedure as
outside one.
