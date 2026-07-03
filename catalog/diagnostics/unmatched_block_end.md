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
tests: []
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
issues: []
---

## Behavior

Flags a block-end keyword (`:ENDIF`, `:ENDWHILE`, `:NEXT`, `:ENDCASE`,
`:ENDINLINECODE`, `:ENDTRY`, `:ENDPROC`, `:ENDREGION`) that arrives while
**no** block is open — the block stack is empty. The range covers the end
keyword.

Gated by the `CheckUnclosedBlocks` option, which defaults on and has no
user-facing configuration key; per-rule severity/off is available through
`ssl.diagnostics.rules`.

It must NOT flag:

- a correctly closed block of any kind;
- an end keyword that does not match the innermost open block: if a
  matching opener exists deeper in the stack the intervening openers are
  reported as `unclosed_block` (recovery), otherwise the end keyword is
  reported as `mismatched_block_end` — never as this code;
- an opener left open at end-of-file — that is `unclosed_block`.

## Examples

### Flags

```ssl
nCount := 1;
:ENDIF;
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
leak into this rule.
