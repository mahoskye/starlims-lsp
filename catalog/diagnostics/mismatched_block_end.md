---
id: diag.mismatched_block_end
title: Block end keyword does not match the open block
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
    note: Present since the first commit as part of the unclosed-block scanner.
  - date: 2026-03-28
    ref: "commit be7a174"
    note: >-
      Stack recovery added: when a matching opener exists deeper in the
      stack, the blocks above it are reported as unclosed_block instead of
      reporting a mismatch, and scanning resynchronizes.
  - date: 2026-04-30
    ref: "PR #3 (v0.4.0, commit d744511)"
    note: Stable diagnostic code assigned; behavior unchanged.
issues: []
---

## Behavior

Part of the block-pairing scanner (`CheckUnclosedBlocks`, on by default).
Flags a block-end keyword (`:ENDIF`, `:ENDWHILE`, `:NEXT`, `:ENDCASE`,
`:ENDTRY`, `:ENDPROC`, `:ENDREGION`, `:ENDINLINECODE`) that closes a
different construct than the innermost open block, when no matching opener
exists anywhere below it on the block stack. The diagnostic is an error
ranged on the end keyword; the mismatched opener stays on the stack (and is
typically also reported as `unclosed_block` at end of file).

This code is reserved for the truly-crossed case. It must NOT fire when:

- blocks are properly paired and nested;
- a block-end appears with no open block at all — that is
  `unmatched_block_end`;
- the end keyword mismatches the innermost block but a matching opener sits
  deeper in the stack (e.g. `:WHILE` containing an unclosed `:IF` followed
  by `:ENDWHILE`) — the scanner recovers by reporting the intervening
  blocks as `unclosed_block` and consuming the matching opener instead
  (be7a174).

## Examples

### Flags

```ssl
:IF bReady;
	nCount := 1;
:ENDWHILE;
```

### Does not flag

```ssl
:IF bReady;
	nCount := 1;
:ENDIF;
```

### Does not flag

```ssl
nCount := 1;
:ENDIF;
```

### Does not flag

```ssl
:WHILE bReady;
	:IF nCount > 0;
		nCount := nCount - 1;
:ENDWHILE;
```

## Rationale

Crossed block terminators are unambiguous structural errors — the compiler
cannot pair them, so error severity is warranted. The value of this entry
is the three-way split with its sibling codes: `unmatched_block_end` (no
block open), `unclosed_block` (opener never closed), and this code (closer
crosses the innermost opener with no deeper match). The recovery behavior
added in be7a174 keeps one missing `:ENDIF` from cascading into a mismatch
report on every later terminator — the third Does-not-flag fence pins that.
