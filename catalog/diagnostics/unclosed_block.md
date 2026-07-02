---
id: diag.unclosed_block
title: Block opened but never closed
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
  - date: 2026-01-10
    ref: "commit 442fa69 (initial commit)"
    note: >-
      checkUnclosedBlocks shipped in the initial commit, including the
      recovery path that reports skipped openers when an end keyword
      matches a block deeper in the stack.
  - date: 2026-04-30
    ref: "PR #3 (v0.4.0, commit d744511)"
    note: Stable diagnostic code assigned; behavior unchanged.
issues: []
---

## Behavior

Flags a block opener that is never closed. The tracked pairs are
`:IF`/`:ENDIF`, `:WHILE`/`:ENDWHILE`, `:FOR`/`:NEXT`,
`:BEGINCASE`/`:ENDCASE`, `:BEGININLINECODE`/`:ENDINLINECODE`,
`:TRY`/`:ENDTRY`, `:PROCEDURE`/`:ENDPROC`, `:REGION`/`:ENDREGION`.
The range covers the opener and the message names the expected terminator
(`Unclosed ':IF' - expected ':ENDIF'`).

Two paths emit this code:

- **End of file**: every opener still on the stack when the file ends is
  reported.
- **Recovery**: when an end keyword does not match the innermost open
  block but does match one deeper in the stack, every opener above the
  match is reported as unclosed and the stack unwinds to the match — so
  an `:IF` missing its `:ENDIF` inside a procedure is caught at the
  `:ENDPROC`, on the `:IF` line, without cascading.

Gated by the `CheckUnclosedBlocks` option, which defaults on and has no
user-facing configuration key; per-rule severity/off is available through
`ssl.diagnostics.rules`.

It must NOT flag:

- correctly paired blocks, however deeply nested;
- a stray end keyword with nothing open — that is `unmatched_block_end`;
- an end keyword that matches no open block at any depth — that is
  `mismatched_block_end`, and no opener is popped.

## Examples

### Flags

```ssl
:IF nCount > 0;
	nCount := 1;
```

### Flags

```ssl
:PROCEDURE Demo;
	:IF nCount > 0;
		nCount := 1;
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE Demo;
	:IF nCount > 0;
		nCount := 1;
	:ENDIF;
:ENDPROC;
```

## Rationale

An unterminated block swallows the rest of the file, so this is an error
reported on the opener — the line the user must return to. The recovery
path is the important design decision: without it a single missing
`:ENDIF` would misalign every later block and produce a wall of
mismatches; with it the one real culprit is named and matching resumes at
the enclosing block, which is exactly what the second Flags fence pins.
