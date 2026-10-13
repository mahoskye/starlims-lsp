---
id: diag.stray_block_keyword
title: Middle clause keyword outside its block
kind: diagnostic
status: active
authority: authoritative
schema_ref: null
default_severity: error
severity_overridable: true
suppressible: true
tests:
  - internal/providers/audit255_test.go
history:
  - date: 2026-10-13
    ref: "issue #292 (catalog audit #255)"
    note: >-
      Added. A middle clause keyword outside its block validated clean:
      single_catch, single_finally, and catch_order_before_finally each
      said that placement was "owned by the block-structure checks", but
      no check reported it. Implemented in the block-pairing scanner,
      whose stack already knows what is open. 0 findings over 6,228
      production files.
issues: ["#292"]
---

## Behavior

Flags a middle clause keyword with no block of its kind open around it:

- `:ELSE` outside any `:IF`;
- `:CATCH` or `:FINALLY` outside any `:TRY`;
- `:CASE` or `:OTHERWISE` outside any `:BEGINCASE`.

The search for the owning block runs from the innermost open block
outward and stops at the enclosing `:PROCEDURE` or `:BEGININLINECODE` —
a procedure or inline-code unit is compiled on its own, so an `:IF`
opened outside it does not contain an `:ELSE` inside it. The range covers
the keyword; the message names the block it needs
(`':ELSE' is outside any ':IF' block`). Error severity.

Part of the block-pairing scanner (`CheckUnclosedBlocks`, on by default,
no user-facing configuration key); per-rule severity or `off` is
available through `ssl.diagnostics.rules`.

It must NOT flag:

- a middle keyword inside its block, however deeply nested in other
  blocks of that block (`:IF` containing a closed `:WHILE`, then `:ELSE`);
- a middle keyword whose block is open further out but crossed by an
  unclosed inner block (`:IF`, `:WHILE`, `:ELSE`) — the inner block is
  the mistake, and the end-keyword recovery reports it as
  `unclosed_block`;
- `:EXITCASE` — its placement is `exitcase_last_in_branch`'s;
- keyword text in strings, comments, or `:REGION` bodies.

## Examples

### Flags

```ssl
:PROCEDURE Demo;
:DECLARE n;
:CATCH;
n := 1;
:FINALLY;
n := 2;
:ENDPROC;
```

### Flags

```ssl
:DECLARE n;
n := 1;
:ELSE;
n := 2;
```

### Flags

```ssl
:DECLARE n;
:CASE n == 1;
	n := 2;
:OTHERWISE;
	n := 3;
```

### Flags

```ssl
:IF .T.;
:PROCEDURE Demo;
:ELSE;
:ENDPROC;
:ENDIF;
```

### Does not flag

```ssl
:PROCEDURE Demo;
:DECLARE n;
:IF n > 0;
	:WHILE n > 0;
		n := n - 1;
	:ENDWHILE;
:ELSE;
	n := 1;
:ENDIF;
:BEGINCASE;
:CASE n == 1;
	n := 2;
	:EXITCASE;
:OTHERWISE;
	n := 3;
	:EXITCASE;
:ENDCASE;
:TRY;
	n := 1;
:CATCH;
	n := 0;
:FINALLY;
	n := 2;
:ENDTRY;
:ENDPROC;
```

### Does not flag

```ssl
:DECLARE s;
/* :ELSE in a comment;
s := ":CATCH in a string";
```

## Rationale

These clauses exist only as parts of their blocks in the grammar
(`ConditionalStatement`, `TryBlock`, `SwitchStatement`), so one outside
its block cannot compile, and the error is reported on the clause itself
— the token the author must move or delete. A stray end keyword already had a
rule (`unmatched_block_end`); middle keywords fell through every check
(issue #292). Stopping the owner search at the procedure boundary is what
the fourth Flags fence pins. The crossed-block case is left to the
end-keyword recovery so one missing `:ENDWHILE` is reported once, on the
`:WHILE`, rather than again on every clause that follows it.
