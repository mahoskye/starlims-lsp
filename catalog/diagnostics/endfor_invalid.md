---
id: diag.endfor_invalid
title: ":ENDFOR used to terminate a FOR loop"
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
  - date: 2026-03-28
    ref: "commit be7a174"
    note: >-
      Introduced in the keyword-form check: :ENDFOR is a recognized token
      but not a valid SSL keyword; FOR loops terminate with :NEXT.
  - date: 2026-03-30
    ref: "commit f6e78ef"
    note: >-
      Data source keyword-form variant added with identical :ENDFOR
      handling, so data source files get the same message.
  - date: 2026-04-30
    ref: "PR #3 (v0.4.0, commit d744511)"
    note: Stable diagnostic code assigned; behavior unchanged.
issues: []
---

## Behavior

Flags every colon-prefixed `:ENDFOR` keyword token (case-insensitive, so
`:endfor` also fires) with the message that FOR loops must be terminated
with `:NEXT`. `:ENDFOR` is special-cased out of the generic
`unknown_keyword` rule because the intent is unambiguous and deserves a
targeted fix-it message at error severity. Two emit sites exist — the
normal and the data-source keyword-form checks — with identical severity
and message, so the rule behaves the same in both file kinds.

It must NOT flag:

- FOR loops correctly terminated with `:NEXT`;
- the word ENDFOR appearing as an identifier, in strings, or in comments —
  only colon-prefixed keyword tokens are inspected.

## Examples

### Flags

```ssl
:PROCEDURE Demo;
	:DECLARE i;
	:FOR i := 1 :TO 10;
		i := i + 1;
	:ENDFOR;
:ENDPROC;
```

### Does not flag

```ssl
:PROCEDURE Demo;
	:DECLARE i, nTotal;
	nTotal := 0;
	:FOR i := 1 :TO 10;
		nTotal := nTotal + i;
	:NEXT;
:ENDPROC;
```

## Rationale

`:ENDFOR` is what every ENDIF/ENDWHILE-symmetric language would use, so it
is a high-frequency guess that SSL rejects — the loop terminator is `:NEXT`.
Rather than the generic "Unknown SSL keyword" warning, this earns a
dedicated error with the exact replacement (be7a174), and the data source
variant (f6e78ef) keeps the message consistent everywhere. Note the
unterminated `:FOR` will typically also raise an unclosed-block diagnostic;
that is a separate rule.
