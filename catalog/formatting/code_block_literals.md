---
id: fmt.code_block_literals
title: Code-block literal canonical form
kind: formatter
status: active
authority: authoritative
schema_ref: expressions.code_block_literals
config: []
tests:
  - internal/providers/formatting_test.go
  - internal/lexer/lexer_test.go
history:
  - date: 2026-07-23
    ref: "issue #91"
    note: >-
      Normalization introduced. Code blocks lexed as atomic tokens and
      passed through verbatim before this; the schema's canonical
      `{|params| expression}` form (R42) makes the shape authoritative.
  - date: 2026-10-13
    ref: "issue #255 (catalog audit)"
    note: >-
      The lexer counted braces inside string literals in the body: `"{"`
      swallowed the rest of the file (each format appended another ';')
      and `"}"` ended the literal early. String spans are now skipped.
      The body also takes keyword/literal casing, Me/Base receivers and
      built-in call casing; with commaSpacing off its comma spacing is
      left as written; an empty body gains no stray space; and the
      nested-block bail-out asks the lexer instead of searching for "{|".
issues: ["#91"]
---

## Behavior

A code-block literal canonicalizes to the schema's `{|params| expression}`
form (R42): parameters trimmed and separated by comma-space, exactly one
space after the closing `|`, and the body spaced under the configured
operator/comma rules (with `ssl.format.commaSpacing` off, the body's comma
spacing is left as written). The body takes the same canonical forms as
any statement: keyword and literal casing (`.and.` → `.AND.`, `.t.` →
`.T.`, `nil` → `NIL`, `me:` → `Me:`; fmt.keyword_case) and built-in call
casing (fmt.builtin_function_case). String content inside the body is
preserved verbatim (D7) — braces inside a string literal neither open nor
close the code block — and unary signs stay glued to their operands. An
empty body is written `{|params|}`.

Normalization is conservative: a literal that cannot be handled safely — a
missing parameter delimiter, a nested code block, or a multi-line body —
passes through byte-for-byte. The canonical shape must never risk the
literal's meaning.

## Examples

### Before

```ssl
fnAdd := {|a,b|a+b};
fnPred := {|x|x>=10.AND.x<=20};
```

### After

```ssl
fnAdd := {|a, b| a + b};
fnPred := {|x| x >= 10 .AND. x <= 20};
```

### Idempotent

```ssl
fnT := {|x| x * 2};
fnNeg := {|n| -n};
```

The body takes keyword, receiver and built-in casing; braces inside a
string are string content, and the statement after the literal still
formats normally (issue #255):

### Before

```ssl
fnP := {|o|me:IsReady(o).and.o!=nil.and.len(o)>0};
fnBrace := {|s|s+"{"};
aPair := {1,2};
```

### After

```ssl
fnP := {|o| Me:IsReady(o) .AND. o != NIL .AND. Len(o) > 0};
fnBrace := {|s| s + "{"};
aPair := {1, 2};
```

The bail-outs pass through verbatim — a nested code block and a
multi-line body:

### Idempotent

```ssl
fnOuter := {|x|Eval({|y|y+1}, x)};
fnMulti := {|x|x+
1};
```

## Rationale

The schema names one canonical shape (R42), so the formatter may apply it
mechanically (D12). The atomic-token passthrough predated any decision —
the conformance review (issue #91) surfaced it as an undocumented gap and
the normalize-per-schema option was chosen. The conservative bail-outs
exist because a code block is executable data: a normalization that could
change what `Eval` sees is worse than no normalization.
