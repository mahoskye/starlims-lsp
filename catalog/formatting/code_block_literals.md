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
history:
  - date: 2026-07-23
    ref: "issue #91"
    note: >-
      Normalization introduced. Code blocks lexed as atomic tokens and
      passed through verbatim before this; the schema's canonical
      `{|params| expression}` form (R42) makes the shape authoritative.
issues: ["#91"]
---

## Behavior

A code-block literal canonicalizes to the schema's `{|params| expression}`
form (R42): parameters trimmed and separated by comma-space, exactly one
space after the closing `|`, and the body spaced under the configured
operator/comma rules (dot logical operators also take their canonical
uppercase, fmt.keyword_case). String content inside the body is preserved
verbatim (D7), and unary signs stay glued to their operands.

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

## Rationale

The schema names one canonical shape (R42), so the formatter may apply it
mechanically (D12). The atomic-token passthrough predated any decision —
the conformance review (issue #91) surfaced it as an undocumented gap and
the normalize-per-schema option was chosen. The conservative bail-outs
exist because a code block is executable data: a normalization that could
change what `Eval` sees is worse than no normalization.
