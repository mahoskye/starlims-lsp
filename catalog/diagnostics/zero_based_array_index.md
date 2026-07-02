---
id: diag.zero_based_array_index
title: Array indexed with literal 0
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
  - date: 2026-02-02
    ref: "commit 7261172"
    note: >-
      Introduced in the gotcha-detection batch (gotcha #5): SSL arrays are
      1-based, so [0] is always a bug.
  - date: 2026-04-30
    ref: "PR #3 (v0.4.0, commit d744511)"
    note: Stable diagnostic code assigned; behavior unchanged.
issues: []
---

## Behavior

Flags the subscript `[0]` — a `[` whose previous significant token is an
identifier and whose bracketed content is exactly the literal `0`
(whitespace/comments tolerated). SSL arrays and string positions are
1-based (ssl-style-guide: `language.arrays: one_based`), so index 0 can
never address an element. The range covers the `0` literal.

The rule is deliberately literal-only and must NOT flag:

- any other literal index (`aItems[1]`, `aItems[10]`);
- a variable or expression index (`aItems[nIndex]`, `aItems[0 + 1]`) —
  even when it could evaluate to 0, only the bare literal is provable;
- a `[` not preceded by an identifier (e.g. inside an array literal), where
  there is no subscripted variable.

## Examples

### Flags

```ssl
:DECLARE aItems, sFirst;
sFirst := aItems[0];
```

### Does not flag

```ssl
:DECLARE aItems, sFirst;
sFirst := aItems[1];
```

### Does not flag

```ssl
:DECLARE aItems, sFirst, nIndex;
sFirst := aItems[nIndex];
```

### Does not flag

```ssl
:DECLARE aItems, sFirst;
sFirst := aItems[0 + 1];
```

## Rationale

The zero-index habit from 0-based languages is one of the most common SSL
porting mistakes (gotcha #5, commit 7261172), and a literal `[0]` has no
legitimate reading in a 1-based language — hence error severity despite
this being a heuristic token scan. Restricting the match to the exact
literal keeps the guarantee airtight: everything short of provable
(computed indices, variables) is left alone, which the third and fourth
fences pin.
