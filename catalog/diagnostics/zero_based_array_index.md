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
  - date: 2026-08-08
    ref: "issue #152"
    note: >-
      Contextual severity added: when the subscripted identifier is
      reached through colon member access (`dataSet:Tables[0]`), the
      value may be a .NET collection with zero-based indexing, so the
      diagnostic downgrades to a warning with .NET-aware wording. Bare
      identifiers (`aValues[0]`) keep the error.
issues: []
---

## Behavior

Flags the subscript `[0]` — a `[` whose previous significant token is an
identifier and whose bracketed content is exactly the literal `0`
(whitespace/comments tolerated). SSL arrays and string positions are
1-based (ssl-style-guide: `language.arrays: one_based`), so index 0 can
never address a native SSL element. The range covers the `0` literal.

Severity is contextual (issue #152): when the subscripted identifier is
itself preceded by the colon member-access punctuation (`:`), as in
`dataSet:Tables[0]` or chained `object:Property[0]`, the indexed value is
reached through object member access and may be a .NET collection, which
is 0-based — the diagnostic is a warning whose message says zero-based
indexing may be valid for .NET collections. A bare identifier subscript
(`aValues[0]`) is a native SSL array access and stays an error. The
diagnostic code is the same in both contexts.

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

### Flags

```ssl
:DECLARE dataSet, oTable;
dataSet := GetDataSet();
oTable := dataSet:Tables[0];
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
legitimate reading on a native SSL array — hence error severity despite
this being a heuristic token scan. Values reached through colon member
access are different (issue #152): SSL code routinely holds .NET objects
whose collections (`dataSet:Tables[0]`) are legitimately 0-based, so an
error there is a false positive — but a warning is kept because the
member could still be a native SSL array and the reader deserves the
nudge. Restricting the match to the exact literal keeps the guarantee
airtight: everything short of provable (computed indices, variables) is
left alone, which the variable and expression fences pin.
