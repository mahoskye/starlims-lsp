---
id: diag.array_index_whole_number
title: Array index that may not be a whole number
kind: diagnostic
status: active
authority: authoritative
schema_ref: null
default_severity: error
severity_overridable: true
suppressible: true
tests:
  - internal/providers/literal_semantics_test.go
history:
  - date: 2026-10-14
    ref: "issue #339"
    note: >-
      Introduced from the atoms-of-confusion review. Verified on STARLIMS
      12 (test batch P22): a non-integer index raises a generic invocation
      error (`a[2.5]`, `a[3 / 2]`, `a[1.9]`), while a whole-number
      quotient works (`a[4 / 2]`). `/` always divides exactly, so
      `aRows[nCount / 2]` fails whenever nCount is odd. Severity set by
      ssl-style-guide's array_index_whole_number (error).
issues: []
---

## Behavior

Flags an array subscript that is not guaranteed to be a whole number:

- a numeric literal with a fractional part (`aRows[2.5]`);
- a subscript computed with `/` (`aRows[nCount / 2]`,
  `aRows[(nCount + 1) / 2]`, `aRows[nCount / 2 + 1]`). The search looks
  through parentheses, unary operators, and arithmetic, but not into a
  call, a member access, or a nested subscript.

Each subscript of a multi-dimensional index is judged on its own. The
range covers the subscript. `/` always divides exactly in SSL (`3 / 2` is
`1.5`), and a non-integer index raises at run time, so the message
suggests `Integer()`.

It must NOT flag:

- a whole-number literal, including `2.0`;
- a division of two numeric literals that comes out whole (`a[4 / 2]`);
- a quotient inside a call, such as `a[Integer(n / 2)]`;
- other arithmetic (`a[n * 2]`, `a[n + 1]`);
- a division outside a subscript.

## Examples

### Flags

```ssl
:DECLARE aRows, nCount, vMiddle;
vMiddle := aRows[nCount / 2];
```

### Flags

```ssl
:DECLARE aRows, vItem;
vItem := aRows[2.5];
```

### Does not flag

```ssl
:DECLARE aRows, nCount, vItem;
vItem := aRows[Integer(nCount / 2)];
vItem := aRows[4 / 2];
vItem := aRows[2.0];
vItem := aRows[nCount * 2];
```

## Rationale

An index computed by division passes its tests on even sizes and raises on
the next odd one, so it is a real defect rather than a clarity concern; the
style guide sets error severity. The rule flags any quotient a call does
not wrap, because a reader cannot tell from the source that it divides
evenly, except for two literals whose quotient is visibly whole.
