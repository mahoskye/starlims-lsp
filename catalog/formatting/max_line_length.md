---
id: fmt.max_line_length
title: Maximum line length and wrapping
kind: formatter
status: active
authority: style_only
schema_ref: null
config:
  - ssl.format.maxLineLength
tests:
  - internal/providers/formatting_test.go
history:
  - date: 2026-01-10
    ref: "v0.1.0 initial release"
    note: Original wrap pass; 90-column default matches the style guide.
  - date: 2026-05-13
    ref: "PR #20 (v0.7.6), issue #16"
    note: Member-access chains excluded as wrap points (fmt.atomic_property_chains).
  - date: 2026-07-22
    ref: "issue #89"
    note: >-
      Wrapping rebuilt as a whole-line post-format pass: greedy
      latest-fitting packing with the conformance guarantee, subscript
      atomicity, and no breaks on lines touched by multi-line tokens.
      Replaces the token-streaming wrap that could only react at the
      overflowing token (post-wrap lines used to land at 92-107 columns,
      split subscripts, and break inside nested calls).
  - date: 2026-07-22
    ref: "issue #85"
    note: >-
      No-gain guard: a wrap only happens when the token actually fits on
      its continuation line, and never on a line holding only
      indentation. Previously an over-long atomic string was moved below
      its assignment and every re-format grew a blank line.
  - date: 2026-08-28
    ref: "issue #218 (production-corpus idempotence)"
    note: >-
      Wrap fragments of a line that is already an expression
      continuation (open delimiter at line start, operator-led, or
      following a line ending in ':='/','/binary operator) stay at that
      line's indent — the schema's fixed continuation_indent: 1 —
      instead of one deeper, which the second pass flattened back (784
      corpus files). The wrap engine also mirrors the stream's
      lastNonWSToken by ignoring end-of-line comments when classifying
      the next line, and the stream treats a trailing comma as statement
      continuation so wrapped declaration lists (:DECLARE a, b, / c;)
      keep their indent.
issues: ["#85"]
---

## Behavior

Wrapping is a post-format pass over whole physical lines (issue #89): a
line wider than `ssl.format.maxLineLength` (default 90; 0 disables; a tab
counts as `indentSize` columns) is re-flowed at its legal break points:

- after a comma (the comma trails its line);
- after `:=`;
- before a binary operator — `.AND.` / `.OR.` / `.NOT.`, arithmetic,
  compound assignments, and `$` — the operator leads its continuation
  line.

Packing is greedy latest-fitting: each output line keeps as much as fits,
and continuation lines sit exactly one indent level past the original line
(fixed, not proportional to paren depth — fmt.indent_style). A break is
only taken when the following segment actually fits within the limit on
its continuation line (the issue-#85 no-gain guard), so an over-long
atomic token — typically a long string — leaves its line over-long rather
than being split or moved.

Wrapping never happens: inside `[...]` subscripts (the index binds to its
array, the sibling rule to fmt.atomic_property_chains' member-access `:`);
before comparison operators (they bind to their operands); or on any line
touched by a multi-line token — multi-line strings, SQL reflowed by
fmt.sql_in_strings, and multi-line comments manage their own layout.

Guarantee: a line stays over the limit only when a single atomic token
exceeds the budget — never because a legal break sequence was missed.

## Examples

The array literal overflows at `nThirdQuarterRevenue`, so the line wraps at
the preceding comma with a one-level continuation indent:

### Before

```ssl
DoProc("CalculateTotals", {nFirstQuarterRevenue, nSecondQuarterRevenue, nThirdQuarterRevenue, nFourthQuarter});
```

### After

```ssl
DoProc("CalculateTotals", {nFirstQuarterRevenue, nSecondQuarterRevenue,
	nThirdQuarterRevenue, nFourthQuarter});
```

A single long string argument has no legal wrap point and is left over-long
(98 columns) rather than split:

### Idempotent

```ssl
DoProc("ThisIsAVeryLongProcedureNameArgumentThatByItselfPushesTheLineWellPastNinetyColumns");
```

A subscript is never split — the break moves to the comma before the
argument group; a nested call stays whole when an outer-list break exists;
a binary operator leads its continuation:

### Before

```ssl
vRes := CreateUdObject("MyNamespace.MyClassName", {oParentObject:ChildCollection[nChildIndex], sConfigurationKey});
```

### After

```ssl
vRes := CreateUdObject("MyNamespace.MyClassName",
	{oParentObject:ChildCollection[nChildIndex], sConfigurationKey});
```

### Before

```ssl
vRes := DoProc("Wrapper", {DoProc("InnerOne", {sArgumentOne, sArgumentTwo}), DoProc("InnerTwo", {sArgumentThree})});
```

### After

```ssl
vRes := DoProc("Wrapper", {DoProc("InnerOne", {sArgumentOne, sArgumentTwo}),
	DoProc("InnerTwo", {sArgumentThree})});
```

### Before

```ssl
oTargetObjectReference:SomePropertyName := oSourceObjectReference:OtherPropertyName + nAdjustmentValue;
```

### After

```ssl
oTargetObjectReference:SomePropertyName := oSourceObjectReference:OtherPropertyName
	+ nAdjustmentValue;
```

## Rationale

The 90-column default matches the style guide's `max_line_length`. The
never-split list encodes hard-won regressions — splitting strings or
member-access chains (issue #16) changes how the code reads or what it
means, so correctness of meaning beats column compliance; a line that can
only be shortened by such a split stays long.
