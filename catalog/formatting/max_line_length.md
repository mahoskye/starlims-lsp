---
id: fmt.max_line_length
title: Maximum line length and wrapping
kind: formatter
status: draft
authority: style_only
schema_ref: null
config:
  - ssl.format.maxLineLength
tests:
  - internal/providers/formatting_test.go
history: []
issues: []
---

## Behavior

Lines longer than `ssl.format.maxLineLength` (default 90; 0 = unlimited) are
wrapped at argument and expression boundaries, with continuations indented
one level past the statement. Wrapping never splits: string literals,
comments, member-access chains (see fmt.atomic_property_chains), or SQL
parameter placeholders. A line that cannot be shortened without such a split
is left long.

## Examples

### Before

```ssl
DoProc("CalculateTotals", {nFirstQuarterRevenue, nSecondQuarterRevenue, nThirdQuarterRevenue, nFourthQuarter});
```

### After

```ssl
DoProc("CalculateTotals", {nFirstQuarterRevenue, nSecondQuarterRevenue,
	nThirdQuarterRevenue, nFourthQuarter});
```

## Rationale

The 90-column default matches the style guide's max_line_length. The
never-split list encodes hard-won regressions (extension #31/#33: flattened
intentional structure; #16: split chains) — correctness of meaning beats
column compliance.
