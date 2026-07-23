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
    ref: "issue #85"
    note: >-
      No-gain guard: a wrap only happens when the token actually fits on
      its continuation line, and never on a line holding only
      indentation. Previously an over-long atomic string was moved below
      its assignment and every re-format grew a blank line.
issues: ["#85"]
---

## Behavior

When a token would push the current line past `ssl.format.maxLineLength`
(default 90; 0 disables wrapping), the formatter breaks the line at the
nearest legal wrap point and indents the continuation exactly one level
past the statement (fixed, not proportional to paren depth; a tab counts as
`indentSize` columns). Legal wrap points:

- after a comma;
- after `:=`;
- before an identifier or keyword inside parentheses/braces/brackets;
- before the logical operators `.AND.` / `.OR.` / `.NOT.`, arithmetic
  operators, compound assignments, and `$`.

Wrapping never happens: immediately before or after a member-access `:`
(fmt.atomic_property_chains); before comparison operators (they bind to
their operands); or inside a string literal or comment — string tokens are
atomic, so a single long string argument leaves the line over-long rather
than being split or moved — a wrap that would not bring the token within
the limit is not taken at all, and a line holding only indentation is
never wrapped (issue #85). A string about to be reflowed as multi-line SQL
(fmt.sql_in_strings) is also not wrapped before — the SQL engine manages
its own line breaks.

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

## Rationale

The 90-column default matches the style guide's `max_line_length`. The
never-split list encodes hard-won regressions — splitting strings or
member-access chains (issue #16) changes how the code reads or what it
means, so correctness of meaning beats column compliance; a line that can
only be shortened by such a split stays long.
