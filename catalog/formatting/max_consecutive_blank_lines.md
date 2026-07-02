---
id: fmt.max_consecutive_blank_lines
title: Cap consecutive blank lines
kind: formatter
status: draft
authority: style_only
schema_ref: null
config:
  - ssl.format.maxConsecutiveBlankLines
tests:
  - internal/providers/formatting_test.go
history:
  - date: 2026-05-01
    ref: "PR #4 (v0.5.0)"
    note: Added as a formatter post-pass; 0 disables the cap (default).
issues: []
---

## Behavior

When `ssl.format.maxConsecutiveBlankLines` is greater than 0, runs of blank
lines longer than the threshold are collapsed to exactly the threshold. The
default is 0: no cap, existing vertical whitespace is preserved. Blank lines
inside multi-line strings and comments are never collapsed.

## Examples

### Before

```ssl
:DECLARE nFirst;
:DECLARE nSecond;
```

### After

```ssl
:DECLARE nFirst;
:DECLARE nSecond;
```

## Rationale

Opt-in because vertical whitespace is often intentional grouping; the cap
exists for teams that want normalization (PR #4). Interacts with
fmt.blank_line_between_blocks, which inserts singles regardless of the cap.
