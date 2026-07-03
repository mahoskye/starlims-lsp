---
id: fmt.max_consecutive_blank_lines
title: Cap consecutive blank lines
kind: formatter
status: active
authority: style_only
schema_ref: null
config:
  - ssl.format.maxConsecutiveBlankLines
spec_options:
  max_consecutive_blank_lines: 1
tests:
  - internal/providers/formatting_test.go
history:
  - date: 2026-04-30
    ref: "PR #4 (v0.5.0)"
    note: Added as a formatter post-pass; 0 disables the cap (default).
  - date: 2026-07-02
    ref: "issue #37"
    note: >-
      The streaming formatter no longer collapses source blank-line runs to
      one; 0 now genuinely preserves vertical whitespace, and caps > 1 are
      reachable. Capping (when set) is done entirely by the post-pass.
issues: ["#37"]
---

## Behavior

When `ssl.format.maxConsecutiveBlankLines` is greater than 0, a post-format
pass collapses any run of more than that many consecutive blank
(whitespace-only) lines to exactly the threshold. The default is 0: the cap
is disabled and source blank-line runs pass through the formatter
unchanged — the streaming formatter emits exactly the newlines the source
had, so authors keep multi-blank-line groupings through a format (issue
#37). Intermediate caps work too: a cap of 2 turns a five-blank-line run
into two, leaving one- and two-blank runs alone.

Procedure boundaries are the exception: fmt.blank_lines_between_procs
normalizes the run between `:ENDPROC;` and the next `:PROCEDURE` to its own
configured count regardless of this setting (this pass, when enabled, then
caps that too).

The fences below run with the cap at 1 via `spec_options`; the
preserve-at-0 and cap-at-2 behaviors need non-default options that
`spec_options` (entry-wide) cannot express per-fence, so they are pinned by
Go tests (`TestFormat_MaxConsecutiveBlankLines_ZeroPreservesSourceRuns`,
`TestFormat_MaxConsecutiveBlankLines_CapTwo` in
internal/providers/formatting_test.go).

## Examples

Source runs of blank lines collapse to the cap (1 here):

### Before

```ssl
nFirst := 1;



nSecond := 2;
```

### After

```ssl
nFirst := 1;

nSecond := 2;
```

A cap of 1 keeps the single normalized blank line at a procedure boundary:

### Before

```ssl
:PROCEDURE First;
:ENDPROC;

:PROCEDURE Second;
:ENDPROC;
```

### After

```ssl
:PROCEDURE First;
:ENDPROC;

:PROCEDURE Second;
:ENDPROC;
```

## Rationale

The cap exists for teams that want vertical whitespace normalized (PR #4,
v0.5.0). It runs after fmt.blank_line_between_blocks and
fmt.blank_lines_between_procs, so it bounds what those passes insert as
well as what the author wrote. Until issue #37 the streaming formatter
unconditionally collapsed source blank runs to one blank line, which made
`0` and `1` indistinguishable and caps above 1 unreachable; the collapse
now lives only in this opt-in post-pass, so the documented "0 preserves"
contract holds.
