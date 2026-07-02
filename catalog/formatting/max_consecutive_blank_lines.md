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
issues: []
---

## Behavior

When `ssl.format.maxConsecutiveBlankLines` is greater than 0, a post-format
pass collapses any run of more than that many consecutive blank
(whitespace-only) lines to exactly the threshold. The default is 0: the
post-pass is disabled.

Independently of this setting, the streaming formatter itself never emits
more than one consecutive blank line from source whitespace — runs of blank
lines in the *input* are always collapsed to one, even at the default 0
(see Known gaps). The only place more than one blank line survives in
formatter output is the procedure boundary, where
fmt.blank_lines_between_procs adds a newline on top of a preserved blank
line; a cap of 1 (the setting exercised by these fences, via
`spec_options`) trims that back to a single blank line.

## Examples

Source runs of blank lines collapse (here under cap 1; the streaming
formatter produces the same result at any setting):

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

With the cap at 1, the two blank lines that fmt.blank_lines_between_procs
would otherwise leave at an already-separated procedure boundary collapse
to one:

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
well as what the author wrote.

## Known gaps

- The documented contract ("0 disables the cap — existing vertical
  whitespace is preserved") is not what happens: the streaming formatter
  unconditionally collapses source blank-line runs to a single blank line
  before the post-pass ever runs, so `0` and `1` are indistinguishable for
  source whitespace. Authors cannot keep two-blank-line groupings through a
  format. No executable fence can pin this here because `spec_options`
  apply entry-wide (this entry runs with the cap at 1); the collapse itself
  is pinned by the first Before/After pair above.
