---
id: diag.region_end_mismatch
title: Region end marker does not match an open region
kind: diagnostic
status: active
authority: tool
schema_ref: null
default_severity: warning
severity_overridable: true
suppressible: true
tests:
  - internal/providers/providers_test.go
history:
  - date: 2026-07-01
    ref: "catalog feature.folding A5/A6"
    note: >-
      Specified while cataloging folding behavior: endregion pairing was
      name-blind, so mislabeled nested regions silently folded the wrong
      span. This diagnostic surfaces the mismatch instead.
  - date: 2026-07-02
    ref: "issues #26/#28, checkRegionEndMismatch"
    note: >-
      Implemented together with name-aware endregion pairing in
      extractRegions (feature.folding A5/A6).
issues: []
---

## Behavior

Flags a `/* endregion <name>;` marker whose name (case-insensitive) matches
no currently open `/* region <name>;`, and any `/* endregion;` with no open
region at all. Named markers that match an open region — even a non-innermost
one — do not flag; bare `endregion` markers with at least one open region do
not flag. An unnamed `/* region;` opens a region named "Region".

A mismatched marker closes nothing: the pairing in folding
(`feature.folding` A5/A6) skips it, and this diagnostic is the signal that
the author's region structure is broken.

## Examples

### Flags

```ssl
/* region Helpers;
:PROCEDURE Helper;
:ENDPROC;
/* endregion Utils;
```

### Flags

```ssl
:DECLARE nCount;
/* endregion;
```

### Does not flag

```ssl
/* region Helpers;
:PROCEDURE Helper;
:ENDPROC;
/* endregion Helpers;
```

### Does not flag

```ssl
/* region Outer;
/* region Inner;
:DECLARE nCount;
/* endregion Outer;
/* endregion Inner;
```

## Rationale

Region markers are author-written structure; a typo'd or misordered
`endregion` used to get silently absorbed by name-blind LIFO pairing and
the editor folded the wrong span with no signal. Warning severity: the code
still runs (comments have no runtime effect), but navigation is corrupted.
Implemented together with name-aware pairing in feature.folding A5/A6
(issue #26). The last Does-not-flag fence pins that out-of-order named
closes are legal — each name matches an open region, so nothing flags.
