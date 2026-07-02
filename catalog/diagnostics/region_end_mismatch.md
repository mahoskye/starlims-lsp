---
id: diag.region_end_mismatch
title: Region end marker does not match an open region
kind: diagnostic
status: planned
authority: tool
schema_ref: null
default_severity: warning
severity_overridable: true
suppressible: true
tests: []
history:
  - date: 2026-07-01
    ref: "catalog feature.folding A5/A6"
    note: >-
      Specified while cataloging folding behavior: endregion pairing is
      currently name-blind, so mislabeled nested regions silently fold the
      wrong span. This diagnostic surfaces the mismatch instead.
issues: []
---

## Behavior

Flags a `/* endregion <name>;` marker whose name (case-insensitive) matches
no currently open `/* region <name>;`, and any `/* endregion;` with no open
region at all. Named markers that match an open region — even a non-innermost
one — do not flag; bare `endregion` markers with at least one open region do
not flag.

## Examples

### Flags

```ssl
/* region Helpers;
:PROCEDURE Helper;
:ENDPROC;
/* endregion Utils;
```

(As a `planned` entry, the Flags fence runs as an expected failure until
the rule is implemented; the Does-not-flag fence holds trivially today and
must keep holding after implementation.)

### Does not flag

```ssl
/* region Helpers;
:PROCEDURE Helper;
:ENDPROC;
/* endregion Helpers;
```

## Rationale

Region markers are author-written structure; a typo'd or misordered
`endregion` currently gets silently absorbed by name-blind LIFO pairing and
the editor folds the wrong span with no signal. Warning severity: the code
still runs (comments have no runtime effect), but navigation is corrupted.
Implemented together with name-aware pairing in feature.folding A5/A6.
