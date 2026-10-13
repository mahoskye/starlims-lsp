---
id: diag.region_end_mismatch
title: Region end marker with no open region
kind: diagnostic
status: active
authority: tool
schema_ref: null
default_severity: hint
severity_overridable: true
suppressible: true
tests:
  - internal/providers/providers_test.go
history:
  - date: 2026-07-01
    ref: "catalog feature.folding A5/A6"
    note: >-
      First specified with name-matched pairing semantics (endregion <name>
      must match an open region's name).
  - date: 2026-07-02
    ref: "maintainer review of PR #52"
    note: >-
      Corrected: the canonical closer is a bare '/* endregion;' with no
      name (style guide module_structure syntax); trailing text is prose.
      The rule narrows to the one real error — an endregion with no open
      region — implemented alongside LIFO pairing in feature.folding.
  - date: 2026-10-13
    ref: "issue #255 (diagnostics audit, batch B)"
    note: >-
      Severity warning -> hint: an orphan endregion has no runtime effect, so
      by kind it is a style finding (the related style-guide record
      endregion_takes_no_name is info). The region opener now requires the
      whole word `region` (here and in feature.folding): `/* Regional ...;`
      used to open a region and so hid a later orphan endregion.
issues: []
---

## Behavior

Flags a `/* endregion;` marker when no `/* region;` is open, at hint
severity. The canonical
closer takes no name — any trailing text before the `;` is prose and is
ignored — so the only structural error an endregion can express is having
nothing to close. Pairing is innermost-first (LIFO), mirroring
`feature.folding`. Both markers are recognized only as whole words: a
comment such as `/* Regional settings;` opens nothing, so it cannot hide
an orphan endregion.

It must NOT flag:

- balanced `region`/`endregion` pairs, however deeply nested;
- endregion markers carrying trailing prose (e.g.
  `/* endregion Helpers;`) — prose does not participate in pairing;
- an unclosed `/* region;` (no endregion at all) — that region folds to
  end of file by design and is not an error this rule reports.

## Examples

### Flags

```ssl
/* endregion;
:PROCEDURE Helper;
:ENDPROC;
```

### Flags

```ssl
/* Regional settings follow;
:DECLARE nCount;
/* endregion;
```

### Does not flag

```ssl
/* region Helpers;
:PROCEDURE Helper;
:ENDPROC;
/* endregion;
```

### Does not flag

```ssl
/* region Outer;
/* region Inner;
:DECLARE nCount;
/* endregion;
/* endregion Outer is done here;
```

## Rationale

Region markers are author-written structure; an orphan `endregion` used to
be silently ignored, leaving the author unaware their markers are
unbalanced. Hint severity: the code runs unchanged (comments have no
runtime effect); only the editor's region structure is broken, which is a
style finding, not a likely bug. The rule
deliberately does NOT try to validate names: the maintainer confirmed the
canonical syntax is a nameless closer (`/* region Name; ... /* endregion;`
per the style guide), so trailing text is prose — the initial name-matching
specification of this rule was retired before release (history).
