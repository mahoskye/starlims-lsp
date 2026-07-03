---
id: feature.folding
title: Folding ranges
kind: feature
status: active
authority: tool
schema_ref: null
config: []
tests:
  - internal/providers/providers_test.go
history:
  - date: 2025-11-14
    ref: "commit 66fd645"
    note: Folding for IF/WHILE/FOR/CASE/TRY blocks added.
  - date: 2025-12-05
    ref: "vs-code-ssl-formatter #19"
    note: Region markers confirmed as first-class foldable elements.
  - date: 2026-06-29
    ref: "starlims-ssl-reference #12"
    note: >-
      :REGION/:ENDREGION keywords are current SSL, not legacy; distinct from
      /* region; comment markers, which remain the folding mechanism here.
  - date: 2026-07-02
    ref: "issues #26/#27, PR #52"
    note: >-
      Unclosed :PROCEDURE now folds to end of file (A4); single-line
      procedures and region pairs produce no range; orphan endregion
      surfaced by new diag.region_end_mismatch. A name-matching pairing
      scheme was drafted, then retired in maintainer review: the canonical
      closer '/* endregion;' takes no name — pairing is LIFO and trailing
      text on the closer is prose (A5/A6 rewritten accordingly).
issues: []
---

## Behavior

The server answers `textDocument/foldingRange` with one range per foldable
element:

- Block keywords, kind `region`: `:PROCEDURE`/`:ENDPROC`, `:IF`/`:ENDIF`,
  `:WHILE`/`:ENDWHILE`, `:FOR`/`:NEXT`, `:BEGINCASE`/`:ENDCASE`,
  `:TRY`/`:ENDTRY`. Nested blocks each get their own range.
- Region comment markers, kind `region`: `/* region <name>;` …
  `/* endregion;` (marker keyword case-insensitive). A region with no name
  is treated as named "Region".
- Multi-line comments (`/*` … `;` spanning lines), kind `comment`.
- Blocks that fit on a single line — control-flow, procedures, and region
  marker pairs — produce no range.
- An unclosed block (`:PROCEDURE`, `:IF`, ...) folds to the end of the
  file.

Region end markers pair innermost-first (LIFO):

- The canonical closer is a bare `/* endregion;` — it takes no name and
  closes the innermost open region.
- Trailing text before the closer's `;` (e.g. `/* endregion Helpers;`) is
  prose; it is ignored for pairing.
- An `endregion` with no open region closes nothing; the mistake is
  surfaced by the `diag.region_end_mismatch` diagnostic.

## Acceptance

- A1: Given a document with a `:PROCEDURE` containing an `:IF` containing a
  `:WHILE`, when folding ranges are requested, then three separate `region`
  ranges are returned, one per block, each spanning its own start/end lines.
- A2: Given `/* region Helpers;` … `/* endregion;`, when folding ranges are
  requested, then a `region` range spans from the region line to the
  endregion line.
- A3: Given a single-line block `:IF x > 0; :RETURN x; :ENDIF;`, when folding
  ranges are requested, then NO range is returned for that line.
- A4: Given a `:PROCEDURE` with a missing `:ENDPROC`, when folding ranges are requested, then its range extends to the last line of the file.
- A5: Given nested regions `/* region Outer;` `/* region Inner;` `/* endregion;` `/* endregion;`, when folding ranges are requested, then the first closer ends Inner and the second ends Outer (innermost-first pairing).
- A6: Given `/* endregion;` with no open region followed by a balanced `/* region A;` … `/* endregion;` pair, when folding ranges are requested, then the orphan closer closes nothing (surfaced by diag.region_end_mismatch) and region A still folds correctly.
- A7: Given a multi-line comment that is not a region marker, when folding
  ranges are requested, then it folds with kind `comment`, not `region`.

## Rationale

Folding is the primary way large SSL scripts stay navigable, and region
comments are the author's own structure. The canonical marker syntax is
`/* region Name; ... /* endregion;` (style guide module_structure): only
the opener carries a name, so pairing is necessarily innermost-first, and
any text an author writes after `endregion` is prose. A name-matching
scheme was drafted during cataloging and retired in maintainer review —
matching on prose would have turned harmless echo text into silent
mis-folds. The one structural error a closer can express — nothing open to
close — is surfaced by `diag.region_end_mismatch`.

## Known gaps

- `:CLASS` blocks are not foldable.
