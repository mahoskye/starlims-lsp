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
    ref: "issues #26/#27"
    note: >-
      Endregion pairing made name-aware (A5/A6, with diag.region_end_mismatch
      implemented alongside); unclosed :PROCEDURE now folds to end of file
      (A4); single-line procedures and region pairs produce no range.
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

Region end markers must pair by name:

- `/* endregion <name>;` closes the innermost open region whose name matches
  case-insensitively — not blindly the innermost open region.
- A bare `/* endregion;` (no name) closes the innermost open region.
- An `endregion` whose name matches no open region closes nothing; the
  mismatch is surfaced by the `diag.region_end_mismatch` diagnostic rather
  than silently mis-folding an unrelated region.

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
- A5: Given nested named regions `/* region Outer;` `/* region Inner;` `/* endregion Outer;` `/* endregion Inner;` (mismatched close order), when folding ranges are requested, then `endregion Outer` closes the Outer region (name match), not the innermost Inner region.
- A6: Given `/* region A;` … `/* endregion B;` where no region named B is open, when folding ranges are requested, then the endregion closes nothing and region A stays open to end of file (surfaced by diag.region_end_mismatch).
- A7: Given a multi-line comment that is not a region marker, when folding
  ranges are requested, then it folds with kind `comment`, not `region`.

## Rationale

Folding is the primary way large SSL scripts stay navigable, and region
comments are the author's own structure — so the fold must respect the
author's names. The pairing in `internal/providers/symbols.go`
extractRegions matches a named `endregion` to the innermost open region of
the same name, case-insensitively (issues #26/#27, fixed 2026-07-02) — a
mislabeled or misordered marker closes nothing and is surfaced by
`diag.region_end_mismatch` instead of silently folding the wrong span.

## Known gaps

- `:CLASS` blocks are not foldable.
