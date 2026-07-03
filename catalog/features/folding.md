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
- Control-flow blocks that fit on a single line produce no range.
  (Single-line procedures and single-line region pairs currently return a
  degenerate zero-length range — see Known gaps; they should produce none.)
- An unclosed control-flow block folds to the end of the file.
  (`:PROCEDURE` currently does not — see Known gaps / A4.)

Region end markers must pair by name:

- `/* endregion <name>;` closes the innermost open region whose name matches
  case-insensitively — not blindly the innermost open region.
- A bare `/* endregion;` (no name) closes the innermost open region.
- An `endregion` whose name matches no open region closes nothing; the
  mismatch is surfaced by the `diag.region_end_mismatch` diagnostic
  (planned) rather than silently mis-folding an unrelated region.

## Acceptance

- A1: Given a document with a `:PROCEDURE` containing an `:IF` containing a
  `:WHILE`, when folding ranges are requested, then three separate `region`
  ranges are returned, one per block, each spanning its own start/end lines.
- A2: Given `/* region Helpers;` … `/* endregion;`, when folding ranges are
  requested, then a `region` range spans from the region line to the
  endregion line.
- A3: Given a single-line block `:IF x > 0; :RETURN x; :ENDIF;`, when folding
  ranges are requested, then NO range is returned for that line.
- A4: Given a `:PROCEDURE` with a missing `:ENDPROC`, when folding ranges are requested, then its range extends to the last line of the file. (planned)
- A5: Given nested named regions `/* region Outer;` `/* region Inner;` `/* endregion Outer;` `/* endregion Inner;` (mismatched close order), when folding ranges are requested, then `endregion Outer` closes the Outer region (name match), not the innermost Inner region. (planned)
- A6: Given `/* region A;` … `/* endregion B;` where no region named B is open, when folding ranges are requested, then the endregion closes nothing and region A stays open to end of file (surfaced by diag.region_end_mismatch). (planned)
- A7: Given a multi-line comment that is not a region marker, when folding
  ranges are requested, then it folds with kind `comment`, not `region`.

## Rationale

Folding is the primary way large SSL scripts stay navigable, and region
comments are the author's own structure — so the fold must respect the
author's names. The current implementation pairs endregion markers to the
innermost open region while discarding the name written on the marker
(`internal/providers/symbols.go` extractRegions), which silently accepts
mislabeled or misordered nested regions and folds the wrong span. A5/A6
specify the intended pairing; they are `(planned)` until the matcher is
name-aware, tracked with `diag.region_end_mismatch`.

## Known gaps

- Name-blind endregion pairing: `endregion <name>` never validates against
  the open region's name (symbols.go extractRegions builds end events with
  no Name). Covered by A5/A6 (planned); fix is a follow-up PR citing this
  entry.
- Unclosed `:PROCEDURE` does not fold to end of file: the parser assigns the
  unclosed procedure a degenerate single-line range (start == end, still
  returned to the client) instead of extending it to the last line. Unclosed
  control-flow blocks (`:IF` etc.) do extend to EOF, as
  TestGetFoldingRanges_UnclosedBlock shows. Covered by A4 (planned); fix is
  a follow-up PR citing this entry (starlims-lsp #27).
- Single-line `:PROCEDURE`s and single-line region marker pairs return
  degenerate zero-length ranges instead of none — only control-flow blocks
  are filtered by the single-line rule (buildFoldingRanges). Harmless in
  most clients but contrary to the contract above; fold into the #27 fix.
- `:CLASS` blocks are not foldable.
