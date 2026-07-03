---
id: feature.cross_file_resolution
title: Cross-file target resolution
kind: feature
status: active
authority: tool
schema_ref: null
config: []
tests:
  - internal/server/script_identity_test.go
  - internal/server/cross_file_test.go
history:
  - date: 2026-07-02
    ref: "cross-file milestone plan"
    note: >-
      Resolver subsystem introduced on top of the workspace index
      (be7a174): script identity derivation, the dispatch/include
      resolution chain, and the open-document overlay. First consumed by
      go-to-definition, hover, and completion in follow-up PRs.
  - date: 2026-07-03
    ref: "RunDS navigation PR"
    note: >-
      Data-source resolution added for RunDS targets (A15-A17), with the
      script/data-source partition: dispatch and include resolution no
      longer return data-source files, and RunDS resolution returns only
      them.
issues: []
---

## Behavior

The workspace index derives a **script identity** for every indexed file
and resolves dotted cross-file targets against it. All matching is
case-insensitive at every segment.

**Identity derivation** (path components, case-insensitive, tolerant of
Windows-style `/C:/...` paths):

- `.../Server Scripts/CATEGORY/SCRIPT.<ext>` → category `CATEGORY`,
  script `SCRIPT`, anchored. The analogous `Data Sources` anchor applies
  to data source files.
- `.../Applications/APP/MODULE/Server Scripts/SCRIPT.<ext>` → category
  `MODULE` (the app name is runtime context, not part of the namespace),
  anchored.
- Any other location → no category, script = basename minus its SSL
  extension (longest suffix first, so `NAME.ssl.txt` → `NAME`),
  unanchored — the flat-layout fallback.

**Dispatch resolution** (`DoProc`/`ExecFunction` string targets):

- 1-part targets are same-script by language semantics and never resolve
  cross-file. (A bare `ExecFunction("Name")` workspace fallback was
  deliberately omitted; revisit with usage evidence.)
- 2-part `A.B`: rule 1 — category `A` + script `B` → the script's **entry
  point** (the file-level `:PARAMETERS` line when present, else the first
  line); rule 2 — script-basename `A` containing procedure `B` → that
  procedure (the flat-layout form). When both rules hit, both candidate
  sets are returned.
- 3+-part `…Cat.Script.Proc`: category chain first (all leading segments
  joined form the category); when it yields nothing, degrade to
  script-basename match on the second-to-last segment.
- Final fallback for either shape: the last segment as a
  **workspace-unique** procedure name — applied only when no other rule
  hit and exactly one candidate exists. Two or more candidates yield
  nothing (the uniqueness gate keeps flat-layout guessing quiet).
- **Truthful null**: a resolved script that lacks the named procedure
  contributes nothing — resolution never lands "near" a target.

**Include resolution** (`:INCLUDE` targets, already unquoted): dotted
`Category.Script` uses the category chain, degrading to a basename match;
bare `Name` is a basename match. Targets resolve to the file (line 0).

**Data-source resolution** (`RunDS` string targets): dotted
`Category.Name` uses the category chain, degrading to a basename match;
bare `Name` is a basename match — 1-part RunDS targets DO resolve, unlike
dispatch targets, because a data source is always a separate file. Targets
resolve to the data-source file's entry (the file-level `:PARAMETERS` line
when present, else the first line).

**Script/data-source partition**: dispatch and include resolution consider
only non-data-source files; data-source resolution considers only
data-source files (`.ds`/`.ds.txt`). `RunDS` is the only dispatcher that
reaches data sources, and it reaches nothing else.

**Candidate ordering and cap**: anchored canonical-layout matches order
before flat matches, path-lexicographic within each group, capped at 10.

**Visibility**: `/*@private;` and `/*@protected;` procedures resolve for
navigation (following author intent); completion surfaces filter them out
separately (feature.completion, when cross-file completion lands).

**Open-document overlay**: resolutions into currently-open documents
re-derive their target line from the live document cache, so unsaved
edits never produce stale jump targets; a procedure deleted in the live
buffer drops that candidate entirely.

## Acceptance

- A1: Given a file at `Server Scripts/LIMS_UTILS/HELPERS.srvscr` (any component casing), when identity is derived, then category is `LIMS_UTILS`, script is `HELPERS`, and the file is anchored.
- A2: Given `Applications/MYAPP/MYMODULE/Server Scripts/TASKS.srvscr`, when identity is derived, then category is `MYMODULE`.
- A3: Given a file outside any anchor (e.g. `repo/lib/Helpers.ssl.txt`), when identity is derived, then script is `Helpers` (longest extension stripped), with no category and no anchor.
- A4: Given an indexed script with a file-level `:PARAMETERS`, when a 2-part `Category.Script` target resolves, then the result is that script's entry point at the `:PARAMETERS` line (line 0 when the script has none).
- A5: Given a flat-layout file `Helpers.ssl` containing `:PROCEDURE CalculateTotal`, when `Helpers.CalculateTotal` resolves, then the result is that procedure's line in that file.
- A6: Given `Cat.Script.Proc` where the category is unknown to the workspace, when it resolves, then the script-basename degradation finds `Script` and returns procedure `Proc`.
- A7: Given a target whose script resolves but whose procedure does not exist in it, when it resolves, then NO result is returned.
- A8: Given targets differing from the indexed names only by case, when they resolve, then they resolve identically to the exact-case forms.
- A9: Given two files matching the same target (one anchored, one flat), when it resolves, then both candidates are returned with the anchored match first.
- A10: Given a target matching no category or script rule whose last segment names a procedure found in exactly one workspace file, when it resolves, then that procedure is returned; given the same shape with the procedure in two files, then nothing is returned.
- A11: Given `:INCLUDE` targets `SharedLib` (bare) and `LIMS_UTILS.HELPERS` (dotted), when they resolve, then each yields the target file at line 0, with dotted targets degrading to a basename match when the category is unknown.
- A12: Given a resolution into a document that is open with unsaved edits, when it resolves, then the target line reflects the live buffer, and a procedure no longer present in the buffer is dropped from the candidates.
- A13: Given a `/*@private;`-annotated procedure, when a dispatch target names it, then it still resolves (navigation is not filtered).
- A14: Given a 1-part dispatch target, when it resolves, then the cross-file resolver returns nothing.
- A15: Given a data-source file at `Data Sources/QUERIES/ORDERS.ds`, when the data-source target `QUERIES.ORDERS` resolves, then the result is that file's entry (its file-level `:PARAMETERS` line when present, else line 0).
- A16: Given a flat-layout data-source file `Orders.ds`, when the 1-part data-source target `Orders` resolves, then the file is returned — 1-part data-source targets resolve by basename.
- A17: Given a script and a data source sharing a name, when a dispatch or include target names it, then only the script is returned; when a data-source target names it, then only the data source is returned.

## Rationale

The workspace index (be7a174) already held every procedure's location;
this entry gives it the identity layer that dispatch strings and
`:INCLUDE` targets actually name. The degradation chain exists because
the maintainer's real workspaces are frequently flat checkouts, not
canonical STARLIMS export trees — the canonical anchors are authoritative
when present, but resolution must not depend on them. The uniqueness gate
on the procedure-name fallback, the truthful-null rule, and the candidate
cap all guard the same value: navigation must never guess loudly. The
1-part `ExecFunction` fallback and any `ssl.workspace.*` layout settings
were considered and deferred until real usage argues for them;
`/*@protected;` is collapsed into the private flag until a reachability
difference is demonstrated.
