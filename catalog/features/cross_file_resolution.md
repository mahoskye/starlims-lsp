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
  - internal/server/workspace_index_test.go
  - internal/server/uri_test.go
  - internal/server/uri_server_test.go
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
  - date: 2026-07-03
    ref: "include-aware diagnostics PR"
    note: >-
      Include declaration closure added (A18-A19): resolved :INCLUDE
      targets contribute their declared variable names to the including
      file's diagnostics, transitively with a cycle guard. Consumed by
      diag.undeclared_variable and diag.invalid_sql_param.
  - date: 2026-08-07
    ref: "issue #143 (ssl-style-guide#37)"
    note: >-
      Resolver audited against the namespace doctrine adopted upstream:
      applications and server-script categories share a single namespace,
      so a dotted path's first segment always names exactly one container.
      The resolver was already consistent — it models categories only and
      has no application-vs-category precedence hedging. (The 2-part
      Cat.Script vs Script.Proc dual interpretation is shape ambiguity
      within the language, not a namespace collision, and is unaffected.)
      Class-only dispatch classification (DispatchResolvesToClassOnly)
      added for diag.execfunction_class_target.
  - date: 2026-10-13
    ref: "issue #255"
    note: >-
      ProcedureInfo.IsPrivate now requires an annotation in force — the
      exact lowercase form on the line immediately before :PROCEDURE —
      matching diag.visibility_annotation. A near-miss spelling or an
      annotation detached by a blank line or another comment used to mark
      the procedure private (and hide it from completion) although the
      runtime leaves it public.
  - date: 2026-10-13
    ref: "issue #255 (features audit)"
    note: >-
      Truthful null enforced: when a rule finds the named script but the
      script lacks the procedure, neither the basename degradation nor the
      workspace-unique fallback runs (they used to land in an unrelated
      script, which rename then treated as an unambiguous caller). A7
      strengthened; class-only classification gets its criterion (A20).
  - date: 2026-10-13
    ref: "issue #304"
    note: >-
      The include closure reports whether it is complete (A23). A
      document whose :INCLUDE could not be read no longer gets
      undeclared_variable findings: on the production corpus's
      unmodified product code, 134 of the 210 --strict findings in files
      with an :INCLUDE named a variable the included script declares.
  - date: 2026-10-13
    ref: "issue #258"
    note: >-
      One canonical file URI (A21). The index keyed files by an unescaped
      URI built from the path while VS Code sends `%20`-escaped ones, so
      the open-document overlay never applied, cross-file rename skipped
      call sites, workspace symbols listed an open file twice, a didClose
      re-index added a second entry, and Delete events missed. On Windows
      the drive letter broke the path conversion both ways and the index
      found nothing. Index maintenance hardened (A22): a file whose parse
      panics is logged and skipped, watched-file re-indexing runs on the
      background worker, and a delete supersedes an in-flight parse.
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
  joined form the category); when the chain finds no script, degrade to
  script-basename match on the second-to-last segment.
- Final fallback for either shape: the last segment as a
  **workspace-unique** procedure name — applied only when no rule found
  the named script and exactly one candidate exists. Two or more
  candidates yield nothing (the uniqueness gate keeps flat-layout guessing
  quiet).
- **Truthful null**: a resolved script that lacks the named procedure
  contributes nothing, and its presence stops the later rules — neither
  the degradation nor the fallback runs — so resolution never lands
  "near" a target in some other script.
- **Class-only classification** (for `diag.execfunction_class_target`):
  a dotted target is class-only when it has at least one candidate and
  every candidate is a `:CLASS` file.

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

**Include declaration closure** (for diagnostics): `:INCLUDE` splices the
included script's full text, so every variable name the included script
declares (`:DECLARE`/`:PUBLIC`/`:PARAMETERS`, anywhere in the file) counts
as declared in the including file. The closure is transitive — included
scripts' own `:INCLUDE`s contribute too — with a visited-set cycle guard,
and an ambiguous include target contributes the union of all its
resolution candidates (erring toward fewer false positives). Unresolvable
targets contribute nothing. Open included documents contribute their
live-buffer declarations, consistent with the open-document overlay.
Consumers: `diag.undeclared_variable` and `diag.invalid_sql_param`.
The closure also reports whether it is complete: every `:INCLUDE` target
in it resolved to a readable file. A document with an `:INCLUDE` whose
closure is incomplete — a target that does not resolve, or no workspace
index at all, as in `--validate` — gets no `undeclared_variable`
findings, since the unread script may declare any name (DECISIONS.md
D10, issue #304). `invalid_sql_param` keeps its single-file behavior.

**Candidate ordering and cap**: anchored canonical-layout matches order
before flat matches, path-lexicographic within each group, capped at 10.

**Visibility**: `/*@private;` and `/*@protected;` procedures resolve for
navigation (following author intent); completion surfaces filter them out
separately (feature.completion, when cross-file completion lands). A
procedure counts as annotated only when the annotation is in force — the
exact form on the line immediately before `:PROCEDURE` — the same verdict
diag.visibility_annotation reports (issue #255).

**Open-document overlay**: resolutions into currently-open documents
re-derive their target line from the live document cache, so unsaved
edits never produce stale jump targets; a procedure deleted in the live
buffer drops that candidate entirely.

**File identity**: a file has one URI inside the server. Every URI a
client sends (document requests, watched-file events, workspace roots)
is canonicalized at the handler boundary, and the index builds its keys
from paths with the same function, so the open-document store and the
index always agree on which file is which. The canonical form is the
one VS Code sends: unreserved characters and `/` literal, every other
byte percent-encoded (`Server%20Scripts`), and on Windows a lower-case,
escaped drive letter (`file:///c%3A/...`). Converting back to a path
strips the slash before a Windows drive letter and maps a URI authority
to a UNC path. URIs in responses use the canonical form.

**Index maintenance**: the startup walk and every later re-index (a
watched-file Created or Changed event, a document closing) parse on a
bounded background worker, never on the request goroutine. A file whose
parse panics is logged to stderr and skipped. A Delete event, or a newer
re-index of the same file, supersedes any parse of it still in flight,
so a stale parse cannot re-add a deleted file.

## Acceptance

- A1: Given a file at `Server Scripts/LIMS_UTILS/HELPERS.srvscr` (any component casing), when identity is derived, then category is `LIMS_UTILS`, script is `HELPERS`, and the file is anchored.
- A2: Given `Applications/MYAPP/MYMODULE/Server Scripts/TASKS.srvscr`, when identity is derived, then category is `MYMODULE`.
- A3: Given a file outside any anchor (e.g. `repo/lib/Helpers.ssl.txt`), when identity is derived, then script is `Helpers` (longest extension stripped), with no category and no anchor.
- A4: Given an indexed script with a file-level `:PARAMETERS`, when a 2-part `Category.Script` target resolves, then the result is that script's entry point at the `:PARAMETERS` line (line 0 when the script has none).
- A5: Given a flat-layout file `Helpers.ssl` containing `:PROCEDURE CalculateTotal`, when `Helpers.CalculateTotal` resolves, then the result is that procedure's line in that file.
- A6: Given `Cat.Script.Proc` where the category is unknown to the workspace, when it resolves, then the script-basename degradation finds `Script` and returns procedure `Proc`.
- A7: Given a target whose script resolves but whose procedure does not exist in it, when it resolves, then NO result is returned — even when another script defines a workspace-unique procedure of that name.
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
- A18: Given file A with `:INCLUDE B;` where B declares `:PUBLIC gShared;` and B itself includes C declaring `:PUBLIC gDeep;`, when A's include declaration closure is computed, then it contains both `gShared` and `gDeep`; a cyclic include chain (C including A) terminates without error.
- A19: Given an include target resolving to two candidate files with different declarations, when the closure is computed, then it contains the union of both candidates' declared names; an unresolvable include target contributes nothing.
- A20: Given a dotted dispatch target whose every candidate is a `:CLASS` file, when class-only classification runs, then it is true; when any candidate is an ordinary script, or there is no candidate, then it is false.
- A21: Given workspace files under a path containing a space and a client that sends `%20`-escaped URIs (and, on Windows, `file:///c%3A/...` drive URIs), when the client opens, closes, renames across, or reports watched-file events for those files, then each file is indexed under exactly one key — the open-document overlay applies, cross-file rename reaches the closed callers, workspace symbols list an open file once, a didClose re-index leaves one entry, and a Delete event removes it — and the URIs in responses are the client's own spelling; Windows paths and URIs convert in both directions (drive letters, percent-encoding, UNC).
- A22: Given background indexing, when a file's parse panics, then the panic is logged and only that file is skipped; when watched-file Created/Changed events arrive, then the handler returns without parsing and the files are indexed on the background worker; and when a Delete event arrives while a re-index of the same file is queued or parsing, then the file is not added back.
- A23: Given a document with `:INCLUDE B;` and a name declared nowhere, when diagnostics run with undeclared checking on, then `undeclared_variable` flags the name only when B resolves through the workspace index (the closure is complete); when B does not resolve, or there is no workspace index, then the document gets no `undeclared_variable` finding, and a document without an `:INCLUDE` is checked as before (issue #304).

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
