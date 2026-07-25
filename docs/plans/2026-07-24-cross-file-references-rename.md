# Implementation Spec: Cross-file textDocument/references and textDocument/rename

> Tracking issue: [#125](https://github.com/mahoskye/starlims-lsp/issues/125). Rev 2, adversarially reviewed 2026-07-24; product decisions resolved — see §7.

Repo: /home/maho/dev/starlims-projects/starlims-lsp (Go LSP for STARLIMS SSL v11)
Date: 2026-07-24 (rev 2 — incorporates senior-engineer review F1-F9)

## 0. Corrected premise: what already exists

References and rename are NOT missing. Both are shipped, wired, and spec'd —
but explicitly **same-file only**, by normative catalog contract:

- `internal/server/server.go:236` — `capabilities.ReferencesProvider = true`
- `internal/server/server.go:252` — `capabilities.RenameProvider = &protocol.RenameOptions{...}` (with prepare support)
- `internal/server/handler.go:460` — `handleReferences` → `providers.FindReferencesWithScope` (single document)
- `internal/server/handler.go:875,906` — `handlePrepareRename` / `handleRename` → `providers.PrepareRename` / `providers.Rename` (single document)
- `internal/providers/definition.go:129` — `FindReferencesWithScope`: scope-aware, case-insensitive, whole-word,
  token-classified (comments/non-dispatch strings excluded, DoProc/ExecFunction dispatch strings included — issue #43),
  includeDeclaration honored from the parsed symbol (issue #42)
- `internal/providers/rename.go` — prepare-rename rejection rules, new-name validation, edits via the shared
  reference search, `Changes` map keyed by the single URI

Normative spec (behavior catalog is the source of truth, enforced by
`go test ./internal/catalog/`; note `catalog/README.md:93` — the `maxDrafts`
ratchet is at 0, so amended entries must land as `active`, fully reviewed,
never `draft`):

- `catalog/features/references.md` — "Results are single-file only; locations in other files MUST NOT be returned."
- `catalog/features/rename.md` — "All edits MUST be returned as a single WorkspaceEdit against the current document; no other files are modified."
- Pin test: `TestHandleReferences_StaySingleFileWithWorkspaceIndex`
  (`internal/server/cross_file_test.go:295`).

So this feature is a **spec amendment + extension**, not greenfield. The catalog
entries, their acceptance criteria, the tests that cite them, and the pin test
must change in the same PR as the code (the catalog forbids silent disagreement).

## 1. Existing cross-file infrastructure (survey)

### Workspace index — `internal/server/workspace_index.go`
- `WorkspaceIndex` holds `files map[string]*FileSymbols` plus secondary lookups
  `byScriptName`, `byCategory`, `byProcName` (all lowercase-keyed), maintained via
  `addToLookupsLocked`/`removeFromLookupsLocked` under one RWMutex.
- `FileSymbols` stores **definitions only**: `Procedures []IndexedProcedure`
  (name, params, start/end line, doc, IsPrivate), script identity
  (`ScriptName`, `Category`, `HasLayoutAnchor`), `EntryParameters`/`EntryParamsLine`,
  `DeclaredVars`, `IncludeTargets` (raw strings, **no positions**), `IsClass`, `IsDataSource`.
- **No usage/call sites are indexed.** This is the gap for definition→usages.
- Resolution chain (spec `catalog/features/cross_file_resolution.md`):
  `ResolveDispatchTarget` (2-part Category.Script → entry; Script.Proc flat; 3+-part
  category chain with basename degradation; workspace-unique-proc fallback with
  uniqueness gate; truthful null), `ResolveIncludeTarget`, `ResolveDataSourceTarget`
  (script/data-source partition). Anchored-first ordering, 10-candidate cap.

### Providers-side contract — `internal/providers/crossfile.go`
- `WorkspaceResolver` interface keeps providers pure (providers never import server).
  **Note (review F4): `ResolvedTarget` carries only URI/Line/Kind — `ProcName` and
  `IsEntry` are dropped at the overlay boundary (`cross_file.go:43-85`). The
  site→definition matcher therefore cannot ride this interface; see §3.2.**
- `DispatchTargetAt` / `DataSourceTargetAt` / `IncludeTargetAt`: cursor-position
  extraction of string targets. **`DispatchTargetAt` is line-regex based; multi-line
  and concatenated targets are explicitly out of scope (`crossfile.go:59-63`) — see F5.**
- `ExtractIncludeTargets(tokens)` — whole-file include extraction (strings only, no ranges).
- `FindDefinitionCrossFile` is the wiring model: dispatch/RunDS/include under cursor →
  resolver; 1-part dispatch stays same-file; fall through to word-based logic.
  Member access (`MemberAccessAt`, `crossfile.go:286-300`) resolves **same-file
  UDObject shapes only** — no cross-file class-method navigation exists today.

### Server glue — `internal/server/cross_file.go`
- `liveResolver` implements `WorkspaceResolver` over the index, nil-index-safe, with the
  **open-document overlay**: resolutions into open docs re-derive lines from the live
  parse cache; procedures deleted in the live buffer are dropped (truthful null).
- `includeDeclaredVariables`: transitive include closure with cycle guard — the model
  for any include-based traversal.

### Incremental updates
- `handleDidClose` re-indexes the closed file (`server.go:366`);
  `handleDidChangeWatchedFiles` (`server.go:380-393`) re-indexes/removes on watcher
  events (dynamic registration for `.srvscr .ssl .ssl.txt .ds .ds.txt`).
- `IndexFile` replaces a file's `FileSymbols` wholesale and rebuilds its lookup rows —
  any new per-file data added to `FileSymbols` gets incremental maintenance for free.
- `DocumentManager` (`internal/server/cache.go`): open-doc content + parse cache,
  `OpenURIs()`, `ParseDocument(uri, version)`.

### Two-phase pattern to copy
`handleWorkspaceSymbol` (`handler.go:~516-575`): phase 1 = open documents from the live
cache; phase 2 = workspace index skipping open URIs.

## 2. SSL semantics that drive the design

**(Reframed per review F1 — these are scoping decisions consistent with the
existing resolver spec, NOT complete language truths.)**

### 2.1 Cross-file reference channels in the language

SSL has no imports. Cross-script procedure calls happen through FOUR channels:

1. **String dispatch** — `DoProc("...")` / `ExecFunction("Cat.Script.Proc")`
   dotted targets, `RunDS("Cat.Name")`, `:INCLUDE`. Fully modeled by the
   resolver (`cross_file_resolution.md`).
2. **Class methods** — `CreateUdObject` instantiates a **user-defined class by
   script name** (`internal/constants/generated_functions.go`, `createudobject`:
   "Creates a dynamic object or instantiates a user-defined class"); method
   calls on the instance (`obj:MethodName()`) then reference procedures in the
   class script via **bare identifiers, not strings**. Likewise `Base:Method()`
   after `:INHERIT` reaches the base class script
   (`catalog/diagnostics/inherit_qualified_name.md`). The LSP does **not** model
   either today — `MemberAccessAt` covers same-file UDObject shapes only.
3. **`:INCLUDE` splicing** — an includer of the definition file splices its full
   text, so a **1-part `DoProc("Foo")` in an includer is a genuine runtime call**
   to the included file's `Foo`.
4. Same-file identifier references and 1-part dispatch (already handled).

### 2.2 v1 scoping decisions (not language claims)

- **Channel 1 (string dispatch) is the v1 scope.** This keeps references/rename
  in exact parity with what go-to-definition can already resolve.
- **Channel 2 (class methods)**: since definition/hover don't resolve cross-file
  `obj:Method()` either, **references** merely inherit an existing, documented
  blind spot — parity survives. **Rename does NOT survive it**: renaming a
  procedure in a class file silently breaks every cross-file `obj:Method()` /
  `Base:Method()` call site the design cannot see. Hence decision **D8**: gate
  cross-file rename for procedures in `IsClass` files (`FileSymbols.IsClass`,
  `workspace_index.go:43`, already indexed — the gate is cheap). Recommended:
  refuse cross-file rename for class-file procedures with a clear nil result
  (and, later, a `window/showMessage`); same-file rename inside the class file
  may remain, but note `Me:Method()` self-calls are bare identifiers already
  covered by the same-file search, while external callers are not.
- **Channel 3 (include splice)**: treating a 1-part `DoProc("Foo")` in another
  file as "not a reference" is a **deliberate v1 scoping decision** consistent
  with the resolver's 1-part rule (`cross_file_resolution.md:58-60`, A14;
  `workspace_index.go:497-499`), not a semantic fact. A reverse-include walk
  could close it later (the forward data — `IncludeTargets` — is indexed).
- Both blind spots (class methods, include-spliced 1-part calls) MUST be
  documented in the amended catalog entries under a **Known gaps** section so
  the contract is honest about what rename cannot see.
- **Case-insensitivity** at every segment; all matching lowercases already.
- **Variables**: locals/params procedure-scoped (same-file forever). `:PUBLIC`
  variables leak across files via `:INCLUDE` (the diagnostics closure models
  this); usage sites are unindexed. Defer to Phase C (D3).
- **Ambiguity is first-class**: references may be liberal; rename conservative.

## 3. Architecture

Index = candidate discovery. Request time = precise, fresh ranges.
The index answers "which files contain a call site whose target could name this
procedure"; locations/edits are recomputed from current content (live buffer for
open docs, disk read at rename time), which also solves stale-index correctness.

### 3.1 Data-structure changes

**`internal/providers/crossfile.go` — new whole-file call-site extractor:**

```go
type CallSiteKind int // CallDispatch, CallDataSource, CallInclude

type CallSite struct {
    Kind   CallSiteKind
    Raw    string   // unquoted dotted target, e.g. "LIMS_UTILS.HELPERS.CalculateTotal"
    Range  Range    // 0-based range of the string CONTENT (or include path)
}

func ExtractCallSites(tokens []lexer.Token) []CallSite
```

Token-walk implementation (not the line regex): identifier token `doproc` /
`execfunction` / `runds` (case-insensitive) → skip whitespace/comments → `(` →
skip → first `"..."`/`'...'` string token → emit site with the string-content
range. This is the forward form of the existing `isDispatchTargetMatch`
walk-back (`definition.go:334`) and handles multi-line calls the line regex
misses. Include sites reuse `includePathAfter` (already returns start/end
columns). Bracket strings and empty strings are not legal dispatch syntax —
skip. Concatenated targets (`"CAT." + sName`) are not extracted — pin with a
test (F8).

**F5 (extractor/cursor asymmetry):** `ExtractCallSites` is token-based, but
`DispatchTargetAt` (the cursor-side helper) is line-regex based, so references
would list a multi-line site that cannot itself initiate references/rename.
Resolve in the same change by **porting `DispatchTargetAt` to the token walk**
(preferred — one source of truth; reuse the extractor and pick the site whose
range contains the cursor). If deferred instead, the asymmetry MUST be stated
in the amended catalog entries.

**`internal/server/workspace_index.go` — extend `FileSymbols`:**

```go
type IndexedCallSite struct {
    Kind      int    // dispatch | datasource | include
    Raw       string
    Line      int    // 0-based
    StartChar int    // 0-based, string content start
    EndChar   int
}
// FileSymbols gains:
CallSites []IndexedCallSite
```

Populated in `IndexFile` from `providers.ExtractCallSites(tokens)` (tokens
already produced there). `IndexFile` replaces `FileSymbols` wholesale and
`RemoveFile` drops it, so incremental maintenance via didClose/watcher is
automatic. Optional `byCallLastSeg` secondary map: **skip in v1** (`SearchSymbols`
precedent — linear scan with a last-segment prefilter is the same cost class);
add only if profiling demands.

**New index query:**

```go
// CallSitesFor returns every indexed call site (skipping skipURIs) whose
// target's last segment equals lastSeg (case-insensitive), with its file URI.
func (wi *WorkspaceIndex) CallSitesFor(lastSeg string, kinds, skipURIs map[string]struct{}) []URICallSite
```

### 3.2 Matching a site to a definition (server side — F4 corrected wiring)

The matcher **cannot** go through `providers.WorkspaceResolver`: the overlay
conversion (`cross_file.go:43-85`) maps `IndexResolution` →
`providers.ResolvedTarget`, which has only URI/Line/Kind
(`crossfile.go:27-31`) — `ProcName` and `IsEntry` are dropped, and the matcher
needs both. Specify instead:

```go
// on liveResolver (server package), consuming IndexResolution directly:
// siteTargetsDefinition(siteRaw, defURI, procName) (matched, unambiguous bool)
//   res := wi.ResolveDispatchTarget(siteRaw)          // raw IndexResolutions
//   res = r.overlayResolutions(res)                   // see below
//   matched    = any candidate with URI==defURI && (equalFold(ProcName, procName) || (procName=="" && IsEntry))
//   unambiguous = len(res) == 1 && matched
```

`overlayResolutions` is a refactor of the existing `overlay` that applies the
open-document stale-procedure-drop logic (live-buffer re-derivation, truthful
null for procedures deleted in unsaved edits) but **returns `[]IndexResolution`
with `ProcName`/`IsEntry` intact**; the current `overlay` becomes a thin
ResolvedTarget-mapping wrapper over it. This guarantees references/rename agree
exactly with go-to-definition (same degradation chain, uniqueness gate,
truthful null, data-source partition, overlay semantics) without widening the
providers contract.

### 3.3 References (phase A)

Server-side orchestration (new `internal/server/references_crossfile.go`)
calling existing pure providers — the flow needs index scans, disk reads, and
live caches. Steps:

1. **Identify the subject.**
   - Cursor on a dotted dispatch string (`DispatchTargetAt`, token-ported per F5)
     → resolve to the definition (first candidate; zero candidates → same-file
     behavior only).
   - Cursor on an identifier → existing same-file logic; if it names a
     `:PROCEDURE` in this file, subject = (thisURI, procName).
   - Locals/params/:PUBLIC vars and everything else: **unchanged single-file
     path, return early** (v1 scope).
2. **Definition-file references.** Run the existing `FindReferencesWithScope`
   against the definition file's content — live cache if open, `os.ReadFile`
   otherwise — positioned at the `:PROCEDURE` declaration. **F2 (corrected):
   this alone is NOT sufficient.** `isDispatchTargetMatch`
   (`definition.go:334-367`) only accepts a string whose ENTIRE content equals
   the matched word — i.e. 1-part `DoProc("Foo")`. A dotted self-call
   `ExecFunction("CAT.SELF.Proc")` **inside the definition file** is invisible
   to the same-file search. Therefore: additionally run `ExtractCallSites` on
   the definition file, resolve-and-match each site via §3.2, and **dedupe**
   against the same-file result set (the only possible overlap is the 1-part
   whole-content case; dedupe by (line, startChar)). Without this, references
   miss dotted self-sites and rename leaves them un-edited → broken call.
3. **Cross-file call sites:**
   - Open documents (excluding the definition file, covered in step 2):
     `ExtractCallSites(cache.Tokens)` on each open doc's live parse.
   - Indexed files, skipping open URIs: `wi.CallSitesFor(procName, ...)`.
   - For each candidate site: `siteTargetsDefinition`. Ambiguous resolutions
     that *include* the target still count (D2 — precedent: definition already
     returns multi-candidate results, `handler.go:444-456`).
   - Location range = the string-content range (matches how same-file dispatch
     refs report today: full string content, `references.md` A7).
4. **includeDeclaration**: declaration = the `:PROCEDURE` line in the def file;
   existing issue-#42 logic handles exclusion; call sites are always uses.
5. **Cap** cross-file locations (constant, e.g. 500 — `maxSymbolResults` flavor).
6. Nil index → behavior identical to today.

Entry-point references (2-part `Cat.Script` sites, `:INCLUDE` sites, RunDS for
`.ds` files): the matcher supports them (`procName==""`/IsEntry); ship in
phase A if the subject-identification story is clean, else Phase C (D5).

### 3.4 Rename (phase B)

**prepareRename** (`providers.PrepareRename`): add a branch *before* the
string-context rejection: if `DispatchTargetAt` matches and the cursor sits
within the **last segment**, return range = last-segment range, placeholder =
last segment. Cursor on earlier segments (category/script) → still reject
(D4). Everything else unchanged. **F3: this contradicts current normative text
beyond the single-file line** — see §4 step 8 for the exact catalog/test
amendments that must land in the same PR.

**Rename orchestration** (server-side, `rename_crossfile.go`):

1. Resolve subject as references does. Non-procedure subjects → existing
   same-file `providers.Rename`, unchanged.
2. **D8 class-file gate (F1)**: if the definition file's `FileSymbols.IsClass`
   (or, for an open def file, `isClassFileFromTokens` on the live tokens) —
   **refuse cross-file rename** (recommended: return nil edits; follow-up:
   `window/showMessage` explaining that class-method call sites are invisible).
   Renaming a class procedure would silently break every cross-file
   `obj:Method()` / `Base:Method()` site the design cannot see.
3. Validate `newName` with existing `isValidIdentifier` — unchanged.
4. **Definition-file edits**: existing `providers.Rename` on the def file's
   current content (live or disk) **plus dotted self-site edits from the F2
   pass** (`ExtractCallSites` on the def file, last-segment replacement,
   deduped against the provider's whole-string 1-part edits).
5. **Cross-file edits**: for each matching site, **re-extract from current
   content at request time** — closed files are read from disk, tokenized,
   `ExtractCallSites` re-run, and only then is the edit computed. Never emit an
   edit from indexed positions alone (stale-index safety). Edit = replace the
   **last segment only**: `Range{Line, StartChar + len(raw) - len(lastSeg) ..
   EndChar}` from the fresh extraction; quotes and earlier segments untouched.
6. **Ambiguity rule (conservative)**: only edit sites whose resolution is a
   single candidate equal to the renamed definition; ambiguous sites skipped
   (D1).
7. Build `WorkspaceEdit.Changes` keyed by URI. Optional enhancement:
   `DocumentChanges` + `OptionalVersionedTextDocumentIdentifier` for open docs.
8. Nil index → exactly today's single-file rename.

### 3.5 Capability changes

None. `ReferencesProvider`/`RenameProvider` (+prepare) already registered.

## 4. Step-by-step implementation order

Phase A — references (independent, lower risk, ship first):
1. `providers.ExtractCallSites` + unit tests; **port `DispatchTargetAt` to the
   token walk in the same change (F5)**, keeping its existing cursor tests green.
2. `FileSymbols.CallSites` in `IndexFile`; `CallSitesFor`; tests via
   `newResolverIndex`/`writeAndIndex`.
3. `overlayResolutions` refactor of `liveResolver.overlay` (F4) +
   `siteTargetsDefinition`; overlay-semantics tests.
4. Rewire `handleReferences`: subject identification; def-file refs **including
   the F2 dotted-self-site pass with dedupe**; two-phase site scan. Replace the
   pin test `TestHandleReferences_StaySingleFileWithWorkspaceIndex` with
   cross-file assertions + a nil-index single-file regression test.
5. Amend `catalog/features/references.md` (status stays `active` — maxDrafts is
   0): drop the single-file MUST; add cross-file behavior + acceptance
   (proposed: A10 cross-file dispatch sites; A11 1-part exclusion as a scoping
   rule cross-referencing cross_file_resolution A14; A12 open-doc overlay; A13
   nil-index fallback; A14 dotted self-sites in the definition file (F2)); add a
   **Known gaps** section (class-method channel, include-spliced 1-part calls,
   concatenated strings); history entry; tests list. If D5 ships RunDS/:INCLUDE
   references, also amend `references.md:57-58` ("only legitimate string-context
   references are DoProc/ExecFunction first arguments") to enumerate the new
   string contexts (F3-adjacent). CHANGELOG; regenerate STATUS.md.

Phase B — rename:
6. `PrepareRename` dispatch-last-segment branch + tests.
7. Server-side rename orchestration: **D8 IsClass gate first**, F2 def-file
   self-site edits, fresh re-extraction, ambiguity gate, WorkspaceEdit assembly.
8. **Amend `catalog/features/rename.md` precisely (F3)** — three normative
   touchpoints beyond the single-file line, all in the same PR:
   - Behavior bullet (`rename.md:35-38`) "prepare-rename MUST reject … when the
     cursor is inside a string literal or comment" → rewritten to carve out the
     dispatch-target last segment (comments and all other strings still reject).
   - Acceptance **A4** (`rename.md:66`) → split: A4a cursor in a non-dispatch
     string/comment rejects; A4b cursor on a dispatch-target last segment
     returns the segment range and renames workspace-wide.
   - The citing tests `internal/providers/rename_test.go:98,114`
     ("prepare rename to fail inside string") → updated to non-dispatch strings,
     plus a new dispatch-segment success test.
   - Drop "no other files are modified"; add cross-file acceptance (WorkspaceEdit
     spans def + caller files; last-segment-only; ambiguity skip; **IsClass
     refusal (D8)**; nil-index fallback); Known gaps section mirroring
     references. History entry; CHANGELOG.

Phase C — optional follow-ups: entry-point/:INCLUDE/RunDS "who references this
file", `:PUBLIC`-via-include variable references/rename, reverse-include
1-part-call channel, class-method modeling (which would let D8 relax),
versioned document changes, `byCallLastSeg` fast path.

## 5. Test plan

Conventions: `internal/server/cross_file_test.go` temp-dir helpers
(`newResolverIndex`/`writeAndIndex`), `[spec feature.x/An]` comments; provider
unit tests in `internal/providers`.

Unit (providers):
- ExtractCallSites: single/multi-line, `DoProc`/`ExecFunction`/`RunDS`, both
  quote styles, exact ranges, comments between name and paren, no false sites
  from mentions in comments/strings; **concatenated target `"CAT." + sName`
  produces no site (F8 pin)**; bracket strings skipped.
- DispatchTargetAt after the token-walk port: existing cases + multi-line call.
- PrepareRename: dispatch last segment allowed with correct range;
  category/script segments rejected; **non-dispatch** strings and comments still
  rejected (rename_test.go updates per F3).

Integration (server):
- References from declaration in B finds `ExecFunction("CAT.B.Proc")` in A
  (anchored), `DoProc("B.Proc")` flat, 3-part degradation, case variants.
- **F2: dotted self-site** — `ExecFunction("CAT.B.Proc")` inside B itself is
  returned exactly once (dedupe against the same-file pass).
- References from the call-site string in A returns the declaration in B
  (includeDeclaration=false excludes it) and B's in-file uses.
- Truthful null: same-named proc in an unrelated script not matched; **1-part
  `DoProc("Proc")` in another file NOT returned** (scoping rule, incl. an
  includer of B — documents the include-splice gap).
- **:CLASS-file case (F1)**: (a) references on a class-file procedure behave
  identically to a non-class file for dispatch sites; (b) rename of a class-file
  procedure is refused cross-file (D8 gate), nil edits.
- Open-doc overlay: caller open with unsaved new/deleted site → live result;
  definition deleted in live buffer → site no longer matches.
- Nil workspace index → byte-identical to current single-file behavior (both).
- Rename: WorkspaceEdit spans B (declaration, uses, same-file 1-part dispatch,
  **dotted self-sites**) + A (last segment only; quotes and `CAT.B.` prefix
  intact; exact supplied casing).
- Rename ambiguity: two candidate files → site skipped.
- Rename staleness: file rewritten on disk after indexing without re-index →
  edits computed from fresh content (or file skipped when the site vanished).
- Invalid new name rejected with zero edits.
- Catalog conformance (`go test ./internal/catalog/`) green; no draft entries.

## 6. Scoping recommendation

1. **Ship references first** (Phase A) — read-only, ambiguity-tolerant,
   exercises the whole pipeline with no file-corruption risk.
2. **Rename second** (Phase B) with the D8 class gate, ambiguity gate, F2
   self-site pass, and fresh re-extraction.
3. v1 subjects: **non-class-file procedures for rename; all procedures for
   references**. Variables same-file; script/data-source rename excluded.

## 7. Product decisions — RESOLVED 2026-07-24

All decisions below were resolved with the maintainer on 2026-07-24:
**D1** skip ambiguous sites silently; **D2** include ambiguous sites in
references; **D3** defer `:PUBLIC`-across-`:INCLUDE` to Phase C; **D4**
script/data-source rename out of scope; **D5** file-level references
(`:INCLUDE`/`RunDS`/entry-point) ship in Phase A if subject-identification is
clean, else slip to a later phase; **D6** confirmed (dispatch-strings-only,
extends existing contract); **D8** refuse cross-file rename of class-file
procedures with an explanatory prepare-rename rejection. Original option
analysis retained below for the catalog Rationale sections.

- **D1 — Rename at ambiguous call sites.** (a) skip silently [recommended],
  (b) abort whole rename, (c) edit anyway. Skipping can leave a broken call if
  the site really targeted the renamed proc; editing can break a call to the
  other candidate. Document in catalog Rationale; revisit with
  `window/showMessage`.
- **D2 — References at ambiguous sites.** Include when the target is among
  multiple candidates. Recommendation: yes — precedent already in the tree:
  definition returns multi-candidate results (`handler.go:444-456`).
- **D3 — `:PUBLIC` variables across `:INCLUDE`.** Reverse-include walk +
  on-demand text scan. Defer to Phase C; catalog entries state variables are
  same-file in v1.
- **D4 — Script/data-source rename** (file renames, middle-segment rewrites).
  Out of scope; prepare-rename rejects non-last segments.
- **D5 — Entry-point / `:INCLUDE` / `RunDS` references.** Cheap once CallSites
  exist; Phase A if subject-identification is clean, else Phase C. If shipped,
  `references.md:57-58` string-context normative line must be amended (F3).
- **D6 — String-literal call-site policy (confirm-only).** Already normatively
  pinned: dispatch strings ARE renamed/referenced (`rename.md` A7/A8,
  `references.md` A7/A9, issue #43); the cross-file amendment extends the same
  line, it does not open a new decision.
- **D8 — Class-file procedure rename (NEW, from review F1).** Procedures in
  `IsClass` files are callable cross-file via `obj:Method()` /`Base:Method()`
  bare identifiers the LSP cannot see. Options: (a) refuse cross-file rename
  for class-file procedures [recommended — cheap gate on the already-indexed
  `IsClass` flag; honest failure beats silent breakage], (b) allow with a
  warning, (c) allow silently. Relax only when/if class-method resolution is
  modeled (Phase C+).

(Former D7 — 1-part `ExecFunction("Name")` workspace fallback — dropped: already
normatively decided by `cross_file_resolution.md` A14 and implemented at
`workspace_index.go:497-499`; references/rename simply inherit it, restated in
§2.2 as a scoping rule.)

## 8. Known gaps to document in the amended catalog entries

- **G1 (F1) — class-method channel.** `obj:Method()` / `Base:Method()` calls
  into class scripts are invisible (no cross-file member resolution exists);
  references under-report for class files, rename is gated by D8.
- **G2 (F1) — include-spliced 1-part calls.** `DoProc("Foo")` in an includer of
  the definition file is a real runtime call but is excluded by the 1-part
  scoping rule; closable later via a reverse-include walk.
- **G3 (F7) — position encoding.** No `positionEncoding` negotiation; the code
  uses byte columns assuming ASCII (see `colonStartsToken` comment,
  `handler.go`). Pre-existing, but multi-file WorkspaceEdits widen the blast
  radius of any non-ASCII line. Note in catalog; no v1 code change.
- **G4 (F8) — concatenated dispatch strings.** `DoProc("CAT." + sVar)` is never
  extracted, referenced, or renamed. Pin with a test; state in Known gaps.
- **G5 (F9) — uniqueness-gate drift after rename.** Renaming a procedure can
  change whether the workspace-unique-proc fallback fires for OTHER sites
  (a previously-ambiguous bare-name target may become unique, or vice versa),
  silently changing where navigation resolves afterward. Inherent to the
  resolver design; document, do not engineer around in v1.

## 9. Critical files

- /home/maho/dev/starlims-projects/starlims-lsp/internal/server/workspace_index.go — FileSymbols/CallSites, CallSitesFor, lookup maintenance, IsClass
- /home/maho/dev/starlims-projects/starlims-lsp/internal/providers/crossfile.go — ExtractCallSites, DispatchTargetAt token port, WorkspaceResolver contract
- /home/maho/dev/starlims-projects/starlims-lsp/internal/server/cross_file.go — liveResolver, overlayResolutions refactor (F4), siteTargetsDefinition
- /home/maho/dev/starlims-projects/starlims-lsp/internal/server/handler.go — handleReferences / handlePrepareRename / handleRename rewiring
- /home/maho/dev/starlims-projects/starlims-lsp/internal/providers/rename.go — PrepareRename dispatch-segment branch, validation (reused)
- Also: internal/providers/definition.go (FindReferencesWithScope, isDispatchTargetMatch — F2 root cause),
  catalog/features/references.md, catalog/features/rename.md (F3 amendments incl. A4 + rename_test.go:98,114),
  internal/server/cross_file_test.go (pin test replacement, :CLASS cases)
