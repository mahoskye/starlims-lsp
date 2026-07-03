# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [0.11.0] - 2026-07-03

`:` member access on shape-inferred UDObjects (spec:
`catalog/features/hover.md` A15-A16, `catalog/features/definition.md`
A14-A15).

### Added
- **Member hover.** Hovering the member in `oObj:Prop`, where `oObj` has
  a CreateUDObject-inferred shape (initializer literals, `:prop :=`
  augmentation, `:clone()`, cross-procedure propagation — issues #7/#19),
  shows the property's name, inferred value type, receiver, and
  definition line. Closes the long-standing "property access after `:`
  has no hover" gap.
- **Member go-to-definition.** The member navigates to where the shape
  learned the property: the `CreateUDObject` initializer key or the
  first augmenting assignment.

### Changed
- On a shaped receiver, an unknown member now returns null for hover and
  definition (truthful null) instead of potentially matching an
  unrelated same-named symbol. Unshaped receivers are unchanged.

## [0.10.0] - 2026-07-03

Cross-file milestone follow-ups: `RunDS` navigation and `:INCLUDE`-aware
diagnostics (spec: `catalog/features/cross_file_resolution.md` A15-A19).

### Added
- **RunDS data-source navigation.** Go-to-definition and hover inside
  `RunDS("Category.Name")` strings resolve to the workspace data-source
  file, landing on its file-level `:PARAMETERS` line. Bare
  `RunDS("Name")` resolves by basename too — unlike dispatch targets, a
  data source is always a separate file.
- **`:INCLUDE`-aware diagnostics.** `:INCLUDE` is a full-splice textual
  paste, so variable names declared by resolved include targets
  (`:DECLARE`/`:PUBLIC`/`:PARAMETERS`) now count as declared in the
  including file for `undeclared_variable` and `invalid_sql_param`.
  Transitive with a cycle guard; ambiguous targets contribute the union
  of their candidates; open included files use their live buffer.
  Without a workspace index, behavior is single-file as before.

### Changed
- **Script/data-source partition.** `DoProc`/`ExecFunction`/`:INCLUDE`
  resolution no longer returns `.ds`/`.ds.txt` files; `RunDS` resolution
  returns only them.

## [0.9.0] - 2026-07-03

The cross-file navigation release (milestone 1 of the cross-file plan;
normative spec: `catalog/features/cross_file_resolution.md`).

### Added
- **Cross-file go-to-definition.** Dotted `DoProc`/`ExecFunction` targets
  and `:INCLUDE` paths (bare, dotted, or quoted; cursor on the keyword or
  the path) jump across the workspace: `ExecFunction("Cat.Script")` lands
  on the target script's entry-point `:PARAMETERS`,
  `"Cat.Script.Proc"` on the procedure, includes on the file. Ambiguous
  targets return multiple locations, canonical-layout candidates first.
- **Cross-file hover.** Dispatch-target strings become the second
  string-context hover exception (after SQL placeholders): resolved
  targets show the procedure's signature/docblock with a
  "defined in `Cat.Script`" origin, or the entry-point summary for
  2-part targets; `:INCLUDE` statements hover with the resolved script's
  summary.
- **Segment-aware dispatch-string completion.** Inside
  `DoProc("…")`/`ExecFunction("…")`: same-file procedures plus workspace
  category names before any dot (the deliberate noise floor), a
  category's scripts after `Category.`, and a script's procedures after
  `Category.Script.` or flat `Script.` — with `/*@private;` and
  `/*@protected;` procedures excluded. `.` remains a non-trigger
  character.
- **Workspace script identity.** Files under canonical export anchors
  (`Server Scripts/CATEGORY/`, `Applications/APP/MODULE/Server Scripts/`,
  `Data Sources/CATEGORY/`) resolve as `Category.Script`; anything else
  degrades gracefully to basename matching plus a workspace-unique
  procedure-name fallback — flat checkouts navigate without the export
  tree. All matching is case-insensitive.

### Changed
- Workspace index now carries procedure docblocks, visibility flags,
  script identity, and entry-point parameters (workspace-symbol search
  behavior unchanged).
- Docblock extraction no longer loses the docblock when a
  `/*@private;`/`/*@protected;` annotation sits between it and
  `:PROCEDURE`.
- Definition responses may return multiple locations for ambiguous
  cross-file targets (single-location responses keep the previous wire
  shape).


## [0.8.0] - 2026-07-02

The behavior-catalog release: every diagnostic rule, LSP feature
contract, and formatter decision now has exactly one normative entry
under `catalog/`, enforced by executable specs in `go test ./...`.
When code and catalog disagree, the build fails. The full 118-entry
review surfaced and fixed the issues below (#25-#48).

### Added
- **Behavior catalog** (`catalog/`): one entry per behavior with
  machine-checked frontmatter, executable Flags/Does-not-flag fences
  run through the real diagnostics pipeline, formatter Before/After
  golden pairs, and feature acceptance criteria traced to Go tests via
  `[spec <id>/A<n>]` tags. Cross-cutting decisions in
  `catalog/DECISIONS.md`; generated `docs/reference/DIAGNOSTICS.md`
  and `docs/STATUS.md` are staleness-checked.
- **`region_end_mismatch` diagnostic** (warning): flags a
  `/* endregion;` with no open `/* region;` to close — previously the
  orphan marker was silently ignored while folding mis-paired.
- **`ssl.diagnostics.unusedVariables` setting** (#48): the opt-in
  unused_variable check was only reachable programmatically; it now
  has a client-facing toggle.
- **Constructor signature help over LSP** (#40): `Email{` now serves
  the class's constructor signatures on the wired token-based path;
  the dead text-based entry point was deleted.
- **DoProc/ExecFunction array parameter hints** (#46): a lone
  string-literal first argument resolving to a same-file procedure
  hints the argument-array elements with that procedure's parameter
  names — previously documented but stubbed.
- **Orphaned-prose comment signal** (#25): a terminated `/*` comment
  followed (no paragraph break) by a line starting with two adjacent
  bare identifiers — the signature of comment prose stranded as code —
  now warns, covering both multi-line comments and a `;` on the
  comment's first line. All settled issue-#6 false-positive guards
  preserved.
- **CI workflow** (#29): gofmt, vet, and the full test suite (catalog
  conformance included) on every push and PR.

### Fixed
- **Folding** (#26, #27): unclosed `:PROCEDURE` folds to end of file
  like other unclosed blocks (was a degenerate single-line range);
  single-line procedures and region pairs produce no range. Region
  pairing semantics confirmed: the canonical closer `/* endregion;`
  takes no name and closes the innermost open region (LIFO); trailing
  text on a closer is prose.
- **`include_in_procedure` mis-tag** (#30): the in-procedure
  `:INCLUDE` check emitted `include_early`'s code, making the two
  situations inseparable in `ssl.diagnostics.rules`.
- **Member-access false positives** (#32): `oSvc:Email(...)` no longer
  flags `class_instantiation_curly`; `oObj:Me` no longer flags
  `me_outside_class` — identifiers preceded by member-access `:` are
  member names.
- **Definition** (#41): the scope fallback no longer surfaces another
  procedure's local for an out-of-scope name.
- **References** (#42, #43): `includeDeclaration: false` is honored
  from use sites, not just the declaration line; matches no longer
  leak into comments or unrelated string literals. DoProc/ExecFunction
  dispatch strings remain matches (and rename keeps updating them —
  now normative).
- **Document symbols** (#44): `selectionRange` covers exactly the
  procedure name, not the `:PROCEDURE ` keyword prefix.
- **Workspace symbols** (#45): open `:CLASS` documents report their
  procedures as Method (6), matching the index's classification.
- **Formatter** (#33-#39): blank lines between procedures normalize to
  the configured count instead of accumulating, and land above an
  attached doc comment instead of detaching it; `builtinFunctionCase`
  no longer rewrites inside strings and comments; space before a comma
  is removed; standalone comments indent to block depth instead of
  column 0; `maxConsecutiveBlankLines: 0` actually preserves source
  blank runs; the final statement at EOF gets its semicolon;
  `trimTrailingWhitespace: false` is honored.
- **Signature help**: commas inside an array-literal argument no
  longer advance the enclosing call's active parameter index.
- **`scientific_notation` fix-it** (#47): suggestions no longer
  reproduce invalid explicit `+` exponent signs (`9E+1` now suggests
  `9.0E1`).

### Removed
- **`region_legacy` diagnostic** (#28): its premise was wrong —
  `:REGION`/`:ENDREGION` are current, supported SSL (GetRegion body
  capture); the construct the style guide dropped was C#-style
  `#region`. Any `ssl.diagnostics.rules` override for the slug becomes
  a harmless no-op.
- **Three never-implemented diagnostic codes** (#31):
  `identifier_too_short` (would contradict the style guide's blessing
  of loop counters), `return_from_constructor` (covered by
  `constructor_return_value`; bare `:RETURN;` is legal),
  `inherit_qualified_name` (the schema accepts both base-name forms —
  nothing to flag). Catalog entries preserved as `status: removed`.

## [0.7.7] - 2026-05-13

Picks up the ssl-style-guide changes from upstream commit `386d57e`
(2026-05-13 sync) and incorporates them into the LSP.

### Added
- **`unqualified_field_assignment` diagnostic.** Warns when a class
  method assigns to a bare identifier matching a `:DECLARE`d class
  field. In SSL a bare LHS creates a method-local — it does NOT write
  to the field — so the field stays unchanged and the user has a
  silent footgun. The fix is `Me:fieldName := ...` (or `Base:` for an
  inherited field). Suppressed when a method-local `:DECLARE` or
  `:PARAMETERS` entry of the same name shadows the field. Covers `:=`
  and the compound forms (`+=`, `-=`, `*=`, `/=`, `^=`, `%=`).
- **`Request` / `Response` endpoint ambients.** SSL endpoint scripts
  run with two pre-injected runtime identifiers in scope. A new
  `ssl.diagnostics.endpointPatterns` workspace setting (list of
  case-insensitive path substrings) plus a leading-docblock
  `Endpoint:` marker (scanned in the first ~30 lines) activate
  endpoint mode. In endpoint files the ambients no longer trigger
  `undeclared_variable`, hover surfaces their documentation, and
  completion offers them. In non-endpoint files behavior is unchanged:
  using `Request` / `Response` still flags as undeclared (which it
  should — they fail at runtime there).
- **Forward-looking docs note for `.NET` method dispatch on built-in
  types** (`docs/features/completion.md`). When the LSP eventually
  grows type-aware member completion or unknown-member diagnostics
  on `:` access, receivers typed `string` / `number` / `date` /
  `array` / `boolean` / `netobject` must be treated as .NET
  passthrough rather than flagged. Tracked in #22.

### Changed
- **Terminology: `:CLASS DECLARE` slots are "fields", not
  "properties".** Doc-comment fix on the `Me:` / `Base:` completion
  context description. The rest of the LSP's user-facing "property"
  usage already correctly refers to built-in class accessors,
  SSLExpando, AddProperty, and CreateUdObject members.

## [0.7.6] - 2026-05-13

### Fixed
- **Issue #14 — `equals_vs_strict_equals` false positive on `!=`.**
  The diagnostic that warned about `!=` with a string operand has been
  removed: `!=` is the well-defined exact-match negation operator in SSL
  and using it (e.g. `:IF oCurrent:status != "Done";`) is a valid,
  non-misleading pattern. The companion warning on bare `=` string
  comparisons (suggesting `==`) is unaffected. Reported in
  vs-code-ssl-formatter#78.
- **Issue #16 — Wrap no longer splits `oVar:property`.** The line-wrap
  rule for long lines treated the member-access `:` as an ordinary
  punctuation boundary, so `someFunc(oVar:propertyName, …)` could be
  broken into `oVar:\n    propertyName`. `canWrapBefore` now refuses to
  split immediately before or after a TokenPunctuation `:`, keeping the
  receiver and member glued together. Reported in
  vs-code-ssl-formatter#76.

### Added
- **Issue #15 — Blank line between sibling control-flow blocks.** A new
  post-format pass inserts a blank line between adjacent
  `:IF` / `:WHILE` / `:FOR` / `:BEGINCASE` / `:TRY` blocks at the same
  indent so a wall of closely-grouped blocks reads as distinct units.
  Gated by the new `blankLineBetweenBlocks` formatting option (default
  `true`). Reported in vs-code-ssl-formatter#77.
- **Issue #17 — Procedure docblock surfaces in hover and completion.**
  The parser now attaches the leading `/* … ;` comment block that
  precedes each `:PROCEDURE` to its `ProcedureInfo`, parsing
  `Description:`, `Parameters: name - desc` lines, and `Returns:` into a
  structured `ProcedureDoc`. Hover and completion documentation panels
  weave these in alongside the existing parameter list. Reported in
  vs-code-ssl-formatter#75.
- **Issue #18 — In-script procedure name completion inside DoProc /
  ExecFunction strings.** The completion handler previously suppressed
  all suggestions when the cursor was inside a string literal. It now
  detects when that string is the first argument of `DoProc(…)` or
  `ExecFunction(…)` and offers procedures defined in the current
  document, inserting just the bare name (no DoProc snippet). Reported
  in vs-code-ssl-formatter#74.
- **Issue #19 — UDObject property tracking augments and propagates.**
  `BuildUDObjectShapesWithProcedures` extends shape inference with two
  new passes: property assignments (`oVar:newProp := …`) add `newProp`
  to oVar's shape (creating an implicit shape if none existed), and
  `DoProc("Bar", {oFoo, …})` propagates oFoo's shape to Bar's first
  parameter so completions inside the callee see the same property set
  the caller built up. Passes iterate to fixpoint so shapes built up in
  one procedure flow through to its callees. Reported in
  vs-code-ssl-formatter#73.

## [0.7.5] - 2026-05-08

### Fixed
- **Issue #11 — `:` trigger should only suggest keywords when ':' begins a
  new token.** Previously, completing after an unknown identifier (e.g.
  `foo:`) fell through to the keyword inventory, producing a noisy popup
  in member-access positions. The trigger-character branch in
  `handleCompletion` now checks the character immediately before the typed
  `:` and returns an empty list unless it is whitespace or start-of-line.
- **Issue #12 — `:` trigger no longer produces `::KEYWORD` on accept.**
  Keyword completions returned for a `:` trigger now carry an explicit
  `TextEdit` whose range covers the typed `:`, so accepting `:IF` always
  yields exactly `:IF` regardless of editor word-boundary heuristics.

## [0.7.4] - 2026-05-06

### Added
- **Issue #7 — UDObject shape inference + `clone()` propagation.** The server
  now scans `<var> := CreateUDObject({{"key", val}, ...})` assignments and
  infers a property shape for the LHS variable. `<var>:clone()` calls
  inherit that shape on the new variable. Property completions fire on
  `oVar:` member access for any variable with a tracked shape. Coarse value
  types (`string`, `boolean`, `number`, `array`) are surfaced in the
  completion detail.

### Changed
- **Issue #8 — completion auto-trigger trimmed to `:` only.** `,`, `.`, and
  `(` were removed; they fired completions during normal typing
  (list/decimal/expression entry, function-call argument lists) and
  Enter-selected the wrong token. `:` is the sole remaining trigger because
  it is both the SSL keyword prefix (`:DECLARE`) and the member-access
  operator (`obj:prop`). On a `:` trigger with no context-aware match
  (Me/Base, built-in class, or shaped variable), the server returns only
  keyword completions — no procedures, variables, or snippets. The full
  inventory is reserved for explicit `Ctrl+Space` invocation.
- **Issue #9 — signature help auto-trigger is now opt-in.** The popup no
  longer reappears on every keystroke inside a call by default. Set
  `ssl.intellisense.signatureHelp.autoTrigger: true` to restore the previous
  behavior. Hover and explicit invocation (`Ctrl+Shift+Space`) work
  regardless of the setting.

### Fixed
- **Issue #6 — false positive on `comment_text_after_terminator` across a
  paragraph break.** The multi-line "broken-out keyword" heuristic now
  suppresses when there's a blank line or another standalone comment between
  the suspect comment and the alleged code text. The original positive case
  (mid-comment stray `;` immediately followed by a bare keyword) still fires.

## [0.7.3] - 2026-05-02

### Fixed
- **Issue #55 — `ssl.globals` setting was being ignored.** Settings sent via
  `initializationOptions` weren't applied until the first
  `workspace/didChangeConfiguration`, so workspace-level globals never made
  it to diagnostics. Initialization now applies the option payload directly.
- **Issue #56 — `:INCLUDE` deep paths flagged as undeclared.** Identifiers in
  `:INCLUDE File_Helpers.FileWork;` were being typed as `dot_property_access`
  and run through the undeclared-variable check. Diagnostics now skip dotted
  segments that appear as the operand of `:INCLUDE`.
- **Issue #63 — confusing wording on `equals_vs_strict_equals`.** Reworded to
  lead with what the user is doing right ("`!=` is exact-match negation")
  before explaining the asymmetry, instead of reading like `!=` is being
  flagged as wrong.
- **Issue #64 — inline SQL formatter mangled single-line SQL.**
  `FormatSQLInString` now leaves short single-line SQL strings alone, and
  when wrapping IS required it keeps the opening `"` glued to the preceding
  `:=` / `(` / `,` and the closing `"` on the line that owns the trailing
  punctuation.

### Fixed (SQL formatter regressions, rules A–F)
Six pinned regressions in the SQL formatter and surrounding string
formatting, surfaced by user-reported fixtures:
- **Rule A** — every `JOIN` starts a new line.
- **Rule B** — `AND`/`OR` inside a `CASE`-in-`SELECT` indents past `WHEN`.
- **Rule C** — projections never split from their `AS` alias; long
  projections move to their own line on overflow instead.
- **Rule D** — argument-list wraps hang-indent under the opening `(`.
- **Rule E** — closing `"` stays attached to trailing args.
- **Rule F** — opening `"` stays on the assignment line.

### Internal
- Test fixtures pin all six SQL formatter regression rules so future changes
  surface deviations in CI.

## [0.7.2] - 2026-05-01

### Fixed
- **Initialized-handler deadlock on Windows.** The `initialized` notification
  handler synchronously called `client/registerCapability` to register file
  watchers, but glsp dispatches notifications on the same goroutine that
  reads incoming messages — so the server blocked waiting for a response on
  the goroutine that would have to deliver it. Manifested as
  `fatal error: all goroutines are asleep - deadlock!` and
  `Cannot call write after a stream was destroyed` on the client. The
  registration now runs in a background goroutine. The bug had always been
  theoretically broken; v0.7.0 evidently shifted Windows timing enough to
  make it deterministic.

## [0.7.1] - 2026-05-01

### Fixed
- **Panic recovery in diagnostic collection.** Wrapped `collectDiagnostics`
  in `recover()` so any panic in a diagnostic check surfaces as a single
  `internal_error` diagnostic plus a stack trace in the LSP output channel
  — the editor stays usable and bug reports include actionable detail
  instead of the server process dying.
- **Static linking for the host (linux-amd64) build.** The host build was
  linking against the build runner's glibc; cross-compiled targets were
  already static. All targets in the Makefile now build with
  `CGO_ENABLED=0`, so users on systems with older glibc no longer hit
  silent startup failures.

## [0.7.0] - 2026-05-01

### Added
- **`procedure_declaration_syntax` rule.** New error-level diagnostic that
  catches two common procedure-declaration typos:
  - `PROCEDURE Name(...)` (missing leading colon) — previously misfired
    `direct_procedure_call` on the procedure name. The new rule explains
    the actual problem: definitions are `:PROCEDURE Name;` (colon prefix,
    trailing semicolon, no parens; arguments via `:PARAMETERS`).
  - `:PROCEDURE Name(...)` — parens after a valid keyword. Same fix, same
    message.

### Changed
- **`mixed_type_operator` no longer false-fires** on three common patterns
  the v0.6.0 inferencer mishandled:
  - **Uppercase-leading identifiers.** `DCUparseCat` (capital `D` is the
    start of an acronym) was being read as a `d`-prefixed Hungarian date.
    Type inference now requires a strict-case lowercase prefix before an
    uppercase rune; the lenient match still drives the `hungarian_notation`
    enforcer rule.
  - **Indexed access (`arr[i]`).** Was typed as the array, not the element.
    Now treated as opaque element type — no warning.
  - **Member access (`Me:Foo`, `obj:bar`).** Same opaque treatment.
  Regression-guard: `"abc" + 5` still emits the warning, so the rule isn't
  silenced for genuine literal mismatches.
- **`class_member_order` no longer enforces Constructor position.** Real
  legacy classes routinely place the constructor first; the rule now only
  enforces `:INHERIT` < `:DECLARE` < methods.
- **Paren-aware top-level operator scan in `inferExpressionType`.** Operator
  splits now respect parenthesis depth so expressions like `(a + b) * c`
  type correctly.

### Removed
- **`skipped_param_spacing` rule.** Pure stylistic noise about whether
  `{a, , b}` should be `{a,,b}`. Removed; the slug is gone from
  recognized-rule lists.

## [0.6.0] - 2026-04-30

### Added
- **Per-element documented exceptions surfaced in hover.** Function and class
  hovers now include the documented `## Exceptions` table, `## Caveats`, and
  `Don't` lists straight from ssl-docs (vendored at
  `internal/constants/data/ssl-element-meta.json`). Hovering `ExecFunction`,
  for example, now shows the canonical exception messages
  ("Please provide at least one parameter for ExecFunction" /
  "Wrong parameters for {functionName}") inline with the signature.
- New `constants.LookupMeta(name)` runtime accessor for the metadata index.
  The data is `go:embed`-ed into the binary; no external file lookup at
  runtime. Diagnostic checks can call `LookupMeta` to quote canonical
  exception text or weight checks against documented per-element caveats —
  this PR only wires hover; future PRs can route exceptions into specific
  diagnostics.

### Tests
- `TestGetHover_FunctionExceptions` pins the canonical exception text on
  ExecFunction hover so accidental loader regressions surface in CI.

## [0.5.0] - 2026-04-30

### Added
- **Server-side per-rule severity overrides.** New
  `DiagnosticOptions.RuleOverrides` (a slug -> severity map) drops or remaps
  diagnostics by `Code`. Recognized values: `off` (drop), `info`, `warn`,
  `warning`, `error`. Wired into `ssl.diagnostics.rules` from client
  initialization options and configuration changes; the override is applied
  inside `collectDiagnostics` so every consumer (LSP `publishDiagnostics`,
  `--validate`, future tooling) honors it uniformly.
- **Suppression comments.** Two forms recognized in source:
  - `/* @ssl-disable <slug>[, <slug>...]; */` — file-scope: silences every
    matching diagnostic in the document.
  - `/* @ssl-disable-next-line <slug>[, <slug>...]; */` — line-scope: silences
    matching diagnostics on the line directly after the comment.
  Slug `*` matches any code (full silence). The set of recognized slugs is the
  one defined in `internal/providers/diagnostic_codes.go`.
- **Three new formatting options** (close the gap with the VS Code extension's
  client-only fallbacks):
  - `TrimTrailingWhitespace` (default `true`) — strips trailing space/tab from
    every formatted line.
  - `MaxConsecutiveBlankLines` (default `0` = no cap) — caps runs of blank
    lines at the configured count.
  - `BuiltinFunctionCase` (`"preserve"` default, `"PascalCase"` to canonicalize)
    — rewrites built-in function call sites to the published inventory casing
    (e.g. `len(x)` -> `Len(x)`). User-defined identifiers and non-call uses
    are untouched.
  Wired through from `ssl.format.{trimTrailingWhitespace, maxConsecutiveBlankLines, builtinFunctionCase}`.
- **`constants.CanonicalFunctionNames()`** helper — lowercase->PascalCase map
  over the full inventory; used by the casing rewriter and available to other
  consumers.

### Tests
- `TestRuleOverrides_DropAndRemap` — pins drop + remap behavior for `off` /
  `info` and verifies unknown override values pass through unchanged.
- `TestSuppressionComments_FileScopeAndNextLine` — pins file-scope and
  next-line suppression and the `*` wildcard.
- `TestFormat_TrimTrailingWhitespace`, `TestFormat_MaxConsecutiveBlankLines`,
  `TestFormat_BuiltinFunctionCase_PascalCase` — pin each post-format pass.

## [0.4.0] - 2026-04-30

### Added
- **`Code` field on `providers.Diagnostic`** populated at every emit site and
  propagated to `protocol.Diagnostic.Code` over the LSP wire. Clients can now
  identify findings by stable slug instead of message text — enabling reliable
  quick-fix code actions, suppression comments, and per-rule severity
  overrides. Where `ssl-style-guide.schema.yaml` defines a `lints` rule slug
  the code uses that slug verbatim (e.g. `parameters_first`,
  `prefer_exitcase`, `udobject_array_in_clause`, `exitfor_in_finally`);
  parser/lexer-level findings get slugs derived from the producing check
  function. The full list lives in `internal/providers/diagnostic_codes.go`.
- **Tests** assert that every emitted diagnostic carries a non-empty `Code`
  and pin a few representative codes via spot-checks, so accidental rename
  regressions are caught.

### Fixed
- **`InitializeResult.serverInfo.version`** now reflects the build-time
  version string instead of a stale hardcoded `"0.2.0"`. The `version`
  variable in `internal/server/server.go` is now overridden via
  `server.SetVersion` from `cmd/starlims-lsp/main.go`, which already pulls
  the version from `-X main.version=...` (set by the Makefile from
  `git describe --tags`). Previously the LSP reported "0.2.0" to clients
  on the wire even when the binary was actually 0.3.0.

## [0.3.0] - 2026-05-01

### Added
- **SSL element reference integration** — the canonical inventory now lives at
  `internal/constants/data/ssl-element-reference.json` (vendored snapshot from
  `ssl-style-guide`) and is consumed via `go generate` to produce per-category
  `internal/constants/generated_*.go` files. New `cmd/gen-inventory` codegen
  tool replaces hand-curated keyword/operator/function/class lists with the
  published reference (446 elements: 38 keywords, 32 operators, 3 literals,
  8 types, 29 classes, 6 special forms, 330 functions).
- **Class hover** now enumerates published constructors, properties, and
  methods drawn from the reference. Operator hover now appends the
  `type_behavior` table (e.g. `+=` shows number/string/date combinations).
  New hover for the 8 core SSL value types (`array`, `boolean`, `codeblock`,
  `date`, `netobject`, `number`, `object`, `string`) showing runtime type,
  supported operators, and members. New hover for the 6 special forms
  (`access-modifiers`, `base`, `code-block`, `code-organization`,
  `constructor`, `me`) showing canonical syntax blocks.
- **Constructor signature help** when the cursor is inside `<ClassName>{...}`
  built-in instantiation. Each constructor form appears as a separate
  signature, with parameter descriptions sourced from the reference.
- **Context-aware completions** in the LSP completion handler
  (`internal/server/handler.go`). When the cursor sits in a recognized
  context the server returns a focused list instead of the full inventory:
  - `<BuiltInClass>{` — constructor signatures (snippet form) for that class
  - `Me:` / `Base:` inside a `:CLASS Foo;` file — `Foo`'s methods and properties
  - `<BuiltInClass>:` — that class's methods and properties
  - Any other context falls back to the existing full completion list
- **`GetClassMemberCompletions(className)` / `GetClassConstructorCompletions(className)`**
  exported helpers in `internal/providers/completion.go` are wired into the
  server-side completion dispatcher (see context-aware completions above)
  and are also available to other consumers.
- **Class-name collision diagnostic** warns when `:CLASS Foo;` declares a
  user class whose name shadows a built-in (Email, SQLConnection, etc.).
- `--export-signatures` JSON now includes `classes[].constructors`,
  `classes[].properties`, `classes[].methods`, and a top-level `operators`
  array with `type_behavior` rows.

### Changed
- **Built-in class inventory grew from 22 to 29.** The published reference
  exposes `CDataColumn`, `CDataColumns`, `CDataField`, `CDataRow`,
  `SQLConnection`, `SSLError`, and `SSLSQLError` as user-facing classes.
  These were previously hidden by the LSP's exclusion list as
  "return-only/internal" types but are documented in ssl-docs.
- **Built-in function inventory shrank from 354 to 330.** The published
  reference no longer documents the legacy/licensing helpers `LPrint`,
  `TraceOn`/`TraceOff`, `SqlTraceOn`/`SqlTraceOff`, `StationName`,
  `UndeclaredVars`, `In64BitMode`, `NetFrameworkVersion`,
  `GetExecutionTrace`, `SetLocationOracle`/`SetLocationSQLServer`,
  `GetForbiddenAppIDs`/`GetForbiddenDesignerAppIDs`, and the licensing
  helpers (`IsFeatureAuthorized`, `IsFeatureBasedLicense`, `IsDemoLicense`,
  `GetLicenseInfoAsText`, `ResetFeatures`, `GetInstallationKey`,
  `GetFeaturesAndNumbers`, `GetNumberOfInstrumentConnections`,
  `GetNumberOfNamedConcurrentUsers`, `GetNumberOfNamedUsers`). Calls to
  these names now show "unknown function" diagnostics.
- `SSLClassNames` and `SSLFunctionNames` derive from generated data; the
  hand-maintained legacy/excluded/supplemental machinery is gone. Curated
  function signatures with rich parameter descriptions remain in
  `signatures.go` and are overlaid on top of the generated inventory.

## [0.2.0] - 2026-03-03

### Added
- **CLI Validation Mode** (`--validate`) - Validate SSL files from the command line with structured JSON output
  - File-based validation: `starlims-lsp --validate script.ssl`
  - Stdin support: `echo '...' | starlims-lsp --validate --stdin`
  - Designed for agent skills, CI pipelines, and programmatic use
- **Gotcha Diagnostics** - 7 new diagnostic checks for common SSL mistakes:
  - Direct procedure calls without DoProc/ExecFunction (Gotcha #1)
  - Zero-based array indexing detection (Gotcha #5)
  - Named SQL parameters (`?name?`) in functions that don't support them (Gotcha #7)
  - Dot notation for property access instead of colon (Gotcha #8)
  - Assignment operator (`:=`) in IF/WHILE/CASE conditions (Gotcha #9)
  - Parentheses for class instantiation instead of curly braces (Gotcha #15)
  - Missing quotes in ExecFunction procedure name arguments
- Helper functions for identifier pattern matching in diagnostics
- Comprehensive tests for all gotcha diagnostic checks
- MIT License and project disclaimer
- Comprehensive documentation (DOCUMENTATION.md)
- `.golangci.yml` for standardized linting
- `.editorconfig` for consistent formatting
- Benchmark tests for lexer and parser (small/medium/large document sizes)
- Test coverage for `FindReferences` with `includeDeclaration=false`
- Edge case tests for block depth diagnostics
- `ssl.diagnostics.globals` configuration for declaring pre-defined global variables that cannot be reassigned

### Changed
- Improved `FindReferences` to properly respect `includeDeclaration` parameter
- Enhanced region pattern regex to better handle SSL comment syntax
- Added safety guards for edge cases in block depth diagnostics
- Updated VS Code extension reference to `vs-code-ssl-formatter`
- Improved code documentation with explanatory comments
- Strengthened test assertions in formatting and handler tests
- Replaced magic numbers with named constants in tests
- Updated gotchas.md with LSP detection status for all documented gotchas
- Improved `checkTokenErrors` to skip dot property patterns (avoids duplicate diagnostics)

### Fixed
- Block depth diagnostic no longer uses hardcoded character position
- Region name extraction handles trailing semicolons correctly
- Test for `SSLExpando` class instantiation now uses correct `{}` syntax

### Removed
- **`ssl-validator` standalone binary** - Consolidated into `starlims-lsp --validate`

## [0.1.0] - 2026-01-10

### Added
- Initial LSP server implementation
- **Completion** for keywords, built-in functions, classes, procedures, and variables
- **Hover** information for keywords, functions, classes, and user-defined symbols
- **Signature help** for built-in functions
- **Go to Definition** for procedures and variables
- **Find References** for all symbols
- **Document Symbols** (outline) for procedures, variables, and regions
- **Workspace Symbols** (open documents only)
- **Diagnostics** including:
  - Unclosed block detection (`:IF` without `:ENDIF`, etc.)
  - Unmatched parentheses and brackets
  - Block nesting depth warnings
  - Opt-in Hungarian notation warnings
- **Document formatting** for SSL and embedded SQL
- **Range formatting** support
- **Folding Ranges** for procedures, regions, and comments
- **Code Snippets** for common SSL patterns
- Cross-platform builds (Linux, macOS, Windows)
- Configuration via `workspace/didChangeConfiguration`

### SQL Formatting Styles
- `standard` - Simple clause breaks per style guide (default)
- `canonicalCompact` - Balanced with indented AND/OR and smart wrapping
- `compact` - Minimal breaks, fits on fewer lines
- `expanded` - Each column/condition on own line

[Unreleased]: https://github.com/mahoskye/starlims-lsp/compare/v0.2.0...HEAD
[0.2.0]: https://github.com/mahoskye/starlims-lsp/compare/v0.1.0...v0.2.0
[0.1.0]: https://github.com/mahoskye/starlims-lsp/releases/tag/v0.1.0
