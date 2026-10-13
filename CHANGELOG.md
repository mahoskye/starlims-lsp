# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Fixed
- **`global_assignment` no longer flags qualified members.** A property or
  class field whose name matches a predefined or configured global, such as
  `oSummary:pending := 0;` (`Pending` is a status global) or
  `oContext:MYUSERNAME := ...`, belongs to its object, not to the global.
  It was reported as an error. Members after `:` are now skipped, including
  nested access and comments or whitespace around the colon. A bare
  assignment such as `Pending := 2;` is still reported, with the same range.
- **`undeclared_variable` skips label and block names.** The name after
  `:LABEL`, `:REGION` and `:BEGININLINECODE` is not a variable reference,
  but `--strict` reported it as undeclared (#272).
- **The predefined globals a base installation declares are recognized.**
  `undeclared_variable` knew only `MYUSERNAME`, so reading `CRLF`,
  `UserLang`, `sCC`, `Session` or `PublicConsts` warned under `--strict`.
  The list is now read from a vendored copy of the reference's
  predefined-globals guide. A name the guide says a base installation
  does *not* declare (`CR`, `LF`, `TAB`, `sTab`, `sSQ`, `sDQ`,
  `MYUSERID`, ...) still warns, and the message names the replacement,
  such as `Chr(13)`. `global_assignment` still checks only `MYUSERNAME`
  among them, because the application's own session code assigns the
  others (#270).
- **`unused_variable` counts `?name?` markers.** A parameter read only
  through `SQLExecute` substitution was reported as never used (#270).

### Changed
- **`global_assignment` is a warning** (was an error), and its message no
  longer says the assignment is impossible. The reference says nothing
  makes a predefined global read-only: the assignment succeeds and changes
  the value for every routine that reads it. That is a likely bug, not a
  failure.
- **`Request` and `Response` are never undeclared.** The reference says
  they can be read in any script, so `undeclared_variable` no longer flags
  them outside endpoint scripts (DECISIONS.md D4, amended). Completion
  still offers them only in endpoint scripts.
### Added
- **`exitcase_last_in_branch` (error).** `:EXITCASE` is valid only as the
  last statement directly in a `:CASE` or `:OTHERWISE` branch. A
  statement after it, an `:EXITCASE` nested inside `:IF`, `:WHILE`, `:FOR`
  or `:TRY` within the branch, and an `:EXITCASE` outside any `:BEGINCASE`
  all fail to compile, and none was reported. `prefer_exitcase` still
  counts a nested `:EXITCASE`, so a clause is reported once (#265, #273).
### Added
- **`include_quoted_target` (error).** A quoted `:INCLUDE` target, such as
  `:INCLUDE "Common.Helpers";`, does not compile, and it validated clean.
  The target must be an identifier or a qualified identifier. The range
  covers the string, and the message gives the unquoted statement, so a
  client quick fix can unquote it. Go-to-definition still resolves the
  quoted form while it's being fixed (#266).
### Added
- **`datasource_default_required` is back (error), scoped to the data
  source's own parameters.** On STARLIMS 12 a data source, in SSL or SQL
  mode, fails when it runs if a script-level `:PARAMETERS` name has no
  inline `:=` default ("No default value for one or more parameters."),
  whatever the caller passes. The rule was removed in v0.16.0 because it
  seemed to flag valid production files. Those were the `:PARAMETERS` of
  procedures defined inside a data source, which are ordinary procedure
  signatures, and they are now skipped (#263, #269).
- **`inline_default_outside_datasource` (error).** `:PARAMETERS sName :=
  "a";` is valid only in a data source. In a script or procedure the
  statement takes names only and doesn't compile. This is the mirror of
  `no_default_statements_in_datasource` (#264).
### Added
- **`inherit_qualified_name` (error).** `:INHERIT BaseCounter;` doesn't
  compile: the parent must be named `Category.ClassName`. The rule was
  reserved but never implemented, because the style guide used to accept
  both forms. It no longer does (#269).
- **`endclass_invalid` and `builder_directive_outside_datasource`
  (errors).** `:ENDCLASS`, and `:DSN`/`:TABLENAME`/`:NULLASBLANK`/
  `:INVARIANTDATECOLUMNS` outside a data source, are known invalid forms.
  They were reported as `unknown_keyword` warnings, and now get their own
  codes and messages (#269).

### Changed
- **`named_sql_param_unsupported` is an error** (was a warning). A
  `?name?` marker in a positional SQL function fails or binds the wrong
  values at run time. This raises an existing code to error, so a file
  that only had this warning now validates as invalid (#269).

### Fixed
- **Catalog `schema_ref`s resolve again.** Thirteen entries pointed at
  style-guide schema paths that had moved (`lints.compile_errors.*` now
  sits under `.rules`, and similar). The style guide's catalog
  cross-check now resolves every reference (#269).
### Added
- **`invalid_native_member` (warning).** A colon member that the reference
  documents as raising on an SSL value is reported, such as `aItems:Count`
  (`Invalid property: Count`; use `ALen()` or `:Length`), `aItems:Append()`,
  a wrong-case native member such as `dToday:year`, or any member on a code
  block. The receiver's type must be certain from structure (every
  assignment in its scope agrees), never from its Hungarian prefix, because
  real code labels .NET collections, where `:Count` is valid, with an `a`
  prefix. `++`/`--` on a value that is certainly a string is reported too
  (#267).
- **`--strict`: `invalid_sql_param` agrees with `undeclared_variable`.** A
  variable created by assignment (`sStatus := "A";` with no `:DECLARE`)
  exists at run time, and `undeclared_variable` already treated that
  assignment as a declaration, but `invalid_sql_param` reported every
  `?sStatus?` marker for it. On the production corpus `--strict` reported
  9,478 of these findings; 540 remain.
### Breaking
- **`--validate` rejects unknown flags instead of reading them as file
  paths.** A dash-prefixed argument `--validate` does not know now exits
  2, names the flag on stderr, and writes nothing to stdout. Before
  this, a consumer that passed a flag newer than its pinned binary got
  a phantom `{"file": "--new-flag", "valid": false}` entry beside a
  result computed without the flag, and exit 1 told it nothing.
  **Downstream:** a runner must treat exit 2 as a failed call, with
  empty stdout and the reason on stderr. A runner that parses stdout
  unconditionally now sees empty output where it used to see a phantom
  entry. Guards that look for `--`-prefixed `file` entries, like the one
  in the MCP server's diagnose runner, still matter for binaries older
  than this release, but this binary never produces such an entry. Pass
  `--` before any file whose name starts with a dash. `--format`
  already rejected unknown flags, and now honors `--` too (#262).
- **A `--validate` usage error exits 2, not 1.** Calling it with no
  inputs exits 2, matching `--format`, so exit 1 always means "an input
  has errors or could not be read", never "the call was wrong" (#262).

### Added
- **The `--validate` / `--format` CLI contract is written down.**
  `catalog/features/cli_contract.md` specifies the JSON schema, `valid`
  semantics, exit codes, `--stdin`, batching order, every flag, and what
  each profile (`--ds`, `--info`, `--hungarian`, `--hungarian-types`,
  `--strict`) turns on. These are the details that consumers which gate
  or score SSL with the CLI depend on and an editor never sees. Fifteen
  acceptance criteria enforce it, including two pins that fail the
  build at the moment a breaking change is made: the set of codes that
  can make `valid` false, and the JSON field sets. DECISIONS.md D16
  records the versioning policy. A break ships in a minor release under
  `### Breaking`. A new default-on warning or a profile gaining a check
  is a minor release with its corpus delta. Added fields, new flags,
  and false-positive fixes may ship in a patch (#262).
- **Every `--validate` result carries the tool `version`.** It is the
  same value `--version` prints, repeated on each per-input object so a
  consumer's logs show which binary scored them. It is an added field,
  not a wrapper: the top-level array that consumers index is unchanged
  (#262).
- **A false-positive budget for `--strict`, with its first
  measurement.** The targets are at most 1% misfires, meaning findings
  that contradict the profile's own semantics, and at most 1.5
  strict-only findings per document on corpus code that already
  validates. The current baseline misses both: 3.0 findings per
  document across 6,166 documents, about 60% of them from two misfire
  classes. `invalid_sql_param` flags a placeholder whose name the
  script assigns, a name `undeclared_variable` accepts.
  `unused_variable` flags a parameter whose only read is a `?name?`
  placeholder. Without those two classes the residual is 1.21 per
  document. The contract entry records the numbers and the known gaps
  (#262).
- **A short SQL `WHEN` predicate stays on its `WHEN` line.** `AND`/`OR`
  inside a `CASE ... WHEN` condition always broke onto new lines. A
  predicate that fits with its `THEN` now stays inline, as in the style
  guide's example; only one that overflows breaks before each `AND`/`OR`
  (#286).
- **SQL `CASE` lays out the same way in every clause.** Only a `CASE` in
  the SELECT list came out the way the compact SQL reference shows it.
  In `WHERE`, `ORDER BY` and `UPDATE ... SET` the formatter moved `CASE`
  onto its own line at the clause column with `WHEN`/`ELSE` only 2
  under it and `END` back at the clause column — reformatting the
  reference's own `WHERE CASE` example, and in `SET col = CASE` leaving
  `WHEN` left of the assignment it belongs to. A `CASE` nested in a
  `THEN` collapsed back to the outer `CASE`'s column, and a `THEN` value
  was always joined onto its `WHEN` line. Now `CASE` stays where it is
  written, `WHEN`/`ELSE` sit 4 past it, `END` aligns with it, a nested
  `CASE` starts 4 past its `WHEN`, and when any branch would overflow
  with its value inline, every `THEN` value of that `CASE` goes 4 past
  its `WHEN`. A short `CASE` inside an aggregate's argument list
  (`SUM(CASE WHEN ... END)`) stays inline, as in the reference. The
  formatter also no longer alters a token while doing this: `ELSE -1`
  had become `ELSE - 1`, which reads as a subtraction. SQL-mode data
  sources are still passed through by `--format` unchanged — that is
  intended, and `fmt.sql_in_strings` now says so (#271).
### Added
- **`--version` names the SSL reference data it was built with.** A
  second line reads `SSL reference data: <repo>@<commit> (<date>)`, from
  a new `internal/constants/data/provenance.json` that every data refresh
  updates. `go test -v ./internal/constants/` logs the same commit and
  how many days old it is. Until now nothing showed the vendored data was
  behind until someone noticed wrong hover text (#254).

### Changed
- **Function hover, signature help, and completion now come from the SSL
  reference instead of a hand-maintained copy.** The roughly 2,700 lines
  of curated signatures in `signatures.go` had drifted from the
  reference. The `SQLExecute` hover called its second parameter a
  "friendly name for logging", while the exceptions listed directly
  beneath named the same argument `sConnectionName`. Every type was
  `variant`, and descriptions repeated parameter names back. Signatures
  are now generated from the vendored reference: its parameter names,
  real types (`string`, `boolean or string`, ...), optional or required
  status, documented defaults, and return descriptions. The hover reads
  top to bottom as the call form, what the function does, each
  parameter, what it returns, and then the documented exceptions.
  Signature-help labels, completion snippet placeholders, and inlay hints
  use the same names, so `SubStr` now hints `sSource:` instead of
  `source:`. A test checks every function's names, types, and defaults
  against the vendored JSON. The only hand-written content left is five
  documented overrides, each filling a gap in the extracted data.
  Examples: `CreateUdObject` is shown as its three call forms rather
  than one flat parameter list, and `SortArray` gets back a parameter row
  the extraction dropped. The maximum argument count for every function
  is unchanged, so no diagnostic changes (#252).
- **Keyword, operator, literal, and `Me`/`Base`/`Constructor` hovers
  render the reference.** Each shows the published summary, its syntax
  block, and the reference's caveats and Don't list, replacing one-line
  descriptions that had gone stale. For example, `:PUBLIC` was described
  as call-stack scoped and the `Constructor` hover stated a member order
  that is only a convention. The `=` / `==` / `!=` string semantics now
  come from the reference too: `=` prefix-matches, and `!=` negates
  exact equality, not `=` (#252).
- **Endpoint, SQL-placeholder, and data-source directive hovers were
  corrected against the reference.** `Request`/`Response` no longer claim
  to exist only in endpoint scripts (they do not raise elsewhere), and
  show one merged list of caveats. A `?name?` hover says `SQLExecute`
  binds the variable's value instead of splicing it into the SQL, that an
  array expands to one `?` per element, and that no other SQL function
  substitutes named placeholders. The `:NULLASBLANK` hover warns that
  `.T.`/`.F.` are silently ignored, so write `true`/`false` (#252).
- **The bundled SSL reference data is current again.** Both vendored
  files were re-synced from `starlims-ssl-reference`, which had gathered
  two months of corrections since the last sync (2026-08-07). No element
  was added or removed (still 460). The changes are corrections to
  existing entries: 88 functions (signatures, parameter defaults and
  descriptions, return descriptions), 9 classes, 1 returned object, 4
  keywords, 8 operators, and all 8 core types. 171 elements have revised exception lists, most
  now quoting the exact runtime message. Hover, signature help, and
  completion pick these up directly. Two corrections are worth knowing:
  the `!=` operator hover now says strings compare exactly (it described
  prefix matching before), and `SSLExpando{}` is no longer offered as a
  constructor, because it does not compile (#254).
- **Hover on a core type no longer lists members it does not have.** The
  reference now documents `aValues:Append(x)`, `sValue:IsEmpty()` and the
  like as runtime errors for array, boolean, codeblock, date, netobject,
  number, and string values. Their hover says the type has no
  SSL-defined `:` members and names the function to use for each common
  task (`ALen`, `AAdd`, `Empty`, ...). `object` keeps its member table.
  The inventory generator now fails on a members table it does not
  recognize, so this cannot silently turn into empty rows again (#254).
- **The docs point at the upstream sources of truth.** `AGENTS.md`,
  `README.md`, and `docs/README.md` named a gitignored local
  `dev/ssl-style-guide/` checkout as authoritative, which had gone six
  months stale. They now point at the upstream `ssl-style-guide` and
  `starlims-ssl-reference` repos (#254).
- **The LSP names the STARLIMS version it models.** Decision D17: STARLIMS
  12 as verified by the SSL reference. A diagnostic's compile-error or
  run-time claim is judged against that, a known v11/v12 difference
  reports what fails on either version, and there is no version setting
  (#261).
- **The prose docs match the catalog again.** `AGENTS.md`, the README,
  the docs hub, ARCHITECTURE, ROADMAP, and VISION were brought up to date,
  and every SSL example in `AGENTS.md` was checked against the SSL
  reference. About twenty corrections followed. Among them:
  `CToD("2024-12-25")` returns an empty date under the default date
  format, `Eval` does not run a string, `:INHERIT` and `CreateUdObject`
  need category-qualified names, and `UsrMes` had its arguments reversed
  (#259).
An audit of inherited and legacy behavior (#257). Every compatibility
shim, alias, fallback, and setting was traced to its origin and its
consumers, and given a decision. The full record is in
`docs/reviews/legacy-inventory.md`. No setting, CLI flag, diagnostic
code, or output format changes for either distribution.

### Fixed
- **Docs no longer call `:REGION` and `:BEGININLINECODE` "legacy".** Both
  are current, supported SSL (bodies stored for `GetRegion()` /
  `GetInlineCode()`). #28 settled that when it removed `region_legacy`,
  but several docs still said otherwise, including one that claimed the
  LSP emits an info diagnostic for `:REGION`. The keyword hover text is
  replaced by the reference's own in #287.
- **CONFIGURATION.md matches the server.** `builtinFunctionCase` defaults
  to `"PascalCase"`, not `"preserve"`. `endpointPatterns` are
  case-insensitive substrings, not globs (the old example could never
  match). `max_block_depth` is info-tier, not a warning. `unusedVariables`
  is a client setting. `signatureHelp.autoTrigger` applies only at
  startup. Section 10 now maps the VS Code extension's setting names to
  server keys instead of presenting server keys as VS Code settings, and
  Appendix A lists all settings.
- `diag.deprecated_keyword` catalog frontmatter: `authority: style_only`,
  `schema_ref: lints.deprecated_keywords`. The style guide owns the rule
  now. Behavior and warning severity are unchanged.

### Removed
- **Dead internals, no behavior change.** Removed: the unread `root`
  argument of `Parser.ExtractVariables`, and `style != "compact"` guards
  that could no longer be false after `compact` became an alias. (The
  unread `procedures` argument of `GetSignatureHelpWithProcedures` and the
  unreachable curated signatures are left to #287, which replaces the
  curated signature data with the reference's.)

### Deprecated
- **`ssl.format.sql.style: "compact"`** was already a deprecated alias
  for `canonicalCompact` (#219). It now has a removal plan. First the VS
  Code extension drops it from its enum and migrates stored values. Then
  the next LSP minor release removes the alias. Nothing changes in this
  release. `--format` callers are unaffected: the CLI cannot select a SQL
  style.
### Added
- **Four rules from the reference's data-sources guide (#260).**
  - `empty_parameters` (error): `:PARAMETERS;` with no names. A parameter
    list must name at least one parameter, in any file.
  - `datasource_sql_question_placeholder` (error): a `?name?` marker in a
    SQL-mode data source. Parameters there are referenced as `@name`.
  - `nullasblank_value` (warning): `:NULLASBLANK` takes the word `true` or
    `false`. A value such as `.F.` is silently ignored.
  - `invariant_date_columns_quoted` (warning): a quoted
    `:INVARIANTDATECOLUMNS` name keeps its quotes and matches no column.
### Changed
- **Two severities move to match the kind of finding (breaking for CLI
  consumers that key on severity).** `trailing_skip_commas` goes from
  hint to info, joining the opt-in advisory tier. Its catalog entry said
  it was hint "like its sibling `empty_optional_param_array`", but that
  sibling, which covers the `{}` form of the same style-guide clause
  ("omit trailing optional parameters"), was already info. It is now
  dropped by default; `--info` or an `ssl.diagnostics.rules` entry brings
  it back. `region_end_mismatch` goes from warning to hint: an orphan
  `/* endregion;` is a comment and has no runtime effect, so it is a
  style finding, not a likely bug. Across 6,228 production files that
  moves 10 findings from warning to hint and drops 32 hints from the
  default output. No file changes `valid` (#255).

- **Catalog authority now mirrors the style guide's rule records.**
  `not_preferred_operator` → `style_only`, `runsql_non_dml` →
  `advisory`, `single_catch` → `authoritative`, `step_spacing` and
  `trailing_skip_commas` → `style_only`. Each entry had been written
  before the style guide grew a record for its rule. `spaced_skip_commas`
  and `trailing_skip_commas` now cite `lints.parameter_skipping_style`.
  The two stay separate rules: they transcribe different clauses of that
  lint and have different fixes (#255).

### Fixed
- **Member calls no longer trip built-in rules (D10).**
  `raiseerror_in_catch`, `runsql_non_dml`, and `trailing_skip_commas`
  found calls by scanning for an identifier followed by `(`, so
  `oLog:RaiseError(...)` inside `:CATCH`, a class's own
  `Me:RunSql("SELECT ...")`, or `s:Substring(1,)` all reported as though
  they were the built-in. They now read call sites from the expression
  tree and skip qualified calls. `runsql_non_dml` also lets a statement's
  leading literal alone decide its keyword, so
  `"INSERT INTO t " + "SELECT ..."` is no longer reported as a SELECT, and
  its write-guards see every string piece of the argument (#255).

- **`step_zero_literal` reads the whole step expression.** It checked
  only the first token after `:STEP`, so `:STEP 0 + nInc`, a step of 2,
  reported an infinite loop. The step must now be a zero literal,
  optionally negated (#255).

- **One finding per defect where two rules used to report it.** A
  C-style `PROCEDURE Name(...)` was reported both as
  `procedure_declaration_syntax` and, since the statement grammar
  landed, as `unexpected_token` on the name. `unexpected_token` now
  defers. A spaced skip-comma run that ends a call (`Foo(a, , )`) is left
  to `trailing_skip_commas`, whose fix settles both (#255).

- **Structure findings that were being masked now report.** Statements
  after a duplicate `:FINALLY` were counted as the first `:FINALLY`'s
  body, so an empty first clause escaped `finally_empty`. A second
  script-level `:PARAMETERS` flagged only when another statement came
  between the two lists; it now flags as a second block-level list
  always did. A comment beginning with the letters "region"
  (`/* Regional settings;`) opened a fold region, which also hid a later
  orphan `/* endregion;`; the marker must now be a whole word (#255).

- **A `:FOR` limit glued to `:STEP` gets a pointer at the missing
  space.** With an identifier, call, or subscript limit,
  `:TO nMax:STEP 2` parses `:STEP` as a member of the limit, and the
  header failed with a bare "Unexpected number '2'". `step_spacing` never
  saw that shape, because only a numeric limit leaves `:STEP` lexed as the
  keyword. The `unexpected_token` message now ends
  `(missing space before ':STEP'?)` (#255).

  Measured over 6,228 production files with `--info`, these fixes move
  one finding (a spaced run now left to `trailing_skip_commas`); every
  other position is unchanged.

Two follow-ups were filed rather than changed here. #291: rules that
`unexpected_token` defers to at warning severity (`scientific_notation`,
`unknown_keyword`, `unknown_token`) let a statement that does not parse
validate as `valid: true`. #292: no rule reports a stray `:CATCH`,
`:FINALLY`, or `:ELSE` outside its block.
### Added
- **SQL-content checks run on SQL-mode data-source bodies (#294).** A data
  source whose body is plain SQL got only the semicolon and placeholder
  checks. It now also gets `unicode_literal_prefix`, `unjustified_collate`
  (a SQL comment before the statement is the justification) and the
  info-tier advisories that read only SQL text: `sql_comma_join`,
  `sql_legacy_outer_join`, `sql_inconsistent_alias`, `sql_dialect_mix`
  and `sql_select_star`.

### Changed
- **`sql_injection` now judges the whole first argument, not its first
  token.** The rule reads the argument's expression tree. It flattens the
  `+` chain and reports the call when a string literal (SQL text) meets a
  runtime value. Concatenating constants only (`"SELECT …" + " WHERE …"`)
  no longer warns, because no runtime value reaches the SQL. A leading
  `BuildStringForIn(...)` no longer hides a value spliced in later in the
  same chain. A leading call (`Upper(sSql) + "…"`) or a parenthesized
  argument is no longer missed. On a production corpus the default
  surface drops by 50 warnings, all of them constant-only concatenations,
  and 2 real splices are newly caught (#255).
- **`sql_select_star` no longer flags `EXISTS (select * …)`.** An
  existence probe never reads its columns, so the coupling concern does
  not apply. The catalog entry already listed it as a non-goal while the
  code flagged it. These probes were 301 of the corpus's 447 hits. The
  rule is now linked to the schema's `lints.performance.avoid_select_star`
  (#255).
- **Catalog authorities now mirror the style guide.**
  `udobject_array_in_clause` is `authoritative`, as its style-guide
  record says. `sql_literal_splice` is `advisory` (the
  `prevent_sql_injection` record's Bad example is exactly this idiom).
  `sql_comma_join`, `sql_dialect_mix`, `sql_legacy_outer_join`,
  `sql_inconsistent_alias` and `sql_suspect_placeholder` are `tool`,
  because no style-guide record covers them (#255).

### Fixed
- **A non-ASCII character in a SQL string could wipe out a file's
  diagnostics.** `udobject_array_in_clause` upper-cased the string and
  then sliced the copy by offsets into the original. Case mapping can
  change byte length: a dotless `ı` is two bytes, and its upper case `I`
  is one. Enough of them before an `IN (?oX:aIds?)` placeholder sliced
  past the end, and the panic guard replaced every diagnostic in the file
  with a single `internal_error`. The lookback now upper-cases ASCII
  only, so offsets stay aligned (#255).
- **Placeholder ranges are right in multi-line SQL strings.**
  `udobject_array_in_clause`, `complex_sql_placeholder` and
  `invalid_sql_param` put the range on the string's first line, at a
  column past its end, wherever the placeholder actually was. Multi-line
  strings are the style guide's layout for SQL, so this was the common
  case, and it also misplaced the extension's quick-fix. The three
  checks now share one helper that follows the string's newlines (#255).
- **`unicode_literal_prefix` no longer flags `'N'`.** The check was a raw
  `\bN'` regex. The quote before `N` counts as a word boundary, so every
  `'N'` flag literal matched, along with `?'N'?` placeholders and `N'`
  text in SQL comments. It now runs on the SQL lexer's token stream: an
  `N` word directly followed by a quoted literal. On a production corpus
  it went from 691 hits to 0; none of the 691 was a real `N'…'` literal
  (#255).
- **`unjustified_collate` ignores `COLLATE` inside SQL literals and
  comments.** It now matches the lexed SQL word rather than a regex over
  the raw string (#255).
- **`sql_literal_splice` no longer counts an apostrophe in a SQL
  comment.** `-- it's fine` read as an open literal. That flagged a
  splice where nothing was concatenated, and it also hid every other SQL
  advisory on that string. Quotes are now counted on the SQL lexer's
  stream, outside comments, and only a piece that is an operand of `+`
  counts as a splice (#255).
- **`sql_inconsistent_alias` no longer reads a `TOP 10` count as a
  column.** `select top 10 col, x as y` treated `10 col` as a bare alias
  and flagged a consistent list. The whole TOP clause (`TOP n`,
  `TOP (n)`, `PERCENT`, `WITH TIES`) is now skipped (#255).
- **`mixed_type_operator` reads the operator's real operands, and stops
  guessing types from elsewhere in the file.** The `+` check looked at
  the token on each side of the operator and typed names from a
  file-wide record of their last assignment, so a `vValue := 0;` in one
  procedure made `vValue + " units"` in another a "numeric + string"
  mismatch, and an assignment *after* a use typed the use. It also read
  constant-style names (`MAX_ROWS`) and loop counters as numeric, and
  flagged `date + number`, the one mixed pairing `+` supports. The rule
  now runs on the expression tree: operands are typed from literals,
  strict Hungarian prefixes, documented builtin return types, and nested
  operations, and nothing is inferred from assignments. Precedence is
  honored (`nCount * 2 + sLabel`), a call on the left is typed as one on
  the right always was (`Upper(sA) + 1`), and `"a" + nCount + "b"`
  reports once rather than twice. Measured over 6,228 production SSL and
  data-source files, 3,723 reports became 108; 3,554 of the removed ones
  were constant-style names typed numeric, and the 68 new ones are
  prefixed names whose type the assignment record had overridden (#255).
- **`max_block_depth` follows the indentation a reader sees.** A
  `:CASE`/`:OTHERWISE` arm counted as a level of its own, so a
  `:BEGINCASE` cost one level more than an `:IF` whose body sits at the
  same indentation, and every `:ELSE`/`:CATCH`/`:FINALLY` body of an
  over-deep block was reported again on top of the block. Branch bodies
  now sit at the depth of the block they continue and are never reported
  themselves: one diagnostic per offending block. On the production
  corpus 3,786 reports became 1,059, with none added (#255).
- **`negative_logic` requires the negation to govern the whole
  condition.** `:IF .NOT. bDone .AND. bReady;` is
  `(.NOT. bDone) .AND. bReady` — unary operators bind tighter than every
  binary one — so swapping its branches cannot remove the negation the
  rule asks to drop. The condition is now read from the expression tree
  (75 of 895 corpus reports) (#255).
- **`nil_in_operations` no longer double-reports a member call on NIL.**
  In `1 + NIL:ToString()` the nearest token to the `+` is NIL, but the
  operand is the member call, which `nil_method_call` already reports.
  The rule now reads operands from the expression tree and fires only
  when the operand is the NIL literal itself. Severity stays `warning`;
  whether a guaranteed runtime failure should be `error` is an open
  question on #255.
- **`mixed_error_handling_families` sees a script's own handler.** Each
  `:PROCEDURE` boundary also closed the top-level span, so a script's
  `:TRY` before its procedures and its `:ERROR;` handler after them —
  where the grammar puts a legacy handler — never met. All top-level
  code is now one span (#255).
- **`nested_iif` reports each nested call once.** At three or more
  levels an inner `IIF` was reported once per enclosing `IIF`, as
  identical diagnostics on one range (#255).
- **`max_params_warning` is ranged on the procedure name**, not on the
  start of the `:PROCEDURE` line (#255).

### Changed
- **Catalog authority now mirrors the style guide record for ten
  diagnostics**, and stale severity prose left over from the info-tier
  move is corrected: `me_outside_class` and `mixed_type_operator` are
  authoritative, `missing_otherwise` and `max_params_warning` advisory,
  `max_block_depth`, `mixed_error_handling_families`, `negative_logic`,
  and `nested_iif` style_only, and `nil_in_operations` and
  `nil_not_empty_string` authoritative. No severity changed (#255).
### Breaking
- **`code_block_structure` is an error, and `:INHERIT` placement is a new
  error code.** `{|| expr}` (a code block with no bound variable) is a
  compile-time error according to the reference, but it shipped at
  warning. A `:INHERIT` that does not directly follow `:CLASS` is also a
  compile error, and it reported only as an info-tier `class_member_order`
  note, hidden by default. That case now reports as `inherit_position`, at
  error. Neither appears anywhere in the 6,228-file private corpus, so no
  document that validated before fails now (#255).

### Added
- **`equals_empty_string` reports `x = ""`, a comparison that is always
  true.** For strings, `=` is a prefix match that is true whenever the
  right operand is empty, so `:IF sStatus = "";` takes its branch for every
  string value. It was reported only by `equals_vs_strict_equals` at info,
  which is hidden by default. It is now a warning. `"" = x` is a correct
  emptiness test and stays silent, and so does a bare `x = "";` statement,
  which is a mistyped assignment. There are two findings in the private
  corpus, both in conditions (#255).

### Changed
- **The operator type checks run on the expression tree.**
  `arithmetic_type_mismatch`, `dollar_string_only`,
  `code_block_comparison` and `for_numeric_values` used to compare adjacent
  tokens. Their type heuristic read loop-counter names (`i`, `x`, ...) and
  UPPER_SNAKE constants as numbers, so `"A" $ ALLOWED_CODES` flagged. It
  also missed date arithmetic, so `dDue - 1` and `dEnd - dStart` flagged as
  well. Each operand is now a whole subtree, and the pair is judged against
  the reference's operator type tables. The rules newly cover `%`, `^`,
  `**`, same-type pairs the operator rejects (`.T. - .F.`), and the
  `!=` / `<>` / `#` comparisons of a code block, which raise at runtime
  like `==`. Real precedence also surfaces a real bug: `.NOT. "a" $ sList`
  is `(.NOT. "a") $ sList`. In the private corpus, `arithmetic_type_mismatch`
  goes from 26 findings to 10 and `dollar_string_only` from 4 to 5. The `+`
  path (`mixed_type_operator`) is unchanged (#255).
- **`default_on_declare_line` flags a `:DEFAULT` on a name that is not a
  parameter, on any line.** It used to flag only a `:DECLARE` and a
  `:DEFAULT` sharing one line. That always co-fired with
  `default_after_parameters`, misreported a default on a real parameter
  that merely shared the line, and missed the defect everywhere else. The
  slug is unchanged. Corpus: 35 findings, all in one script that already
  fails `default_after_parameters` (#255).
- **`branch_target_label` checks a literal target against the labels in its
  procedure.** A typo such as `Branch("LABEL SKP")` now flags, and so does a
  lowercase `"labelskip"`, because the keyword is uppercase and the match is
  exact. A scope that uses `:INCLUDE` is not judged (#255).
- **`error_handler_structure` checks handler placement.** It now reports an
  `:ERROR;` with nothing before it to protect, a `:RESUME;` with no
  `:ERROR;` in its procedure (which does not compile), and an `:ERROR`
  followed directly by any block boundary. None of these appear in the
  corpus (#255).
- **`invalid_limstypeex_comparison` follows the documented comparison
  semantics.** String comparison is case-sensitive and `=` is a prefix
  match, so `== "numeric"` and `= "String"` now flag, while `= "NUM"` and
  `"STRINGS" = LimsTypeEx(x)` no longer do. `<>` and `#` are checked, and
  inequality findings say "always true". One corpus document gains a
  finding and stops validating: its guard compares against `"String"` and
  can never pass (#255).
- **`base_outside_class` warns rather than errors in a procedure-only
  file**, the include-library tier `me_outside_class` has had since #171
  (#255).

### Fixed
- **Member access no longer trips `base_standalone` or
  `createudobject_builtin_misuse`.** `oUri:Base` and
  `oFactory:CreateUdObject("Email")` are members of some other object and
  never warn (D10) (#258).
- **One typo, one finding.** `bA AND (bB)` and `NOT (bA)` no longer also
  report as `direct_procedure_call`. The operand after a prefix bare `Not`
  no longer also reports as `unexpected_token`. An `:ENDFOR` that closes a
  `:FOR` no longer leaves `unclosed_block` on the loop. An unquoted
  `ExecFunction(Module.Proc)` path no longer also reports
  `dot_property_access`. `:CATCH` with a missing `;` says so, instead of
  calling the next statement an exception variable (#255).
- **`assignment_in_condition` ignores bracketed assignments.**
  `:IF (oRow := Fetch()) != NIL;` is the deliberate assign-and-test form
  and no longer warns (#255).
- **`dot_property_access` covers call and subscript receivers.**
  `GetObj().Name` and `aRows[1].Name` were only an `unknown_token` warning
  (#255).
- **Catalog audit of the a–l diagnostic entries.** `authority` is
  corrected on 36 entries to match the style-guide record that enforces
  each one, 10 resolving `schema_ref`s are filled in, and stale rationale is
  rewritten. That includes `datasource_sql_semicolon`'s, which argued for a
  warning the rule does not report. Data-source fences now carry the inline
  `:PARAMETERS` defaults a data source requires (#255).
### Breaking
- **`unknown_token` and `scientific_notation` are errors, so code that
  cannot compile no longer validates as `valid: true`.** When a statement
  does not parse, `unexpected_token` defers to a more specific rule that
  already names the token, and these two were warnings. A backtick, a
  `?`, or a glued `7e2` therefore produced only a warning, and
  `--validate` returned `valid: true` and exit 0. Both are now errors. A
  `--validate` consumer that gates on `valid` or the exit code sees
  failures where it saw passes before.

  Cost on a 6,228-file production corpus: `unknown_token` fires 70 times
  in 7 files, all genuinely broken code; two more files now fail
  validation. `scientific_notation` fires nowhere. `unknown_keyword` stays
  a warning, because a gap in the keyword list must not fail validation.
  `unexpected_token` no longer defers to it, so an unknown colon form in a
  position the grammar cannot accept is also reported as the parse
  failure it is (#291).

### Added
- **`stray_block_keyword` reports a middle clause keyword outside its
  block.** `:ELSE` outside an `:IF`, `:CATCH` or `:FINALLY` outside a
  `:TRY`, and `:CASE` or `:OTHERWISE` outside a `:BEGINCASE` validated
  clean. Three catalog entries said the placement was "owned by the
  block-structure checks", but no such check existed. It is an error, and
  the owner search stops at the enclosing procedure (#292).
- **`unqualified_field_read` reports a bare read of a class field inside
  a method.** A bare name in a method never reaches the field: it raises
  `Variable [name] is undefined!` or silently reads a caller's or a
  public variable. The existing assignment rule claimed the opposite for
  reads. This is a warning and skips a read that the assignment rule
  already covers. On the production corpus: 26 findings in 5 files, each
  a field read with no local of that name (#260).

### Changed
- **`zero_based_array_index` on a .NET collection is an info-tier
  advisory, not a warning.** The style guide prescribes
  `dataSet:Tables[0]`, and every one of the 245 production findings was
  that path, so the default surface was warning on the prescribed form.
  A bare SSL array indexed with `[0]` is still an error. Detection now
  reads the expression tree and checks every dimension: `aRows[1][0]` and
  `aRows[0, 1]` used to be missed (#255).
- **Visibility annotations are judged the way the runtime reads them.**
  Only the exact lowercase `/*@private;` or `/*@protected;` on the line
  immediately before `:PROCEDURE` takes effect. A near-miss spelling, a
  blank line, or another comment in between leaves the procedure public.
  These cases are now `visibility_annotation` warnings, where before they
  got the info-tier "annotation in use" note. `ProcedureInfo.IsPrivate`,
  which hides private procedures from cross-file completion, uses the
  same verdict (#255).
- **`unqualified_field_assignment` sees every write form.** It now reads
  the expression tree instead of scanning for `name :=`. That adds
  `nTotal++`, element writes (`aItems[1] := x`) and the inner targets of
  a chained assignment. A method-local `:PUBLIC` now shadows the field.
  Production: 14 to 15 findings (#255).

### Fixed
- **One mistake, one finding, across the structural rules (#255).**
  - An empty `:TRY` body is reported once, not twice.
  - `:ENDFOR` no longer also reports its `:FOR` as unclosed.
  - A stray `:ENDIF` inside a procedure is `unmatched_block_end`, as it is
    at top level, instead of "does not match ':PROCEDURE'".
  - A missing inner closer (`Len(aV[1);`) is one unclosed `[`.
  - An extra closer (`{a, b}});`) is one unmatched `}`; each of these
    last two used to produce two errors.
  - An unterminated string after an operand is reported only by
    `unterminated_string`.
- **A leading UTF-8 byte-order mark is no longer an unknown token.**
  Files read from disk by `--validate` reported it, and the formatter
  writes it back unchanged (#255).
- **Dot-style access after a call or a subscript is named as such.**
  `oObj:Get().Value` and `aRows[1].Value` are `dot_property_access` with
  its colon fix-it, not a generic `unknown_token` (#255).
### Added
- **`--format --ds` treats input as a data source.** It mirrors
  `--validate --ds` for extensionless input such as stdin. `.ds` and
  `.ds.txt` paths imply it (#255).
- **`:REGION`/`:ENDREGION` and `:BEGININLINECODE`/`:ENDINLINECODE` fold.**
  Both are current SSL with multi-line bodies, but neither produced a
  folding range (#255).

### Fixed
- **Rename and references no longer cross into a different symbol of
  the same name.** With a workspace open, the server chose the
  procedure's cross-file path from the spelling alone. Renaming the
  member in `oRec:CalcTotal`, or a script-level variable named like a
  procedure, renamed the *procedure* in every calling file and left the
  intended name untouched. The choice now uses the identifier's role, as
  the single-file path already did. A dispatch string now counts as a
  procedure occurrence, so renaming a variable `Calc` no longer rewrites
  `DoProc("Calc")` (#255, #258).
- **Member names are no longer renameable.** A receiver's type is not
  resolved, so renaming `oA:Subject` edited every `:Subject` in the file,
  including the built-in property of an `Email{}` object. Prepare-rename
  and rename now refuse a member name. Rename also repeats prepare's
  string and comment guard for clients that skip prepare, and rejects
  `NIL` and built-in class names as new names (#255, #258).
- **Rename and references respect a procedure's own local.** Renaming a
  script-level or `:PUBLIC` variable also renamed another procedure's own
  `:DECLARE` of that name and its uses. Definition already treats that
  local as shadowing the outer name, and rename and references now agree
  (#258).
- **Rename and references use character columns after non-ASCII text.**
  Ranges were byte offsets, so `sName := "Größe" + sName;` renamed to
  `sNew` became `sNew := "Größe" + sNsNew`. Two places matched a
  lower-cased copy of a line and then sliced the original. Case mapping
  that changes byte length (`İ`, `ẞ`) could misplace that slice or panic,
  and both now search the original line (#255, #258).
- **A script that names a missing procedure no longer resolves to another
  script.** When `Cat.Script.Proc` found the script but the script lacked
  `Proc`, the workspace-unique fallback still ran and landed in an
  unrelated script that happened to define `Proc`. Go-to-definition jumped
  there, and rename treated the call site as an unambiguous caller. A
  found script that lacks the procedure now resolves to nothing, as the
  catalog always said (#255).
- **Go-to-definition fixes.**
  - A 1-part `DoProc(` whose string sits on the next line now resolves.
  - `DoProc("Helper")` written inside a comment no longer navigates.
  - A word inside an ordinary string or a comment returns nothing instead
    of jumping to its declaration.
  - Procedure locations now cover the name itself, not a guessed span
    from column 0.
  - The member of a built-in class instance (`oMail := Email{}` followed
    by `oMail:Subject := …`) is no longer treated as a UDObject property
    (#255, #258).
- **Document symbols fixes.**
  - Nested comment regions nest. A procedure inside an inner region used
    to be listed twice, once under each region.
  - Parameter children appear when a bare `:PARAMETERS` lists its names
    on the following lines, and each child now points at its own name
    instead of column 0.
  - A region's selection range now covers its name (#255).
- **Inlay hints skip member calls.** `sName:Replace("a", "b")` is a .NET
  method and `Me:Left(...)` is a class method. Both were labelled with
  the parameter names of the SSL built-in that shares their name (#255).
- **`--format` no longer leaves a marker-free script unformatted.** The
  CLI ran the data-source SQL-mode classifier on every input. That
  classifier treats content with no colon keyword and no `:=` as SQL, so
  a small script such as `usrmes( "hi" ) ;` came back untouched. The
  passthrough now applies only to data sources (#255).
- **Variable completion lists only variables in scope.** It offered every
  declaration from every procedure, duplicates included. It now offers
  the cursor's procedure's locals and parameters, script-level
  declarations, and publics, one item per name (#258).
- **The workspace index skips dot-directories.** Copies of scripts under
  `.git`, `.history`, or `.claude/worktrees` were indexed, so workspace
  symbols listed every procedure twice and cross-file resolution saw
  phantom duplicates. The background walk and watcher events now ignore
  them (#255).

## [0.24.0] - 2026-09-21

The strict profile. `--validate --strict` turns on the three checks an
editor withholds because a human mid-edit has names that are
legitimately undeclared or unused — a non-interactive consumer only ever
sees finished code, where each is a defect. It costs 0.2 findings per
file on code that already validates.

Measuring it against production corpora then surfaced eight defects, and
most of this release is those. Two of them broke a stated contract:
formatting was not idempotent (A6), and in one shape grew a file by a
line on *every* pass. Two were silent — a comment closed C-style hides
the next statement, and a misplaced semicolon in a SQL data source is
rejected outright by STARLIMS rather than merely being unwise. The rest
are classification and false-positive fixes found the same way (#249).

### Added
- **`--strict` on `--validate`, enabling the three checks the editor leaves
  off.** `undeclared_variable`, `unused_variable`, and `invalid_sql_param`
  are gated off by default for a good reason — an editor reports them
  continuously while someone is mid-edit, when a name is legitimately
  undeclared or unused for the next few keystrokes. A non-interactive
  consumer never sees that state: an agent or a CI gate is handed finished
  code, where each of the three is a defect. Until now `--validate` had no
  way to say so, because it hardcoded `DefaultDiagnosticOptions()` and the
  three options had no flag.

  Measuring the profile over 1,923 SSL and data-source files put the cost
  at close to nothing on the code that matters: across the 1,509 files
  that validate without errors today, `--strict` adds 320 findings — 0.2
  per file, with 1,324 of them picking up nothing at all and the worst
  single file reporting 20. The volume concentrates instead in files that
  already fail to parse, which is where a consumer wants telling. All
  three are warning or hint severity, so `--strict` never flips `valid`
  or the exit code by itself.

  Each flag keeps one concern, as `--hungarian-types` does: `--strict`
  implies neither `--info` nor the Hungarian checks (#249).

- **A SQL bind whose name collides with an SSL keyword no longer
  misclassifies the document.** Data-source mode detection looks for
  what SQL cannot contain — a `:KEYWORD` token or a `:=` — and
  `WHERE s = :default` produced a `:DEFAULT` keyword token, so a plain
  SQL data source read as SSL. That is the loud direction to get wrong:
  every SQL reserved word in the body then reports as an undeclared
  variable.

  A colon keyword now counts only in statement-leading position — the
  start of the body or just after a `;` — which is where SSL writes them
  and where a bind never is. Binds whose name was not an SSL keyword
  were always safe; the colliding names are safe now too. Verified
  across 1,610 production data sources: no document changes
  classification (#249).

- **`datasource_sql_semicolon` is now an error, and catches the banner
  form it used to mask.** A SQL-mode data source whose banner closes
  `*/;` is rejected by STARLIMS with `Invalid SQL statement: remove any
  misplaced semicolons(;)` — confirmed against a live server. The rule
  previously hedged ("may fail on some database platforms") at warning
  severity, and the banner case was not reported at all: criterion A24
  consumed that `;` as "comment furniture" so the directive scan could
  proceed, and treated masking it as a verdict that it was harmless.

  Masking stays — the header scan needs it — but the unmasked header
  region is now checked separately, so the `;` is reported while the
  split still works. A banner closed with a plain `*/` is clean.

  The test is a separator with no statement in front of it, not the
  `*/;` spelling: the semicolon is what the server objects to, and
  keying on emptiness also catches the shapes a pattern misses — `*/`
  with the `;` on the next line, a `;` after a `--` banner, and a
  doubled `;;` between directives, all of which slipped through
  silently. A stray separator is now scanned past rather than ending
  the header scan, which used to drop the directives into the body and
  misclassify the document as SSL.

  The corpus shows why this went unnoticed: `*/;` appears in 85% of
  SSL-mode data sources, where the `;` genuinely is the SSL comment
  terminator and nothing is wrong, against 2% of SQL-mode ones — the
  distribution you would expect from a form that breaks the server.
  Across 1,610 production data sources the change reports 27
  occurrences in 17 files, taking the count that fail validation from 4
  to 23. Those 19 additional documents were already broken at runtime
  (#249).
- **`comment_swallows_code`: a comment closed C-style hides the next
  statement, and nothing reported it.** SSL comments end at the first
  `;` and at nothing else — the grammar is
  `CommentStatement ::= "/*" {Character} ";"`, so a `*/` is ordinary
  comment text with no closing effect. A comment "closed" that way
  therefore runs on to the next `;` and takes the statement in between
  with it. That code never executes while still reading as live code in
  the editor.

  `c_style_comment_closer` covered only the inert `*/;` spelling, where
  the `;` really does terminate and nothing is hidden. The spelling that
  actually hides code drew nothing: a swallowed `:PROCEDURE` surfaced
  only as a puzzling `unmatched_block_end` at some later `:ENDPROC`, and
  a swallowed `:DECLARE` or assignment surfaced as nothing at all — the
  file validated clean while behaving differently than it reads.

  The new rule reports at warning severity and names the swallowed
  fragment. It does not fire on `*/;`, on banner decoration before the
  terminator, or on SQL-mode data sources, where `/* ... */` is a real
  SQL comment. Measured over 4,842 production files it reports 74
  occurrences across 29 files (0.6%) (#249).

### Fixed
- **The strict trio no longer runs on data-source documents.** A data
  source does not follow SSL scoping: its builder directives take bare
  identifiers as *values* (`:DSN := starlims;` — `starlims` is a DSN
  name, not a variable read), its `:PARAMETERS` carry inline `:=`
  defaults, and its SQL body names columns and keywords that are not SSL
  identifiers at all. With `--strict` every data source therefore
  reported its own directives and SQL keywords as undeclared variables —
  16 findings on a five-line example, against 2 without the flag. The
  three checks are now forced off for a data-source document whatever
  the caller asks for; `checkDataSourceUndeclaredPlaceholders` remains
  the dialect's own placeholder check (#249).
- **Formatting is idempotent again (A6).** Two independent defects broke
  it, both found by formatting a production corpus twice and diffing.

  A SQL string holding an **unterminated `/*`** was reflowed as if the
  opener began a block comment: the remaining lines were swallowed into
  it and a trailing blank line was emitted, so every pass added one more
  line and the file grew *without bound*. Such content is now refused by
  `IsReformattableSQLString` and byte-preserved, alongside the existing
  unbalanced-quote guard it sits next to.

  Separately, an **orphan `*/` after a comment SSL already terminated at
  its `;`** (`/* note;*/`, which lexes as `*` then `/`) got a leading
  space from the operator path that the whitespace path did not give it
  on the following pass, so pass 1 and pass 2 disagreed. An operator
  glued to a comment now takes no leading space, making the two paths
  agree — the same fix shape as #218 for commas.

  Re-measured after both: 0 non-idempotent files across a 300-file SSL
  sample (previously 4) and 1,610 data sources (#249).
- **A data-source banner written in SSL comment style no longer
  misclassifies the document.** `SplitDataSourceHeader` masked only
  terminated `/* ... */` banners, so a `/*` closed by `;` aborted the
  header scan outright: the header came back empty, the builder
  directives fell into the body, and their `:=` read as a strong SSL
  marker. A plain SQL data source under such a banner classified as SSL
  and had its SQL body lexed as SSL — `?name?` placeholders reporting as
  unknown tokens, dotted columns as property access.

  A banner above a directive header is now treated as header furniture
  whatever comment style it uses, and the leading-unterminated-comment
  signal decides only the headerless shapes it was written for: a
  comment-only stub and an SSL data source opening with an unterminated
  comment both still classify as SSL (A16/A22 preserved). The SQL-mode
  decision is now one function shared with the diagnostics pipeline
  rather than duplicated in it. No file in a 1,610-document production
  corpus changes classification; the fix matters for the shape a
  generator can produce (#249).
- **A unary sign is no longer read as arithmetic.** `arithmetic_type_mismatch`
  took the previous significant token as its left operand without asking
  whether the operator had a left operand at all. In `{-1}` that token is
  the array literal's own `{`, which infers as an array, so a negative
  number at the head of an array literal reported "Non-numeric type in
  arithmetic operation '-': array - numeric". `{1, -2}` was unaffected,
  which is why it went unnoticed: only a *leading* sign sat next to the
  brace.

  A sign is now binary only when the previous significant token can end a
  value — an identifier, a literal, or a closing `)`, `]`, `}`. After an
  opening bracket, a comma, an assignment, another operator, or a
  keyword it is unary and no arithmetic finding applies. The fence sits
  below the NIL check so `NIL - 1` keeps reporting under
  `nil_in_operations`, and a genuine binary mismatch such as
  `sLabel - 1` still reports (#249).


## [0.23.0] - 2026-09-20

A small correction release. An explicit `+` sign in a scientific-notation
exponent turned out to be valid SSL, contrary to the style guide, which
is corrected in schema 1.7.2: the lexer now reads `9.0E+1` as one number
instead of leaving its pieces unreported, and `scientific_notation` keeps
the author's sign in its suggestion. It also pins unary plus, which SSL
does not have, as `unexpected_token`'s finding, with a fence and a test
(#244).

### Fixed
- **An explicit `+` exponent sign is valid SSL, and the LSP now reads it.**
  The lexer refused to fold `9.0E+1` into a number, stopping at `9.0` and
  leaving `E`, `+`, `1` that no rule reported: `scientific_notation` only
  handles the shapes lacking a decimal point, and `unexpected_token`
  deferred to it. So the decimal form drew nothing at all, while `9E+1`
  drew a suggestion to drop a sign that was never invalid. Now `9.0E+1`
  and `1.5e+3` lex as one number, `scientific_notation`'s suggestions keep
  the sign the author wrote (`9E+1` → `9.0E+1`), and `unexpected_token`
  defers only for the shapes `scientific_notation` really reports, so a
  well-formed number glued to a stray `E` (`2.0E+nVar`) is reported. The
  style guide's "explicit plus signs are not supported" clause is its
  error, tracked in ssl-style-guide#73; its decimal-point and
  leading-digit requirements stand. (#246)

## [0.22.0] - 2026-09-20

Closes the silence behind issue #240. Four kinds of broken code validated
clean — `:FOR EACH x IN y;`, a line of English prose, `foo bar baz;`, a
statement missing its `;` — because the parser computed that each was
incomplete and nothing surfaced it, and an unterminated string swallowed
the rest of a file without a word. Two always-on errors now report them,
anchored on the offending token or the unclosed opener. The production
corpus shaped both: assignment turned out to be an expression in SSL,
strings turned out to span lines, and the one false positive the corpus
exposed — a call chain continued on the next line — was the lexer's, not
the code's, and is fixed here too.

### Added
- **`unexpected_token` diagnostic** (error, always on): the first token in
  a statement that the SSL statement grammar cannot accept, anchored on
  that token with a message naming it and what was expected
  (`Unexpected identifier 'is' - expected an operator or ';'`). Closes
  the gap behind issue #240, where `:FOR EACH x IN y;`, a line of English
  prose, `foo bar baz;`, and a statement missing its `;` before the next
  line all validated clean: the expression parser already knew each one
  was incomplete and threw the knowledge away. The `:FOR` header is held
  to its one shape (`:FOR i := a :TO b [:STEP c];`) with the expected
  piece named, and `:FOR EACH` gets told SSL has no such form. It is
  declaration-independent — LIMS accepts undeclared variables; what it
  rejects is a token after an expression that is not a valid
  continuation — and it defers to the lexer- and operator-level rules
  that already name the same token, so a mistake is reported once. Being
  an error, it is the first #240 finding that flips `--validate`'s
  `valid` flag. Catalog: `diag.unexpected_token`.
- **`unterminated_string` diagnostic** (error, always on): a string
  literal that reaches end of file without its closing `"`, `'`, or `]`,
  reported on the opening delimiter
  (`Unterminated string literal - expected a closing '"' before end of
  file`). SSL strings span lines — the production corpus closes 18,006
  of them across a line break — so an unterminated one swallows the rest
  of the file by the language's rule, and until now nothing said so: in
  issue #240 the apostrophe in "doesn't" hid a trailing `.` that
  `unknown_token` would have caught and every statement after it. The
  lexer now records whether it saw the closer (`Token.Unterminated`),
  which is the only exact source — a bracket string's text can end in
  `]` and still be open (`[[a]`). Catalog: `diag.unterminated_string`.

### Changed
- **`StatementExprs.Complete` is stricter.** A tree containing an
  `ExprUnknown` (a dangling operator, a broken argument list) no longer
  counts as complete, so tree-driven rules make no claims about it; a
  final statement ending at end of file without `;` now does count as
  complete, and a bare `:RETURN;` yields no statement rather than an
  incomplete one. `StatementExprs` gains `Unexpected` and `Expected`.
- **The expression AST accepts assignment as an expression** in every
  value position — chained (`a := b := c;`), as an array-literal element or
  call argument, as a `:RETURN` value, in a condition — mirroring what LIMS
  compiles (a production corpus of 6,228 files uses the chained form in
  twenty places). The tree carries it as a binary node nesting to the
  right; typing already read a `:=` node as its right-hand value. Only a
  statement's own left-hand side is not a value position, so
  `StmtAssign` classification is unchanged.

### Fixed
- **A call chain continued on the next line is no longer mangled.** The
  lexer decided whether a `:` opens a keyword by looking only at the token
  immediately before it, so in

  ```ssl
  :RETURN txt:ToString()
      :Replace("##TITLE##", sTitle)
      :Replace("##USER##", sUser);
  ```

  each `:Replace` lexed as a keyword: `unknown_keyword` warned on every
  one, and the formatter — worse — recased them to `:REPLACE` and forced
  a `;` after the receiver line, splitting valid code into three broken
  statements. The look-back now skips whitespace and comments to the
  previous significant token, and yields to a real keyword so a lost `;`
  before a line-leading `:IF` still lexes the keyword. The parser and
  formatter treat a line-leading member-access `:` as a continuation
  (one extra indent level, no semicolon before it). Over 6,228
  production files `unknown_keyword` fired 3 times, all this shape.

## [0.21.0] - 2026-08-29

Splits the shared Hungarian gate so a consumer can take the correctness
signal without the convention audit. Measuring the pair over 6,228
production files showed `hungarian_notation` reporting 31,219 names
against `hungarian_type_mismatch`'s 477 — correct reports every one, since
53.6% of that codebase's declared names carry no recognized prefix, but a
65:1 ratio that made the pair unusable together on the LLM-facing surface.
The rules were already independent in logic; only the switch was shared.

### Added
- **`ssl.diagnostics.hungarianTypes` setting and `--hungarian-types` CLI
  flag**, gating `hungarian_type_mismatch` on its own. `--hungarian` and
  `ssl.diagnostics.hungarianNotation` keep enabling both rules.

### Changed
- **`hungarian_notation` and `hungarian_type_mismatch` are gated
  separately.** They were two independent findings sharing one switch, and
  measurement showed why that was wrong: over 6,228 production files the
  convention audit reports **31,219** names against the type check's
  **477** — a 65:1 ratio — because 53.6% of that codebase's 57,647
  declared names carry no recognized prefix. Every one of those reports is
  correct; the codebase simply does not use the convention. But sharing a
  switch forced any consumer wanting the correctness signal to accept a
  whole-codebase naming audit, which for the LLM-facing MCP surface meant
  `hungarian_notation` was >50% of the output in 40% of files and 100% of
  it in 14%.

  The two rules were already independent in logic — the type check only
  ever inspects names that already carry a prefix, so it is silent on
  exactly the code the other floods — so this splits only the gate.
  `ssl.diagnostics.hungarianNotation` no longer enables
  `hungarian_type_mismatch`; use `ssl.diagnostics.hungarianTypes` (or both).

## [0.20.0] - 2026-08-29

A single-feature release, cut so the downstream MCP binaries can reach the
Hungarian checks. `hungarian_type_mismatch` shipped in v0.19.0 gated
behind an editor setting the `--validate` CLI could not reach, which left
it unreachable for the agent and CI callers the CLI exists for; passing
`--hungarian` to a v0.19.0 binary read it as a filename and returned a
phantom failed result. This release makes the flag real.

### Added
- **`--hungarian` flag on the `--validate` CLI.** Enables the two opt-in
  Hungarian checks for that run — `hungarian_notation` (a declared name
  carries no recognized prefix) and `hungarian_type_mismatch` (the type a
  prefix promises disagrees with the assigned expression). Both are gated
  behind the `ssl.diagnostics.hungarianNotation` editor setting, which the
  CLI had no way to reach, so agent skills and CI callers could not get at
  them at all — `hungarian_type_mismatch` shipped in v0.19.0 unreachable
  from the CLI. The flag is orthogonal to `--info`: neither implies the
  other, and without a flag the CLI default matches the editor default.
  The flags are now covered by an acceptance criterion
  (`feature.diagnostics_pipeline` A28); `--info` previously had none.

## [0.19.0] - 2026-08-29

The expression-AST release. Milestone 1 of issue #184 shipped the tree
itself in v0.18.0 with no consumers; this release is what the tree was
for. Two diagnostics graduate from token scanning and Hungarian
guesswork to real call and type analysis, a new opt-in rule turns SSL's
naming convention into an enforceable type annotation, and three
correctness bugs the tree exposed are fixed — one of which, rename
rewriting `oRec:sName` properties and like-named `:PROCEDURE` headers,
silently corrupted code. Identifier resolution moved from lines to
statements, so a bare `:DECLARE` no longer hides every name it declares
from diagnostics, document symbols, rename, and the workspace index.
Every judgment the new typing makes is a definite type or "unknown", and
unknown is never evidence: the operator result matrix comes from the
element inventory rather than hand-written rules, and combinations the
language documents no result for make no claim. Verified against the
6,228-file production corpus — the default diagnostic surface is
unchanged at 8,794 diagnostics across 43 codes, with no panics and no
throughput regression. Issue #184 is closed. Downstream consumers pick
this up via the ssl-style-guide MCP binaries and vs-code-ssl-formatter
extension bumps (scope: vs-code-ssl-formatter#95 — the rule-id enum needs
the new `hungarian_type_mismatch` code).

### Added
- **`hungarian_type_mismatch`** (issue #184, opt-in): cross-checks the type
  a variable's Hungarian prefix promises against the type its assigned
  expression actually produces — `nCode := SubStr(sText, 1, 4)` stores a
  string in a number-named variable, `:DEFAULT bFlag, ""` gives a boolean
  a string default. SSL's naming convention encodes a type annotation on
  every variable; with an expression tree that annotation is enforceable,
  which is the stronger `CheckHungarianNotation` #184 proposed. Shares the
  existing `ssl.diagnostics.hungarianNotation` setting (default off) and is
  separately silenceable through `ssl.diagnostics.rules`. Both ends demand
  definite evidence, so anything partial stays silent. Corpus measurement
  over 4,620 production files: 459 hits in 272 files (5.9%).

- **Coarse expression typing** (`internal/providers/expr_types.go`,
  issue #184): literals, operator results, and builtin return types from
  the element inventory, with an opt-in mode that additionally reads
  Hungarian prefixes on identifiers and member names as type evidence.
  Every judgment is a definite type or "unknown", and unknown is never
  treated as evidence — an expression that cannot be resolved makes no
  claim rather than a guess. Binary operator results come from the element
  inventory's documented type matrix rather than hand-written rules, so a
  combination the language documents no result for (`aList + sText`,
  `nCount * sText`) makes no claim; assuming string concatenation there
  cost 94 false positives in the first corpus run.

- **`parser.StatementKind`**: `StatementExprs` now names the statement
  shape it came from (assignment, `:DEFAULT`, `:FOR` header, condition,
  `:RETURN`, bare expression), so consumers can tell them apart without
  re-reading tokens.

- **Formatter regressions inherited from vs-code-ssl-formatter**
  (`internal/providers/extension_regressions_test.go`). The extension
  removed its fallback TypeScript formatter — a corpus run showed it was
  non-idempotent on 18% of files and appended stray semicolons to SQL in
  `.ds` documents — making formatting LSP-only. That suite encoded real
  user bug reports, so the scenarios not already covered here were
  carried over: keyword casing inside an array subscript, no token
  merging across line breaks in a multi-line SQL string, an end-of-line
  comment not swallowing the next statement, wrapped lines never
  starting with a comma, qualified `Table.Column` never split on wrap,
  and semicolon enforcement across the whole block-opener family. Cases
  where this formatter deliberately differs (statement consolidation,
  comma-list wrapping with visual alignment, blank lines between `:CASE`
  arms) are recorded in the same file as decisions rather than gaps.

### Changed
- **`builtin_excess_arguments` and `format_arg_not_array` now run on the
  expression AST** (issue #184). Both rules shipped on token scanning and
  Hungarian guesswork and recorded the expression tree as their upgrade
  path; they are the tree's first diagnostic consumers.
  `parser.CollectCalls` builds a call-site index — callee, receiver, and
  argument subtrees — once per document, and `collectDiagnostics` shares
  it across every call-shaped rule.
  - `builtin_excess_arguments` now excludes a method call by it *being* a
    method call at any receiver shape (`aDocs[1]:Left(...)`,
    `GetDoc():Left(...)`), and takes each surplus argument's range from
    its own subtree.
  - `format_arg_not_array` types both sides instead of matching tokens: a
    receiver is any expression inferring to a string
    (`AllTrim(sTpl):Format`, `Me:sTemplate:Format`), and the second
    argument is judged by inferred type rather than having to be a single
    token — so `sA + sB`, `AllTrim(sA)`, `Len(sA)`, and `nCount > 3` flag
    where they were previously unprovable and silent. Identifiers now
    need a documented Hungarian prefix to claim a type, so `xThing`,
    `vThing`, and loop counters are unknown rather than presumed scalar.
  Both changes are output-identical on the 6,228-file production corpus
  (97 and 0 hits, before and after), and end-to-end `--validate`
  throughput is unchanged within run-to-run noise.

### Fixed
- **Rename rewrote unrelated symbols that merely shared a name** (issue
  #184). Rename and reference search matched identifiers by word, so
  renaming a variable `sName` also edited the property in `oRec:sName`
  and a `:PROCEDURE sName;` header — silently corrupting code the user
  never meant to touch. Go-to-definition on a member name jumped to the
  like-named local, and hover reported the local's declaration for it.
  Identifier occurrences are now classified by role from the expression
  tree (`parser.IdentifierRoles`): variable reference, member name, call
  callee, class name, declared name, or procedure header. Occurrences
  playing a different role than the symbol under the cursor are not
  references to it. Positions the tree cannot resolve stay unclassified
  and keep the prior word-match behavior, so coverage does not regress.

- **Declarations were resolved by line, so a bare `:DECLARE` hid every name
  it declared** (issue #184). `ExtractVariables` read a declaration's names
  out of an AST node grouped by line, and the undeclared check's
  declaration-site exemption asked whether a declaring keyword sat on the
  *same line*. A declaration written as

  ```ssl
  :DECLARE
      sDebugSQL,
      sSQL;
  ```

  therefore registered none of its names and exempted none of them, so
  every name flagged itself at its own declaration. Names and binding
  spans now come from `parser.CollectDeclarations` /
  `parser.DeclarationSpans`, which read the statement through its
  terminating `;`. On the production corpus this was 241 of 2,060
  `undeclared_variable` hits (11.7%); afterwards no flagged name appears
  on a declaration line anywhere in its own file. Declared names also feed
  document symbols, rename, the workspace index, and every name-shaped
  diagnostic, so the loss was not confined to one check.

- **`builtin_excess_arguments` crashed on a surplus run ending in skipped
  argument slots.** `Left(sText, nA, nB,,)` indexed the argument list at
  -1 while building the diagnostic range and panicked; the pipeline's
  panic recovery turned that into a single `internal_error` diagnostic,
  so the file lost its real diagnostics. Argument ranges now come from
  the argument subtrees, which are always well-formed.

- **Semicolon enforcement skipped bare `:BEGINCASE`.** It was the only
  block opener that never got a semicolon: the token after it is always
  `:CASE` or `:OTHERWISE`, both continuation keywords, so the lookahead
  in `needsSemicolonAtLineEnd` bailed out before deciding. `:BEGINCASE`
  takes no operand, so it is a complete statement on its own and is now
  terminated regardless of what follows — matching `:IF`, `:TRY`,
  `:WHILE`, and `:PROCEDURE`, and the `:BEGINCASE;` form the catalog
  documents throughout. No corpus impact (0 of 6,228 files change; real
  code already writes the semicolon) and full-corpus idempotence is
  unaffected.

## [0.18.0] - 2026-08-28

The production-corpus release. A private, production-representative
corpus of 6,228 STARLIMS server scripts and data sources became the
project's standing test bed, and this release is what it produced:
twelve new diagnostics from the runtime-verification issue batch
(#185–#200), the opt-in **info severity tier** for assistant/LLM
consumers (with seven new SQL advisories and four advisory
reclassifications), an adversarial formatting review
(docs/reviews/2026-08-28) whose every finding was fixed — comment
preservation, spliced-literal byte-preservation, ODBC escape atomicity,
and full-corpus idempotence (1,008 unstable files → 0) — plus the
corpus-owner-decided SQL conventions (identifier case preserved by
default, `compact` retired, `standard` reworked) and milestone 1 of the
expression-level AST (#184). Final formatter harness: zero panics, zero
content mutations, zero formatting-introduced diagnostics, zero
non-idempotent files. Downstream consumers pick this up via the
ssl-style-guide MCP binaries and vs-code-ssl-formatter extension bumps
(scopes: vs-code-ssl-formatter#95, ssl-style-guide#59 — one new
required setting, `ssl.diagnostics.infoDiagnostics`, plus
`ssl.format.sql.identifierCase`).

### Changed
- **SQL style decisions from the formatting review (#219,
  corpus-owner-decided).** Four convention changes:
  - `ssl.format.sql.identifierCase` added (`preserve` | `lower` |
    `upper`), default **preserve** — force-folding identifiers is
    dialect-conditional (it breaks queries on SQL Server case-sensitive
    collations) and rewrote the corpus's uppercase house style; the old
    always-lowercase behavior is now the `lower` opt-in. Double-quoted
    identifiers and ODBC escape interiors are preserved regardless.
  - The `compact` SQL style is retired: accepted as a deprecated alias
    for `canonicalCompact` (its half-multiline output was internally
    inconsistent).
  - The `standard` style now respects `maxLineLength` (proactive
    wrapping, previously 130+ column predicate lines) with fixed
    one-level continuation indent instead of open-paren alignment
    (DECODE arms no longer drift to column ~70).
  - A rewrite of a detected-SQL string always takes the rule-F
    multi-line form — never an in-place padded single line — so the
    string's runtime value changes only when real relayout happens.
  - canonicalCompact: the SELECT item following a multi-line group
    (DECODE/CASE/window) starts a fresh continuation line, keeping the
    alias with its block instead of packing after the closing paren.

### Fixed
- **SQL formatter: ODBC escapes and placeholders are atomic (#217).**
  Three related defects: (1) the closing `}` of an `{fn …}` escape glued
  to a following token (`}AS owner`, `}itemid` — invalid SQL on both
  target DBMSs); (2) a `?…?` placeholder whose interior holds quoted
  content was fragmented by the SQL lexer and respaced — such spans
  (including suspect corpus patterns like `?'<<username>>'?`) are now
  byte-preserved as atomic placeholders, neither legitimized nor
  rewritten; (3) case-folding reached inside escapes — ODBC type names
  were lowercased as identifiers (`SQL_VARCHAR` → `sql_varchar`) and
  scalar-function casing depended on inventory membership. Inside
  `{…}`: the marker is canonical lowercase, the function name after
  `{fn` and `SQL_*` tokens are uppercase, and every other interior
  token keeps the author's casing.
- **Formatter is idempotent over the entire production corpus (#218).**
  Format-twice differed on 1,008 of 5,136 corpus files; now zero. Five
  root causes, each with a pinned regression: (1) comma-before-closer
  spacing — the no-whitespace path wrote `,, )` while the whitespace
  path suppressed to `,,)`, oscillating forever; (2) wrap fragments of a
  line that is already a continuation indented one deeper than the
  stream formatter's fixed continuation level and were flattened back
  on the next pass; (3) trailing commas at delimiter depth 0 now count
  as statement continuations, so wrapped `:DECLARE`/`:PARAMETERS` lists
  keep their indent; (4) space before a captured end-of-line comment
  compounded with the flush's two-space separator; (5) the wrap engine
  now ignores end-of-line comments when classifying the next line,
  matching the stream formatter. The sweep also surfaced a content
  hazard, fixed here: **SQL `--` line comments in reflowed strings now
  always end their output line** — the reflow was gluing following code
  into the comment (`-- note` + `NOT EXISTS` → the DBMS reads the
  predicate as comment text).
- **Formatter: multiple end-of-line comments on one line all survive
  (#215).** The pending-EOL-comment slot merged by clobbering, silently
  deleting all but the last comment (`x := ""; /*old; /*note;` lost
  `/*old;`). Production corpus: 2 files were losing deliberately kept
  commented-out code.
- **Formatter: concatenation-continued SQL character literals are
  byte-preserved (#216).** Detected-SQL strings with an unbalanced
  single-quote count end (or begin) inside an open `'` literal continued
  across concatenation; reflowing them injected whitespace/newlines into
  literal content — malformed `{d '…'}` ODBC date escapes, `IN ('`/
  `LIKE ('` patterns gaining whitespace. 403 corpus files were exposed;
  such fragments now pass through untouched.
- **Formatter: trailing-whitespace trim no longer reaches inside
  multi-line string literals.** The post-pass trimmed line-ends inside
  string content (343 corpus files), violating the string-bytes
  contract; it now skips line-ends that fall within string, code-block,
  and region-body tokens.
- **Formatter: no forced semicolon after a bare declaration keyword.** A
  line ending in `:PARAMETERS` (list on continuation lines) received a
  forced `;`, truncating the statement and orphaning the list — one
  corpus file materialized five `default_after_parameters` errors from
  formatting alone. `:PARAMETERS`/`:DECLARE`/`:PUBLIC`/`:DEFAULT`/
  `:INCLUDE`/`:INHERIT` now count as non-statement-ending keywords.

  Corpus effect of the four fixes: formatting introduces **zero** new
  error-severity diagnostics (was 1 file), zero string-content
  mutations, and non-idempotent files drop 1,008 → 807.

### Changed
- **The info severity tier is now opt-in** (`ssl.diagnostics.infoDiagnostics`,
  default off; `--validate --info` on the CLI). Info diagnostics are
  repositioned as advisory detail — style observations and idiom notes
  aimed at assistant/LLM consumers and teams that want the full picture —
  and are dropped by default so the everyday surface stays
  errors/warnings/hints. A rule explicitly listed in
  `ssl.diagnostics.rules` always shows regardless of the gate (including
  rules remapped *to* info); that is also the per-rule promotion path.
  Hints are not gated.
- **Advisory rules moved into the info tier.** Four long-standing rules
  reclassified from warning/hint to info because they are observations,
  not actions: `max_block_depth` (complexity threshold),
  `limit_public_vars` (shared-state stance, fires on every `:PUBLIC`),
  `max_params_warning` (API-shape threshold), and `negative_logic`
  (readability preference). Together with the nine already-info rules
  and this release's additions, the tier holds eighteen rules —
  enumerated in CONFIGURATION.md §5.8.
- **`nil_method_call` understands qualification (#207).**
  `Me:oClient := NIL` no longer registers the bare member name, a
  `:`-qualified occurrence (`Me:oClient:Send()`) no longer matches
  tracked locals, and tracking resets per procedure — a teardown can no
  longer poison the whole file. Eliminated the production corpus's
  single largest false-positive class (1,391 hits, 7.3% of the run).

### Added
- **Seven info-tier SQL advisories (#220).** Observations about embedded
  SQL that the formatter cannot act on, detected over the SQL lexer's
  token stream (comments and character literals can never false-trigger)
  and all gated by `ssl.diagnostics.infoDiagnostics`:
  `sql_comma_join` (pre-ANSI comma joins — 486 corpus statements),
  `sql_legacy_outer_join` (Oracle `(+)`), `sql_inconsistent_alias`
  (mixed `AS`/bare aliases in one SELECT list), `sql_literal_splice`
  (a `'…'` literal continued across concatenation — the formatter's
  byte-preserve class and the injection surface; suggest `?param?`),
  `sql_dialect_mix` (Oracle-only and MSSQL-only idioms in one
  statement), `sql_select_star`, and `sql_suspect_placeholder`
  (a `<<…>>` template marker stacked inside `?…?`; plain `?'Y'?`
  quoted-literal placeholders are an established idiom — 271 corpus
  uses — and never flag).
- **Expression-level AST (#184, milestone 1).** New lazy expression parser
  in `internal/parser` (`ParseExpression`,
  `ExtractStatementExpressions`): precedence-climbing over the existing
  token stream, grammar-faithful to `ssl-ebnf-grammar.md` (power
  right-associative, unary binds tighter than power, member/call/
  subscript/instantiation as postfix, skipped arguments explicit,
  assignment-in-group idiom supported). Nothing runs during structural
  parsing — trees are built on demand, and unresolvable regions degrade
  to `ExprUnknown` silently. Corpus validation: 98.8% of 41,848
  expression-bearing statements across 3,300 SSL files parse complete in
  0.6s (remaining misses are non-SSL fragment content in documentation
  corpora). `dev/exprcoverage` measures this and is the regression
  harness for future grammar work. No diagnostic behavior changes yet —
  consumers arrive in later milestones.
- **`c_style_comment_closer` (#208 discussion).** Info-tier note on
  comments closing `*/;` — valid, purely stylistic (SSL never sees the
  `*/`; the `;` is the real terminator), but the `*/` encodes a wrong
  mental model of where SSL comments end. The first rule designed for
  the opt-in info tier.
- **`step_zero_literal` (#199).** Warns on a `:FOR` loop whose `:STEP` is a
  provable literal zero (`0`, `0.0`, `-0`) — the loop variable never
  advances, so the loop cannot terminate once entered. Variable or
  expression steps are left alone.
- **`exitcase_after_return` (#190).** Hints on an `:EXITCASE` that
  immediately follows a branch-level `:RETURN` inside a `:BEGINCASE` —
  the `:RETURN` already leaves the procedure, so the `:EXITCASE` is
  unreachable. A common generated/refactored pattern given the guidance
  to end every `:CASE` with `:EXITCASE`.
- **`mixed_error_handling_families` (#191).** Warns when a procedure
  combines the legacy `:ERROR;`/`:RESUME;` marker statements with
  structured `:TRY`/`:CATCH` — the legacy handler can intercept a raised
  error before the `:CATCH` sees it. Statement-position detection only:
  `:ERROR` in expression position (`LimsString(:ERROR)` in a handler, a
  corpus-observed pattern) does not count as the legacy family.
- **`runsql_non_dml` (#195).** Warns on a `RunSQL` call whose SQL begins
  (after stripping leading SQL comments) with `SELECT` or `WITH` — the
  result is silently discarded; use a result-returning API instead.
  `SELECT ... INTO` and `WITH`-wrapped DML are recognized as writes and
  left alone.
- **`unicode_literal_prefix` (#196).** Info tier: notes `N'...'` Unicode
  literal prefixes in embedded SQL.
- **`unjustified_collate` (#197).** Info tier: notes `COLLATE` in
  embedded SQL when no comment directly precedes the containing
  statement.
- **`trailing_skip_commas` (#193).** Hints on skip-commas immediately
  before a call's `)` — the runtime NIL-pads missing trailing arguments,
  so they add nothing. Interior skips (positional placeholders) and array
  literals are untouched.
- **`spaced_skip_commas` (#193).** Info tier: notes `, ,` skip-comma
  pairs written with whitespace between them; the adjacent `,,` form is
  the preferred style. Promote via `ssl.diagnostics.rules` for the
  originally proposed warning severity.
- **`format_arg_not_array` (#194).** Warns when `sFmt:Format` receives a
  provably scalar second argument (or more than two arguments) — Format
  takes ONE array holding every replacement value. Hungarian-heuristic on
  both sides; `String:Format` (.NET, legitimately variadic) is excluded.
- **`visibility_annotation_usage` (#198).** Info tier: notes every
  effective `/*@private;`//`/*@protected;` annotation for teams that
  prefer procedures unannotated; never double-reports annotations the
  always-on `visibility_annotation` rule already flags.
- **`builtin_excess_arguments` (#200).** Warns on a builtin call passing
  more arguments than the element inventory's signature accepts — the
  SSL compiler silently drops the surplus (never evaluated), so wrong
  arity survives indefinitely. Variadic and unknown-arity builtins never
  flag; `:`-qualified method calls are excluded.
- **`invalid_limstypeex_comparison` (#187).** Errors on a comparison
  (`=`, `==`, `!=`, either operand order) between `LimsTypeEx(...)` and a
  string literal outside its fixed result set (NIL, STRING, NUMERIC,
  LOGIC, DATE, ARRAY, CODEBLOCK, OBJECT, SSLVALUE) — such a guard is
  provably dead; the chronic bug is `"NUMBER"` for `"NUMERIC"`.

## [0.17.0] - 2026-08-12

A real-world-corpus false-positive batch: a validation run over ~5,200
stock SSL files surfaced eight false-positive classes (issues #164–#171),
all fixed here. The headline change makes `:REGION` bodies opaque at the
lexer — alone responsible for 27% of corpus validation failures. Also
emits stable rule codes in `--validate` JSON output (PR #172). Downstream
consumers pick this up via the ssl-style-guide MCP binaries and
vs-code-ssl-formatter extension bumps.

### Added
- **Stable rule codes in `--validate` JSON output.** Each diagnostic in the
  CLI's JSON now carries its `code` slug alongside severity and message, so
  corpus tooling can bucket failures by rule.

### Changed
- **`:REGION` bodies are opaque payload (#164).** The lexer captures
  everything between `:REGION <name>;` and a line-leading `:ENDREGION` as a
  single raw token: region bodies are stored text retrieved via
  `GetRegion()`, not SSL, so no diagnostic ever fires on them and the
  formatter passes them through verbatim. Stock scripts wrapping
  HTML/JS/XML/SQL templates in regions — 27% of corpus validation failures —
  now validate clean. An unclosed region still reports `unclosed_block`.
- **`direct_procedure_call` severity is tiered (#167).** Calling a
  `:PROCEDURE` declared in the same file keeps the error (dispatch bypass is
  provable); an unknown bare callable warns instead — it cannot be
  distinguished from a vendor built-in missing from the published inventory
  (`SetLocationSQLServer`, `LimsCleanUp`, `SetAMPM` in stock scripts, the
  largest post-region corpus bucket at 90 files).
- **`me_outside_class` warns in include-library files (#171).** A classless
  file consisting solely of `:PROCEDURE` blocks is the shape of an
  `:INCLUDE` library compiled into a class, where `Me` is valid at runtime —
  such files warn instead of erroring. Any top-level statement restores the
  error.
- **`zero_based_array_index` tracks .NET derivation (#166).** A variable
  whose most recent assignment comes from a colon member call or a
  `LimsNetConnect`/`LimsNetCast` result (`aBytes := oInt:ToByteArray();`)
  downgrades a later `[0]` to the .NET warning introduced in #152. A
  non-.NET reassignment restores the error.

### Fixed
- **`bare_logical_operator` on identifiers named And/Or/Not (#165).** The
  check was position-blind; WSDL-generated proxy classes really do declare
  members named `And`/`Or`. It now fires only in expression-operator
  positions (`And`/`Or` between operands, `Not` as prefix) — declaration
  lists, assignment targets, and member access never flag.
- **Comments ended statements in placement checks (#170).** A comment token
  mid-statement reset statement tracking in `default_after_parameters` and
  `parameters_first`, so a multi-line `:PARAMETERS` list with inline
  comments "ended" at the first comment and the following `:DEFAULT` (or the
  remaining parameters) flagged. Only `;` ends a statement now.
- **`parameters_first` contradicted `include_early` (#168).** `:INCLUDE`
  counted as a top-level statement, flagging the include-then-parameters
  pattern the style guide itself prescribes. It is a paste-time directive
  and is now placement-transparent. `:BEGININLINECODE ... :ENDINLINECODE`
  is also modeled as a scope: a named inline-code block's leading
  `:PARAMETERS` is judged against the block, not the script.
- **`global_assignment` ignored in-file declarations (#169).** A declared
  local case-insensitively colliding with a status keyword (loop variable
  `iS` vs `IS`) flagged, as did system-init scripts assigning the `:PUBLIC`
  global they just created. An in-file `:DECLARE`/`:PARAMETERS`/`:PUBLIC`
  declaration now suppresses the check for that name.

## [0.16.0] - 2026-08-08

A SQL data-source hardening batch: two new data-source diagnostics, an
inverted `.ds` classifier so SSL checks stop firing on SQL, and a cluster
of false-positive fixes for qualified names, class-method dispatch, and
.NET indexing. Downstream consumers pick this up via the ssl-style-guide
MCP binaries and vs-code-ssl-formatter extension bumps.

### Added
- **`datasource_undeclared_placeholder` diagnostic (ssl-style-guide#51/#53).**
  A `@name` placeholder in a SQL-mode data-source body with no matching
  `:PARAMETERS` declaration warns — it is not substituted and fails when the
  query executes. Structural exclusions keep `@@` system functions, declared
  placeholders (any casing), `@name` inside string literals and SQL comments,
  and `DECLARE`-scripted bodies silent.
- **`datasource_sql_semicolon` diagnostic (#154).** A bare `;` outside
  comments and string literals in a SQL-mode data-source body warns: the body
  runs as a single SQL command, and `;` statement separators are not part of
  the data-source format and may fail on some database platforms. Honors rule
  overrides; never fires outside data-source files.

### Changed
- **`.ds` files classify as SQL by default (#153).** SQL-vs-SSL detection was
  inverted. A data-source file is now SQL unless its body (directive /
  `:PARAMETERS` header split off) carries a strong, SQL-exclusive SSL marker —
  a non-directive colon keyword, a `:=` assignment, or a leading unterminated
  `/*` comment. The former structural-SQL detector rejected valid queries it
  could not distinguish from English prose (a SELECT list with implicit column
  aliases, `col alias`), leaking SSL diagnostics like `bare_logical_operator`
  onto legitimate SQL `and`. Genuine SSL data sources keep the full diagnostic
  set.
- **Zero-based indexing on .NET objects is a warning, not an error (#152).**
  The pattern is valid against .NET collections, so it no longer blocks as an
  error.

### Removed
- **`datasource_default_required` rule (#147, ssl-style-guide#48).** The
  data-source builder accepts `:PARAMETERS` without inline `:=` defaults
  (`:PARAMETERS sName, nCount := 10;` is valid), so the rule is gone — no
  default-related diagnostic fires on a defaultless data-source parameter.

### Fixed
- **`undeclared_variable` false positives on declaration names (#149, #155).**
  The qualified base name in `:INHERIT Category.ScriptName;` (#149) and the
  class name in `:CLASS Name;` (#155) were flagged as undeclared variables.
  Both are declarations, not variable uses — same skip-until-semicolon
  exemption as `:INCLUDE` paths — and no longer flag; ordinary undeclared
  identifiers still do.
- **`dot_property_access` on `:INHERIT` qualified names (#149).** The dots in
  an `:INHERIT Category.ScriptName;` base name are path separators, not
  property access, and no longer flag.
- **`doproc_in_class` on qualified script references (#151).** A qualified
  `DoProc`/`ExecFunction` dispatch to a script procedure from a class method no
  longer reports a spurious compile error — only genuinely class-targeted
  dispatch does.
- **SQL-mode classification robustness (#148, #154).** SQL data sources now
  stay SQL-classified when a column or table name collides with a SQL
  builtin-function name (`set FORMAT = …`, `delete from FORMAT`), when SQL
  comments or quoted literals contain semicolons (`'all;msoffice->pdf'`), and
  when a banner comment precedes the builder-directive header or the file is
  comment-only — cases that previously fell back to SSL parsing and drew false
  diagnostics.

## [0.15.0] - 2026-08-07

Clears the whole open-issue backlog (#132, #138–#143) in one batch:
three new diagnostics — including the first cross-file one — two
formatter fixes, data-source parity for the `--validate` CLI, and an
element-data refresh carrying the upstream error-handling doctrine.
Downstream consumers pick this up via ssl-style-guide#34 (MCP binaries,
`ssl_diagnose --ds` wiring) and vs-code-ssl-formatter#90 (extension).

### Added
- **`raiseerror_in_catch` diagnostic (#142).** From the RaiseError
  placement doctrine (ssl-style-guide#36): a `RaiseError(` call whose
  nearest enclosing `:TRY` section is a `:CATCH` block warns — the error
  handler must not become the thing that crashes. Raise-only helpers,
  `:TRY`-body raises, and nested handlers stay silent.
- **`execfunction_class_target` diagnostic (#143).** Cross-file check:
  an `ExecFunction` dispatch string that resolves through the workspace
  index to class files only errors — class files have no script entry
  point and their methods are not invokable this way
  (ssl-style-guide#42). Conservative by design: any resolution mix with
  ordinary scripts stays quiet, and workspace-less consumers
  (`--validate`) skip the check.
- **Element data refresh.** Vendored reference/meta pick up the
  error-handling and logging sweeps: RaiseError/GetLastSSLError/CATCH
  doctrine content in hover, ExecFunction and DoProc class-file caveats,
  operator titles with symbols, corrected RunSQL return semantics
  (caveats coverage 363→400 elements, best practices 409→458).
- **`:DECLARE` initializer diagnostic (#138).** `:DECLARE x := 1;` was
  silently accepted even though authoritative SSL permits only a
  comma-separated identifier list — class-level "constants" declared
  this way are never assigned at runtime. Each inline `:=` in a
  `:DECLARE` statement is now an error (`declare_initializer`), in
  every context: procedure locals, script level, class fields, and
  data-source files.
- **`--validate --ds` flag.** Declares stdin content a data-source
  document, since piped input has no `.ds` extension to detect; SQL-mode
  suppression and the data-source rule set then apply.

### Changed
- **`prefer_exitcase` accepts `:RETURN` terminators (#139).** A `:CASE`/
  `:OTHERWISE` clause whose final statement is `:RETURN` no longer
  demands an unreachable `:EXITCASE`. Final-statement position only — a
  conditional `:RETURN` mid-clause still flags, since fall-through past
  it remains possible.

### Fixed
- **SQL formatter: `KEEP (DENSE_RANK ...)` (#132).** `KEEP` now cases as
  a keyword, the compound stays glued to its aggregate with contents
  inline (the WITHIN GROUP treatment), and a following `OVER (...)`
  window spec anchors at the aggregate's column instead of a broken
  continuation line.
- **SQL formatter: split SQL-string assignments converge (#140).** A
  line break between `:=` and a detected SQL string, or between the
  string and its `;`, was preserved forever, so layout depended on how
  the input happened to be split. Those seams now rejoin before layout:
  short SQL lands inline (`sSQL := "SELECT 1";`), long SQL reflows to
  the canonical rule-F shape. Non-SQL strings keep their line breaks.
- **`--validate` false positives on `.ds` files (#141).** The CLI fed
  tokens straight to diagnostic collection, bypassing the SQL-mode
  data-source classification the editor path already had — so every SSL
  check fired on the SQL body of `.ds` files (`dot_property_access` on
  `table.column` names, bare `AND`/`OR`, missing semicolons). The CLI
  now routes through the same classification: plain-SQL data sources
  produce no diagnostics, and the hybrid `:PARAMETERS`-then-SQL shape
  keeps checks on its header only. This also fixes the downstream
  `ssl_diagnose` MCP tool once it picks up this release.

## [0.14.1] - 2026-07-25

### Changed
- **Element data refresh.** The vendored element reference and metadata
  pick up the reference-content fixes from starlims-ssl-reference#16:
  nine expanded summaries and five newly parseable return descriptions
  now show in hover and member completion.

### Added
- **Third-party notices.** `THIRD-PARTY-NOTICES.md` collects the license
  texts of the 19 Go modules statically linked into the release
  binaries; `make notices` regenerates it, and releases attach it
  alongside the binaries automatically.

## [0.14.0] - 2026-07-25

Cross-file references and rename complete the cross-file milestone
(#125); the element reference catches up to the canonical source with
drift guards so it can never silently go stale again (#123); and the
OVER() window-spec layout lands per spec (#122). All work shipped
through adversarially-reviewed specs, and the release itself passed a
four-reviewer deep review (9 pre-tag fixes, including two
corruption-class rename bugs caught before any user hit them).

### Added
- **Cross-file rename (#125 Phase B).** Renaming a procedure — from its
  declaration, an identifier use, or the last segment of a dotted
  dispatch string (prepare-rename now allows exactly that segment) —
  produces a WorkspaceEdit spanning the definition file and every
  caller. The write side is conservative where references are liberal:
  only sites that resolve unambiguously to the renamed procedure are
  edited (last segment only; quotes and prefixes untouched), every edit
  is recomputed from the file's current content at request time (open
  buffers live, closed files re-read from disk — never indexed
  positions), and procedures in `:CLASS` files refuse the cross-file
  path because `obj:Method()`/`Base:Method()` callers are invisible.
  Without a workspace index, rename is unchanged. Catalog entry amended
  (A9–A16 + Known gaps).
- **Cross-file references (#125 Phase A).** Find-references on a
  procedure now returns dotted `DoProc`/`ExecFunction` call sites across
  the workspace — requested from the declaration, an identifier use, or
  the call-site string itself. A token-based call-site extractor feeds
  the workspace index for candidate discovery; every candidate re-runs
  the same dispatch resolution go-to-definition uses (degradation chain,
  uniqueness gate, open-document overlay), so the two features cannot
  disagree. Dotted self-sites inside the definition file — previously
  invisible — are included. `DispatchTargetAt` is now token-walk based,
  so multi-line dispatch calls resolve from the cursor too. Variables
  stay single-file; without a workspace index, behavior is unchanged.
  Catalog entry amended (A10–A14 + Known gaps).
- **Returns-category objects in the element inventory (#123).** The
  vendored element reference and meta JSONs are refreshed from
  starlims-ssl-reference (446 → 460 elements): 12 returns-category
  objects (HttpClient, HttpResponse, SoapClient, SSLRequest,
  SSLResponse, …) and the `Request`/`Response` special forms now reach
  the LSP as `GeneratedReturnsObjectDetails` (+ their meta via
  `LookupMeta`). The generator now fails on any totals key it doesn't
  handle, so an upstream category can no longer be dropped silently.
- **Returns-object member surface (#123).** Hover on a returns-object
  name (`HttpResponse`, `SoapClient`, …) renders its summary, members,
  and element meta like classes do. In endpoint files, `Request:` /
  `Response:` complete from their backing `SSLRequest`/`SSLResponse`
  member sets and the ambient hover now renders from published data;
  outside endpoint files those identifiers stay ordinary variables.
- **Typed-receiver inference (#123).** Variables assigned from producer
  chains — `oClient := WebServices{}:CreateHttpClient()`, follow-on
  hops like `oResp := oClient:GetResponse()`, class constructor
  literals, and class-returning builtins (`GetConnectionByName`) — now
  get member completion and member hover for the class or returns
  object they hold. A typed receiver's unknown member hovers as null,
  never an unrelated symbol. File-global, last-write-wins, mirroring
  UDObject shape inference; cross-procedure propagation deferred.
- **Vendored-data drift guards (#123).** A new test compares both
  vendored JSONs byte-for-byte against a sibling ssl-style-guide
  checkout (skips when absent) and always cross-checks internal totals
  against the generated inventory; CI now verifies `go generate` is a
  no-op on every push.

### Fixed
- **77 broken class-method names in completion (#123).** 42 method rows
  used the `method` JSON key and emitted empty names (invisible to
  completion — all of WebServices' methods among them); 35 more carried
  paren signatures that flowed verbatim into completion insert text
  (inserting e.g. `IsRunning(vBatchId)` literally). Method names are
  now normalized to the bare name at generation time.
- **OVER() window-spec layout (#122).** Long window specs now follow
  sql-canonical-compact-reference §3.1: a space before the paren
  (`OVER (`), each clause (PARTITION BY / ORDER BY / ROWS / RANGE) on
  its own line indented 4 past the window function's column, and the
  closing `) AS alias` on its own line at the function's column. Short
  specs stay fully inline (S48) — previously every spec broke, with a
  glued `OVER(`, a fixed indent unrelated to the function's position,
  and the closer glued to the last clause.

## [0.13.0] - 2026-07-22

Formatter hardening release: every finding from the adversarial
conformance review against the style-guide schema and
sql-canonical-compact-reference is closed (issues #81–#104, #118–#119;
full report in `docs/reviews/2026-07-22-formatter-conformance-review.md`).

### Fixed
- **Corruption-class bugs.** Bracket-quoted SQL strings reflowed with `[`
  as the closing delimiter, destroying the file on the next format (#81).
  English strings containing SQL trigger words ("Select the samples from
  the rack and update…") were detected as SQL and rewritten — detection
  now rejects prose shapes, and only argument 0 of a SQL function is ever
  a SQL candidate (#82). A number literal consumed the dot of a glued
  dot-operator, corrupting later tokens on the line (`nA>=10.and.nB<=20`
  → `nB< = 20`) (#83). Formatting mangled SQL-mode data sources
  (semicolons injected, `:bind` variables re-cased); both formatting and
  diagnostics now recognize the hybrid directive-headed
  `sql_data_source` shape — directives keep their checks, the SQL body
  is left alone (#84, #104).
- **Idempotence (feature.formatting A6).** Over-long atomic strings grew
  a blank line on every format pass (#85); wrapped operator continuations
  lost an indent level on reformat (#86); unterminated strings gained a
  stray semicolon per pass (#87); glued operator pairs double-spaced
  (#88). Format-on-save is now a fixpoint, enforced by a 42-fixture
  format-twice harness and catalog After-fence stability checks (#103).
- **SQL canonical-compact conformance.** INSERT column lists and VALUES
  use block style — no more stranded closing paren (#93); DECODE pairs
  align under the first argument (#94); MERGE multi-line ON conditions
  align under the first condition and UPDATE SET keeps its first
  assignment inline (#95); chained CTEs break to column 0 (#96); long
  `||` concatenations wrap with the operator leading (#97); INSERT ALL
  branches format uniformly with the source SELECT at column 0 (#118);
  `WITHIN GROUP` is a compound clause and `ON OVERFLOW` is no longer
  mistaken for a join ON (#119).
- Range-formatting a mixed tab/space selection no longer dedents the
  block to column 0 (#98); postfix `++`/`--` statements receive semicolon
  enforcement (#99); a statement written after a standalone comment on
  the same line moves to its own line (#101).

### Changed
- **Wrap engine rebuilt** (#89). Line wrapping is now a whole-line
  post-format pass with a conformance guarantee: a line exceeds
  `maxLineLength` only when a single atomic token cannot fit. Breaks land
  after commas, after `:=`, or before binary operators (operator leads
  its continuation); subscripts are atomic like member-access chains; the
  92–107-column overshoots, split subscripts, and inner-call breaks of
  the old streaming wrapper are gone. Continuation lines sit exactly one
  level past the statement line (lexical), including after a trailing
  `:=` or operator.
- **Schema-canonical forms normalize by default** (DECISIONS.md D12).
  Dot logical operators uppercase (`.and.` → `.AND.`) and `me`/`base`
  receivers canonicalize to `Me`/`Base` (#90); code-block literals take
  the `{|params| expression}` shape (`{|a,b|a+b}` → `{|a, b| a + b}`),
  with conservative pass-through for anything unsafe to rewrite (#91);
  `ssl.format.builtinFunctionCase` defaults to `"PascalCase"` — built-in
  call sites take the documented casing out of the box, `"preserve"`
  remains available (#92). **Downstream note:** clients that declare
  their own default for `builtinFunctionCase` (the VS Code extension)
  must flip it to match.
- Output line endings are LF-only per the style-guide schema; CRLF input
  is normalized (documented as feature.formatting A10).

### Added
- **CLI modes** (#100): `starlims-lsp --format --write` (in place),
  `--check` (CI gate: exit 1 listing unformatted files), and
  `--indent-style` / `--indent-size` / `--max-line-length` / `--no-sql`
  flags.
- Catalog entries `fmt.keyword_case` and `fmt.code_block_literals`;
  DECISIONS.md D11 (the formatter reflows, it does not rewrite) and D12;
  SQL layout authority explicitly delegated to
  `sql-canonical-compact-reference.md`, with its ambiguities filed
  upstream (ssl-style-guide#19) (#102).
- Test infrastructure (#103): the idempotence corpus with a ratcheted
  known-failures list, exact want/got assertions replacing the older
  contains-only checks (net −246 lines), and a table-driven SQL layout
  helper.

## [0.12.0] - 2026-07-22

Same-file `DoProc` hover and SQL-mode data sources (spec:
`catalog/features/hover.md` A17-A18,
`catalog/features/diagnostics_pipeline.md` A10-A12).

### Added
- **Same-file dispatch hover.** Hovering the procedure-name string in
  `DoProc("Proc", {...})` — or a bare 1-part `ExecFunction` target —
  shows the local procedure's docblock hover (description, parameters,
  returns, declaration location), matched case-insensitively. Mirrors
  go-to-definition's same-file semantics for 1-part targets; dotted
  targets keep the cross-file workspace path (#78).

### Fixed
- **No SSL diagnostics on plain-SQL data sources.** A `.ds`/`.ds.txt`
  document whose content is a plain SQL statement now produces zero
  diagnostics — SQL's `table.column` qualified names were false-flagging
  `dot_property_access`, with more noise from bare `AND`/`OR` and
  missing-semicolon checks. Content is classified with the formatter's
  structural SQL detection (leading SQL comments tolerated); SSL-mode
  data sources keep their full diagnostic set (#77).

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

[Unreleased]: https://github.com/mahoskye/starlims-lsp/compare/v0.24.0...HEAD
[0.24.0]: https://github.com/mahoskye/starlims-lsp/compare/v0.23.0...v0.24.0
[0.23.0]: https://github.com/mahoskye/starlims-lsp/compare/v0.22.0...v0.23.0
[0.22.0]: https://github.com/mahoskye/starlims-lsp/compare/v0.21.0...v0.22.0
[0.21.0]: https://github.com/mahoskye/starlims-lsp/compare/v0.20.0...v0.21.0
[0.20.0]: https://github.com/mahoskye/starlims-lsp/compare/v0.19.0...v0.20.0
[0.19.0]: https://github.com/mahoskye/starlims-lsp/compare/v0.18.0...v0.19.0
[0.18.0]: https://github.com/mahoskye/starlims-lsp/compare/v0.17.0...v0.18.0
[0.17.0]: https://github.com/mahoskye/starlims-lsp/compare/v0.16.0...v0.17.0
[0.16.0]: https://github.com/mahoskye/starlims-lsp/compare/v0.15.0...v0.16.0
[0.15.0]: https://github.com/mahoskye/starlims-lsp/compare/v0.14.1...v0.15.0
[0.14.1]: https://github.com/mahoskye/starlims-lsp/compare/v0.14.0...v0.14.1
[0.14.0]: https://github.com/mahoskye/starlims-lsp/compare/v0.13.0...v0.14.0
[0.13.0]: https://github.com/mahoskye/starlims-lsp/compare/v0.12.0...v0.13.0
[0.12.0]: https://github.com/mahoskye/starlims-lsp/compare/v0.11.0...v0.12.0
[0.11.0]: https://github.com/mahoskye/starlims-lsp/compare/v0.10.0...v0.11.0
[0.10.0]: https://github.com/mahoskye/starlims-lsp/compare/v0.9.0...v0.10.0
[0.9.0]: https://github.com/mahoskye/starlims-lsp/compare/v0.8.0...v0.9.0
[0.8.0]: https://github.com/mahoskye/starlims-lsp/compare/v0.7.7...v0.8.0
[0.2.0]: https://github.com/mahoskye/starlims-lsp/compare/v0.1.0...v0.2.0
[0.1.0]: https://github.com/mahoskye/starlims-lsp/releases/tag/v0.1.0
