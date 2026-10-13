# Legacy and inherited-behavior inventory — 2026-10-13

> Issue #257 (part of #253). Every deprecated or legacy code path,
> compatibility shim, alias, and fallback in the Go source, every
> user-facing setting, and the LSP's handling of legacy SSL constructs —
> each with why it exists, who depends on it, and a decision: **keep**
> (documented here as intentional), **deprecate** (with a removal plan),
> or **remove**. Items marked *removed in this PR* were dead: no
> consumer and no documented contract.

## Method

- `grep -rniE 'deprecated|legacy|compat|backward|fallback|alias' internal cmd`
  gives 120 hits in non-test Go files (65 more in tests). Each was traced
  to its introducing commit with `git log -S`.
- Consumers checked:
  - **VS Code extension** (`vs-code-ssl-formatter` 1.23.0, bundling
    v0.23.0): `package.json` `contributes.configuration`, the settings
    tree built in `src/lspClient.ts`, and its `--export-signatures`
    use in `src/utils/inventory.ts`.
  - **MCP / `--validate` callers**: the ssl-style-guide MCP server runs
    `--validate --info --hungarian-types --strict [--ds]` and
    `--format [--stdin]`. Other CLI callers use the same two
    commands. None of them reads an LSP settings key.
- SSL-level behavior checked against the published reference
  (`ssl-docs/content/reference/keywords`) and the style-guide rule
  records (`ssl-style-guide/rules`).

Most hits are not legacy code. About 23 are the word "alias" in its SQL
sense (column and table aliases in the SQL formatter and advisories),
3 are "backward" as a scan direction, and about 20 sit in
generated reference data. Those are listed once, under
[Not legacy](#not-legacy-word-matches-only), so the count reconciles.

## 1. Code paths, shims, and fallbacks

### Removed in this PR

| # | Item | Why it existed | Consumer | Decision |
|---|------|----------------|----------|----------|
| C1 | `Parser.ExtractVariables(root *Node)` — the `root` argument | The AST walk it fed was replaced by statement-wise token resolution in #184 (`4dc82f8`). The argument was kept "for API compatibility" and never read again. | None. The package is `internal/`, so only this module can call it. | **Removed.** The signature is now `ExtractVariables()`. `checkUndeclaredVariables` also lost the `ast` parameter, which it took only to pass on. |
| C2 | `GetSignatureHelpWithProcedures(tokens, procedures, …)` — the `procedures` argument (`_ = procedures`) | Introduced in `cdbfee6` when user-defined procedures were deliberately dropped from signature help (feature.signature_help A4). It was "accepted for API compatibility". | None (internal). The handler passed `cache.Procedures` for nothing. | **Removed.** Folded into the existing `GetSignatureHelpFromTokens(tokens, line, column)`. The A4 tests keep their citations and still assert `nil` for direct user-procedure calls. |
| C3 | 43 entries in the curated signature map (`signatures.go`), which was named `legacySSLFunctionSignatures` | A hand-curated overlay that predates the published inventory. `e628475` (#1) made the published reference the inventory, and `buildCanonicalFunctionSignatures` reads the overlay only for published names, so entries for unpublished names (TraceOn, In64BitMode, UndeclaredVars, LimsCleanUp, …) became unreachable. | None. `--export-signatures` output is byte-identical before and after (verified). | **Removed** the 43 unreachable entries. Renamed the map to `curatedFunctionSignatures`, since it is the live overlay, not legacy. New test `TestCuratedSignaturesAreReachable` stops dead entries coming back. |
| C4 | `style != "compact" &&` guards in `SQLFormatter` (5 sites) and the `"compact": Minimal formatting` doc comment | Left behind when #219 (`a825df0`) turned `compact` into an alias. `style` is remapped to `canonicalCompact` a few lines above, so the guards were always true. | None. Output is unchanged. | **Removed.** The alias itself stays (S1). |

### Kept — intentional, now documented

| # | Item | Why it exists | Consumer | Decision |
|---|------|---------------|----------|----------|
| C5 | `FindDefinition` step-3 fallback ("any match wins without procedure info") | `ExtractProcedures` returns `nil` for a script with no `:PROCEDURE` blocks, where every declaration is file-level. Issue #41 (`9ebdc51`) restricted the fallback to file-level declarations when procedure info exists. | Every editor user (go-to-definition in procedure-less scripts). | **Keep.** The misleading "(backward compatibility)" comment was reworded to say what the case actually is. |
| C6 | `FindReferencesWithScope` cursor-line declaration heuristic, and the `resolveDeclarationPosition` any-variable fallback | Lets `includeDeclaration=false` exclude the declaration when symbol info is missing (#42). | Editor users. | **Keep.** |
| C7 | `FindReferences` (wrapper that passes `nil` scope) | Test convenience. Production calls `FindReferencesWithScope`. | Tests only (23 call sites). | **Keep.** Harmless, and it reads well in tests. |
| C8 | Cross-file fallbacks: flat-layout script identity (`deriveScriptIdentity`), basename degradation, and the workspace-unique procedure-name fallback in `ResolveDispatchTarget` | Designed in feature.cross_file_resolution (A1–A3). STARLIMS export trees are not always laid out canonically. A uniqueness gate keeps the guesses quiet. | Editor users (definition, references, rename across files). | **Keep.** Specced in the catalog. |
| C9 | `detectBaseIndent` first-line fallback | Fix for #98: mixed tab/space selections collapsed to column 0. | Range formatting. | **Keep.** Specced as feature.formatting A2. |
| C10 | `SSLLiteralAliases` (`.t.`, `.f.`, `nil` → canonical) | Lets the lexer, hover, and checks recognize the lowercase spellings of the three literals that the formatter canonicalizes (fmt.keyword_case, D12). | Everyone. | **Keep.** These are casing aliases, not legacy syntax. |
| C11 | Lexer `isKeywordWord` / `isLegacyLabelKeywordForm`: glued `:LABELName` forms | SSL accepts glued labels (`:LABELCLEANUP;`), so the lexer must tokenize them as keywords for label_keyword_form, deprecated_keyword, branch_target_label, and unexpected_token to work. | Everyone. | **Keep.** See S-level item L1. |
| C12 | `direct_procedure_call` downgrade to warning for unknown callables ("…if it is a legacy vendor built-in…") | #167. Stock code calls vendor built-ins the published inventory does not list, so calling them an error was a false positive. | Everyone. | **Keep.** |
| C13 | `MaxNumberOfProblems = 100` | Inherited from the original TypeScript server (`442fa69`). Caps published diagnostics per document. | Editor users. | **Keep** the cap. Not client-configurable — see follow-up F2. |
| C14 | `--export-signatures` | Signature export for consumers that cannot link Go. | VS Code extension (`src/utils/inventory.ts`). | **Keep.** It is a consumer contract. |
| C15 | `--hungarian` (both Hungarian checks) next to `--hungarian-types` | v0.21.0 (`f8a0007`): the type check was split out so it can run on code that does not use the convention. | Documented flag. The MCP server uses `--hungarian-types`. Other CLI callers may use either. | **Keep.** |

## 2. Settings audit

The server reads the `ssl.*` tree in `applySettings` (`internal/server/server.go`),
both from `initializationOptions` (#55) and from `didChangeConfiguration`.
There are **no alias keys and no deprecated keys** at the settings level:
every key below is read under exactly one name. The only deprecated item
is a *value* (S1).

| Server key | Read? | Default (code) | Default right? | VS Code extension sends it as | CLI |
|------------|-------|----------------|----------------|------------------------------|-----|
| `format.indentStyle` | yes | `tab` | yes (source guide) | `ssl.format.indentStyle` | `--indent-style` |
| `format.indentSize` | yes | `4` | yes | `editor.tabSize` (tab) / `ssl.format.indentWidth` (space) | `--indent-size` |
| `format.maxLineLength` | yes | `90` | yes | `ssl.format.wrapLength` | `--max-line-length` |
| `format.operatorSpacing` / `commaSpacing` / `semicolonEnforcement` | yes | `true` | yes | same names | — |
| `format.blankLinesBetweenProcs` | yes | `1` | yes | same name | — |
| `format.blankLineBetweenBlocks` | yes | `true` | yes | **not exposed** (default applies) | — |
| `format.trimTrailingWhitespace` | yes | `true` | yes | same name | — |
| `format.maxConsecutiveBlankLines` | yes | `0` | yes | same name | — |
| `format.builtinFunctionCase` | yes | `PascalCase` | yes (D12). **Doc said `preserve` — fixed.** | same name, default `PascalCase` | — |
| `format.sql.enabled` | yes | `true` | yes | same name | `--no-sql` |
| `format.sql.style` | yes | `canonicalCompact` | yes | same name. **Enum still offers deprecated `compact`.** | — |
| `format.sql.keywordCase` | yes | `upper` | yes | same name | — |
| `format.sql.identifierCase` | yes | `preserve` | yes (D14) | same name | — |
| `format.sql.indentSize` | yes | `4` | yes | `ssl.format.sql.indentSpaces` | — |
| `format.sql.maxLineLength` | yes | `90` | yes | `ssl.format.wrapLength` (shared) | — |
| `format.sql.detectSQLStrings` | yes | `true` | yes | **not exposed** (default applies) | — |
| `diagnostics.hungarianNotation` | yes | `false` | yes (D5) | `ssl.naming.hungarianNotation.enabled`, **extension default `true`** | `--hungarian` |
| `diagnostics.hungarianTypes` | yes | `false` | yes (D5) | `ssl.naming.hungarianNotation.checkTypes`, **extension default `true`** | `--hungarian[-types]` |
| `diagnostics.unusedVariables` | yes | `false` | yes (D5) | same name | `--strict` |
| `diagnostics.infoDiagnostics` | yes | `false` | yes (D13) | same name | `--info` |
| `diagnostics.hungarianPrefixes` | yes | `a b d fn n o s v` | yes | `ssl.naming.hungarianNotation.prefixes` (same set) | — |
| `diagnostics.globals` | yes | `[]` | yes | `ssl.globals` | — |
| `diagnostics.maxBlockDepth` | yes | `4` | yes. **Doc said "warning"; the rule is info-tier — fixed.** | `ssl.styleGuide.limitBlockDepth` | — |
| `diagnostics.rules` | yes | `{}` | yes (D2; `warn` is a documented alias of `warning`) | same name; also re-applied client-side | — |
| `diagnostics.endpointPatterns` | yes | `[]` | yes. **Doc said glob; it is substring matching — fixed.** | same name (extension docs already say substrings) | — |
| `inlayHints.enabled` / `minParameterCount` | yes | `true` / `2` | yes | `ssl.intellisense.inlayHints.*` | — |
| `intellisense.signatureHelp.autoTrigger` | yes, **at initialize only** | `false` | yes (#9, D6) | same name | — |

Divergence that is intentional: the extension turns both Hungarian checks
on by default and the server keeps them off. The server default serves
every client (D5). The extension's audience writes new SSL, and the
audience sets the default. Now documented in CONFIGURATION.md §10.2.

Other CONFIGURATION.md corrections in this PR: the §4.1 heading typo
(`sql.format…`); stale `file.go:NN` anchors removed; §8.4 said
`CheckUnusedVars` was not client-exposed (it is, as
`ssl.diagnostics.unusedVariables`); §8.3/§8.5 now name `--strict`; §9.4's
title said "Compact" for an `expanded` example; §10 gave server keys as VS
Code setting names (now a mapping table); Appendix A was missing nine
settings.

### S1 — `ssl.format.sql.style: "compact"` (deprecated value alias)

Retired in #219 (`a825df0`, 2026-08-28) because its half-multiline output
was internally inconsistent. Since then it maps to `canonicalCompact`.

- **Consumer:** the VS Code extension still lists `compact` in its enum
  (labelled deprecated), so users may have it stored in settings.json.
  CLI callers cannot select a SQL style at all.
- **Decision: deprecate, removal planned (follow-up F1).** Order: the
  extension removes the enum value and migrates stored `compact` to
  `canonicalCompact`. The next LSP minor release after that drops the
  alias. An unknown style then formats as `standard`. Each step goes in
  both distributions' release notes.

## 3. SSL-level legacy constructs

Compared with the published reference and the style-guide rule records.
All severities match the style guide. Nothing new needs special handling.

| # | Construct | LSP handling | Reference / style guide | Decision |
|---|-----------|--------------|-------------------------|----------|
| L1 | `:LABEL` / glued `:LABELName` + `Branch()` | `deprecated_keyword` (warning), `label_keyword_form` (error, case), `branch_target_label` (error, target must start with `LABEL`) | Reference: labels are "a legacy control-flow feature", still valid. Style guide `label_is_legacy` advisory/warning; `branch_target_label` authoritative/error. | **Keep.** Accurate, and severities match. |
| L2 | `:ERROR` / `:RESUME` | `deprecated_keyword` (warning), `error_handler_structure` (error), `mixed_error_handling_families` (warning, #191) | Reference: legacy, still executes; `:RESUME` makes `:CATCH` unreachable. Style guide `prefer_try_over_error_resume` style_only/warning, `error_handler_last_section` authoritative/error, `no_mixed_error_handling_families` style_only/warning. | **Keep.** Catalog frontmatter of `diag.deprecated_keyword` corrected: `authority: style_only`, `schema_ref: lints.deprecated_keywords` (it said `tool`/`null`, from before the style guide owned the rule). |
| L3 | `:REGION` / `:ENDREGION` | Parsed, folded, outlined, indented. `region_end_mismatch` (warning) for structure only. No "legacy" diagnostic: `region_legacy` was removed in v0.8.0 (#28) because its premise was wrong. | Reference: current, supported (body stored for `GetRegion()`), not legacy. Style guide `comment_regions_for_grouping` is reviewer-enforced, not LSP. | **Keep.** But the **hover/completion text called `:REGION` and `:BEGININLINECODE` "legacy"**, contradicting #28 and the reference — **fixed** in this PR. So were `docs/ssl-reference/gotchas.md` (which still claimed an Info diagnostic), `docs/features/diagnostics.md`, the style-guide summary, the formatting/completion docs, and AGENTS.md. |
| L4 | `:BEGININLINECODE` / `:ENDINLINECODE` | `inline_code_naming` (error); body parsed as SSL | Reference: current (`GetInlineCode`); body must be valid SSL. | **Keep.** Description fixed with L3. |
| L5 | `<>` and `#` not-equals | `not_preferred_operator` (info tier) | Style guide: not preferred; the operator is named `not-equals-legacy` in the generated operator data. | **Keep.** |
| L6 | Oracle `(+)` outer join in embedded SQL | `sql_legacy_outer_join` (info tier, style_only) | Advisory. | **Keep.** |
| L7 | `:ENDFOR` | `endfor_invalid` (error) | Not valid SSL; `:NEXT` closes `:FOR`. | **Keep.** |
| L8 | Deprecated class members (`SDMSDocument:DownloadDocument` etc., "Always raises an exception") | Hover shows the reference's "Deprecated" text. No diagnostic. | Reference data. | **Keep.** A "calls a member that always raises" check would be new behavior, not legacy cleanup. Not pursued. |

## Not legacy (word matches only)

- **SQL "alias"** (~23): column and table aliases in `sql_formatter.go`,
  plus `sql_inconsistent_alias` and `sql_select_star` in `sql_advisories.go`.
- **"backward" as direction**: `extractProcedureDoc`, signature-help
  context scan, the `IN (` lookback in SQL-param checks.
- **Generated reference data**: `generated_keywords.go` summaries
  ("legacy error handler"), `generated_classes.go` ("Alias of
  Description", "Deprecated. Always raises"), `generated_functions.go`
  (`bR1Compatible`, "legacy built-in RC2, DES, or 3DES",
  `GeneratedFunctionSummaries` "fallback" doc comment),
  `generated_operators.go` (`not-equals-legacy`, "incompatible"),
  `cmd/gen-inventory`. These come from the published reference and change
  only through regeneration.
- **Misc**: `completion.go` `orEmpty(s, fallback)`, `crossfile.go` "never
  a fallback" comment, `expr_types.go` "incompatible", `main.go` help
  text ("Any LSP-compatible editor"), `validate.go` help text.

## Recommended follow-ups (not done here: consumer-affecting or not clear-cut)

| # | Follow-up | Affects |
|---|-----------|---------|
| F1 | Remove the `compact` SQL style alias, in two steps: extension first, LSP second (S1). | VS Code extension users with `compact` stored. MCP/CLI: none. |
| F2 | The extension's `ssl.maxNumberOfProblems` is not forwarded. The server caps at a hardcoded 100. Either the server reads a key (e.g. `ssl.diagnostics.maxNumberOfProblems`) and the extension forwards it, or the extension documents the setting as fallback-provider-only. | VS Code extension. |
| F3 | The extension does not expose `ssl.format.blankLineBetweenBlocks` or `ssl.format.sql.detectSQLStrings`, so VS Code users cannot turn them off. Decide whether to expose them. | VS Code extension. |
| F4 | `intellisense.signatureHelp.autoTrigger` needs a server restart, because trigger characters are fixed in the `initialize` response. Dynamic registration of signature help would apply it live. | VS Code extension (LSP change). |
| F5 | Audit extension-side settings that have no effect while the language server runs (they feed the extension's built-in fallback providers): `ssl.strictStyleGuideMode`, `ssl.naming.hungarianNotation.severity`, `ssl.styleGuide.maxParamsPerProcedure`, `ssl.security.*`, `ssl.intellisense.completion.*`, `ssl.intellisense.customFunctions` / `customClasses`. That audit belongs in the extension repo. | VS Code extension only. |
| F6 | `SQLFormatter` treats an empty `Style` as `standard` in the main pass but as canonical in DECODE alignment (`styleNow`). This is unreachable from both clients, which always send a style. Normalize the style once in `NewSQLFormatter` when the alias is removed (F1). | None today. |
