# Formatter conformance review — 2026-07-22

> Carved into issues #81–#103 (2026-07-22): F1→#81, F2→#82, F3→#83, G15→#84,
> F4→#85, F5→#86, F6→#87, F7→#88, wrap-engine→#89, F8/G1→#90, G2→#91, G3→#92,
> G7/G8→#93, G9→#94, G10/G11→#95, G12→#96, G13→#97, F10→#98, F12→#99, F13→#100,
> comment-adjacent code→#101, catalog/doc gaps + upstream ambiguities→#102,
> test-suite overhaul→#103. Follow-up validation after the report: comment
> preservation confirmed solid (A3 holds — content, headers, multi-line
> interiors all byte-preserved; EOL comments normalized to two spaces, see
> #102); adversarial test-suite review findings recorded in #103.

Adversarial review of `textDocument/formatting` + `--format` against the normative
sources: `ssl-style-guide.schema.yaml` (rules cited as R*), and
`sql-canonical-compact-reference.md` (rules cited as S*), both in the sibling
`ssl-style-guide` repo. Three phases: general adversarial battery, line-wrap
deep-dive, rule-by-rule conformance sweep. Findings are numbered F* (phases 1–2)
and G* (phase 3). Every finding below was reproduced against the current build;
probe fixtures lived in the session scratchpad (`battery/` — recreate from the
examples herein).

## Severity index

| # | Finding | Class |
|---|---------|-------|
| F1 | Bracket-string SQL reflow emits `[` as closing quote; next pass destroys file | P0 corruption |
| F2 | English sentences detected as SQL and rewritten (also: non-first SQL-function args) | P0 corruption |
| F3 | `10.AND.x` lexing corrupts later `<=` on the line (`readNumber` eats the dot) | P0 corruption |
| G15 | No SQL-document gate on formatting: plain-SQL data-source files mangled as SSL | P0 corruption |
| F4 | Wrap before over-long atomic token gains nothing; grows a blank line every pass | P1 idempotence (unbounded) |
| F5 | Wrapped operator continuations lose one indent level on reformat | P1 idempotence |
| F6 | Unterminated string at EOF gains a stray `;` per pass | P1 idempotence |
| F7 | Glued operator pairs emit double space on first pass (`:=  .not.`) | P1 idempotence (converges) |
| F9→wrap-engine | Greedy single-pass wrap: lines land 92–107 cols; bad break points in nested structures | P1 spec violation (R03/R95/S13) |
| F8 | `.and.`/`.or.`/`.not.` never uppercased (R38) | P2 conformance |
| G1 | `me:`/`base:` not canonicalized to `Me:`/`Base:` (R41) | P2 conformance |
| G2 | Code-block literals not normalized: `{|a,b|a+b}` kept verbatim (R42) | P2 conformance |
| G3 | Default `builtinFunctionCase: preserve` contradicts schema exact-casing rule (R45) | P2 decision |
| G7 | INSERT column list: inline+hang-align instead of block style, `)` glued (S34) | P2 SQL layout |
| G8 | VALUES: list left inline but closing `)` moved to col 0 — half-block hybrid (S35/S46) | P2 SQL layout |
| G9 | DECODE: value pairs split across lines with ragged drifting indent (S68) | P2 SQL layout |
| G10 | MERGE `UPDATE SET`: assignments not inline-aligned after SET (S43) | P3 SQL layout |
| G11 | MERGE multi-line ON: `AND` at +2 instead of aligned under first condition (S45) | P3 SQL layout |
| G12 | Chained CTE: `), name AS (` and `) SELECT` glued instead of col-0 breaks (S31) | P3 SQL layout |
| G13 | Long `\|\|` concatenation wraps with trailing operator; spec says leading (S72) | P2 SQL layout |
| F10 | Range format: mixed tab/space selection dedents block to column 0 | P2 |
| F12 | `nX++` / `nX--` statements never get semicolon enforcement | P2 |
| F13 | CLI: no `--write`, no `--check`, no option flags; multi-file output unusable | P2 UX |
| G4 | `<>`/`#` → `!=` canonicalization: lint-only today; formatter opt-in? (R53/R54) | Decision |
| G5 | Statement-order rules (R05, R98–R101): formatter does not reorder | Decision (out of scope?) |
| G14 | Scalar-subquery-in-SELECT alignment: formatter picked flat style; doc shows aligned (S26 vs A4) | Decision + doc |

Resolved as spec'd (no action beyond documenting): CRLF→LF (R02 requires LF);
`:RESUME` indenting the rest of the handler (grammar: resume-mode statements stay
in the error stanza); UNION blank lines (S33); HAVING +2 (S07); long
comparison/member-chain lines left over-limit (fmt.atomic_property_chains).

## P0 details

### F1 — bracket-string SQL closing quote
`FormatSQLInString` (`internal/providers/sql_formatter.go:703`) writes the opening
quote byte at both ends. `sSql := [SELECT ... >90 cols ...];` → block ending `[;`.
Second format swallows the remainder of the file into the string. Fix: map `[`→`]`.

### F2 — SQL detection false positives
`validateSQLStructure` (`sql_formatter.go:788`) passes any "select…from…" /
"update…set…" / "delete…from…" word shape:

```ssl
sMsgA := "Select the samples from the rack and update the status column before continuing with the run";
```
was reflowed + keyword-uppercased (runtime value changed). Spec: reject any run of
≥3 consecutive bare identifiers; require SELECT-list shape (`*` | single expr |
comma-separated); `SET` must be followed by `ident =`; `FROM` target must be
identifier + clause keyword/alias/end. Restrict SQL-function argument formatting to
argument index 0 (use the tracked-but-unused `sqlArgCount`, `formatting.go:238`).
`IsSQLDocument` shares the validator — pin data-source classification with fixtures.

### F3 — number + dot-operator lexing
`readNumber` (`internal/lexer/lexer.go:308`) consumes `.` unconditionally →
`10.` + Unknown(`.nB<`) → `nB< = 20` (meaning destroyed). Fix: consume `.` only
when followed by a digit; Unknown fallback must emit single chars.

### G15 — no SQL-document gate on formatting
Diagnostics gates plain-SQL data sources (`server.go:576`,
`diagnostics.go:107`); `handleFormatting` (`handler.go:823`) and the CLI do not.
A plain-SQL data-source file formatted as SSL gets semicolons injected
(`FROM samples s;`) and bind variables uppercased (`:status` → `:STATUS`).
Spec: same gate as diagnostics — when `IsDataSourceFile && IsSQLDocument`, either
format with the SQL engine (preferred) or return no edits. CLI needs the same
detection (it has no file-type context; gate on `IsSQLDocument` alone or add a flag).

## P1 details

- **F4**: `applyLineWrap` (`formatting.go:542`) must only wrap when the token fits
  after wrapping, and never when the line holds only indentation. Today a 94-char
  string assignment adds one blank line per format pass, forever.
- **F5**: continuation indent is computed two ways — wrap path `indent+1`
  (`formatting.go:565`), re-read path `parenDepth>0 ? 1 : 0` (`formatting.go:473`).
  A source line starting with a binary operator must be treated as a continuation
  (+1). Note: `fmt.semicolon_enforcement`'s own Idempotent example currently shows
  the 0-indent form — the catalog must be updated with whichever convention wins.
- **F6**: when the final token is an unterminated string/comment, skip EOF
  semicolon enforcement / return no edits (ties into the "cannot proceed → leave
  unchanged" clause of `feature.formatting`).
- **F7**: suppress the leading space before an operator when the previous written
  char is already a space.

## Wrap-engine rework (phase-2 conclusion, supersedes point-fix F9)

The single-pass greedy wrapper cannot honor the 90-col limit: legality is checked
only at the token that overflows. Observed: 92–107-col output lines; breaks after
`[` (splitting a subscript); breaks inside inner calls when an outer-comma break
existed; a call split from its `{}` argument. The proactive comma-lookahead branch
(`formatting.go:602`, `estimateRemainingLineLen`) is dead code (only fires on
comma-glued input) and wrong (stops at first `)`).

Spec: buffer the logical line; collect break candidates annotated with paren depth
and kind; solve with preferences — shallowest depth first; after-comma beats
before-operator; never inside `[...]` subscripts; never between a callable and its
opening `(`/`{`. Guarantee: over-limit only when a single atomic token exceeds the
budget. Keep current visual style (operators lead continuations, commas trail,
packed continuations at exactly one extra level). Land with F5; add a
format-twice-byte-equal conformance test over all fixtures.

Current style verified correct and worth preserving: operator-leading continuations
(`+ " completed"`, `.AND. bReady`), atomic strings, flat-list comma packing.

## SSL-side conformance matrix (schema R-rules)

Verified passing: R02 (LF), R04 (final newline), R06–R15 (indent/spacing incl.
delimiter padding removal), R16 (`:STEP` spacing), R17–R19, R20–R22 (blank lines,
incl. `:REGION`), R23/R24/R90 (semicolons + statement splitting), R25–R27 (keyword
casing/canonical forms), R29 (`:LABEL Name`), R30–R34 (comment preservation,
attachment, EOL comments), R39/R40 (`.T.`/`NIL`), R44/R50 (member-colon spacing),
R46 (`/*@private;` stays attached to its `:PROCEDURE` across the blank-line pass),
R47, R48 (no space before call paren), R51/R52 (skipped params `{a,,c}`), R56–R59
(comment/functional regions), R60 (`:CLASS` members at top level), R94 (no
assignment alignment), R96, R106/R107 (data-source directives pass through — but
see G15 for plain-SQL files).

Failing / gaps: R38 (F8), R41 (G1), R42 (G2), R45 (G3 — default `preserve` vs
schema's exact documented casing; recommend flipping the default to `PascalCase`
once #34-style string/comment safety is confirmed, since the style guide is
authoritative), R03/R95 (wrap engine), R53/R54 (G4 — decide whether the formatter
canonicalizes not-preferred operators or leaves them to diagnostics).

Out of formatter scope (recommend explicit catalog note): R05/R98–R101 statement
ordering (reordering is a refactor, not a format), R88 header template generation.

## SQL-side conformance matrix (S-rules)

Verified passing: S01 (major clauses col 0), S02/S03 (packing + col-7
continuation), S05 (leading AND/OR +2), S06 (ON under JOIN), S07 (HAVING +2), S08
(CASE WHEN +4 per doc examples), S09/S10 (keyword upper / identifier lower — incl.
recasing from uppercase input), S12 (trailing commas), S15 (4-space embed), S16
(DISTINCT inline), S17, S18 (short BETWEEN suppression), S19, S20/S27/S28 (flat
subquery style, `)` col 0), S33 (UNION blank lines), S38 (UPDATE SET), S44
(short inline ON), S48 (short OVER inline), S60/S62 (hints preserved inline),
S79/S80 (placeholders verbatim).

Failing: S34 (G7), S35/S46 (G8 — the recurring "stranded `)`" is a
half-implementation of block-style VALUES: closer moved to col 0 without breaking
the list), S68 (G9), S43 (G10), S45 (G11), S31 (G12), S72 (G13 — trailing `||`,
also inconsistent with the SSL-side leading-operator style).

Untested S-rules (recommend fixture coverage when touched): S37 INSERT ALL,
S39/S40 UPDATE-with-subquery, S49–S52 frames/LISTAGG, S53–S57
hierarchical/PIVOT/LATERAL/flashback, S58/S59, S61, S73–S78 DDL.

Open question: `rownum` was uppercased to `ROWNUM` (treated as keyword). Confirm
the intended dialect keyword list covers pseudo-columns, or preserve.

## Style-guide ambiguities to resolve upstream (feed back to ssl-style-guide)

- A1: CASE WHEN indent — summary says 2, every example shows 4. Formatter follows 4.
- A3/A4: two coexisting subquery-indent conventions (flat vs aligned-under-SELECT).
  Formatter uses flat everywhere (G14). Pick one; update S26 or the formatter.
- A6: behavior when a single SQL token exceeds 90 cols is unspecified.
- A7: OVER()/LATERAL inner indent shown only by example — state numbers.
- A5: the "rules A–F" for quote placement live in LSP catalog
  (`fmt.sql_in_strings`), not the SQL reference — cross-link the two docs.

## Documentation gaps (decisions the formatter makes that are written nowhere)

Each needs a catalog `fmt.*` entry (and where noted, a schema/reference edit):

1. **fmt.keyword_case** (new): keyword uppercasing, dot-literal canonicalization
   (`.T.`/`NIL`), dot-operator casing (post-F8), `Me`/`Base` (post-G1),
   `:LABEL` mashed-form normalization. Advertised in `--format --help` but has no
   normative entry today.
2. **fmt.code_block_literals** (new): current behavior = verbatim atom (G2);
   either spec normalization per R42 or record verbatim-by-design.
3. **fmt.eol_normalization** (new or clause in feature.formatting): LF output per
   R02, CRLF input normalized.
4. **fmt.sql layout authority**: catalog's `fmt.sql_in_strings` covers only the
   string boundary (rules A–F). The layout contract should explicitly delegate to
   `sql-canonical-compact-reference.md` as authoritative, so S-rule fixes (G7–G13)
   have a single normative home.
5. **Formatter scope note**: statement ordering, header templates, and operator
   substitution (`<>`→`!=`) are diagnostics/refactor territory, not formatting —
   record explicitly so the boundary is deliberate.
