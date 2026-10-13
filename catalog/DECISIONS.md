# Cross-cutting decisions

Decisions that shape the whole catalog rather than any single entry.
Per-entry decisions live in that entry's `history:` field. Release history
lives in CHANGELOG.md. Newest first.

## D17 — The LSP models STARLIMS 12 as verified by the SSL reference (2026-10-13, issue #261)

The language the LSP models is STARLIMS 12 SSL as verified by the
published SSL reference
([starlims-ssl-reference](https://github.com/mahoskye/starlims-ssl-reference)),
which is written for STARLIMS 11 and verified on STARLIMS 12; its pages
carry `verified_against: [12]`. A catalog entry's `authority` claim —
"compile error", "run-time failure" — is judged against that verified
behavior, not against either version's behavior in the abstract.

Where v11 and v12 are known to differ, a diagnostic reports what fails
on either version (the stricter reading), and the entry's `## Rationale`
names the version that fails. A construct valid on both versions is
never reported. Where a difference is suspected but not confirmed, the
LSP stays silent, for the same reason as D10: absence of knowledge is
silence, never a warning.

There is no version setting. One is added only when a confirmed
difference needs it, and adding it is a new decision. The per-entry
check of existing `authority` fields against this decision is part of
the catalog audit (issue #255).

## D16 — The CLI is a versioned contract; breaks ship in minor releases under `### Breaking` (2026-10-13, issue #262)

`--validate` and `--format` are consumed by programs that gate or score
SSL (a pinned MCP server, agents that must pass `--strict` clean,
evaluation pipelines). The contract they rely on is specified in
feature.cli_contract. It covers flag names and meanings, the JSON field
names, types, and nesting, `valid` semantics, exit codes, the
diagnostic `code` slugs, and which codes can report at error severity
under each profile. Changes to it fall into three classes:

- **Breaking**: removing or renaming a flag or a code; renaming,
  retyping, or removing a JSON field, or reshaping the document;
  changing what `valid` or an exit code means; and a new default-on
  error-severity code, or raising a default-on code to error, because
  either can turn valid code invalid with no change on the consumer's
  side.
- **Behavior change**: a new default-on warning or hint, a profile
  gaining a check, or a severity change below error. A gate that
  requires zero findings feels these, but `valid` does not.
- **Compatible**: added JSON fields, new flags, new opt-in codes,
  message text (never part of the contract, so consumers key off
  `code`), and false-positive fixes.

How releases signal it: the project is pre-1.0, so a **minor** bump is
the breaking bump. Any release with a breaking change is a minor
release, never a patch, and its CHANGELOG section carries a
`### Breaking` subsection that names each break and what a consumer
changes. Behavior changes also require a minor release and are listed
under `### Added` / `### Changed` with the measured corpus delta.
Compatible changes may ship in a patch. Consumers must ignore JSON fields
they do not recognize. That is what lets an added field count as
compatible.

Enforcement: two tests turn the policy into a failing build at the
moment of the change. The set of error-severity codes is pinned in
`cmd/starlims-lsp/testdata/error_codes.txt`, and the JSON field sets are
pinned against the entry's tables (feature.cli_contract A13/A14).
Unknown flags fail loudly (exit 2) instead of being read as file paths,
so a consumer that runs ahead of its pinned binary gets an error rather
than a silently different result.

## D15 — Data-source mode detection tests for SQL-impossible constructs, and defaults to SQL (2026-09-21, issue #249)

Mode detection asks whether a data-source body contains something SQL
**cannot** contain — a colon keyword in statement-leading position, or a
`:=` — rather than whether it resembles SSL. That makes it a one-way
proof instead of a similarity score, which is why it needs no tuning
thresholds. The position requirement is load-bearing: an Oracle-style
bind sits after an operator, so `WHERE s = :default` is not evidence of
the `:DEFAULT` keyword.

When no marker is found the document is SQL. This is deliberate and the
asymmetry is the whole point: a document wrongly called SSL has every
SQL reserved word in it reported as an undeclared variable, while one
wrongly called SQL merely runs fewer checks. Bias toward the quiet
failure.

**Accepted consequence.** An SSL data source whose body carries no colon
keyword and no `:=` — a bare expression or call — reads as SQL. This is
not fixable in kind, because SSL cannot be proven from the *absence* of
markers, and it is bounded: mode detection runs only for documents
already known to be data sources (every call site is gated on
`isDataSourceURI` or `--ds`), so an ordinary `.ssl` script is never
affected. No occurrence was found in a 1,610-document production
corpus. Defaulting such a document to SQL is accepted rather than
worked around.

Signals deliberately **not** added, having been measured against the
same corpus: `.AND.`/`.OR.`/`.NOT.`, `Me:`/`Base:`, `==`, and
`DoProc`/`ExecFunction`/`CreateUdObject` flip nothing. Array-literal
braces must never be used — ODBC escapes (`{fn …}` in 166 files, `{d …}`
in 7) are character-for-character identical to an SSL array literal, so
the signal is SQL-possible and fails the criterion above.

## D14 — SQL conventions are dialect-aware; preserve before canonicalize (2026-08-28, issue #219, PR #225)

Where a SQL canonicalization is safe on one target DBMS but not the
other, the default is to preserve the author's text and offer the
canonical form as opt-in. Concretely: identifier casing defaults to
`preserve` (`ssl.format.sql.identifierCase`) because force-folding
breaks queries on SQL Server case-sensitive collations while Oracle
folds unquoted identifiers anyway; ODBC `{…}` escape interiors are
never identifier-folded and `?…?` placeholders are byte-preserved
whatever their interior (issue #217); and a detected-SQL string with an
unbalanced quote count — a literal continued across concatenation — is
never reformatted at all (issue #216). STARLIMS environments are
specifically Oracle or specifically MSSQL (corpus owner), so
dialect-conditional rewrites are correctness hazards, not style
choices. Companion pruning: the `compact` style was retired as a
deprecated alias for canonicalCompact — an internally inconsistent
layout contract is worse than one fewer style.

## D13 — Info severity is the opt-in advisory tier (2026-08-27, issue #208 discussion, PRs #212/#213)

Info-severity diagnostics are advisory detail — style observations and
idiom notes aimed at assistant/LLM consumers, which can be configured
to run with the tier enabled and write better code — and are dropped by
default (`ssl.diagnostics.infoDiagnostics: false`; `--validate --info`)
so the everyday surface stays errors/warnings/hints. Consequences: an
advisory rule ships as `default_severity: info` with NO dedicated
boolean setting (four short-lived opt-in booleans were consolidated
into the tier before ever releasing); an explicit
`ssl.diagnostics.rules` entry bypasses the gate in both directions, so
per-rule promotion is always available; hints are not gated. The
sorting question for new rules is "actionable or advisory?" — advisory
goes to info.

## D12 — Schema-canonical forms normalize by default (2026-07-23, issues #91/#92)

Where the style-guide schema names one canonical spelling, the formatter
applies it without opt-in: keyword and literal casing (fmt.keyword_case),
built-in function casing (fmt.builtin_function_case — default flipped
from "preserve" to "PascalCase", schema R45), and code-block literal
shape `{|params| expression}` (fmt.code_block_literals, schema R42).
Normalization is bounded by D7: string and comment content is never
touched, and a code block that cannot be normalized safely (nested,
multi-line, malformed) passes through verbatim.

## D11 — The formatter reflows, it does not rewrite (2026-07-23, issue #102)

The formatter changes layout and applies authoritative canonicalizations
(casing, spacing, semicolons) — it never restructures code. Out of scope
by decision, not omission: statement reordering (schema module_structure
R05/R98–R101 — a refactor), documentation-header generation (R88), and
operator substitution (`<>`/`#` → `!=`, schema lints R53/R54 — the
diagnostics' job to flag, the author's to change). Anything that could
alter meaning or authorship intent belongs to diagnostics or refactoring
tools, never to format-on-save. Open questions tracked outside this
decision: code-block literal normalization (issue #91) and the
builtinFunctionCase default (issue #92).

## D10 — `:` member access never warns (2026-07-03, issue #22)

Member access is analyzed only from positive knowledge, and the absence
of knowledge is silence, never a warning. Built-in value types (`string`,
`number`, `date`, `array`, `boolean`, `netobject`) forward unmatched `:`
members to their underlying .NET object at runtime — a legitimate call
surface far too broad to enumerate — so an "unknown member" diagnostic
cannot distinguish a typo from a valid passthrough. Consequences:

- No diagnostic ever fires on the member of a `:` access
  (`sName:SomeRandomDotNetMethod()` is clean under every check; pinned
  by diag.undeclared_variable and diag.direct_procedure_call fences).
- Member hover/definition act only where inference gives real knowledge
  (UDObject shapes — feature.hover A15-A16, feature.definition A14-A15)
  and return truthful null for a shaped receiver's unknown member.
- Any future member completion or diagnostic on typed receivers MUST
  exempt the six passthrough types above.

## D9 — The catalog is normative (2026-07-01)

Every diagnostic, feature contract, and formatter behavior has exactly one
entry under `catalog/`; when code and catalog disagree, one of them has a
bug and the disagreement is resolved explicitly (edit the entry or fix the
code — never both silently). Enforced by `internal/catalog` tests. Origin:
repeated loss of behavior decisions recorded only in PR bodies, and rules
that oscillated because no written target behavior existed (see
diag.comment_termination history).

## D8 — Spec examples are executable (2026-07-01)

`### Flags` / `### Does not flag` fences and `### Before`/`### After` pairs
are run against the real pipeline by `internal/catalog/spec_test.go`;
feature acceptance criteria must be cited by `[spec <id>/A<n>]` tags in Go
tests. Tests exist to cover stated criteria, not to inflate counts.

## D7 — Strings and comments are literal text (2025-11-19, v1.1.0)

No formatter behavior may modify the content of string literals or
comments — spacing, casing, operators, semicolons inside them are the
author's bytes. This binds the line-oriented post-passes too: a line that
begins inside a multi-line string, comment, code block or `:REGION` body is
never edited, separated or capped (issue #255). Two deliberate exceptions:
detected SQL strings (fmt.sql_in_strings), a boundary that was reversed
twice before settling (extension #28 → #50/#51 → #64); and trailing
whitespace at the line ends of a multi-line comment's interior, which
fmt.trim_trailing_whitespace removes because it is invisible and changes
nothing the comment says (string line ends are never trimmed).

## D6 — Quiet-by-default interaction (2026-05-06→14, PRs #10/#13)

Auto-triggered UI (completion popups, signature help) errs on the side of
staying closed: `:` is the only completion trigger, `,`/`.`/`(` never
auto-open, signature help does not re-trigger while it would obscure the
line. A wrong popup that Enter-corrupts code is worse than requiring
Ctrl+Space.

*Note (2026-10-13, issue #255):* D6 governs interactive UI only. It does
not bear on which diagnostics are default-on; that is D5 (as amended) and
D13.

## D5 — Noisy checks are opt-in (pre-v0.4, commit a4df25b)

Checks with inherent false-positive exposure (undeclared variables, unused
variables, SQL parameter mismatch, Hungarian notation) default off and are
enabled explicitly. Default-on is reserved for rules that are wrong in
essentially zero legitimate code (compile errors, syntax-level findings).

**Amended 2026-10-13 (issue #255).** Later decisions refined this, and the
original last sentence no longer describes practice:

- *What default-on means.* A rule ships default-on when its findings are
  real in essentially every case it reports on the production corpus —
  compile errors and run-time failures (error), and likely bugs
  (warning). The bar is the precision of what it reports, measured,
  not "zero legitimate code could ever trigger it".
- *Noisy checks stay opt-in at the core, and the audience decides.* The
  core default stays off for checks with inherent false-positive or
  volume exposure, so each embedder chooses: the VS Code extension turns
  the Hungarian checks on for authors writing new SSL; non-interactive
  consumers enable the undeclared/unused/SQL-parameter trio with
  `--validate --strict` (v0.24.0), because they only see finished code.
- *Advisory findings go to the info tier* (D13) rather than to an opt-in
  boolean per rule.
- *Severity is never the volume lever.* A rule that is too loud is gated
  or split (as `hungarian_notation` / `hungarian_type_mismatch` were),
  never demoted: severity encodes what kind of finding it is (D16 makes
  raising one to error a breaking change).

## D4 — Endpoint scripts get runtime ambients (2026-05-14, PR #23, v0.7.7)

Files matching `ssl.diagnostics.endpointPatterns` or carrying an
`Endpoint:` docblock treat `Request`/`Response` as pre-injected ambients:
never flagged undeclared, not assignable. Pattern for any future
runtime-injected globals.

**Amended 2026-10-13 (issue #260).** The reference now says `Request` and
`Response` can be read in any script — outside an endpoint they do not
raise, though their values do not come from an HTTP caller. They are
therefore never flagged undeclared, in any file. Endpoint classification
(the patterns and the `Endpoint:` marker) still selects the endpoint-only
typed receivers, hover and completion for `Request`/`Response`.

## D3 — Suppression comment grammar (2026-05-01, PR #4, v0.5.0)

`/* @ssl-disable <slug>; */` suppresses a rule for the file;
`/* @ssl-disable-next-line <slug>; */` for the next line; `*` is the
wildcard slug. Suppression is honored uniformly by the pipeline, so every
diagnostic entry is `suppressible: true` unless it documents otherwise.

## D2 — Per-rule severity overrides (2026-05-01, PR #4, v0.5.0)

`ssl.diagnostics.rules` maps a code slug to `off | hint | info | warn |
warning | error`; unknown slugs pass through unchanged. Keys and values
match case-insensitively, and an entry with any other value is ignored as
if absent — it neither changes the severity nor exempts the rule from the
info gate (D13). Applied server-side after collection, so every entry is
`severity_overridable: true` unless it documents otherwise.
`default_severity` in each entry is the pre-override severity. (`hint`
and the case-insensitive, ignore-unknown matching: 2026-10-13, issue
#258.)

## D1 — Stable diagnostic code slugs (2026-05-01, PR #3, v0.4.0)

Every diagnostic carries a machine-readable `Code` slug plus
`Source: "ssl-lsp"`. Where `ssl-style-guide.schema.yaml` defines a lints
rule slug, the code reuses it verbatim; parser/lexer findings the schema
does not enumerate get slugs derived from the producing check. Slugs are
namespaceless snake_case and are the shared currency across the LSP, the
VS Code extension's generated settings enum and quick-fixes, and this
catalog's `diag.*` IDs. Renaming a slug is a breaking change for clients.
