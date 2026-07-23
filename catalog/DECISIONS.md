# Cross-cutting decisions

Decisions that shape the whole catalog rather than any single entry.
Per-entry decisions live in that entry's `history:` field. Release history
lives in CHANGELOG.md. Newest first.

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
author's bytes. Sole deliberate exception: detected SQL strings
(fmt.sql_in_strings), a boundary that was reversed twice before settling
(extension #28 → #50/#51 → #64).

## D6 — Quiet-by-default interaction (2026-05-06→14, PRs #10/#13)

Auto-triggered UI (completion popups, signature help) errs on the side of
staying closed: `:` is the only completion trigger, `,`/`.`/`(` never
auto-open, signature help does not re-trigger while it would obscure the
line. A wrong popup that Enter-corrupts code is worse than requiring
Ctrl+Space.

## D5 — Noisy checks are opt-in (pre-v0.4, commit a4df25b)

Checks with inherent false-positive exposure (undeclared variables, unused
variables, SQL parameter mismatch, Hungarian notation) default off and are
enabled explicitly. Default-on is reserved for rules that are wrong in
essentially zero legitimate code (compile errors, syntax-level findings).

## D4 — Endpoint scripts get runtime ambients (2026-05-14, PR #23, v0.7.7)

Files matching `ssl.diagnostics.endpointPatterns` or carrying an
`Endpoint:` docblock treat `Request`/`Response` as pre-injected ambients:
never flagged undeclared, not assignable. Pattern for any future
runtime-injected globals.

## D3 — Suppression comment grammar (2026-05-01, PR #4, v0.5.0)

`/* @ssl-disable <slug>; */` suppresses a rule for the file;
`/* @ssl-disable-next-line <slug>; */` for the next line; `*` is the
wildcard slug. Suppression is honored uniformly by the pipeline, so every
diagnostic entry is `suppressible: true` unless it documents otherwise.

## D2 — Per-rule severity overrides (2026-05-01, PR #4, v0.5.0)

`ssl.diagnostics.rules` maps a code slug to `off | info | warn | warning |
error`; unknown slugs pass through unchanged. Applied server-side after
collection, so every entry is `severity_overridable: true` unless it
documents otherwise. `default_severity` in each entry is the pre-override
severity.

## D1 — Stable diagnostic code slugs (2026-05-01, PR #3, v0.4.0)

Every diagnostic carries a machine-readable `Code` slug plus
`Source: "ssl-lsp"`. Where `ssl-style-guide.schema.yaml` defines a lints
rule slug, the code reuses it verbatim; parser/lexer findings the schema
does not enumerate get slugs derived from the producing check. Slugs are
namespaceless snake_case and are the shared currency across the LSP, the
VS Code extension's generated settings enum and quick-fixes, and this
catalog's `diag.*` IDs. Renaming a slug is a breaking change for clients.
