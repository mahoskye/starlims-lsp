# Project Roadmap

This document tracks where the project is and what comes next. Shipped
behavior is normatively recorded in the behavior catalog (`catalog/`) and
release history in [CHANGELOG.md](../../CHANGELOG.md) — this page is the
narrative view.

**Last Updated:** 2026-08-28

> Historical note: earlier revisions of this roadmap used aspirational
> v1.x/v2.0 milestone numbers that never matched the released versions.
> Everything described by those milestones (core LSP features, gotcha
> diagnostics, formatting refinements, cross-file workspace features)
> shipped across the real v0.x line — see the CHANGELOG for the
> authoritative mapping.

---

## Shipped (through v0.17.0, 2026-08-12)

All core LSP surfaces are active and spec-backed by the catalog:
completion, hover, signature help, definition/references/rename
(cross-file, including `DoProc`/`ExecFunction` dispatch targets),
document/workspace symbols, folding, inlay hints, snippets, formatting
(SSL + embedded SQL), and a ~110-rule diagnostics suite with stable rule
codes, per-rule severity overrides, and in-file suppression. The
`--validate` and `--format` CLI modes serve CI and agent tooling.

## Unreleased (next release)

The 2026-08-26→28 arc, fully merged and CHANGELOG'd:

- **Twelve new diagnostics** from the runtime-verification issue batch
  (#185–#200), each corpus-validated against ~6,200 production files.
- **The opt-in info tier** (`ssl.diagnostics.infoDiagnostics`): info
  severity repositioned as advisory detail for assistant/LLM consumers;
  18 rules in the tier plus seven new SQL advisories (comma joins,
  `(+)`, literal splices, dialect mixes, `SELECT *`, alias consistency,
  suspect placeholders).
- **Formatter hardening** from the production-corpus review
  (docs/reviews/2026-08-28): all H-series correctness findings fixed —
  comment preservation, concatenation-continued literal byte-preservation,
  ODBC escape/placeholder atomicity, full-corpus idempotence (1,008
  unstable files → 0), and the data-source routing fixes.
- **SQL convention decisions** (#219): `identifierCase` with `preserve`
  default, `compact` style retired, `standard` style reworked to respect
  line length, rewrites always in rule-F form.
- **Expression-level AST, milestone 1** (#184): lazy Pratt parser over
  the token stream, 98.8% statement coverage on the corpus; no consumers
  wired yet.

## Active tracks

| Track | Where it stands |
|-------|-----------------|
| Release cut | `[Unreleased]` is fully written; cut + tag when ready |
| Downstream bumps | vs-code-ssl-formatter#95 (settings: `infoDiagnostics`, `sql.identifierCase`, `compact` deprecation; info tier for LLM-facing surface) and ssl-style-guide#59 (MCP validate `--info`) carry the full scope |
| #184 expression AST | Milestone 1 shipped. Next consumers, each opt-in: signature-driven arity/type checks (graduating `format_arg_not_array` and `builtin_excess_arguments` from heuristics), the Hungarian type cross-check, resolution-based `CheckUndeclaredVars`, expression-aware hover/definition |
| #210 arity verification | 15 builtins' inventory max-arity vs production usage — blocked on live LIMS environment access; upstream table at ssl-style-guide#60 |
| Type inference & class members | Class-member metadata for the 29 built-in classes → typed member completion; partial inference exists for diagnostics (`Email{}`, `CreateUdObject`, return types). Unblocks after the AST consumers |

## Working discipline

- **Spec first:** every behavior change lands with its catalog entry and
  executable fences; `go test ./...` enforces conformance.
- **Corpus validation:** always-on rules and formatter changes are run
  against the production-representative corpus before merging; false
  positives found there become "Does not flag" fences.
- **Info tier for advice:** advisory findings ship as info severity
  (auto-gated), never as new warning noise; explicit
  `ssl.diagnostics.rules` entries promote per team taste.
