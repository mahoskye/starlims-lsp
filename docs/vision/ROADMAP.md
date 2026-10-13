# Project Roadmap

This document tracks where the project is and what comes next. Shipped
behavior is normatively recorded in the behavior catalog (`catalog/`) and
release history in [CHANGELOG.md](../../CHANGELOG.md) — this page is the
narrative view.

**Last Updated:** 2026-10-13

> Historical note: earlier revisions of this roadmap used aspirational
> v1.x/v2.0 milestone numbers that never matched the released versions.
> Everything described by those milestones (core LSP features, gotcha
> diagnostics, formatting refinements, cross-file workspace features)
> shipped across the real v0.x line — see the CHANGELOG for the
> authoritative mapping.

---

## Current direction: the drift audit (epic #253)

The LSP shipped steadily, mostly one issue at a time, while the SSL
reference, the style guide, and the experience of writing real SSL all
moved on. Epic [#253](https://github.com/mahoskye/starlims-lsp/issues/253)
steps back to re-examine the project as a whole. Its workstreams:

| Issue | Workstream |
|-------|------------|
| #254 | Upstream sync: vendored reference data and the stale style-guide checkout |
| #255 | Spec audit: re-verify catalog entries and DECISIONS.md (includes checking each entry's `authority` against [D17](../../catalog/DECISIONS.md)) |
| #256 | Test suite audit: coverage, stale assertions, corpus baseline |
| #257 | Inherited/legacy features and settings: keep, deprecate, or remove |
| #258 | Area-by-area code review of `main` |
| #259 | Prose docs refresh |
| #252 | Hover content review |
| #260 | Reconcile diagnostics with language rules the reference verified on STARLIMS 12 |
| #261 | Decide which STARLIMS version the LSP models — decided: STARLIMS 12 as verified by the reference ([D17](../../catalog/DECISIONS.md)) |
| #262 | Specify the `--validate`/`--format` CLI contract for gating and scoring consumers |

The suggested order is upstream sync first, then the spec, legacy, and
language-rule audits (which decide what behavior *should* be), then tests
and code review against the settled specs, and docs last. Findings
become their own issues; see the
[open issues](https://github.com/mahoskye/starlims-lsp/issues) for the
current list.

## Shipped (through v0.24.0, 2026-09-21)

All core LSP surfaces are active and spec-backed by the catalog:
completion, hover, signature help, definition/references/rename
(cross-file, including `DoProc`/`ExecFunction` dispatch targets, `RunDS`,
and `:INCLUDE`), document/workspace symbols, folding, inlay hints,
snippets, and formatting (SSL and embedded SQL). The diagnostics suite
has stable rule codes, per-rule severity overrides, in-file suppression,
and an opt-in info tier; [docs/reference/DIAGNOSTICS.md](../reference/DIAGNOSTICS.md)
lists every rule. The `--validate` and `--format` CLI modes serve CI and
agent tooling.

Milestones since the previous revision of this page:

- **v0.18.0 — the production-corpus release.** Twelve diagnostics from
  the runtime-verification batch (#185–#200); the opt-in info tier
  ([D13](../../catalog/DECISIONS.md)) with seven SQL advisories; formatter
  hardening from the production-corpus review (full-corpus idempotence);
  dialect-aware SQL conventions ([D14](../../catalog/DECISIONS.md));
  the expression AST itself (#184, milestone 1).
- **v0.19.0 — the expression-AST release.** The tree's consumers:
  `format_arg_not_array` and `builtin_excess_arguments` moved to real call
  analysis, the new opt-in `hungarian_type_mismatch`, statement-based
  declaration resolution, and identifier roles for references, rename,
  and hover. Issue #184 closed.
- **v0.20.0 / v0.21.0 — Hungarian gates.** `--hungarian` on the CLI;
  `hungarian_type_mismatch` split onto its own switch
  (`ssl.diagnostics.hungarianTypes`, `--hungarian-types`).
- **v0.22.0 / v0.23.0 — no more silent failures.** `unexpected_token`
  and `unterminated_string` (#240); signed exponents in scientific
  notation (#244, #246).
- **v0.24.0 — the strict profile.** `--validate --strict` turns on the
  undeclared-variable, unused-variable, and SQL-parameter checks for
  non-interactive consumers, plus eight fixes it surfaced
  (#249, [D15](../../catalog/DECISIONS.md)).

Resolved threads from the previous revision: the downstream setting
bumps (vs-code-ssl-formatter#95, ssl-style-guide#59) shipped; the arity
verification (#210) closed with a runtime-probe verdict; typed-receiver
member completion and hover shipped in v0.14.0 (#123). Member checks on
scalar and array receivers remain open as #267.

## Working discipline

- **Spec first:** every behavior change lands with its catalog entry and
  executable fences; `go test ./...` enforces conformance.
- **Corpus validation:** always-on rules and formatter changes are run
  against the production-representative corpus before merging; false
  positives found there become "Does not flag" fences.
- **Info tier for advice:** advisory findings ship as info severity
  (auto-gated), never as new warning noise; explicit
  `ssl.diagnostics.rules` entries promote per team taste.
- **One target version:** diagnostics judge compile and run-time claims
  against STARLIMS 12 as verified by the SSL reference
  ([D17](../../catalog/DECISIONS.md)).
