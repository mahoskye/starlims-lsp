# Behavior Catalog

This directory is the **normative source of truth** for the behavior of
starlims-lsp: every diagnostic rule, every LSP feature contract, and every
formatter behavior decision has exactly one entry here. When the code and the
catalog disagree, one of them has a bug — the disagreement is never allowed to
stand silently. Conformance is enforced by `go test ./internal/catalog/`
(part of `go test ./...`).

Language rules themselves remain canonical in
`ssl-style-guide/ssl-style-guide.schema.yaml` (sibling repo). Entries here
never restate a language rule; they state what **this tool** does about it
(severity, ranges, false-positive guards, configuration, suppression) and
reference the schema via `schema_ref`.

This format is repo-agnostic. vs-code-ssl-formatter is expected to grow its
own `catalog/` using the same spec; cross-repo references qualify IDs as
`lsp:diag.x` / `ext:feature.x`. Within a catalog, IDs are unqualified.

## Layout and ID scheme

| Kind         | ID              | File                          |
|--------------|-----------------|-------------------------------|
| `diagnostic` | `diag.<slug>`   | `diagnostics/<slug>.md`       |
| `feature`    | `feature.<slug>`| `features/<slug>.md`          |
| `formatter`  | `fmt.<slug>`    | `formatting/<slug>.md`        |

For diagnostics, `<slug>` is **verbatim** the `Diagnostic.Code` value from
`internal/providers/diagnostic_codes.go`. The conformance test enforces a
bijection: every code constant has an entry, every non-`removed`/`planned`
entry has a code constant, and `removed`/`planned` entries must NOT exist in
code.

## Entry format

Strict YAML frontmatter followed by a markdown body with fixed headings.
Unknown frontmatter fields fail the build (typo protection).

```yaml
---
id: diag.comment_termination
title: Comment termination heuristic
kind: diagnostic            # diagnostic | feature | formatter
status: active              # draft | active | removed | planned
authority: tool             # authoritative | style_only | advisory | tool
schema_ref: null            # lints slug path in ssl-style-guide.schema.yaml, or null
default_severity: warning   # diagnostics only: error | warning | info | hint
config:                     # user-facing settings that influence this behavior
  - ssl.diagnostics.rules
severity_overridable: true  # participates in ssl.diagnostics.rules
suppressible: true          # honors @ssl-disable / @ssl-disable-next-line
spec_options: {}            # DiagnosticOptions/FormattingOptions overrides for
                            # the spec-runner, snake_case field names
                            # (e.g. check_undeclared_vars: true)
tests:
  - internal/providers/comment_termination_test.go
history:
  - date: 2026-03-30
    ref: "764a8de -> fe7a3aa -> 0d46f55"
    note: Severity reversed three times in one day; settled on warning.
issues: ["#25"]             # open items challenging current behavior
---
```

`authority: tool` means the style guide is silent and this is an LSP product
decision. `status: removed` preserves the record of behavior deliberately
dropped (with `history` explaining why); `status: planned` specifies behavior
that is not implemented yet.

### Body headings

Required for `active` entries (lint-enforced):

- `## Behavior` — the normative statement of what the tool must do.
- `## Examples` — executable, see Testing model below. Diagnostics need at
  least one `### Flags` and one `### Does not flag` fence; formatter entries
  need at least one `### Before` / `### After` pair.
- `## Acceptance` — features only: numbered criteria (`- A1: ...`) written as
  given/when/then, including what must NOT happen. The loader parses each
  criterion from the bullet's first line only — keep a criterion on one line
  (long is fine), or at minimum keep the `(planned)` marker on the first
  line, or it will not be seen.
- `## Rationale` — why this behavior and this severity; cite `history` refs.

Optional:

- `## Known gaps` — where the code currently disagrees with this entry. Each
  gap links a filed issue. Example fences inside Known gaps are marked
  `expect=fail` (see below).

`draft` entries are exempt from body-heading and example requirements, but
their frontmatter must still parse strictly. The conformance test carries a
`maxDrafts` constant that is ratcheted down as review batches complete.

## Testing model — the spec runs

**Diagnostics.** Every ```` ```ssl ```` fence under `### Flags` is run through
the real diagnostics pipeline and must produce this entry's code at least
once; every fence under `### Does not flag` must never produce it. The
"Does not flag" fences are where false-positive history is permanently
encoded. Options default to `DefaultDiagnosticOptions()` plus the entry's
`spec_options` overrides (needed for opt-in checks such as
`check_undeclared_vars` or `is_endpoint_file`).

**Formatter.** Each `### Before` fence is formatted with
`DefaultFormattingOptions()` plus `spec_options` and must byte-equal the
following `### After` fence. A `### Idempotent` fence must format to itself.
Every `### After` fence is additionally re-formatted and must be stable
(format-twice byte-equal, feature.formatting A6); the corpus-level
counterpart lives in `internal/providers/testdata/idempotence/` with a
ratcheted known-failures list (issue #103).

**Features.** Each acceptance criterion must be cited by a Go test carrying
the literal tag `[spec <id>/A<n>]` (in the test body or a comment). The
conformance test fails on uncited criteria and on citations that point at
nonexistent criteria. Criteria ending with `(planned)` are exempt from the
citation requirement until implemented.

**Expected failures.** A fence whose info string is `ssl expect=fail` (or a
`(planned)` criterion) documents behavior the code does not implement yet —
typically inside `## Known gaps`. The spec-runner executes it as an expected
failure: it does not break the build while the gap is open, but the moment a
fix lands the unexpected pass fails the build, forcing the entry's promotion
in the same PR as the fix. For Before/After pairs the marker may sit on
either fence of the pair. In `status: planned` entries no marker is needed:
every Flags fence and Before/After pair runs as an expected failure
automatically (Does-not-flag fences still assert normally — they hold
trivially before implementation and must keep holding after).

**Only ` ```ssl ` fences execute.** A fence with any other language token
(```` ```text ````, ```` ```json ````, …) under Flags/Does not flag/Before/
After/Idempotent is treated as illustration and skipped by both the
spec-runner and the promotion lints.

## Lifecycle

1. New behavior: write the entry first (`draft` or `planned`), then implement
   against it.
2. Changing behavior: update the entry's `## Behavior` and `history` in the
   same PR as the code change — the spec-runner will not let them diverge.
3. Removing behavior: set `status: removed`, keep the entry and its history.
4. Disputes (an issue argues behavior is wrong): record the issue number in
   `issues:`, settle the dispute by editing the entry, then fix the code in a
   follow-up PR citing the entry.

Cross-cutting decisions that belong to no single entry live in
[DECISIONS.md](DECISIONS.md).
