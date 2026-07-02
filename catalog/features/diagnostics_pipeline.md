---
id: feature.diagnostics_pipeline
title: Diagnostics pipeline (codes, overrides, suppression, panic recovery)
kind: feature
status: active
authority: tool
schema_ref: null
config:
  - ssl.diagnostics.rules
  - ssl.diagnostics.endpointPatterns
tests:
  - internal/providers/providers_test.go
  - internal/server/server_test.go
history:
  - date: 2026-04-30
    ref: "LSP PR #3 (d744511, v0.4.0)"
    note: Every diagnostic must carry a stable Code slug, populated at all
      emit sites and sent over the wire; enables quick fixes, suppression,
      and overrides.
  - date: 2026-04-30
    ref: "LSP PR #4 (49fc459, v0.5.0)"
    note: Per-rule severity overrides (ssl.diagnostics.rules) and
      @ssl-disable suppression comments added; applied inside
      collectDiagnostics so every consumer honors them uniformly.
  - date: 2026-05-01
    ref: "5814391 (v0.7.1)"
    note: Panic recovery wrapped around collectDiagnostics; a check panic
      surfaces as a single internal_error diagnostic instead of killing the
      server.
  - date: 2026-05-13
    ref: "60c10bd (v0.7.7)"
    note: Endpoint-file classification added (ssl.diagnostics.endpointPatterns
      URI substrings, or a leading-docblock `Endpoint:` marker) so
      Request/Response ambients stop false-flagging in endpoint scripts.
issues: []
---

## Behavior

This entry covers the cross-cutting diagnostics machinery, not individual
rules (those are `diag.*` entries).

- Diagnostics are computed and published on document open, change, and
  save, and re-published for every open document when the configuration
  changes. There is no debounce; validation runs synchronously on each
  event. At most 100 diagnostics are published per document (fixed
  MaxNumberOfProblems cap).
- Every emitted diagnostic MUST carry a stable, machine-readable `Code`
  slug (canonical list in `internal/providers/diagnostic_codes.go`) and
  `Source: "ssl-lsp"`, propagated to the LSP wire.
- Severity overrides: `ssl.diagnostics.rules` maps a Code slug to one of
  `off` (drop the diagnostic), `info`, `warn`/`warning`, or `error` (remap
  severity). Diagnostics whose Code is not in the map pass through
  unchanged; an unrecognized override value is a no-op, never a silent
  drop.
- Suppression comments in source are honored (grammar pinned in
  DECISIONS.md D3):
  - `/* @ssl-disable <slug>[, <slug>...]; */` — file scope: drops every
    matching diagnostic in the document.
  - `/* @ssl-disable-next-line <slug>[, <slug>...]; */` — line scope: drops
    matching diagnostics on the line immediately following the comment
    (the comment's last line, for multi-line comments).
  - Slug `*` matches any Code in either form. Slugs are matched
    case-insensitively against the canonical lowercase snake_case form.
- Suppression is applied before overrides; both run inside diagnostic
  collection so every consumer (publishDiagnostics, `--validate`, tooling)
  sees identical results.
- Diagnostics without a Code (defensive) bypass suppression and overrides
  so the user still sees them.
- Endpoint classification: a document is treated as an SSL endpoint script
  (Request/Response become pre-injected ambients for the rules that care)
  when its URI contains one of the `ssl.diagnostics.endpointPatterns`
  substrings (case-insensitive) or a leading-docblock `Endpoint:` marker
  appears in the first ~30 lines. The default-empty pattern list means zero
  false positives out of the box; a marker deeper in the file MUST NOT
  activate endpoint mode.
- A panic in any diagnostic check MUST NOT crash the server: it is
  recovered, the stack trace is logged to stderr, and a single
  error-severity diagnostic with Code `internal_error` and Source `ssl-lsp`
  is emitted at the top of the file.

## Acceptance

- A1: Given any SSL document producing diagnostics, when diagnostics are collected, then every diagnostic has a non-empty Code and Source `ssl-lsp`.
- A2: Given `ssl.diagnostics.rules: {"<slug>": "off"}`, when a document that would emit that slug is checked, then no diagnostic with that Code is produced; and given `"info"`, the diagnostic is produced with Information severity.
- A3: Given an override map with an unrecognized value (e.g. `"bogus"`), when diagnostics are collected, then the diagnostic passes through unchanged — an unknown value never drops anything.
- A4: Given a file containing `/* @ssl-disable <slug>; */`, when diagnostics are collected, then no diagnostic with that Code appears anywhere in the file, while the same file without the directive does emit it.
- A5: Given `/* @ssl-disable-next-line <slug>; */` directly above an offending line, when diagnostics are collected, then that line's matching diagnostic is dropped but the same violation on any other line still flags.
- A6: Given `/* @ssl-disable *; */`, when diagnostics are collected, then every coded diagnostic in the file is silenced.
- A7: Given a diagnostic check that panics, when diagnostics are collected, then the panic does not propagate and the result is a single error-severity `internal_error` diagnostic.
- A8: Given a URI matching an `ssl.diagnostics.endpointPatterns` substring or a file with a leading-docblock `Endpoint:` marker, when the document is classified, then it is treated as an endpoint script; an `Endpoint:` marker past the leading region does NOT activate endpoint mode.
- A9: Given open documents and a `workspace/didChangeConfiguration` notification, when the new settings are applied, then diagnostics are re-published for every open document.

## Rationale

Stable Codes (PR #3, v0.4.0) turn diagnostics from prose into an API:
suppression, overrides, and quick fixes all key off the slug, so codes are
a compatibility surface. Overrides and suppression (PR #4, v0.5.0) live
inside collection rather than the LSP handler so `--validate` and future
consumers cannot diverge from the editor; suppression-before-overrides
ordering means "off" and `@ssl-disable` compose without surprises, and the
unknown-value no-op (A3) exists because silently dropping diagnostics on a
typo would be the worst possible failure mode for a config surface.
Endpoint classification (60c10bd, v0.7.7) is opt-in by pattern or by
in-file marker and restricted to the leading docblock precisely to keep
zero false positives — an "Endpoint:" mention in a random comment must not
change diagnostic behavior. Panic recovery (5814391, v0.7.1) exists because
a single misbehaving check previously took down the whole server ("all
goroutines are asleep"); one loud `internal_error` diagnostic keeps the
editor usable and makes bug reports actionable.

## Known gaps

- A7's regression test (TestGetDiagnostics_PanicRecovery) verifies the
  recovery contract with a synthetic panic rather than driving a real check
  panic through collectDiagnostics — a panic cannot be injected from input
  alone without instrumenting production code.
