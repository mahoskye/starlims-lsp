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
  - internal/providers/sql_mode_test.go
  - internal/providers/region_body_test.go
  - internal/server/server_test.go
  - internal/server/utf16_output_test.go
  - cmd/starlims-lsp/validate_test.go
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
  - date: 2026-07-22
    ref: "#77"
    note: >-
      SQL-mode data-source classification added: a .ds/.ds.txt document
      whose content is a plain SQL statement produces no diagnostics at
      all — SSL checks were false-flagging SQL syntax (dot-qualified
      column names hitting dot_property_access, bare AND/OR, missing
      semicolons).
  - date: 2026-07-22
    ref: "issue #104"
    note: >-
      Hybrid sql_data_source shape (builder directives / :PARAMETERS
      header followed by raw SQL) recognized: the header keeps SSL and
      data-source diagnostics, the SQL body is suppressed. Previously the
      leading directive line classified the whole file as SSL and every
      SSL check fired on the SQL body.
  - date: 2026-08-07
    ref: "issue #141"
    note: >-
      The --validate CLI bypassed SQL-mode data-source classification: it
      fed tokens straight to collection, so every SSL check fired on the
      SQL body of .ds files (dot_property_access on table.column, etc.)
      even though the editor path suppressed them. --validate now routes
      through the same text-path classification, and a --ds flag marks
      stdin content as a data source (stdin has no URI to classify by).
  - date: 2026-08-07
    ref: "issues #147/#148, ssl-style-guide#48"
    note: >-
      Canonical optional header comment handled (schema
      data_source_modules structure: header_comment precedes directives
      and SQL): a data-source document that is only terminated SQL
      comments and whitespace is SQL mode (banner-only stubs errored with
      "SSL comments must end with a semicolon"), and hybrid header
      detection now sees through leading terminated SQL comments (a
      banner before :DSN previously reverted the whole file to SSL
      parsing, silently skipping header checks). A header followed by no
      body at all keeps header-only diagnostics. Also removed
      datasource_default_required from the data-source check set
      (defaultless :PARAMETERS is valid; spec fix in ssl-style-guide#48).
  - date: 2026-08-08
    ref: "issue #154, ssl-style-guide#50/#51"
    note: >-
      SQL-mode classification made robust to column/table names that
      collide with SQL builtin-function names (`set FORMAT = …` fell back
      to SSL parsing, where the SSL comment rule mangled string literals
      like 'all;msoffice->pdf'), per the schema's sql_data_source comment
      semantics (semicolons in SQL comments and quoted literals are
      content). Bare `;` statement separators in a SQL-mode body now warn
      with datasource_sql_semicolon — the one semicolon case the schema
      leaves undefined.
  - date: 2026-08-08
    ref: "ssl-style-guide#51/#53"
    note: >-
      datasource_undeclared_placeholder added to the SQL-body check set:
      the schema's new body_parameters section makes @name a first-class
      token in SQL data-source bodies, so a placeholder with no matching
      :PARAMETERS declaration warns (it is not substituted and fails at
      execute time), with structural exclusions for @@ system functions
      and DECLARE-scripted bodies.
  - date: 2026-08-08
    ref: "issue #153"
    note: >-
      SQL-vs-SSL classification inverted for data-source files: a .ds file
      is SQL by default and SSL only when its body (header split off)
      carries a strong SSL marker — a non-directive colon keyword, a `:=`
      assignment, or a leading unterminated `/*` comment. The former
      structural-SQL detector rejected valid queries it could not tell from
      English prose (a SELECT list with implicit column aliases), leaking
      SSL diagnostics like bare_logical_operator onto legitimate SQL `and`.
      A22 pins both directions.
  - date: 2026-08-12
    ref: "issue #164"
    note: >-
      :REGION bodies made opaque at the lexer: the body between
      `:REGION <name>;` and a line-leading `:ENDREGION` is one raw
      TokenRegionBody token, so no SSL check ever tokenizes it. Stock
      scripts storing HTML/JS/XML/SQL templates in regions were failing
      validation (27% of corpus failures: dot_property_access on HTML
      attributes, equals_vs_strict_equals on JS, etc.). The formatter
      passes bodies through verbatim; unclosed_block still fires on a
      region with no :ENDREGION.
  - date: 2026-08-27
    ref: "issue #208"
    note: >-
      Production-corpus finding: data-source banner comments closing
      `*/;` left the `;` unmasked, aborting SplitDataSourceHeader — the
      inline `:=` defaults then read as a strong SSL marker and 71 SQL
      data sources ran the full SSL pipeline (dot_property_access on
      column refs, bare_logical_operator on SQL and/or). The `;` after a
      terminated leading block comment now masks as comment furniture
      (criterion A24). The corpus owner confirmed `*/;` is valid and
      purely stylistic — SSL never sees the `*/`.
  - date: 2026-08-27
    ref: "issue #208 discussion"
    note: >-
      Info tier made opt-in (ssl.diagnostics.infoDiagnostics, default
      off, criterion A27): info diagnostics repositioned as advisory
      detail for assistant/LLM consumers — the corpus owner's direction
      is that assistants configured with the info tier can write better
      code, while the default human surface stays errors/warnings/hints.
      Explicit ssl.diagnostics.rules entries bypass the gate.
  - date: 2026-08-28
    ref: "issue #184 (expression AST consumers)"
    note: >-
      collectDiagnostics now builds the expression AST's call-site index
      (parser.CollectCalls) once per document and passes it to every
      call-shaped rule, so expression parsing is paid for at most once
      rather than per rule. Measured over the 6,228-file production
      corpus, the index costs ~165ms against ~675ms of lexing and
      replaces ~85ms of per-rule token scanning; end-to-end --validate
      throughput was unchanged within run-to-run noise.
  - date: 2026-08-29
    ref: "downstream request (ssl-style-guide MCP)"
    note: >-
      --validate gained a --hungarian flag, alongside the existing --info,
      so a CLI caller can enable the opt-in Hungarian checks. The MCP
      binaries that serve LLM consumers had no way to reach
      hungarian_type_mismatch, which is gated behind an editor setting the
      CLI did not expose. The flags are also now covered by an acceptance
      criterion (A28); --info had none.
  - date: 2026-10-13
    ref: "issue #258"
    note: >-
      Audit fixes. Panic recovery keeps the diagnostics collected before
      the panicking check (A7; the recovery used to return only the
      internal_error), and now also covers the data-source SQL-mode path,
      lexing, and parsing; internal_error gained a code constant and its
      own catalog entry. Rule overrides: keys match case-insensitively,
      "hint" is a recognized value, and an unrecognized value no longer
      exempts the rule from the info gate (A2, A3).
  - date: 2026-10-13
    ref: "issue #307"
    note: >-
      Published ranges count UTF-16 code units (A32). They carried the
      lexer's rune columns, so each character outside the Basic
      Multilingual Plane (an emoji) earlier on the line put a range one
      unit too far left. The SQL lexer behind the data-source checks
      counted bytes, which also skewed ranges after accented text.
  - date: 2026-10-13
    ref: "issue #304"
    note: >-
      A mid-line `:ENDREGION;` (with its semicolon) now closes a region
      body at the colon. Only a line-leading closer counted, so a JSON
      template ending `}:ENDREGION;` in shipped product code ran to end
      of file and reported unclosed_block. A mid-line `:ENDREGION`
      without its `;` is still body text.
  - date: 2026-10-13
    ref: "issue #258"
    note: >-
      The 100-diagnostic cap now applies after sorting by severity, then
      position (A33). It used to cut the checks' rule-ordered output, so
      an error reported by a late check could be dropped behind 110
      warnings that came before it. Endpoint patterns also match the
      file path, now that every URI is canonicalized to its escaped form.
  - date: 2026-10-13
    ref: "issue #329"
    note: >-
      Text corrected: the data-source check set requires inline
      :PARAMETERS defaults again (datasource_default_required, reinstated
      in #263/#269); the entry still said `:PARAMETERS p1;` was valid and
      the rule removed. A13 no longer calls inline defaults optional.
  - date: 2026-10-14
    ref: "style-guide research review"
    note: >-
      Suppression comments may give a reason after `--`, `—` or `:`, which
      matching ignores (D3 amended). The documented form lost its closing
      `*/`: SSL comments end at `;`, so `; */` left code after the comment.
issues: []
---

## Behavior

This entry covers the cross-cutting diagnostics machinery, not individual
rules (those are `diag.*` entries).

- Diagnostics are computed and published on document open, change, and
  save, and re-published for every open document when the configuration
  changes. There is no debounce; validation runs synchronously on each
  event. At most 100 diagnostics are published per document (fixed
  MaxNumberOfProblems cap). Before the cap applies, the diagnostics are
  ordered by severity (errors, then warnings, info, and hints) and by
  position within a severity, so the cap drops the least severe findings
  and never an error while a warning is kept (issue #258).
- Every emitted diagnostic MUST carry a stable, machine-readable `Code`
  slug (canonical list in `internal/providers/diagnostic_codes.go`) and
  `Source: "ssl-lsp"`, propagated to the LSP wire.
- Published ranges count characters in UTF-16 code units, the encoding the
  server advertises (`positionEncoding: "utf-16"`). Checks compute rune
  columns; the server converts each range against the document's text when
  it publishes, so a character outside the Basic Multilingual Plane (two
  UTF-16 units) earlier on the line cannot shift a range (issue #307). The
  `--validate` CLI reports 1-based rune columns.
- Severity overrides: `ssl.diagnostics.rules` maps a Code slug to one of
  `off` (drop the diagnostic), `hint`, `info`, `warn`/`warning`, or
  `error` (remap severity). Keys and values are matched
  case-insensitively, like suppression slugs. Diagnostics whose Code is
  not in the map pass through unchanged; an entry with an unrecognized
  value is ignored as if absent — never a silent drop, and never an
  exemption from the info gate (issue #258).
- Suppression comments in source are honored (grammar pinned in
  DECISIONS.md D3):
  - `/* @ssl-disable <slug>[, <slug>...] [-- reason];` — file scope: drops every
    matching diagnostic in the document.
  - `/* @ssl-disable-next-line <slug>[, <slug>...] [-- reason];` — line scope: drops
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
  when its URI, or the file path the URI names (so a pattern with a space
  matches the `%20`-escaped URI), contains one of the
  `ssl.diagnostics.endpointPatterns` substrings (case-insensitive) or a
  leading-docblock `Endpoint:` marker
  appears in the first ~30 lines. The default-empty pattern list means zero
  false positives out of the box; a marker deeper in the file MUST NOT
  activate endpoint mode.
- The info severity tier is opt-in: info diagnostics are advisory detail —
  style observations and idiom notes aimed at assistant/LLM consumers and
  teams that want the full picture — and are dropped by default
  (`ssl.diagnostics.infoDiagnostics: false`) so the everyday surface stays
  errors/warnings/hints. A rule explicitly configured in
  `ssl.diagnostics.rules` bypasses the gate: an explicit per-rule severity
  choice always wins, in both directions. The `--validate` CLI mirrors
  this with an `--info` flag. Hint severity is NOT gated — hints render
  subdued in editors already and carry actionable one-keystroke fixes.
- SQL-mode data-source classification: a document classified as a data
  source by URI (`.ds` / `.ds.txt`) is SQL by default and produces NO SSL
  diagnostics — every SSL check would false-flag SQL syntax (`table.column`
  qualified names, bare `AND`/`OR`, statements without `;`). A `.ds` file
  classifies as SSL only when its body — the document with any leading
  builder-directive / `:PARAMETERS` header split off — carries a strong SSL
  marker: a non-directive colon keyword (`:DECLARE`, `:IF`, `:RETURN`, …;
  `:PARAMETERS` and the builder directives are excluded), a `:=`
  assignment, or an unterminated `/* … ` SSL comment leading the document.
  SQL never uses `:KEYWORD` syntax or a bare `:=`, so any such marker is
  decisive evidence of SSL; conversely, a SQL query that fails strict
  statement validation (for example a SELECT list with implicit column
  aliases, `col alias`) still classifies as SQL because it carries no
  marker (issue #153). A data-source document whose body carries a marker
  keeps the full data-source diagnostic set, and a non-data-source
  document is never SQL-classified regardless of content. The
  classification applies identically in every consumer: the editor path
  and the `--validate` CLI (issue #141). For `--validate --stdin`, where
  no URI exists, the `--ds` flag declares the content a data source.
- Data-source header comments (issue #148): the schema's canonical
  structure is `header_comment (optional)` → builder directives
  (optional) → `:PARAMETERS` (optional) → SQL. Accordingly: a data-source
  document containing only *terminated* SQL comments (`/* ... */`, `--`)
  and whitespace is SQL mode — no diagnostics (banner-only stubs are
  valid); and hybrid header detection ignores leading terminated SQL
  comments before the first directive, masking them out of the header
  text so header diagnostics keep their positions and the comment itself
  produces none. A banner closing `*/;` — the production idiom whose `;`
  doubles as the SSL comment terminator — masks including the `;` (plus
  any same-line spacing before it), so the residue cannot abort header
  detection (issue #208). An unterminated `/* text;` SSL comment never
  counts as a SQL comment — such content keeps its SSL diagnostics. A
  recognized header followed by an empty body (directives-only stub)
  keeps header-only diagnostics.
- The data-source check set requires inline `:PARAMETERS` defaults:
  `datasource_default_required` reports a data-source parameter written
  without one (`:PARAMETERS p1;`), because the builder fails at run time
  without it (ssl-style-guide#82, reversing #48). The rule was removed in
  issue #147 and reinstated in #263/#269.
- Region bodies are opaque payload (issue #164): the text between a
  `:REGION <name>;` header and its `:ENDREGION` closer is
  stored for `GetRegion()` retrieval, not executed, so the lexer captures
  it as a single raw token and NO diagnostic ever fires on body content —
  regions legitimately hold HTML, JavaScript, XML, and SQL templates. A
  line-leading `:ENDREGION` (optionally indented) closes the region, and
  so does a complete `:ENDREGION;` later on a line (`}:ENDREGION;`), cut
  at its colon so the text before it stays payload (issue #304). A
  mid-line `:ENDREGION` without its `;` is body text. A region with no closer consumes to EOF as
  raw text and still reports `unclosed_block` on the `:REGION`. The
  formatter emits bodies verbatim (no reindent, no semicolon enforcement).
- A panic in any diagnostic check MUST NOT crash the server: it is
  recovered, the stack trace is logged to stderr, and a single
  error-severity diagnostic with Code `internal_error` (diag.internal_error)
  and Source `ssl-lsp` is emitted at the top of the file, alongside every
  diagnostic the checks before the panicking one collected (issue #258).
  The rest of the pipeline — data-source SQL-mode classification and its
  SQL-body checks, lexing, and parsing — is covered the same way, as are
  the server's document parse and every LSP request.

## Acceptance

- A1: Given any SSL document producing diagnostics, when diagnostics are collected, then every diagnostic has a non-empty Code and Source `ssl-lsp`.
- A2: Given `ssl.diagnostics.rules: {"<slug>": "off"}`, when a document that would emit that slug is checked, then no diagnostic with that Code is produced; given `"info"` or `"hint"`, the diagnostic is produced with Information or Hint severity; and keys and values match case-insensitively (`"Unknown_Token": "OFF"` configures `unknown_token`), like suppression slugs.
- A3: Given an override map with an unrecognized value (e.g. `"bogus"`), when diagnostics are collected, then the entry is ignored as if absent — the diagnostic passes through unchanged (an unknown value never drops anything) and an info-tier rule is not exempted from the info gate by it.
- A4: Given a file containing `/* @ssl-disable <slug>;`, when diagnostics are collected, then no diagnostic with that Code appears anywhere in the file, while the same file without the directive does emit it.
- A5: Given `/* @ssl-disable-next-line <slug>;` (optionally followed by a reason after `--`, `—` or `:`) directly above an offending line, when diagnostics are collected, then that line's matching diagnostic is dropped but the same violation on any other line still flags.
- A6: Given `/* @ssl-disable *;`, when diagnostics are collected, then every coded diagnostic in the file is silenced.
- A7: Given a diagnostic check that panics, when diagnostics are collected, then the panic does not propagate and the result is the diagnostics collected before the panicking check — suppression, overrides, and the info gate applied as usual — plus a single error-severity `internal_error` diagnostic.
- A8: Given a URI matching an `ssl.diagnostics.endpointPatterns` substring or a file with a leading-docblock `Endpoint:` marker, when the document is classified, then it is treated as an endpoint script; an `Endpoint:` marker past the leading region does NOT activate endpoint mode.
- A9: Given open documents and a `workspace/didChangeConfiguration` notification, when the new settings are applied, then diagnostics are re-published for every open document.
- A10: Given a data-source document containing only a SQL statement (optionally preceded by `--` or `/* */` SQL comments), when diagnostics are collected, then the result is empty — no SSL diagnostic (dot_property_access or otherwise) fires on SQL syntax.
- A11: Given a data-source document containing SSL code, when diagnostics are collected, then SSL and data-source diagnostics are produced exactly as before — SQL mode only activates on SQL content.
- A12: Given a non-data-source document whose content is a SQL statement, when diagnostics are collected, then SSL diagnostics still run — SQL-mode classification is scoped to data-source files.
- A13: Given a data-source document whose leading lines are builder directives (`:DSN`/`:TABLENAME`/`:NULLASBLANK`/`:INVARIANTDATECOLUMNS` `:= value;`) or a `:PARAMETERS` statement (with or without inline `:=` defaults — a missing one is `datasource_default_required`'s finding) followed by a SQL statement, when diagnostics are collected, then no diagnostic fires on the SQL body while the header lines keep their SSL and data-source checks.
- A14: Given a `.ds` (or `.ds.txt`) file whose content is plain SQL or the hybrid header-then-SQL shape, when it is validated via the `--validate` CLI file path, then the result matches the editor path — no diagnostic fires on SQL content, header lines keep their checks — and an SSL-content `.ds` file keeps the full data-source diagnostic set.
- A15: Given `--validate --stdin --ds`, when stdin content is validated, then it is classified as a data-source document (SQL-mode suppression and data-source rules apply); without `--ds`, stdin content is treated as an ordinary SSL document.
- A16: Given a data-source document containing only terminated SQL comments (`/* banner */`, `--` lines) and whitespace, when diagnostics are collected, then the result is empty; a document whose comment is the unterminated SSL form (`/* text;`) is NOT comment-classified and keeps SSL diagnostics.
- A17: Given a data-source document whose builder-directive / `:PARAMETERS` header is preceded by a terminated `/* ... */` comment, when diagnostics are collected, then hybrid detection still applies — no diagnostic fires on the comment or the SQL body, header lines keep their SSL and data-source checks at unshifted positions.
- A18: Given a data-source document containing a recognized header (optionally preceded by a terminated comment) and nothing after it, when diagnostics are collected, then header lines keep their checks and no comment-related diagnostic fires.
- A19: Given a SQL data-source body whose column or table names collide with SQL builtin-function names (`set FORMAT = …`, `delete from FORMAT`), or whose SQL comments and quoted string literals contain semicolons (`'all;msoffice->pdf'`), when diagnostics are collected, then the document classifies as SQL mode and no diagnostic fires on any of it.
- A20: Given a SQL-mode data-source body containing a `;` outside comments and string literals, when diagnostics are collected, then exactly one `datasource_sql_semicolon` error fires per such semicolon at its position (offset past the header in the hybrid shape, whose own `;` terminators never flag); the error honors rule overrides and never fires outside data-source files.
- A21: Given a SQL-mode data-source body containing a `@name` placeholder outside comments and string literals with no case-insensitive match among the header's `:PARAMETERS` names (all placeholders, when there is no header), when diagnostics are collected, then a `datasource_undeclared_placeholder` warning fires on the placeholder's span; `@@` system functions, declared placeholders, unused declared parameters, and `DECLARE`-scripted bodies stay silent.
- A22: Given a `.ds` data-source document whose SQL body carries no strong SSL marker — a SELECT whose list uses implicit column aliases (`col alias`), or any query that fails strict SQL-statement validation — when diagnostics are collected, then it classifies as SQL mode and no SSL diagnostic (`bare_logical_operator` on lowercase `and`/`or`, `dot_property_access` on `table.column`, or otherwise) fires; conversely, a `.ds` body carrying a strong SSL marker (a non-directive colon keyword **in statement-leading position** — the start of the body or just after a `;`, which is where SSL writes them and where a SQL bind such as `WHERE s = :default` never is (issue #249) — a `:=` assignment, or a leading unterminated `/* …` comment) classifies as SSL and keeps the full data-source diagnostic set, so a bare `and` in SSL code still flags `bare_logical_operator` (issue #153).
- A27: Given default options, when a document emits an info-severity diagnostic, then it is dropped — the info tier is opt-in (`ssl.diagnostics.infoDiagnostics`); given the setting enabled, the same diagnostic is delivered; and given a rule explicitly configured in `ssl.diagnostics.rules` (including remapped *to* info), that rule's diagnostics are delivered regardless of the gate. Errors, warnings, and hints are never gated.
- A24: Given a data-source document whose leading banner comment closes with `*/;` (optionally with spaces before the `;`) followed by a builder-directive / `:PARAMETERS` header and a SQL body, when diagnostics are collected, then the header still splits — no SSL diagnostic (`dot_property_access`, `bare_logical_operator`, or otherwise) fires on the SQL body, and the header lines keep their checks (issue #208). Masking that `;` is a parsing convenience only: in a SQL-mode document any separator with no statement before it is misplaced, and STARLIMS rejects the document with "Invalid SQL statement: remove any misplaced semicolons(;)", so the unmasked header region is additionally checked and reports a `datasource_sql_semicolon` error. A banner closed with a plain `*/` is clean, and a stray separator no longer ends the header scan (issue #249).
- A25: Given a data-source header whose statements are interleaved with `--` line comments (`:DSN := x;` … `--note` … `:PARAMETERS p := v;`), when diagnostics are collected, then the header split includes every header statement — the comment lines mask to blanks inside the header and no diagnostic fires on them — while a `--` comment run followed by non-header content stays with the body (issue #208).
- A26: Given a SQL data-source body whose only SSL-looking content sits inside line-leading `--` comments (a commented-out directive such as `--:PARAMETERS p := '';`), when diagnostics are collected, then the document classifies as SQL mode — commented-out code is never a strong SSL marker — while a mid-line `--` (SSL decrement territory) does not suppress marker detection (issue #208).
- A23: Given a document whose `:REGION` body holds non-SSL payload (HTML with dotted attributes, JavaScript `&&`/`==`, 0-based indexing), when diagnostics are collected, then no diagnostic fires on any body line; a region missing its `:ENDREGION` still reports `unclosed_block`; and formatting the document leaves every body line byte-identical (issue #164).
- A28: Given the `--validate` CLI, when `--info` or `--hungarian` is passed, then the corresponding opt-in option is enabled for that run — info-severity diagnostics are delivered, and `hungarian_notation` / `hungarian_type_mismatch` fire — and without the flag each stays off, so the CLI default matches the editor default.
- A29: Given the `--validate` CLI, when `--strict` is passed, then `undeclared_variable`, `unused_variable`, and `invalid_sql_param` are all enabled for that run, and no other opt-in option is — `--strict` implies neither `--info` nor the Hungarian checks; without it all three stay off, so the CLI default matches the editor default. None of the three is error severity, so a document whose only findings are strict findings stays `valid` (issue #249).
- A30: Given a data-source document, when a caller enables `CheckUndeclaredVars`, `CheckUnusedVars`, or `CheckSQLParams`, then none of the three runs — the data-source dialect does not follow SSL scoping (directive values are bare identifiers used as values, `:PARAMETERS` carries inline `:=` defaults, and the SQL body names columns and keywords), and `checkDataSourceUndeclaredPlaceholders` is its placeholder check instead. The same options on an ordinary SSL document still report (issue #249).
- A31: Given a `.ds` document whose builder-directive / `:PARAMETERS` header is preceded by a banner written in SSL comment style (a `/*` run closed by `;` rather than `*/`), when the header is split, then the banner is masked to blanks like the terminated form and the header scan reaches the directives beneath it — so the body classifies on its own content and a SQL body stays in SQL mode. The leading-unterminated-comment signal decides only the headerless shapes: a comment-only stub and an SSL data source opening with an unterminated comment both still classify as SSL (issue #249, preserving A16/A22).
- A32: Given a line holding an emoji (one character, two UTF-16 code units) before a flagged construct — an SSL finding such as `!==`, or an undeclared `@name` placeholder in a SQL data-source body — when diagnostics are published, then the range starts and ends at the construct's UTF-16 offsets, not one unit early; a line without such characters is unaffected (issue #307).
- A33: Given a document producing more than 100 diagnostics, when they are published, then the published 100 are the most severe — errors first, then warnings, info, and hints, each severity in position order — so an error is never dropped while a less severe diagnostic is published, whatever order the checks emitted them in (issue #258).

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
editor usable and makes bug reports actionable. SQL-mode data sources
(#77) originally reused the formatter's structural SQL detection —
first-token-is-a-command-keyword plus statement validation — to decide
SQL vs SSL. That detector is tuned to reject English prose inside SSL
string literals, so it has false negatives on real queries: a SELECT list
with implicit column aliases (`col alias`) is indistinguishable from prose
(`the samples`) and failed validation, leaking SSL diagnostics like
`bare_logical_operator` onto a legitimate SQL `and` (#153). The fix
inverts the default (#153): a `.ds` file is overwhelmingly a SQL data
source, so it is SQL unless its body carries a *strong SSL marker* — a
construct SQL never contains. Because the SSL lexer emits a keyword token
only off a leading colon and SQL has no `:KEYWORD` syntax or bare `:=`, a
non-directive colon keyword or a body `:=` is decisive and cheap to detect
(strings and comments are consumed as single tokens, so a marker inside
them does not count). The one SSL keyword that is also a header directive,
`:PARAMETERS`, and its inline `:=` defaults are excluded by splitting the
header off first. Header-comment handling (#148) supplies the last marker:
only a *terminated* `/* ... */` is a SQL comment, so an unterminated
leading `/*` — detected by masking terminated leading comments and seeing
whether a bare `/*` remains, which sidesteps the SSL lexer's habit of
stopping a comment at the first `;` — marks SSL and keeps its
comment-termination diagnostic.
