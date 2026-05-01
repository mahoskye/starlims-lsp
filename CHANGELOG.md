# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

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

[Unreleased]: https://github.com/mahoskye/starlims-lsp/compare/v0.2.0...HEAD
[0.2.0]: https://github.com/mahoskye/starlims-lsp/compare/v0.1.0...v0.2.0
[0.1.0]: https://github.com/mahoskye/starlims-lsp/releases/tag/v0.1.0
