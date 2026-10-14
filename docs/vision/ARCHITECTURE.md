# LSP Architecture

This document describes how starlims-lsp is built. It is descriptive: the
normative behavior of every feature, diagnostic, and formatter rule is its
entry in the [behavior catalog](../../catalog/) ([D9](../../catalog/DECISIONS.md)).
File and function names are given so you can find the code; read the code
for details.

**Last updated:** 2026-10-13 (v0.24.0 plus unreleased)

---

## Overview

starlims-lsp is one Go binary with two faces:

- **Language server.** Talks to an editor over stdio with JSON-RPC (LSP).
- **CLI.** `--validate` (diagnostics as JSON), `--format` (formatter),
  `--export-signatures` (the built-in inventory as JSON), and `--version`.

Both faces call the same feature code in `internal/providers`, so the CLI
and the editor report the same diagnostics and produce the same formatting.

```
             stdio / JSON-RPC
  Editor  <------------------->  internal/server  --+
                                                    |
  CI / agents --> cmd/starlims-lsp (--validate,     +--> internal/providers
                  --format, --export-signatures)    |      (feature logic)
                                                    |
                         internal/parser  <---------+
                         internal/lexer
                         internal/constants (SSL inventory)
```

---

## Protocol Details

| Property | Value |
|----------|-------|
| Library | [`github.com/tliron/glsp`](https://github.com/tliron/glsp), LSP 3.16 protocol types |
| LSP 3.17 additions | `textDocument/inlayHint`, handled by `WrapperHandler` (`internal/server/wrapper_handler.go`) in front of glsp |
| Transport | stdio |
| Encoding | JSON-RPC 2.0 |
| Text sync | Incremental |

---

## Packages

Module `starlims-lsp` (`go.mod`). The import graph runs one way:
`lexer → parser → providers → server → cmd`, with `constants` underneath
all of them. `providers` never imports `server`.

| Package | Responsibility |
|---------|----------------|
| `cmd/starlims-lsp` | The binary: LSP server by default; `validate.go`, `format.go`, `export.go` implement the CLI modes. |
| `cmd/gen-inventory` | Generates `internal/constants/generated_*.go` from the vendored reference data. |
| `cmd/gen-catalog-docs` | Generates `docs/STATUS.md` and `docs/reference/DIAGNOSTICS.md` from `catalog/`. |
| `internal/server` | glsp handlers, document cache, settings, workspace index, cross-file references and rename, the 3.17 wrapper. |
| `internal/providers` | Feature logic: diagnostics, completion, hover, signature help, definition, references, rename, symbols, folding, inlay hints, SSL and SQL formatting, data-source SQL mode, cross-file target extraction. |
| `internal/parser` | Statement/block tree (`parser.go`), the expression AST (`expr*.go`), identifier roles, declaration collection, procedure visibility annotations. |
| `internal/lexer` | The SSL tokenizer and token-position helpers. |
| `internal/constants` | The SSL inventory (functions, classes, keywords, operators, literals, types), mostly generated from vendored reference data, plus hand-curated lists. |
| `internal/catalog` | Loads `catalog/`, renders the generated docs, and holds the conformance and spec tests. |
| `dev/exprcoverage` | Developer tool: expression-AST coverage measurement over a corpus. The rest of `dev/` is ignored (local copies of sibling repos). |

---

## Document Lifecycle

1. **Start.** `main` creates the server (`server.NewSSLServer`) and runs it
   on stdio. No argument, `--stdio`, `--clientProcessId`, or a first
   argument without a dash starts the server; any other dash-prefixed first
   argument that is not a CLI mode exits 2 (feature.cli_contract A16).
   Every URI a client sends is canonicalized at the handler boundary
   (`canonicalURI` in `uri.go`), and the workspace index builds its keys
   with the same helpers, so a file has one URI in the document store and
   the index (feature.cross_file_resolution A21).
2. **`initialize`.** Records the workspace roots, applies
   `initializationOptions` as settings, and advertises capabilities:
   incremental sync, completion triggered on `:` only, signature help
   (trigger characters only when `ssl.intellisense.signatureHelp.autoTrigger`
   is on), rename with prepare, workspace symbols, inlay hints, document
   and range formatting.
3. **`initialized`.** Creates the workspace index, starts indexing in the
   background, and registers file watchers for `*.srvscr`, `*.ssl`,
   `*.ssl.txt`, `*.ds`, and `*.ds.txt`.
4. **`didOpen` / `didChange`.** Stores the text (applying ranged edits) and
   re-validates the document synchronously. There is no server-side
   debounce.
5. **`didSave`.** Re-validates.
6. **`didClose`.** Drops the in-memory document and re-indexes the file
   from disk. **`didChangeWatchedFiles`** re-indexes or removes files that
   are not open. Re-indexing is queued on the index's background worker,
   and a removal supersedes any parse of the same file still in flight.
7. **Feature requests.** Handlers in `handler.go` get the parsed document
   from `DocumentManager.ParseDocument(uri, version)`, call into
   `providers`, and convert the providers' 1-based positions to LSP's
   0-based ones.
8. **`shutdown`.** Stops the indexer.

### Document cache

`DocumentManager` (`internal/server/cache.go`) holds each open document's
text and a `DocumentCache` per version: tokens, statement tree, procedures,
and variables. Parsing happens once per version; every feature request on
that version reuses it.

---

## Lexer and Parser

### Lexer (`internal/lexer`)

Turns text into tokens: keywords, identifiers, strings, numbers,
operators, comments, code blocks, region bodies, and so on. A string that
never closes is marked `Unterminated`, which the diagnostics and the
formatter both respect.

### Statement tree (`internal/parser/parser.go`)

`Parse()` groups tokens into statements and nests them into blocks
(procedures, classes, `:IF`, loops, `:TRY`, `:BEGINCASE`, ...). It also
extracts procedure information (parameters, docblock, `/*@private;`
visibility) and the control-flow blocks that folding uses. Variable
extraction reads declarations statement by statement
(`declarations.go`), so a bare `:DECLARE` declares every name on it.

### Expression AST (`internal/parser/expr*.go`, issue #184)

A Pratt parser that builds expression trees on demand, never during
`Parse()`. It follows the precedence order of the style guide's grammar.
Anything it cannot resolve becomes an `ExprUnknown` node, which consumers
must treat as "no claim" rather than as evidence.

Entry points:

- `ExtractStatementExpressions(tokens)` — one record per statement (kind,
  whether it parsed completely, and the first unexpected token if not).
- `CollectCalls` / `CollectCallsIn` — every call site with its arguments.
- `IdentifierRoles(tokens)` — tags each identifier as variable, member,
  call, instantiation, declared name, or procedure name.
- `CollectDeclarations` / `DeclarationSpans` — declarations by statement.

Consumers:

- Diagnostics build the statement expressions and call sites once per run
  and feed `unexpected_token`, `hungarian_type_mismatch`,
  `format_arg_not_array`, and `builtin_excess_arguments`. Type inference
  for those checks (`providers/expr_types.go`) draws on the generated
  function and operator summaries, and returns "unknown" whenever the
  inventory makes no claim.
- References and rename use identifier roles so that `oRec:sName` (a
  member) is not treated as the variable `sName`.
- Hover uses identifier roles to stay silent on unknown members
  ([D10](../../catalog/DECISIONS.md)).
- The undeclared-variable check uses declaration spans.

---

## Providers (`internal/providers`)

Each feature is a set of plain functions over text, tokens, and the
parsed tree; none of them depend on the server. The main groups:

| Area | Files |
|------|-------|
| Diagnostics | `diagnostics.go` (pipeline and most rules), `diagnostic_codes.go` (every code slug), `unexpected_token.go`, `unterminated_string.go`, `expr_call_checks.go`, `expr_hungarian.go`, `expr_types.go`, `sql_advisories.go`, `datasource_sql.go` |
| Data sources | `sql_mode.go` (SQL-or-SSL mode detection, [D15](../../catalog/DECISIONS.md)) |
| Completion, hover, signature help | `completion.go`, `hover.go`, `signaturehelp.go`, `function_docs.go`, `typed_receivers.go`, `udobject_shapes.go` |
| Navigation | `definition.go` (definition and same-file references), `rename.go`, `symbols.go` (document symbols and folding), `crossfile.go` |
| Inlay hints | `inlayhints.go` |
| Formatting | `formatting.go`, `line_wrap.go`, `code_block.go`, `sql_formatter.go`, `sql_lexer.go`, `sql_constants.go` |

### Diagnostics pipeline

Normative contract: [feature.diagnostics_pipeline](../../catalog/features/diagnostics_pipeline.md).

- **Entry.** The server's `validateDocument` calls
  `GetDiagnosticsFromTokens` with the cached tokens and tree, or
  `GetDiagnostics(text)` for data sources. The CLI calls the same
  functions.
- **Data sources.** `.ds` / `.ds.txt` files are classified as SQL or SSL
  by `sql_mode.go`. A SQL-mode document gets only the SQL-body checks; a
  directive header followed by SQL gets SSL checks on the header and SQL
  checks on the body.
- **Collection.** `collectDiagnostics` runs the lexer-level checks, the
  block and delimiter checks, the always-on rules, the expression-tree
  rules, and whichever opt-in checks the options enable (Hungarian;
  undeclared variables, unused variables, and SQL parameters). It
  recovers from a panic in any check and reports it as an
  `internal_error` diagnostic instead of crashing.
- **Post-processing, in order.** Suppression comments
  (`@ssl-disable`, [D3](../../catalog/DECISIONS.md)), per-rule severity
  overrides (`ssl.diagnostics.rules`, [D2](../../catalog/DECISIONS.md)),
  then the info gate, which drops info-severity findings unless the info
  tier is on or the rule has an explicit override
  ([D13](../../catalog/DECISIONS.md)).
- **Output.** Every diagnostic carries a stable `code` slug and
  `source: "ssl-lsp"` ([D1](../../catalog/DECISIONS.md)). The server caps
  each publish at 100 diagnostics, after sorting errors first, then
  warnings, info, and hints, each in position order.

### Formatting

`FormatDocument` / `FormatDocumentRange` re-emit the token stream with
the configured layout, then run the line-wrap engine, code-block literal
normalization, and the SQL-in-strings formatter. String and comment
content is never changed, except detected SQL strings
([D7](../../catalog/DECISIONS.md)). A document with an unterminated
string, or a data source in SQL mode, is returned unchanged.

---

## Cross-File Resolution

Normative contract: [feature.cross_file_resolution](../../catalog/features/cross_file_resolution.md).

### Workspace index (`internal/server/workspace_index.go`)

On `initialized`, every SSL file under the workspace roots is lexed and
parsed in the background (four at a time). Each file gets a `FileSymbols`
record: its procedures (with parameters and docblocks), whether it is a
class or data source, its script identity, its entry-point `:PARAMETERS`,
declared variables, `:INCLUDE` targets, and its `DoProc` /
`ExecFunction` / `RunDS` / `:INCLUDE` call sites. Lookups by script name,
category and script, and procedure name are case-insensitive. Closing a
file and file-watcher events keep the index current; they queue the
re-parse on the same bounded worker, and a per-URI generation counter
stops a parse that a later delete or re-index superseded from committing.
A file whose parse panics is logged and skipped.

### Script identity (`internal/server/script_identity.go`)

The STARLIMS export layout determines what a dotted target means:

- `.../Server Scripts/CAT/SCRIPT.srvscr` is `CAT.SCRIPT`.
- `.../Applications/APP/MODULE/Server Scripts/SCRIPT` is `MODULE.SCRIPT`.
- `.../Data Sources/CAT/NAME.ds` is the data source `CAT.NAME`.
- Anything else is known by its file name only (no category).

### Resolvers

- `ResolveDispatchTarget` resolves `DoProc` / `ExecFunction` strings:
  `"Cat.Script"` (entry point) or `"Script.Proc"` for two parts, the
  category chain first for three or more parts, and finally a procedure
  name unique across the workspace. Data sources are never dispatch
  targets.
- `ResolveIncludeTarget` resolves `:INCLUDE`; `ResolveDataSourceTarget`
  resolves `RunDS` and matches data sources only.
- `CallSitesFor` finds the call sites of a procedure for references and
  rename; `SearchSymbols` serves workspace symbols.

### How features use it

`providers` defines a `WorkspaceResolver` interface (`crossfile.go`); the
server implements it in `liveResolver` (`cross_file.go`), which overlays
the positions of open, unsaved documents on the index. With no index the
resolver returns nothing and features fall back to same-file behavior.

- **Definition and hover** follow dotted dispatch, `RunDS`, and
  `:INCLUDE` targets to their files.
- **Completion** offers script and procedure names inside dispatch strings.
- **References** (`references_crossfile.go`) add workspace call sites of
  a procedure.
- **Rename** (`rename_crossfile.go`) edits a procedure and its dispatch
  sites across files, but only sites that resolve unambiguously, and
  refuses procedures in class files.
- **Diagnostics** receive the variables declared by a file's transitive
  `:INCLUDE` closure (cycle-guarded) and the dispatch targets that resolve
  to class files.

The `--validate` CLI has no workspace, so its cross-file inputs are empty.

---

## Constants and Reference Data (`internal/constants`)

- **Vendored data** (`data/`): `ssl-element-reference.json` (460
  elements: 330 functions, 29 classes, 38 keywords, 32 operators, and the
  literals, types, special forms, and returned objects),
  `ssl-element-meta.json` (exceptions, caveats, best practices), and
  `provenance.json` (the upstream commits). They are synced from the
  published [`starlims-ssl-reference`](https://github.com/mahoskye/starlims-ssl-reference);
  `data/README.md` has the refresh procedure.
- **Generated code**: `generated_*.go`, produced by `cmd/gen-inventory`
  through `go generate ./internal/constants/...`. CI fails if regenerating
  changes anything.
- **Hand-curated**: `constants.go` (keyword, operator, and literal lists,
  block keywords, SQL function lists, data-source directives),
  `canonical.go` (the public function and class inventories),
  `function_overrides.go` (the few documented overrides of signatures
  generated from the reference), and `predefined_globals.go` (parses the
  vendored predefined-globals guide).
- **Drift guard**: `drift_test.go` compares the vendored files with a
  sibling checkout when one is present and validates the provenance.
- `--version` prints the reference-data commit the binary was built with.

---

## Configuration Flow

Settings arrive through `initializationOptions` and
`workspace/didChangeConfiguration`. Both go through `applySettings`
(`internal/server/server.go`), which applies only the fields the client
sent. A configuration change then re-validates every open document. The settings themselves are
documented in [CONFIGURATION.md](../configuration/CONFIGURATION.md).

Two per-document properties are derived, not configured: whether a file
is a data source (by extension) and whether it is an endpoint script
(`ssl.diagnostics.endpointPatterns` or an `Endpoint:` docblock line,
[D4](../../catalog/DECISIONS.md)).

---

## CLI Modes (`cmd/starlims-lsp`)

| Mode | Purpose |
|------|---------|
| `--validate <files>` / `--stdin` | Prints a JSON array of `{file, valid, diagnostics}`; exits 1 if any error. Flags: `--ds`, `--info`, `--hungarian`, `--hungarian-types`, `--strict`. |
| `--format <files>` / `--stdin` | Prints formatted output, or rewrites with `--write`; `--check` exits 1 when a file is not formatted. Layout flags: `--indent-style`, `--indent-size`, `--max-line-length`, `--no-sql`. |
| `--export-signatures` | Prints the built-in inventory as JSON. |
| `--version` | Prints the version and the reference-data provenance. |

`--validate --help` and `--format --help` list the flags.

---

## Error Handling

- **Malformed code** is the normal case for an editor: the lexer and
  parser never fail, and the problems they find become diagnostics.
- **Diagnostics** recover from a panic in any check and report it as an
  `internal_error` diagnostic.
- **Other handlers** return an empty or null result when they have
  nothing truthful to say (for example, hover on an unknown member).

---

## Testing

- **Unit tests** beside each package. Server tests call the handlers
  directly with a stub context rather than over JSON-RPC.
- **Catalog conformance** (`internal/catalog/conformance_test.go`): every
  diagnostic code has exactly one catalog entry, entries lint clean,
  every documented setting appears in CONFIGURATION.md, the generated
  docs are current, and every feature acceptance criterion is cited by a
  test tagged `[spec <id>/A<n>]`.
- **Executable specs** (`internal/catalog/spec_test.go`): every `ssl`
  fence under a catalog entry's Flags / Does not flag / Before / After /
  Idempotent headings runs against the real pipeline
  ([D8](../../catalog/DECISIONS.md)). An `expect=fail` fence must keep
  failing until its gap is fixed.
- **Formatter idempotence** over a corpus in
  `internal/providers/testdata/idempotence/`.
- **Scenario suite** ([`scenarios/`](../../scenarios/README.md)): realistic
  inputs with their expected formatted output and diagnostics, checked
  through the CLI's own format and validate paths, plus invariants
  (idempotence, whitespace-only changes, no gained errors, range
  formatting) on every input. `scenarios/uncovered_codes.txt` ratchets
  diagnostic-code coverage.
- **CI** (`.github/workflows/test.yml`): gofmt, `go vet`, the
  generated-constants freshness check, and `go test ./...`.

`make test` runs everything; `make catalog-check` runs the catalog tests
alone; `make scenarios` / `make scenarios-update` check or regenerate the
scenario suite; `make generate-docs` regenerates the catalog-derived docs after a
catalog edit.

---

## Related Documents

- [catalog/README.md](../../catalog/README.md) — the catalog format and lifecycle
- [catalog/DECISIONS.md](../../catalog/DECISIONS.md) — cross-cutting decisions
- [STATUS.md](../STATUS.md) — implementation status, generated from the catalog
- [CONFIGURATION.md](../configuration/CONFIGURATION.md) — all settings
- [VISION.md](./VISION.md) — project goals and philosophy
- [ROADMAP.md](./ROADMAP.md) — what has shipped and the current direction
