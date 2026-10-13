---
id: feature.cli_contract
title: CLI contract for --validate and --format (gating and scoring consumers)
kind: feature
status: active
authority: tool
schema_ref: null
config: []
tests:
  - cmd/starlims-lsp/cli_contract_test.go
  - cmd/starlims-lsp/validate_test.go
history:
  - date: 2026-04-30
    ref: "LSP PR #3 (v0.4.0)"
    note: >-
      --validate ships with a JSON array of per-file results (`file`,
      `valid`, `diagnostics`) and exit codes 0/1; every diagnostic gains a
      stable `code` slug, which is what makes the output machine-usable.
  - date: 2026-08-29
    ref: "issue #236, #238 (v0.20.0, v0.21.0)"
    note: >-
      --info and --hungarian, then --hungarian-types, added so a CLI
      caller can reach the opt-in tiers. Shipping them exposed the
      unknown-flag trap: an older binary read the new flag as a file path
      and returned a phantom invalid entry beside the real result.
  - date: 2026-09-21
    ref: "issue #249 (v0.24.0)"
    note: >-
      --strict added for finished-code consumers: undeclared_variable,
      unused_variable, and invalid_sql_param.
  - date: 2026-10-13
    ref: "issue #262"
    note: >-
      The contract written down for consumers that gate or score with the
      CLI rather than read it in an editor. Three changes came with it:
      unknown flags now fail with exit 2 instead of becoming file paths,
      and a missing-input usage error moved from exit 1 to 2 to match
      --format, so 1 always means a finding; every result object carries
      the tool `version`; and the --strict false-positive budget was
      measured and set (see False-positive budget). The versioning policy
      is DECISIONS.md D16.
  - date: 2026-10-13
    ref: "issue #255 (features audit, PR #299)"
    note: >-
      --format gained --ds, and its SQL-mode passthrough applies only to
      data sources (.ds/.ds.txt or --ds), as --validate's does; content
      alone no longer decides it.
  - date: 2026-10-13
    ref: "issue #258"
    note: >-
      An unknown dash-prefixed first argument exits 2 instead of starting
      the language server (A16). A mistyped mode such as `--valdiate`
      used to start the server, which then waited on stdin and hung the
      CI job that ran it. No arguments, `--stdio`, and
      `--clientProcessId` still start the server.
issues: []
---

## Behavior

This entry is the contract between the `--validate` / `--format` command
line and the programs that consume it non-interactively: the style
guide's MCP server, agents that must pass `--validate --strict` clean,
and evaluation pipelines that accept, reject, or score SSL by the JSON.
For those consumers a false positive rejects or penalizes correct code,
so every observable detail below is part of the contract and changes
only under the versioning policy at the end.

### Invocation

```text
starlims-lsp --validate [flags] <file> [<file> ...]
starlims-lsp --validate [flags] --stdin
starlims-lsp --format   [flags] <file> [<file> ...]
starlims-lsp --format   [flags] --stdin
```

- The mode flag (`--validate`, `--format`) must be the **first**
  argument; the binary picks its mode from `argv[1]` alone. The language
  server starts when there is no argument, when the first is `--stdio`
  (which LSP clients append) or `--clientProcessId[=<pid>]` (the other
  launcher convention), or when the first does not start with a dash. Any
  other dash-prefixed first argument (a mistyped mode such as
  `--valdiate`, or a mode flag given without its mode) is a usage error:
  exit 2, an `Error: unknown flag "<arg>"` line on stderr, and nothing on
  stdout. It never starts a server that would wait on stdin.
- Mode flags may appear anywhere after the mode, before, between, or
  after file arguments, with identical results. Repeating a flag is
  harmless, and the profile flags compose by union.
- **Unknown flags are usage errors.** Any argument longer than one
  character that starts with `-` and is not a flag of that mode is
  rejected: exit 2, an `Error: unknown flag "<arg>"` line on stderr, and
  nothing on stdout. It is never read as a file path.
- `--` ends flag parsing: every later argument is a file path, even one
  that starts with a dash. A lone `-` is an ordinary path, not stdin.
- `--help` / `-h` prints the mode's help on stdout and exits 0.
- The CLI runs single-file. There is no workspace index, so `:INCLUDE`
  targets are not resolved, and no editor settings apply
  (`ssl.diagnostics.rules`, `ssl.diagnostics.globals`,
  `ssl.diagnostics.endpointPatterns`). In-source suppression comments
  (`@ssl-disable`, `@ssl-disable-next-line`) are honored, exactly as in
  the editor (feature.diagnostics_pipeline).

### --validate flags and profiles

Every run starts from the editor's default diagnostic options. Each flag
switches on exactly what its row says and implies no other flag.

| Flag | Turns on |
|------|----------|
| (none) | The editor default surface: errors, warnings, and hints; info is dropped |
| `--stdin` | Read one input from stdin, reported as `file: "stdin"` |
| `--ds` | Classify **every** input in the run as a data-source document (files ending `.ds` / `.ds.txt` already are); needed for stdin, which has no extension |
| `--info` | The info-severity advisory tier (D13) |
| `--hungarian` | `hungarian_notation` and `hungarian_type_mismatch` |
| `--hungarian-types` | `hungarian_type_mismatch` only |
| `--strict` | `undeclared_variable`, `unused_variable`, `invalid_sql_param` (warning/hint severity; feature.diagnostics_pipeline A29) |
| `--help`, `-h` | Print help, exit 0 |
| `--` | End of flags |

### --validate output

Stdout is one JSON array (pretty-printed; whitespace is not part of the
contract) with one object per input: the stdin input first when
`--stdin` is given, then each file argument in argument order. Consumers
that batch files correlate results **by index**, because `file` holds
only the base name and two paths can share one.

```json
[
  {
    "file": "script.ssl",
    "version": "v0.25.0",
    "valid": false,
    "diagnostics": [
      {
        "line": 3,
        "column": 6,
        "severity": "error",
        "message": "Unexpected token ...",
        "source": "ssl-lsp",
        "code": "unexpected_token"
      }
    ]
  }
]
```

Result object:

| Field | Type | Meaning |
|-------|------|---------|
| `file` | string | Base name of the file, or `"stdin"` |
| `version` | string | Tool version, the value `--version` reports (`dev` for an unstamped build) |
| `valid` | boolean | `false` if and only if `diagnostics` holds at least one `error` |
| `diagnostics` | array | Diagnostic objects; `[]` when clean, never `null` |

Diagnostic object:

| Field | Type | Meaning |
|-------|------|---------|
| `line` | integer | 1-based line of the range start |
| `column` | integer | 1-based column of the range start, counted in Unicode code points |
| `severity` | string | `error`, `warning`, `info`, or `hint` |
| `message` | string | Human-readable text. **Not** part of the contract; key off `code` |
| `source` | string | `ssl-lsp` for every pipeline diagnostic; `ssl-validate` for an input the CLI could not read |
| `code` | string | The stable rule slug (a `diag.<code>` catalog entry). Present on every `ssl-lsp` diagnostic; absent only on `ssl-validate` read failures |

Diagnostic order within an object is deterministic for a given binary
and input but is not otherwise specified.

**`valid` semantics.** `valid` is a per-input verdict: no
error-severity diagnostic. Warnings, info, and hints never flip it.
`--info`, `--hungarian`, `--hungarian-types`, and `--strict` never flip
it on their own, because none of the checks they enable reports at error
severity. `--ds` is different. It changes how the document is classified
and so which rules apply, so it can flip `valid` in either direction. An
`@ssl-disable` comment that silences an error also makes the input
valid. A file that cannot be read (missing, a directory,
no permission) yields an object with `valid: false` and exactly one
`error` diagnostic with `source: "ssl-validate"` and no `code`; the other
inputs are still validated.

**Exit codes.**

| Code | --validate | --format |
|------|------------|----------|
| 0 | Every object is `valid` | Success; with `--check`, every input is already formatted |
| 1 | At least one object is not `valid` (including unreadable inputs). The JSON is still written | An input could not be read or written, or `--check` found one to change |
| 2 | Usage error: unknown flag, no inputs. Nothing on stdout | Usage error: unknown flag, bad flag value, conflicting modes, no inputs |

**Why the version is on each object.** The document's top level is an
array, and consumers index it directly. Wrapping it in an object to hold
one top-level `version` would break every one of them. Repeating the
version on each result is an added field, which the policy below treats
as compatible.

### --format output

`--format` emits no JSON. Default mode writes formatted text to stdout.
With several files, each file's output is separated by a blank line, and
a `==> path <==` header for each file goes to stderr. `--write` rewrites
files in place and names each changed file on stderr. `--check` writes
nothing to stdout and names each file that would change on stderr.
`--stdin` cannot be combined with file arguments or `--write`, and
`--write` cannot be combined with `--check`. Its flags are `--stdin`,
`--write`/`-w`, `--check`, `--indent-style <tab|space>`,
`--indent-size <n>`, `--max-line-length <n>`, `--no-sql`, `--ds`,
`--help`/`-h`, and `--`. A data source (a `.ds`/`.ds.txt` path, or any
input under `--ds`) whose content is SQL-mode passes through unchanged;
every other input is formatted as SSL (feature.formatting). A consumer
that needs the tool version calls `--version`.

### Versioning policy

DECISIONS.md D16 holds the full policy. The contract surface is
everything in this entry: flag names and meanings, the JSON field names,
types, and nesting, `valid` semantics, exit codes, the diagnostic `code`
slugs, and which codes each profile can emit at error severity.

- **Breaking**: removing or renaming a flag or a `code`; renaming,
  retyping, or removing a JSON field, or reshaping the document; changing
  what `valid` or an exit code means; a new default-on error-severity
  code, or raising an existing default-on code to error, since either can
  turn valid code invalid. A breaking change ships in a **minor** release
  while the project is pre-1.0, never a patch, under a `### Breaking`
  subsection of that release's CHANGELOG section, which says what breaks
  and what a consumer changes.
- **Behavior change**: a new default-on warning or hint, a profile gaining
  a check, or a severity change below error. These ship in a minor
  release under `### Changed` or `### Added`, with the measured corpus
  delta, so gate owners can decide before they move their pin.
- **Compatible**: an added JSON field (consumers must ignore fields they
  do not know), a new flag, a new opt-in code, message text, and fixes
  that remove false positives. These may ship in a patch.

The set of codes that can make `valid` false is pinned in
`cmd/starlims-lsp/testdata/error_codes.txt` (A13), so a change to it
cannot land without someone deciding it is breaking.

## False-positive budget

A gate rejects code, so `--strict` is held to a stated false-positive
budget, measured on the private production corpus (6,228 SSL and
data-source documents; only aggregate numbers are recorded in public). A
**misfire** is a finding that contradicts the profile's own semantics,
for example a placeholder flagged as undeclared when `undeclared_variable`
accepts the same name as declared. Misfires count as false positives no
matter how good the code is. Every other finding is correct by its rule's
specification. The corpus does not follow the conventions these rules
encode, so most of those findings are correct reports about code no gate
would accept.

**Targets.**

- T1: misfires are at most **1%** of strict-only findings on corpus
  documents that validate without errors.
- T2: strict-only findings average at most **1.5 per document** on those
  documents, which bounds a gate owner's triage cost.
- T3: `--strict` never flips `valid` or the exit code (zero flips,
  pinned by feature.diagnostics_pipeline A29).

**Baseline (2026-10-13, main after v0.24.0).** Of 6,228 documents, 6,166
validate without errors. On those, the default surface is 8,415
findings (1.36 per document). `--strict` adds 18,489 (3.00 per document).
3,317 documents gain nothing, and the most any single document gains is
96. By code: `invalid_sql_param` 9,369, `unused_variable` 7,381,
`undeclared_variable` 1,739. No document changed `valid` (T3 holds).

Two misfire classes account for about 11,000 of the 18,489 findings,
roughly 60%:

- `invalid_sql_param` on a `?name?` whose name the script assigns
  without declaring: 8,813. `undeclared_variable` treats that assignment
  as declaring the name, and `SQLExecute` resolves it at run time, so
  the two strict rules disagree about the same name.
- `unused_variable` on a declared name, usually a `:PARAMETERS` name,
  whose only read is a `?name?` placeholder: up to 2,211. `SQLExecute`
  reads that variable, but the rule counts only identifier tokens as
  uses.

T1 and T2 are therefore **not met**. With both classes fixed, the
residual is about 7,465 findings (1.21 per document), which would meet
T2. Of the residual, 1,705 are `undeclared_variable` reports of names
that the script neither declares nor assigns, mostly application globals
that the single-file CLI cannot see. Those are correct by specification,
but a consumer gating multi-script code should expect them.

Until T1 is met, `--strict` is gate-grade only for self-contained code
that declares every name it uses and reads each parameter outside SQL
placeholders, which is the code an agent is asked to produce. Recheck the
baseline whenever any of the three rules changes.

## Acceptance

- A1: Given a `--validate` run whose arguments parse, when it completes, then stdout is exactly one JSON array with one object per input — the `--stdin` input first, then each file argument in argument order — and every object carries `file` (base name, or `stdin`), `version` (string), `valid` (boolean), and `diagnostics` (an array, `[]` when clean, never null).
- A2: Given any input producing diagnostics, when it is validated, then each diagnostic object carries integer `line` and `column` (1-based; columns counted in Unicode code points), `severity` from {error, warning, info, hint}, `message`, and `source`, and every `ssl-lsp` diagnostic carries a non-empty `code`.
- A3: Given an input, when it is validated under any combination of profile flags, then `valid` is false if and only if at least one diagnostic has severity `error` — warnings, info, and hints never flip it.
- A4: Given a `--validate` run, when every object is valid the exit code is 0, and when at least one is not the exit code is 1 — the JSON is written in both cases.
- A5: Given a file argument that cannot be read, when the run completes, then its object has `valid: false` and exactly one `error` diagnostic with `source: "ssl-validate"` and no `code`, the other inputs are still validated and reported, and the exit code is 1.
- A6: Given a dash-prefixed argument `--validate` does not know (`--future-flag`, `-x`), when the run starts, then it exits 2 with an error on stderr naming that argument and writes nothing to stdout — the argument is never read as a file path.
- A7: Given `--` among the arguments, when they are parsed, then every later argument is a file path even when dash-prefixed, and a lone `-` is always a file path, never a flag.
- A8: Given a `--validate` run with no inputs (no file arguments and no `--stdin`), when it starts, then it exits 2 with a usage error on stderr and writes nothing to stdout.
- A9: Given any `--validate` run, when results are written, then every object's `version` equals the version `--version` reports — including the object for an unreadable input.
- A10: Given profile flags placed before, between, or after file arguments, or repeated, when the run completes, then the output is identical to the same flags given once up front, and flags compose by union.
- A11: Given each `--validate` profile flag, when the run's diagnostic options are built, then the flag switches on exactly the options in this entry's profile table and nothing else, starting from the editor defaults.
- A12: Given `--format` with an unknown dash-prefixed argument, when it starts, then it exits 2 naming the argument on stderr with nothing on stdout; `--` ends its flag parsing; `--check` exits 1 when an input would change and 0 when none would; and a usage error (conflicting modes, no inputs) exits 2.
- A13: Given the active catalog diagnostics whose default severity is `error`, when the conformance test runs, then their slugs equal the pinned list in `cmd/starlims-lsp/testdata/error_codes.txt` — adding, removing, or renaming one fails the build until the pin is updated together with a CHANGELOG `### Breaking` note (D16).
- A14: Given the result and diagnostic JSON objects, when their field names are enumerated, then they equal exactly the field sets in this entry's tables — adding a field requires updating this entry; renaming or removing one is breaking (D16).
- A15: Given `--ds` and a file argument without a data-source extension, when the file is validated, then it is classified as a data-source document exactly as a `.ds` file would be — `--ds` applies to every input in the run, not only stdin.
- A16: Given a dash-prefixed first argument that is not a mode, `--help`/`-h`, `--version`/`-v`, `--stdio`, or `--clientProcessId[=<pid>]` (for example `--valdiate`, or `--strict` without `--validate`), when the binary starts, then it exits 2 with an error on stderr naming the argument and writes nothing to stdout — it never starts the language server; given no arguments, `--stdio`, or `--clientProcessId`, the language server starts.

## Rationale

The CLI started as a convenience for CI and agent skills. It is now
how three kinds of consumer judge SSL without ever opening an editor: a
pinned MCP server, agents that must pass `--strict` clean, and
evaluation pipelines that score by `valid` and `code`. Those consumers
depend on details an editor never sees, such as the JSON shape, exit
codes, and the behavior of an unknown flag. Leaving those unwritten made
them accidental.

Rejecting unknown flags fixes the one failure that has already
happened. Reading `--hungarian` as a file path on a binary that predated
it produced a phantom `valid: false` entry beside a result computed with
the wrong options, and the consumer could not tell the difference
without a guard keyed to the flag's spelling. A loud exit 2 with nothing
on stdout cannot be mistaken for a result. `--format` already behaved
this way. The usage-error exit code moved from 1 to 2 for the same
reason: a gate that sees 1 should be able to rely on it meaning "your
code has a problem", never "you called me wrong".

The version rides on each result because the top-level array is load
bearing for existing consumers. One repeated string is cheap, and it
lets a consumer's log say which binary scored it. That matters because
one consumer runs an unpinned local build.

The error-code pin (A13) and the field-set pin (A14) turn the versioning
policy from prose into a failing test at the moment a breaking change is
made, which is when the CHANGELOG note is cheapest to write.

## Known gaps

- `--strict` misses its false-positive budget (T1, T2) because of the
  two misfire classes recorded above. Fixing them belongs to the
  `invalid_sql_param` and `unused_variable` entries, under the audit epic
  #253.
- The CLI does not apply the editor's leading-docblock `Endpoint:` marker
  classification, so under `--strict` an endpoint script's `Request` and
  `Response` report as undeclared where the editor would accept them
  (feature.diagnostics_pipeline A8; tracked under #253).
