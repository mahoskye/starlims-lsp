# Scenario suite

Expected outcomes for realistic SSL. Each scenario holds one input and
what the CLI produces for it: the formatted text and the diagnostics under
one or more flag profiles. The catalog says what each behavior *is*, one
small fence at a time. Scenarios record what the tool *does* to whole
files, where many rules meet. A change in behavior shows up here as a diff
to review, even when no catalog fence moves.

The suite runs in `go test ./...` (`cmd/starlims-lsp/scenario_test.go`)
through the same functions `--format` and `--validate` use.

## Precedence

The catalog stays normative ([D9](../catalog/DECISIONS.md)). An expected
section records current behavior, so it can record a bug. When an outcome
here contradicts the catalog or the style guide, the scenario has found a
bug. Do not "fix" the expected text by hand. Open an issue, and note it in
the scenario's header (`Known issue: #NNN ...`) until the fix lands and
`-update` rewrites the section.

## File format

One `.txtar` file per scenario ([txtar](https://pkg.go.dev/golang.org/x/tools/txtar)):

```text
What the scenario covers and why: the rules it exercises, the shape it
reproduces, the issue that prompted it.
-- input.ssl --
:DECLARE sFirstName,
sSuffix;
-- format --
:DECLARE sFirstName,
         sSuffix;
-- diagnostics --
-- diagnostics [--strict --info] --
2:10-2:17 warning unused_variable: ...
```

- **Header** (required): free text before the first section.
- **Input** (required, first section): `input.ssl`, or `input.ds` /
  `input.ds.txt` for a data source. The extension classifies the input
  exactly as a CLI path would. A txtar section always ends in a newline,
  so an input cannot end without one.
- **`format`**: the `--format` output.
- **`diagnostics`**: the `--validate` findings, one per line, sorted by
  position: `startLine:startCol-endLine:endCol severity code: message`
  (1-based, like the CLI's JSON). Empty means no findings.
- **Flag variants**: a bracketed suffix runs a section with those CLI
  flags, e.g. `format [--indent-style space --max-line-length 60]` or
  `diagnostics [--hungarian]`. Any `--format` / `--validate` flag works.

Every scenario carries `format`, `diagnostics` and
`diagnostics [--strict --info]`. Add variants when a scenario is about an
option.

## Invariants

Every scenario is also checked, under each `format` variant's flags, for
contracts that need no expected text:

- nothing panics (a recovered panic surfaces as `internal_error`);
- formatting is idempotent (feature.formatting A6);
- formatting changes only whitespace, letter case and semicolons;
- the formatted output has no error the input did not already have, under
  the default and `--strict --info` profiles;
- range-formatting any line of the formatted output leaves it unchanged
  (feature.formatting A2).

A scenario with an unremarkable outcome still earns its place by putting
its input through all of these.

## Workflow

```bash
go test ./cmd/starlims-lsp -run TestScenario            # check
go test ./cmd/starlims-lsp -run TestScenario -update    # write expected sections
```

`make scenarios` and `make scenarios-update` do the same.

To add a scenario, write the header and input, run `-update`, then
**read the generated sections** against the catalog and the style guide
before committing. Generation records behavior, and only that review
makes it an expectation. In a change that alters behavior, review the
diff of every scenario it touches the same way.

## Coverage

`uncovered_codes.txt` lists the active diagnostic codes that no
scenario's expected diagnostics contain. `TestScenarioCoverage` keeps it
exact: a covered code must leave the list, and a new code needs a scenario
or an entry. `-update` rewrites it.

## Layout

Directories group scenarios by area: `formatting/`, `diagnostics/`,
`datasources/`, `classes/`, `programs/` (whole realistic scripts), and
`edge-cases/`. Inputs drawn from production code must be anonymized
first: generic identifiers, no real procedure, table or file names.
