# Production-corpus baseline

The standing false-positive and noise baseline for starlims-lsp, measured
against a private, production-representative STARLIMS application export.
The corpus is private: this document records **aggregate numbers only** —
no file names, identifiers or code. Re-run it after any change that can
move diagnostic volume or formatter output, and append a dated section
rather than editing an old one.

## 2026-10-13 — v0.24.0 (issue #256)

**Binary:** `starlims-lsp` v0.24.0, built from `main` at `2cac68a`
(`go build -ldflags "-X main.version=v0.24.0" ./cmd/starlims-lsp`). The
formatter rows were also re-run with the issue #256 branch build: its
formatted output is byte-identical to v0.24.0's on every corpus file, and
its diagnostic totals match in all three profiles.

**Corpus:** 6,228 SSL files — 4,620 server scripts (`*.srvscr`) and 1,608
data sources (`*.ds`). Client scripts (`*.clientscript`) are a different
language and are excluded. Every file is valid UTF-8; 35 contain non-ASCII
text.

### Commands

Run from the corpus root. Paths contain spaces, so the file list is
null-delimited, and `xargs -s` is raised so each profile runs as one
invocation (several invocations would split the JSON into several arrays).
`--validate` JSON is pretty-printed, so count codes with
`grep -oE '"code": *"[a-z_]+"'`, not a compact `"code":"…"` pattern.

```bash
find . \( -name '*.srvscr' -o -name '*.ds' \) -print0 | sort -z > list.0
xargs -0 -s 2000000 starlims-lsp --validate          < list.0 > default.json
xargs -0 -s 2000000 starlims-lsp --validate --info   < list.0 > info.json
xargs -0 -s 2000000 starlims-lsp --validate --strict < list.0 > strict.json
grep -oE '"code": *"[a-z_]+"' default.json | wc -l

# Formatter, on a copy of the files:
xargs -0 -s 2000000 starlims-lsp --format --check < list.0   # files that reformat
xargs -0 -s 2000000 starlims-lsp --format --write < list.0   # pass 1
xargs -0 -s 2000000 starlims-lsp --format --write < list.0   # pass 2: must change nothing
xargs -0 -s 2000000 starlims-lsp --validate < list.0 > formatted.json
```

`--validate` returns results in argument order, which is how per-file
before/after and per-provenance numbers are paired with the file list.

### Diagnostics by profile

| Profile | Total | error | warning | info | hint | Files with any finding | Files invalid (error) | Distinct codes |
|---|---:|---:|---:|---:|---:|---:|---:|---:|
| default | 8,800 | 145 | 7,465 | — | 1,190 | 1,665 | 62 | 45 |
| `--info` | 24,425 | 145 | 7,465 | 15,625 | 1,190 | 4,686 | 62 | 65 |
| `--strict` | 27,666 | 145 | 18,754 | — | 8,767 | 3,387 | 62 | 48 |
| `--strict --info` | 43,291 | 145 | 18,754 | 15,625 | 8,767 | 5,170 | 62 | 68 |

`--strict` adds 18,866 findings over the default profile: 9,478
`invalid_sql_param`, 7,577 `unused_variable` and 1,811
`undeclared_variable`. None is error severity, so the invalid-file count
does not move.

### Formatter

| Measure | v0.24.0 |
|---|---:|
| Files that reformat (`--format --check`) | 5,107 / 6,228 |
| Files changed by a second pass | 0 (idempotent) |
| Files that gain an error diagnostic after formatting | 0 |
| Files that lose an error diagnostic after formatting | 0 |
| Diagnostic delta after formatting | −66 `comment_text_after_terminator`, −1 `named_sql_param_unsupported` |

### Comparison with earlier baselines

| Measure | v0.21.0 | v0.23.0 | v0.24.0 |
|---|---:|---:|---:|
| Default total | 8,794 | 8,797 | 8,800 |
| error / warning / hint | 119 / 7,485 / 1,190 | 125 / 7,482 / 1,190 | 145 / 7,465 / 1,190 |
| `--info` total | 24,419 | — | 24,425 |
| `--strict` total | (no flag) | (no flag) | 27,666 |
| Files that reformat | 5,114 | — | 5,107 |
| Idempotent / files gaining errors | yes / 0 | — | yes / 0 |

The v0.23.0 column comes from the editor extension's corpus baseline,
whose totals cross-check this harness. The v0.23.0 → v0.24.0 movement is
fully explained by three changes in the v0.24.0 release notes, and none is
a regression:

- `arithmetic_type_mismatch` 281 → 26 warnings: the false-positive fixes
  in #249.
- `comment_swallows_code` 0 → 248 warnings: the new rule for a comment
  closed C-style that hides the next statement.
- `datasource_sql_semicolon` 10 warnings → 20 errors: promoted to error
  (STARLIMS rejects the statement) and now catching the banner form it
  used to mask.

The v0.21.0 → v0.23.0 step is +6 errors and −3 warnings; the errors are
the two always-on rules v0.22.0 added for #240, `unexpected_token` (5)
and `unterminated_string` (1). Formatting reformats 7 fewer files
than at v0.21.0, after the v0.22–v0.24 formatter fixes; it remains
idempotent with no file gaining an error.

### By provenance

The export can be split by where the code came from, using the export's
object paths and metadata: object paths carrying the deployment's own
naming, source-control checkout notes on an object, and references to
deployment-named objects in code. Every one of the 6,228 files classifies:

| Class | Files | Meaning |
|---|---:|---|
| Base product | 1,214 | STARLIMS product code with no sign of local change |
| Base product, locally modified | 953 | Product objects with checkout notes or deployment references |
| Deployment-written | 4,061 | Code the deployment wrote |

A diagnostic on unmodified base-product code is the strongest
false-positive signal available, because that code ships and runs.

| Profile | Class | Findings | Per file | error | warning | info | hint | Files invalid |
|---|---|---:|---:|---:|---:|---:|---:|---:|
| default | Base product | 1,501 | 1.24 | 22 | 1,114 | — | 365 | 20 |
| default | Base, modified | 484 | 0.51 | 29 | 311 | — | 144 | 5 |
| default | Deployment | 6,815 | 1.68 | 94 | 6,040 | — | 681 | 37 |
| `--info` | Base product | 4,737 | 3.90 | 22 | 1,114 | 3,236 | 365 | 20 |
| `--info` | Base, modified | 1,699 | 1.78 | 29 | 311 | 1,215 | 144 | 5 |
| `--info` | Deployment | 17,989 | 4.43 | 94 | 6,040 | 11,174 | 681 | 37 |
| `--strict` | Base product | 4,071 | 3.35 | 22 | 2,274 | — | 1,775 | 20 |
| `--strict` | Base, modified | 3,162 | 3.32 | 29 | 730 | — | 2,403 | 5 |
| `--strict` | Deployment | 20,433 | 5.03 | 94 | 15,750 | — | 4,589 | 37 |

Rates per 1,000 files for the codes where the classes differ most
(default profile, plus the three `--strict` checks):

| Code | Base product | Base, modified | Deployment |
|---|---:|---:|---:|
| `mixed_type_operator` | 51.9 | 14.7 | 897.8 |
| `sql_injection` | 350.9 | 86.0 | 294.0 |
| `zero_based_array_index` | 114.5 | 64.0 | 11.1 |
| `direct_procedure_call` | 68.4 | 21.0 | 21.9 |
| `exitcase_after_return` | 56.0 | 45.1 | 10.1 |
| `me_outside_class` | 42.8 | 2.1 | 2.2 |
| `trailing_skip_commas` | 19.8 | 1.0 | 1.7 |
| `raiseerror_in_catch` | 14.8 | 4.2 | 1.2 |
| `nil_method_call` | 4.1 | 0.0 | 0.0 |
| `invalid_sql_param` (`--strict`) | 257.8 | 57.7 | 2,243.3 |
| `unused_variable` (`--strict`) | 1,161.4 | 2,370.4 | 962.3 |
| `undeclared_variable` (`--strict`) | 697.7 | 382.0 | 147.7 |

Error-severity findings on unmodified base-product code, 22 in 20 files:
`global_assignment` 8, `unclosed_block` 3, `unmatched_block_end` 2,
`direct_procedure_call` 2, `bare_logical_operator` 2,
`comment_text_after_terminator` 2, `comment_termination` 1,
`datasource_sql_semicolon` 1, `unclosed_delimiter` 1.

Reading the split:

- `mixed_type_operator` and `invalid_sql_param` concentrate in
  deployment-written code (17× and 9× the base-product rate). That is the
  profile of a house idiom, not of a rule misfiring on product code.
- The codes that fire *more* on base-product code than on deployment code
  — `zero_based_array_index` (10×), `me_outside_class` (19×),
  `trailing_skip_commas` (12×), `raiseerror_in_catch` (12×),
  `direct_procedure_call` (3×), `nil_method_call` (base only), and
  `undeclared_variable` under `--strict` (5×) — are the false-positive
  candidates to triage first, together with the 22 base-product errors.
  Some will be real (shipped code is not necessarily correct, and the
  export includes product code no longer called), but each is a lead.

The classification is reliable at the class level, not file by file: a
base-product object that was changed locally without a checkout note is
counted as unmodified.

The STARLIMS 12 base-layer captures are not part of this run; they extend
the corpus for the version-target work in #261.
