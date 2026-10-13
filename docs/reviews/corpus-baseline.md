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

## 2026-10-13 — base-product false-positive triage (issue #304)

**Binaries:** "before" is the v0.25.0 merge train (`train/v0.25.0` at
`4475e67`), which already carries the severity and D10 changes of the
open PRs; "after" is the issue #304 branch built on it. Same corpus,
commands and provenance classes as the v0.24.0 section above. The rates
in the issue were measured on v0.24.0; the "before" column here is the
train, so some leads had already moved.

Each lead was read on unmodified base-product code, the strongest
false-positive signal: a true positive (TP) is code that really is wrong
or dead, a false positive (FP) is valid code the rule misreads. FPs got
a guard, a catalog "Does not flag" fence and a test; TPs did not.

### Totals

| Profile | Before | After | Base product, before → after |
|---|---:|---:|---:|
| default | 4,905 | 4,898 | 1,264 → 1,257 |
| `--info` | 17,284 | 17,041 | 4,388 → 4,243 |
| `--strict` | 11,505 | 11,266 | 2,729 → 2,509 |
| error findings | 236 | 231 | 39 → 34 |
| files invalid | 55 | 50 | 15 → 10 |

Deployment-written code moves only where a guard applies to it too
(`zero_based_array_index` −41 at `--info`, `undeclared_variable` −13
under `--strict`). Nothing gained findings except two kinds. The 2
`direct_procedure_call` errors became warnings. The code after the
fixed region is now analyzed, and it gets one `--info` finding.

### Error-severity findings on base-product code

| Code | Before | After | Verdict by pattern |
|---|---:|---:|---|
| `unknown_token` | 24 | 24 | TP: unfilled `?` placeholders in two code-generator template scripts; they cannot compile |
| `unclosed_block` | 3 | 2 | TP ×2: a `:PROCEDURE` with no `:ENDPROC` before end of file. FP ×1: a `:REGION` whose closer shares the payload's last line (`}:ENDREGION;`), fixed |
| `unmatched_block_end` | 2 | 2 | TP: one `:ENDIF` too many |
| `direct_procedure_call` | 2 | 0 | FP: a procedure named after an unpublished built-in calls the built-in from its own body; now the warning tier |
| `bare_logical_operator` | 2 | 2 | TP: legacy script using bare `and` |
| `comment_text_after_terminator` | 2 | 0 | FP: a keyword-named local assigned on the line after a two-line comment, fixed |
| `comment_termination` | 1 | 1 | TP: scratch data source ending in an unterminated comment |
| `datasource_sql_semicolon` | 1 | 1 | TP: `*/;` banner in a SQL-mode body, which STARLIMS rejects |
| `unclosed_delimiter` | 1 | 1 | TP: missing `)` in a template script |
| `invalid_limstypeex_comparison` | 1 | 1 | TP: compares the type name in mixed case, never true |

`global_assignment` (8 errors in the issue) is a warning on the train.
2 are TP: an undeclared local named like a status global writes the
global. 6 are intended writes of the user global in the product's own
sign-in and session-start code. They are FP by intent, but no
single-file signal separates sign-in code from a script clobbering the
user, so there is no guard; suppression is the remedy there.

### Codes that fire more on base-product code

Rate per 1,000 files (base product / base, modified / deployment).

| Code | Before | After | Verdict by pattern |
|---|---|---|---|
| `me_outside_class` | 42.8 / 2.1 / 2.2 | unchanged | FP by role: every base hit is the include-library warning tier (issue #171), in class partials compiled into a class through `:INCLUDE`. Single-file analysis cannot see the including class; the fix is a reverse-include lookup, deferred by decision (#125 Phase C). No guard |
| `trailing_skip_commas` (`--info`) | 19.8 / 1.0 / 1.7 | unchanged | TP: a redundant trailing empty argument, mostly one idiom copied across instrument data-capture scripts |
| `raiseerror_in_catch` | 14.8 / 4.2 / 1.2 | unchanged | TP by the style guide's doctrine: catch, wrap and re-raise, which is the doctrine's own "Bad" example |
| `zero_based_array_index` (`--info`) | 114.5 / 66.1 / 11.1 | 0.0 / 6.3 / 1.0 | FP: every base hit indexed the .NET DataSet through `:Tables`; such chains are now silent. Other member receivers keep the advisory |
| `undeclared_variable` (`--strict`) | 270.2 / 108.1 / 75.8 | 94.7 / 101.8 / 72.6 | 213 fewer. 210 were in files with an `:INCLUDE` that `--validate` cannot read: 134 name a variable the included script declares (FP), and the check cannot know about the other 76, so the file is now silent. 3 were class instantiations `Name{}` (FP), fixed. The remaining 115 are mostly publics that server-start scripts declare and caller-scope variables of calculation scripts. That is the opt-in check by design, remedied with `ssl.diagnostics.globals` |
| `direct_procedure_call` | 68.4 / 21.0 / 21.9 | unchanged | Of the 81 warnings: 57 call vendor server-side built-ins that the published reference does not list. That is the issue #167 warning tier by design, and the fix belongs in the reference. 24 are TP: client-side form functions and names with no record, in legacy form-era code under server-script folders. The 2 errors are above |
| `nil_method_call` | 4.1 / 0.0 / 0.0 | 0.8 / 0.0 / 0.0 | FP ×4: a use guarded by `!Empty(v)` in the enclosing or the same `.AND.` condition (3), and a NIL assignment in a branch that ends in `:RETURN` (1), fixed. FP ×1 left: the NIL branch and the use sit under two separate tests of one condition. No guard is precise enough for that |

### Formatter

Output is byte-identical to the train's on 6,227 of the 6,228 files.
The exception is the file with the `}:ENDREGION;` region: the code after
that region used to be swallowed into the region body and left
verbatim, and is now formatted like the rest of the file. Both builds
reformat 5,346 files, and a second pass changes nothing.

On both builds, 15 files (13 deployment-written, 2 base product) gain
`branch_target_label` errors after formatting. The formatter
(fmt.keyword_case) splits a compact `:LABELName;` into `:LABEL Name;`,
but the runtime matches a `Branch()` target against the label text
exactly, so a target written for the compact form no longer reaches its
label. The formatter changes what the code does there. This is already
present on the train, not caused by this branch, and is a follow-up.
