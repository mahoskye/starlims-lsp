# SSL element reference data

`ssl-element-reference.json` and `ssl-element-meta.json` are vendored
snapshots of the canonical SSL element inventory and its per-element
metadata, generated from
[`starlims-ssl-reference`](https://github.com/mahoskye/starlims-ssl-reference)
(checked out locally as `ssl-docs`):

| File | Produced by | Published at |
|------|-------------|--------------|
| `ssl-element-reference.json` | [`ssl-style-guide`](https://github.com/mahoskye/ssl-style-guide)'s `tools/generate_element_reference.py`, run over the reference's `content/reference` pages | `ssl-style-guide/ssl-style-guide/ssl-element-reference.json` |
| `ssl-element-meta.json` | the reference repo's `tools/extract_reference_meta.py` | `ssl-docs/content/data/ssl-element-meta.json` (ssl-style-guide re-vendors a copy) |

`provenance.json` records the upstream commits the two files came from.
`starlims-lsp --version` prints it, and `go test -v ./internal/constants/`
logs it with the snapshot's age.

## Refresh procedure

Copy **both** files — they version together, and refreshing only one
desynchronizes hover metadata from the inventory (the drift test will
object). First check whether ssl-style-guide's copies are current with
the reference:

```bash
# from the ssl-style-guide checkout
bun tools/check-reference-drift.mjs
```

If it reports no content drift, copy from ssl-style-guide:

```bash
# from this repo's root, with the sibling checkouts alongside
cp ../ssl-style-guide/ssl-style-guide/ssl-element-reference.json \
   ../ssl-style-guide/ssl-style-guide/ssl-element-meta.json \
   internal/constants/data/
```

If ssl-style-guide lags the reference, generate the reference JSON from
the current reference checkout instead, and take the meta file from the
reference repo directly (do not commit anything to ssl-style-guide from
here):

```bash
python3 ../ssl-style-guide/tools/generate_element_reference.py \
  --ssl-docs ../ssl-docs --out internal/constants/data/ssl-element-reference.json
cp ../ssl-docs/content/data/ssl-element-meta.json internal/constants/data/
```

Then, in every case:

```bash
# record the upstream commits: reference = ssl-docs HEAD,
# generator = ssl-style-guide HEAD (full hash and committer date)
git -C ../ssl-docs log -1 --format='%H %cs'
git -C ../ssl-style-guide log -1 --format='%H %cs'
$EDITOR internal/constants/data/provenance.json

go generate ./internal/constants/...
go test ./...
git diff
```

Review the generated `internal/constants/generated_*.go` diff, triage
every signature, parameter, and exception change in the PR body, and
commit the JSON refreshes, `provenance.json`, and the generated changes
together.

## Drift guards

`internal/constants/drift_test.go` compares each vendored file
byte-for-byte against the copy its producer publishes, when that sibling
checkout is present: the reference JSON against `../ssl-style-guide`
(override with `SSL_STYLE_GUIDE_DIR`, pointing at the inner
`ssl-style-guide/` directory) and the meta JSON against `../ssl-docs`
(override with `SSL_DOCS_DIR`, pointing at `content/data/`). It always
cross-checks each file's internal totals and the generated
`InventoryTotals`, and validates `provenance.json`. CI additionally
verifies `go generate` is a no-op on every push. The generator refuses
to run if the JSON contains a totals key it doesn't handle, or a type
members row in a shape it doesn't recognize, so upstream format changes
fail loudly instead of being dropped.

## Why vendored?

The LSP must build offline. Vendoring the JSON makes the build
hermetic — `go generate` reads only from this file, never from the
sibling repo. Refresh is a deliberate human step.
