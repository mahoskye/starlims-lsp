# SSL element reference data

`ssl-element-reference.json` and `ssl-element-meta.json` are vendored
snapshots of the canonical SSL element inventory and its per-element
metadata, generated from
[`starlims-ssl-reference`](https://github.com/mahoskye/starlims-ssl-reference)
(checked out locally as `ssl-docs`) by the
[`ssl-style-guide`](https://github.com/mahoskye/ssl-style-guide) repo.

Source paths in the style-guide repo:
`ssl-style-guide/ssl-element-reference.json` and
`ssl-style-guide/ssl-element-meta.json`.

## Refresh procedure

When `ssl-style-guide` ships an inventory update, copy **both** files —
they version together, and refreshing only one desynchronizes hover
metadata from the inventory (the drift test will object):

```bash
# from this repo's root, with the ssl-style-guide repo checked out alongside
cp ../ssl-style-guide/ssl-style-guide/ssl-element-reference.json \
   ../ssl-style-guide/ssl-style-guide/ssl-element-meta.json \
   internal/constants/data/

go generate ./internal/constants/...
go test ./...
git diff
```

Review the generated `internal/constants/generated_*.go` diff and commit
both JSON refreshes and the generated changes together.

## Drift guards

`internal/constants/drift_test.go` compares the vendored files
byte-for-byte against the sibling `ssl-style-guide` checkout when one is
present (override the location with `SSL_STYLE_GUIDE_DIR`), and always
cross-checks each file's internal totals and the generated
`InventoryTotals`. CI additionally verifies `go generate` is a no-op on
every push. The generator itself refuses to run if the JSON contains a
totals key it doesn't handle, so a new upstream category cannot be
dropped silently again.

## Why vendored?

The LSP must build offline. Vendoring the JSON makes the build
hermetic — `go generate` reads only from this file, never from the
sibling repo. Refresh is a deliberate human step that lines up with
ssl-style-guide releases.
