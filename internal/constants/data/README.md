# SSL element reference data

`ssl-element-reference.json` is a vendored snapshot of the canonical SSL
element inventory generated from
[`ssl-docs`](https://github.com/mahoskye/ssl-docs) by the
[`ssl-style-guide`](https://github.com/mahoskye/ssl-style-guide) repo.

Source path in the style-guide repo:
`ssl-style-guide/ssl-element-reference.json`.

## Refresh procedure

When `ssl-style-guide` ships an inventory update:

```bash
# from this repo's root, with the ssl-style-guide repo checked out alongside
cp ../ssl-style-guide/ssl-style-guide/ssl-element-reference.json \
   internal/constants/data/ssl-element-reference.json

go generate ./internal/constants/...
go test ./...
git diff
```

Review the generated `internal/constants/generated_*.go` diff and commit
both the JSON refresh and the generated changes together.

## Why vendored?

The LSP must build offline. Vendoring the JSON makes the build
hermetic — `go generate` reads only from this file, never from the
sibling repo. Refresh is a deliberate human step that lines up with
ssl-style-guide releases.
