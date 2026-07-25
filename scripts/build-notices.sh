#!/usr/bin/env bash
# Assemble THIRD-PARTY-NOTICES.md from a `go-licenses save` output tree.
# Usage: scripts/build-notices.sh <licenses-dir> <output-file>
set -euo pipefail

SRC="${1:?usage: build-notices.sh <licenses-dir> <output-file>}"
OUT="${2:?usage: build-notices.sh <licenses-dir> <output-file>}"

{
  echo "# Third-party notices"
  echo
  echo "The \`starlims-lsp\` binaries statically link the following third-party Go"
  echo "modules. Their license texts are reproduced below, as required by their"
  echo "respective licenses."
  echo
  find "$SRC" -iname 'LICENSE*' -type f | sort | while read -r f; do
    mod="$(dirname "${f#"$SRC"/}")"
    [ "$mod" = "starlims-lsp" ] && continue
    echo "## $mod"
    echo
    echo '```'
    cat "$f"
    echo
    echo '```'
    echo
  done
} > "$OUT"

echo "Wrote $OUT"
