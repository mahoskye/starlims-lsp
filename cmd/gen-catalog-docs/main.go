// Command gen-catalog-docs regenerates the catalog-derived documentation:
// docs/reference/DIAGNOSTICS.md (full rule table) and docs/STATUS.md
// (feature/formatter dashboard). Invoked via `go generate ./internal/catalog`
// or `make generate-docs`. Output starts with a DO NOT EDIT marker; the
// conformance test in internal/catalog verifies the files are current.
package main

import (
	"flag"
	"fmt"
	"os"
	"path/filepath"

	"starlims-lsp/internal/catalog"
)

func main() {
	root := flag.String("root", ".", "repository root (go:generate runs in the package dir)")
	flag.Parse()

	entries, err := catalog.Load(filepath.Join(*root, "catalog"))
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
	if len(entries) == 0 {
		fmt.Fprintf(os.Stderr, "no catalog entries under %s/catalog — wrong -root?\n", *root)
		os.Exit(1)
	}

	for relPath, content := range catalog.GeneratedDocs(entries) {
		path := filepath.Join(*root, relPath)
		if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
			fmt.Fprintln(os.Stderr, err)
			os.Exit(1)
		}
		if err := os.WriteFile(path, []byte(content), 0o644); err != nil {
			fmt.Fprintln(os.Stderr, err)
			os.Exit(1)
		}
		fmt.Println("wrote", path)
	}
}
