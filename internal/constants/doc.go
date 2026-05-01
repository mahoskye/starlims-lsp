// Package constants holds the SSL language inventory consumed by the LSP
// providers. The inventory is partly hand-curated (rich function signatures
// in signatures.go, keyword/operator/literal descriptions in constants.go)
// and partly generated from the canonical ssl-element-reference.json
// snapshot.
//
// Generated files are named generated_*.go and start with the standard
// "Code generated" marker. Regenerate them after refreshing the JSON at
// data/ssl-element-reference.json (see data/README.md):
//
//	go generate ./internal/constants/...
package constants

//go:generate go run ../../cmd/gen-inventory -in data/ssl-element-reference.json -out .
