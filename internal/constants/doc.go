// Package constants holds the SSL language inventory consumed by the LSP
// providers. Element content — function signatures, keyword, operator,
// literal, type, and class descriptions — is generated from the vendored
// ssl-element-reference.json snapshot (issue #252); the hand-written
// remainder is language structure (keyword sets, block keywords, operator
// tokens) and the documented function overrides in function_overrides.go.
//
// Generated files are named generated_*.go and start with the standard
// "Code generated" marker. Regenerate them after refreshing the JSON at
// data/ssl-element-reference.json (see data/README.md):
//
//	go generate ./internal/constants/...
package constants

//go:generate go run ../../cmd/gen-inventory -in data/ssl-element-reference.json -out .
