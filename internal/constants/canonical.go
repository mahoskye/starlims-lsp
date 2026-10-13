package constants

import (
	"strings"
)

// Function and class inventory are sourced from the published SSL element
// reference (see the generated_*.go files). The published reference defines
// 330 user-facing functions and 29 user-facing classes.
//
// Function signatures — parameter names, types, required-ness, defaults,
// descriptions, return type — are generated from the same reference
// (GeneratedFunctionSignatures). They used to be hand-curated here, and
// drifted from the reference until hover described a parameter under one
// name while the exceptions beneath it used another (issue #252). The only
// hand-written signature content left is the small, documented set of
// overrides in function_overrides.go.

// SSLFunctionNames contains the canonical developer-facing SSL function
// inventory, sourced from the published ssl-element-reference.json
// (330 functions).
var SSLFunctionNames = GeneratedFunctionNames

// SSLClassNames contains the canonical developer-facing SSL class inventory,
// sourced from the published ssl-element-reference.json (29 classes).
var SSLClassNames = GeneratedClassNames

var sslFunctionLookup = buildLowercaseSet(SSLFunctionNames)
var sslClassLookup = buildLowercaseSet(SSLClassNames)

// SSLFunctionSignatures contains the canonical function signature inventory:
// the generated reference signatures with functionOverrides applied.
var SSLFunctionSignatures = buildCanonicalFunctionSignatures()

func buildLowercaseSet(values []string) map[string]struct{} {
	set := make(map[string]struct{}, len(values))
	for _, value := range values {
		set[strings.ToLower(value)] = struct{}{}
	}
	return set
}

func buildCanonicalFunctionSignatures() map[string]FunctionSignature {
	signatures := make(map[string]FunctionSignature, len(SSLFunctionNames))
	for _, name := range SSLFunctionNames {
		lower := strings.ToLower(name)
		sig, ok := GeneratedFunctionSignatures[lower]
		if !ok {
			sig = FunctionSignature{Name: name}
		}
		if o, ok := functionOverrides[lower]; ok {
			sig = o.apply(sig)
		}
		sig.Name = name
		signatures[lower] = sig
	}
	return signatures
}

// GetFunctionSignature returns the signature for a function name (case-insensitive).
func GetFunctionSignature(name string) (FunctionSignature, bool) {
	sig, ok := SSLFunctionSignatures[strings.ToLower(name)]
	return sig, ok
}

// CanonicalFunctionNames returns a lowercase->canonical-PascalCase map for
// the entire built-in function inventory. Useful for casing rewriters and
// other tooling that needs to round-trip user-supplied identifiers back to
// their published form.
func CanonicalFunctionNames() map[string]string {
	out := make(map[string]string, len(SSLFunctionNames))
	for _, name := range SSLFunctionNames {
		out[strings.ToLower(name)] = name
	}
	return out
}
