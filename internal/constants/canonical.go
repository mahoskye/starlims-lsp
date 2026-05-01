package constants

import (
	"strings"
)

// Function and class inventory are sourced from the published SSL element
// reference (see GeneratedFunctionNames and GeneratedClassNames in
// generated_*.go). The published reference defines 330 user-facing functions
// and 29 user-facing classes, naturally excluding the legacy and internal
// helpers the LSP previously had to filter by hand.
//
// Curated function signatures with rich parameter descriptions still live in
// signatures.go and are overlaid by buildCanonicalFunctionSignatures below.
// Curated descriptions for the 6 supplemental functions (_AND, _NOT, _OR,
// _XOR, Branch, Eval) live in supplementalFunctionSignatures further down.

// SSLFunctionNames contains the canonical developer-facing SSL function
// inventory, sourced from the published ssl-element-reference.json
// (330 functions). Per-function curated signatures still live in
// signatures.go and are overlaid below.
var SSLFunctionNames = GeneratedFunctionNames

// SSLClassNames contains the canonical developer-facing SSL class inventory,
// sourced from the published ssl-element-reference.json (29 classes).
var SSLClassNames = GeneratedClassNames

var sslFunctionLookup = buildLowercaseSet(SSLFunctionNames)
var sslClassLookup = buildLowercaseSet(SSLClassNames)

var supplementalFunctionSignatures = map[string]FunctionSignature{
	"_and": {
		Name:        "_AND",
		Description: "Performs a bitwise AND and returns the numeric result.",
		ReturnType:  "numeric",
		Parameters: []FunctionParameter{
			{Name: "operand1", Type: "numeric", Required: true, Description: "First numeric operand."},
			{Name: "operand2", Type: "numeric", Required: true, Description: "Second numeric operand."},
		},
	},
	"_not": {
		Name:        "_NOT",
		Description: "Performs a bitwise NOT and returns the numeric result.",
		ReturnType:  "numeric",
		Parameters: []FunctionParameter{
			{Name: "operand", Type: "numeric", Required: true, Description: "Numeric operand to invert bitwise."},
		},
	},
	"_or": {
		Name:        "_OR",
		Description: "Performs a bitwise OR and returns the numeric result.",
		ReturnType:  "numeric",
		Parameters: []FunctionParameter{
			{Name: "operand1", Type: "numeric", Required: true, Description: "First numeric operand."},
			{Name: "operand2", Type: "numeric", Required: true, Description: "Second numeric operand."},
		},
	},
	"_xor": {
		Name:        "_XOR",
		Description: "Performs a bitwise XOR and returns the numeric result.",
		ReturnType:  "numeric",
		Parameters: []FunctionParameter{
			{Name: "operand1", Type: "numeric", Required: true, Description: "First numeric operand."},
			{Name: "operand2", Type: "numeric", Required: true, Description: "Second numeric operand."},
		},
	},
	"branch": {
		Name:        "Branch",
		Description: "Transfers control to a label target defined with :LABEL.",
		ReturnType:  "variant",
		Parameters: []FunctionParameter{
			{Name: "target", Type: "variant", Required: true, Description: "Label token text, such as \"LABEL SKIP\"."},
		},
	},
	"eval": {
		Name:        "Eval",
		Description: "Executes a code block or callable value with optional arguments and returns its result. Accepts a variadic number of arguments (xArg1, xArg2, ...).",
		ReturnType:  "variant",
		Parameters: []FunctionParameter{
			{Name: "code", Type: "variant", Required: true, Description: "Code block or callable value to execute."},
			{Name: "arg1", Type: "variant", Required: false, Description: "Optional first argument."},
			{Name: "arg2", Type: "variant", Required: false, Description: "Optional second argument."},
			{Name: "arg3", Type: "variant", Required: false, Description: "Optional third argument."},
			{Name: "arg4", Type: "variant", Required: false, Description: "Optional fourth argument. Additional arguments beyond arg4 are also accepted (variadic)."},
		},
	},
}

// SSLFunctionSignatures contains the canonical function signature inventory.
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

		switch {
		case supplementalFunctionSignatures[lower].Name != "":
			sig := supplementalFunctionSignatures[lower]
			sig.Name = name
			signatures[lower] = sig
		case legacySSLFunctionSignatures[lower].Name != "":
			sig := legacySSLFunctionSignatures[lower]
			sig.Name = name
			signatures[lower] = sig
		default:
			signatures[lower] = FunctionSignature{
				Name:        name,
				Description: "Built-in SSL function documented in the dev/ssl-style-guide inventory.",
				ReturnType:  "variant",
			}
		}
	}

	return signatures
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
