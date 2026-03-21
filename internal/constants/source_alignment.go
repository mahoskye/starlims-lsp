package constants

import (
	"slices"
	"strings"
)

var excludedLegacySSLFunctionNames = map[string]struct{}{
	"addtoapplication":          {},
	"break":                     {},
	"callbuiltinfunction":       {},
	"executedatasource":         {},
	"getallclientscripts":       {},
	"getclientscriptreferences": {},
	"getformreferences":         {},
	"limscleanup":               {},
	"mergeglobalresources":      {},
	"mergehtmlform":             {},
	"mergexfd":                  {},
	"prepareform":               {},
	"prepareformclientscript":   {},
	"processxfdformforimport":   {},
	"resetapplication":          {},
	"setampm":                   {},
	"syncdesignresources":       {},
	"syncprogramaticresources":  {},
	"tryconnect":                {},
}

var sourceOnlySSLFunctionNames = []string{
	"_AND",
	"_NOT",
	"_OR",
	"_XOR",
	"Branch",
	"Eval",
}

var excludedLegacySSLClassNames = map[string]struct{}{
	"cdatacolumn":          {},
	"cdatacolumns":         {},
	"cdatafield":           {},
	"cdatarow":             {},
	"cdatatable":           {},
	"enterpriseimpexbase":  {},
	"sqlconnection":        {},
	"sslcompilererror":     {},
	"sslcompilererrorlist": {},
}

var sourceOnlySSLClassNames = []string{}

var sourcePreferredSSLFunctionNames = map[string]string{
	"aadd":              "AAdd",
	"aeval":             "AEval",
	"aevala":            "AEvalA",
	"afill":             "AFill",
	"alen":              "ALen",
	"arraycalc":         "ArrayCalc",
	"arraynew":          "ArrayNew",
	"ascan":             "AScan",
	"ascanexact":        "AScanExact",
	"buildarray":        "BuildArray",
	"buildarray2":       "BuildArray2",
	"buildstring":       "BuildString",
	"buildstring2":      "BuildString2",
	"comparray":         "CompArray",
	"delarray":          "DelArray",
	"deleteinlinecode":  "DeleteInlineCode",
	"endlimsoleconnect": "EndLimsOleConnect",
	"extractcol":        "ExtractCol",
	"getinlinecode":     "GetInlineCode",
	"getregion":         "GetRegion",
	"getregionex":       "GetRegionEx",
	"ldir":              "LDir",
	"limsoleconnect":    "LimsOleConnect",
	"lwait":             "LWait",
	"usrmes":            "UsrMes",
}

// SSLFunctionNames contains the source-aligned developer-facing SSL function inventory.
var SSLFunctionNames = buildSourceAlignedNames(legacySSLFunctionNames, excludedLegacySSLFunctionNames, sourceOnlySSLFunctionNames, sourcePreferredSSLFunctionNames)

// SSLClassNames contains the source-aligned developer-facing SSL class inventory.
var SSLClassNames = buildSourceAlignedNames(legacySSLClassNames, excludedLegacySSLClassNames, sourceOnlySSLClassNames, nil)

var sslFunctionNameSet = buildFoldSet(SSLFunctionNames)
var sslClassNameSet = buildFoldSet(SSLClassNames)

var sourceOnlyFunctionSignatures = map[string]FunctionSignature{
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
		Description: "Executes a code block or callable value with optional arguments and returns its result.",
		ReturnType:  "variant",
		Parameters: []FunctionParameter{
			{Name: "code", Type: "variant", Required: true, Description: "Code block or callable value to execute."},
			{Name: "arg1", Type: "variant", Required: false, Description: "Optional first argument."},
			{Name: "arg2", Type: "variant", Required: false, Description: "Optional second argument."},
			{Name: "arg3", Type: "variant", Required: false, Description: "Optional third argument."},
			{Name: "arg4", Type: "variant", Required: false, Description: "Optional fourth argument."},
		},
	},
}

// SSLFunctionSignatures contains the source-aligned function signature inventory.
var SSLFunctionSignatures = buildSourceAlignedFunctionSignatures()

func buildSourceAlignedNames(legacy []string, excluded map[string]struct{}, sourceOnly []string, preferred map[string]string) []string {
	seen := make(map[string]string, len(legacy)+len(sourceOnly))

	for _, name := range legacy {
		lower := strings.ToLower(name)
		if _, skip := excluded[lower]; skip {
			continue
		}
		if _, ok := seen[lower]; !ok {
			seen[lower] = preferredName(name, preferred)
		}
	}

	for _, name := range sourceOnly {
		lower := strings.ToLower(name)
		if _, ok := seen[lower]; !ok {
			seen[lower] = preferredName(name, preferred)
		}
	}

	names := make([]string, 0, len(seen))
	for _, name := range seen {
		names = append(names, name)
	}

	slices.SortStableFunc(names, func(a, b string) int {
		return strings.Compare(strings.ToLower(a), strings.ToLower(b))
	})

	return names
}

func preferredName(name string, preferred map[string]string) string {
	if preferred == nil {
		return name
	}

	if canonical, ok := preferred[strings.ToLower(name)]; ok {
		return canonical
	}

	return name
}

func buildFoldSet(values []string) map[string]struct{} {
	set := make(map[string]struct{}, len(values))
	for _, value := range values {
		set[strings.ToLower(value)] = struct{}{}
	}
	return set
}

func buildSourceAlignedFunctionSignatures() map[string]FunctionSignature {
	signatures := make(map[string]FunctionSignature, len(SSLFunctionNames))

	for _, name := range SSLFunctionNames {
		lower := strings.ToLower(name)

		switch {
		case sourceOnlyFunctionSignatures[lower].Name != "":
			sig := sourceOnlyFunctionSignatures[lower]
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
