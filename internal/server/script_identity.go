package server

import (
	"path/filepath"
	"strings"
)

// sslExtensionsByLength lists the indexable extensions longest-first so that
// stripping "NAME.ssl.txt" yields "NAME", not "NAME.ssl".
var sslExtensionsByLength = []string{".ssl.txt", ".ds.txt", ".srvscr", ".ssl", ".ds"}

// stripSSLExtension removes the longest matching SSL extension from a file
// basename, case-insensitively. Unknown extensions are left untouched.
func stripSSLExtension(base string) string {
	lower := strings.ToLower(base)
	for _, ext := range sslExtensionsByLength {
		if strings.HasSuffix(lower, ext) {
			return base[:len(base)-len(ext)]
		}
	}
	return base
}

// deriveScriptIdentity maps a filesystem path to its STARLIMS script
// identity (spec feature.cross_file_resolution/A1-A3).
//
// Canonical export-tree anchors, matched case-insensitively on path
// components (so Windows-style "/C:/..." paths work unchanged):
//
//	.../Server Scripts/CATEGORY/SCRIPT.srvscr            -> (CATEGORY, SCRIPT, true)
//	.../Applications/APP/MODULE/Server Scripts/SCRIPT.…  -> (MODULE, SCRIPT, true)
//	.../Data Sources/CATEGORY/NAME.ds                    -> (CATEGORY, NAME, true)
//
// Without an anchor the identity degrades to the basename with its SSL
// extension stripped and no category — the flat-layout fallback.
func deriveScriptIdentity(path string) (category, script string, anchored bool) {
	comps := strings.Split(filepath.ToSlash(path), "/")
	// Drop empty components (leading slash, doubled separators).
	clean := comps[:0:0]
	for _, c := range comps {
		if c != "" {
			clean = append(clean, c)
		}
	}
	if len(clean) == 0 {
		return "", "", false
	}

	script = stripSSLExtension(clean[len(clean)-1])

	isAnchor := func(comp string) bool {
		lower := strings.ToLower(comp)
		return lower == "server scripts" || lower == "data sources"
	}

	// Global form: .../<anchor>/CATEGORY/FILE
	if len(clean) >= 3 && isAnchor(clean[len(clean)-3]) {
		return clean[len(clean)-2], script, true
	}

	// Application form: .../Applications/APP/MODULE/<anchor>/FILE
	if len(clean) >= 5 && isAnchor(clean[len(clean)-2]) &&
		strings.EqualFold(clean[len(clean)-5], "Applications") {
		return clean[len(clean)-3], script, true
	}

	return "", script, false
}
