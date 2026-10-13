// Package constants — predefined-globals loader.
//
// The names a base STARLIMS installation declares for every server script
// come from the reference guide "Predefined Globals"
// (starlims-ssl-reference content/guides/predefined-globals.md), vendored
// verbatim at internal/constants/data/predefined-globals.md. The tables
// are parsed at init instead of hand-copied, so a refresh is a file copy
// (issue #270).
package constants

import (
	_ "embed"
	"regexp"
	"strings"
)

//go:embed data/predefined-globals.md
var predefinedGlobalsDoc string

// SSLPredefinedGlobals contains the globals a base STARLIMS installation
// declares for every server script: the platform objects (except the
// endpoint ambients Request and Response, which DECISIONS.md D4 scopes to
// endpoint scripts) and the publics the application's server-start scripts
// create. Names are recognized case-insensitively as pre-declared. Status
// names are installation data and live in SSLStatusKeywords.
var SSLPredefinedGlobals []string

// SSLWriteCheckedGlobals are the predefined globals diag.global_assignment
// always checks. Nothing makes a predefined global read-only, and the
// application's own login and session-start scripts assign the user and
// site globals (STARLIMSDEPT, STARLIMSSITECODE, ...), so widening the
// always-on error to the whole list would flag code that ships (issue
// #270). The set stays what it was before the list was vendored.
var SSLWriteCheckedGlobals = []string{"MYUSERNAME"}

// SSLNonBaseGlobals maps names that look like globals but a base
// installation does not declare (reading one raises "Variable [<name>] is
// undefined!") to the replacement the reference recommends. Keys are
// upper-case.
var SSLNonBaseGlobals map[string]string

// SSLEndpointAmbients are the platform objects that are meaningful only in
// an endpoint script (DECISIONS.md D4).
var SSLEndpointAmbients = []string{"Request", "Response"}

var backtickName = regexp.MustCompile("`([^`]+)`")

func init() {
	SSLPredefinedGlobals, SSLNonBaseGlobals = parsePredefinedGlobals(predefinedGlobalsDoc)
}

// parsePredefinedGlobals reads the guide's tables. Declared names are the
// backticked names in the first column of every table under the "Provided
// by the platform", "Declared by the application" and "In batch jobs"
// sections; non-base names come from the "Names that are not predefined"
// table, whose second column is the replacement.
func parsePredefinedGlobals(doc string) ([]string, map[string]string) {
	var declared []string
	seen := map[string]bool{}
	nonBase := map[string]string{}
	ambient := map[string]bool{}
	for _, a := range SSLEndpointAmbients {
		ambient[strings.ToUpper(a)] = true
	}

	section := ""
	for _, line := range strings.Split(doc, "\n") {
		if strings.HasPrefix(line, "## ") {
			section = strings.TrimSpace(strings.TrimPrefix(line, "## "))
			continue
		}
		if !strings.HasPrefix(line, "|") || strings.HasPrefix(line, "|-") || strings.HasPrefix(line, "| Name") {
			continue
		}
		cells := strings.Split(strings.Trim(line, "|"), "|")
		names := cellNames(cells[0])
		switch section {
		case "Provided by the platform", "Declared by the application", "In batch jobs":
			for _, n := range names {
				up := strings.ToUpper(n)
				if seen[up] || ambient[up] {
					continue
				}
				seen[up] = true
				declared = append(declared, n)
			}
		case "Names that are not predefined":
			use := ""
			if len(cells) > 1 {
				use = strings.ReplaceAll(strings.TrimSpace(cells[1]), "`", "")
			}
			// "`CR`, `LF` | `Chr(13)`, `Chr(10)`" pairs by position; a
			// single replacement ("`TAB`, `sTab` | `Chr(9)`") or a phrase
			// ("`sTK`, or `Chr(39)`") applies to every name in the row.
			uses := strings.Split(use, ",")
			for i, n := range names {
				if len(names) > 1 && len(uses) == len(names) {
					nonBase[strings.ToUpper(n)] = strings.TrimSpace(uses[i])
				} else {
					nonBase[strings.ToUpper(n)] = use
				}
			}
		}
	}
	return declared, nonBase
}

// cellNames returns the identifier names in a table cell. A cell can name
// several globals (`CR`, `LF`) and its names may be links
// ([`Request`](...)); only backticked text that is a bare identifier
// counts.
func cellNames(cell string) []string {
	var out []string
	for _, m := range backtickName.FindAllStringSubmatch(cell, -1) {
		name := m[1]
		if isIdentifier(name) {
			out = append(out, name)
		}
	}
	return out
}

func isIdentifier(s string) bool {
	if s == "" {
		return false
	}
	for i, r := range s {
		switch {
		case r == '_', r >= 'A' && r <= 'Z', r >= 'a' && r <= 'z':
		case i > 0 && r >= '0' && r <= '9':
		default:
			return false
		}
	}
	return true
}

// IsPredefinedGlobal reports whether name (any case) is a base-installation
// predefined global.
func IsPredefinedGlobal(name string) bool {
	up := strings.ToUpper(name)
	for _, g := range SSLPredefinedGlobals {
		if strings.ToUpper(g) == up {
			return true
		}
	}
	return false
}
