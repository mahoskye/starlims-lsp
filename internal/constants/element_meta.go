// Package constants — element metadata loader.
//
// Provides runtime access to the per-element exception/caveat/best-practice
// metadata extracted from ssl-docs (vendored at
// internal/constants/data/ssl-element-meta.json).
//
// The metadata layers onto whatever name lookup callers do — hover code
// and future diagnostics can call LookupMeta(name) and get back the
// documented exception text. Lookups are case-insensitive, keyed on the
// element's canonical short name (e.g. "ExecFunction", "AAdd",
// "SQLConnection") which matches how SSLFunctionNames and friends are
// already keyed.
package constants

import (
	_ "embed"
	"encoding/json"
	"strings"
	"sync"
)

//go:embed data/ssl-element-meta.json
var elementMetaBytes []byte

// DocumentedException is one row from an ssl-docs `## Exceptions` table.
type DocumentedException struct {
	Trigger string `json:"trigger"`
	Message string `json:"message"`
}

// BestPractices captures the do/don't admonitions from an ssl-docs page.
type BestPractices struct {
	Do   []string `json:"do,omitempty"`
	Dont []string `json:"dont,omitempty"`
}

// ElementMeta is the structured prose metadata for a single SSL element.
// All fields are optional — not every page documents every section.
type ElementMeta struct {
	DocID         string                `json:"doc_id"`
	DocStatus     string                `json:"doc_status"`
	DocSource     string                `json:"doc_source"`
	Exceptions    []DocumentedException `json:"exceptions,omitempty"`
	Caveats       []string              `json:"caveats,omitempty"`
	BestPractices BestPractices         `json:"best_practices,omitempty"`
}

type metaFileEntry struct {
	ID            string                `json:"id"`
	ElementType   string                `json:"element_type"`
	Title         string                `json:"title"`
	DocStatus     string                `json:"doc_status"`
	SourcePath    string                `json:"source_path"`
	Exceptions    []DocumentedException `json:"exceptions"`
	Caveats       []string              `json:"caveats"`
	BestPractices BestPractices         `json:"best_practices"`
}

type metaFile struct {
	Version  string          `json:"version"`
	Source   string          `json:"source"`
	Elements []metaFileEntry `json:"elements"`
}

var (
	metaIndexOnce sync.Once
	metaIndex     map[string]ElementMeta
)

// LookupMeta returns the metadata for an element by canonical short name
// (case-insensitive). The second return value is false when the name is
// unknown or the meta file is empty/malformed.
func LookupMeta(name string) (ElementMeta, bool) {
	loadMetaIndex()
	if metaIndex == nil {
		return ElementMeta{}, false
	}
	m, ok := metaIndex[strings.ToLower(name)]
	return m, ok
}

func loadMetaIndex() {
	metaIndexOnce.Do(func() {
		var raw metaFile
		if err := json.Unmarshal(elementMetaBytes, &raw); err != nil {
			// Don't crash the server on a malformed embed; just leave the
			// index empty so LookupMeta returns false. This is defensive —
			// the file is generated and should always parse.
			return
		}
		metaIndex = make(map[string]ElementMeta, len(raw.Elements))
		for _, e := range raw.Elements {
			short := elementShortName(e.ID, e.Title)
			if short == "" {
				continue
			}
			metaIndex[strings.ToLower(short)] = ElementMeta{
				DocID:         e.ID,
				DocStatus:     e.DocStatus,
				DocSource:     e.SourcePath,
				Exceptions:    e.Exceptions,
				Caveats:       e.Caveats,
				BestPractices: e.BestPractices,
			}
		}
	})
}

// elementShortName extracts the lookup key. The doc ID format is
// `ssl.<type>.<name>` where <name> is lowercased and may contain hyphens
// (operator slugs). For canonical-name lookup callers use the original
// title casing (e.g. "ExecFunction") which we normalize to lowercase.
func elementShortName(id, title string) string {
	if title != "" {
		return title
	}
	if i := strings.LastIndex(id, "."); i >= 0 {
		return id[i+1:]
	}
	return id
}
