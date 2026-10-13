package constants

import (
	_ "embed"
	"encoding/json"
	"fmt"
)

// Provenance of the vendored element data (issue #254). The two JSON files
// in data/ are snapshots of upstream repos that keep moving; recording the
// exact upstream commit they came from makes staleness visible — in
// `starlims-lsp --version` and in the drift test's log — instead of
// surfacing only as wrong hover text. data/provenance.json is updated by
// hand as part of every refresh (see data/README.md).

//go:embed data/provenance.json
var provenanceBytes []byte

// UpstreamCommit pins one upstream repository at the commit the vendored
// data was produced from.
type UpstreamCommit struct {
	Repo   string `json:"repo"`
	Commit string `json:"commit"`
	Date   string `json:"date"` // committer date of Commit, YYYY-MM-DD
}

// ShortCommit returns the first seven characters of the commit hash.
func (u UpstreamCommit) ShortCommit() string {
	if len(u.Commit) > 7 {
		return u.Commit[:7]
	}
	return u.Commit
}

// String renders "repo@shortsha (date)".
func (u UpstreamCommit) String() string {
	return fmt.Sprintf("%s@%s (%s)", u.Repo, u.ShortCommit(), u.Date)
}

// ElementDataProvenance records where data/ssl-element-reference.json and
// data/ssl-element-meta.json came from. Reference is the canonical SSL
// reference repo (its content/data/ssl-element-meta.json is the meta file,
// and its content/reference pages are the generator's input); Generator is
// the ssl-style-guide commit whose tools/generate_element_reference.py
// produced the reference JSON.
type ElementDataProvenance struct {
	Reference UpstreamCommit `json:"reference"`
	Generator UpstreamCommit `json:"generator"`
}

// DataProvenance is the parsed data/provenance.json. A malformed file
// leaves it zero-valued; TestDataProvenance guards against that.
var DataProvenance = loadProvenance()

func loadProvenance() ElementDataProvenance {
	var p ElementDataProvenance
	_ = json.Unmarshal(provenanceBytes, &p)
	return p
}
