package providers

import (
	"strings"
	"testing"
)

// Region bodies are opaque payload stored for GetRegion() — no diagnostic
// fires on body content, an unclosed region still reports unclosed_block,
// and formatting leaves body lines byte-identical (issue #164).
// [spec feature.diagnostics_pipeline/A23]
func TestGetDiagnostics_RegionBodyIsOpaque(t *testing.T) {
	src := `:PARAMETERS psName;
:REGION Html;
<div onclick="if(a && b[0] != null) frames[0].go();">x.y.z</div>
var x = a == b;
:ENDREGION;
:RETURN GetRegion("Html");
`
	opts := DefaultDiagnosticOptions()
	if diags := GetDiagnostics(src, opts); len(diags) != 0 {
		t.Errorf("expected no diagnostics for region body payload, got %d: %+v", len(diags), diags)
	}
}

// [spec feature.diagnostics_pipeline/A23]
func TestGetDiagnostics_UnclosedRegionStillFlags(t *testing.T) {
	src := ":REGION Html;\n<div>x.y</div>\n"
	opts := DefaultDiagnosticOptions()
	diags := GetDiagnostics(src, opts)

	found := false
	for _, d := range diags {
		if d.Code == CodeUnclosedBlock {
			found = true
		}
	}
	if !found {
		t.Errorf("expected unclosed_block for region with no :ENDREGION, got %+v", diags)
	}
}

// [spec feature.diagnostics_pipeline/A23]
func TestFormatDocument_RegionBodyByteIdentical(t *testing.T) {
	body := []string{
		`<div onclick="if(a&&b[0]) x.go()">`,
		"   oddly   spaced\tpayload",
		"</div>",
	}
	src := ":REGION Html;\n" + strings.Join(body, "\n") + "\n:ENDREGION;\n:RETURN GetRegion(\"Html\");\n"

	edits := FormatDocument(src, DefaultFormattingOptions())
	if len(edits) != 1 {
		t.Fatalf("expected 1 edit, got %d", len(edits))
	}
	for _, line := range body {
		if !strings.Contains(edits[0].NewText, line+"\n") {
			t.Errorf("region body line altered by formatter:\nwant %q\ngot:\n%s", line, edits[0].NewText)
		}
	}
}
