package constants

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// The vendored guide must yield the base-installation names the reference
// documents, and must not leak the non-base names or the endpoint
// ambients into the declared set (issue #270).
func TestPredefinedGlobalsParsed(t *testing.T) {
	for _, want := range []string{
		"Session", "PublicConsts", "MYUSERNAME", "UserLang", "STARLIMSDEPT",
		"STARLIMSSITECODE", "METHODDEVELOPER", "CRLF", "sCC", "sSC", "sTK",
		"sPK", "DQ", "PLATFORMA", "StationName", "TRANSACTION_ID",
		"glb_In_Batch", "CALDATEFORMAT", "GlbDefaultWorkPath",
		"GlbDefaultTempDirectory", "GlbImpExpPath", "cGlobalStationID",
	} {
		if !IsPredefinedGlobal(want) {
			t.Errorf("%s: want a predefined global; parsed %v", want, SSLPredefinedGlobals)
		}
	}
	if !IsPredefinedGlobal("myusername") {
		t.Error("lookup must ignore case")
	}
	for _, not := range []string{"Request", "Response", "CR", "LF", "TAB", "sTab", "sDQ", "sSQ", "MYUSERID", "MYLANGID", "MYDEPT", "MYSITE", "NIL", "Done"} {
		if IsPredefinedGlobal(not) {
			t.Errorf("%s: must not be a declared predefined global", not)
		}
	}
	for name, use := range map[string]string{"CR": "Chr(13)", "LF": "Chr(10)", "TAB": "Chr(9)", "sTab": "Chr(9)", "sDQ": "Chr(34)", "sSQ": "sTK", "MYUSERID": "MYUSERNAME", "MYLANGID": "UserLang", "MYDEPT": "STARLIMSDEPT", "MYSITE": "STARLIMSSITECODE"} {
		got, ok := SSLNonBaseGlobals[strings.ToUpper(name)]
		if !ok || !strings.HasPrefix(got, use) {
			t.Errorf("%s: non-base replacement = %q (present %v), want it to mention %q", name, got, ok, use)
		}
	}
}

// TestPredefinedGlobalsMatchesReference is a dev-machine drift guard like
// TestVendoredDataMatchesSibling: the vendored guide must byte-equal the
// reference checkout's copy when one is present (refresh is a plain cp).
func TestPredefinedGlobalsMatchesReference(t *testing.T) {
	root := repoRoot(t)
	refDir := os.Getenv("SSL_REFERENCE_DIR")
	if refDir == "" {
		refDir = filepath.Join(root, "..", "ssl-docs")
	}
	canonicalPath := filepath.Join(refDir, "content", "guides", "predefined-globals.md")
	canonical, err := os.ReadFile(canonicalPath)
	if os.IsNotExist(err) {
		t.Skipf("reference checkout not found at %s (set SSL_REFERENCE_DIR to override)", refDir)
	}
	if err != nil {
		t.Fatal(err)
	}
	if string(canonical) != predefinedGlobalsDoc {
		t.Errorf("vendored predefined-globals.md has drifted from the reference.\nRefresh with:\n  cp %s internal/constants/data/predefined-globals.md", canonicalPath)
	}
}
