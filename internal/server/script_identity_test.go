package server

import "testing"

// Identity derivation: canonical anchors, application form, flat fallback.
// [spec feature.cross_file_resolution/A1]
// [spec feature.cross_file_resolution/A2]
// [spec feature.cross_file_resolution/A3]
func TestDeriveScriptIdentity(t *testing.T) {
	cases := []struct {
		name     string
		path     string
		category string
		script   string
		anchored bool
	}{
		{"canonical global", "/export/Server Scripts/LIMS_UTILS/HELPERS.srvscr", "LIMS_UTILS", "HELPERS", true},
		{"canonical mixed case anchor", "/export/server scripts/AA/helpers.srvscr", "AA", "helpers", true},
		{"application form", "/x/Applications/MYAPP/MYMODULE/Server Scripts/TASKS.srvscr", "MYMODULE", "TASKS", true},
		{"data sources anchor", "/export/Data Sources/QUERIES/ORDERS.ds", "QUERIES", "ORDERS", true},
		{"windows drive path", "/C:/export/Server Scripts/CAT/SCRIPT.srvscr", "CAT", "SCRIPT", true},
		{"flat ssl", "/repo/lib/Helpers.ssl", "", "Helpers", false},
		{"flat longest extension", "/repo/lib/Helpers.ssl.txt", "", "Helpers", false},
		{"flat ds.txt", "/repo/Orders.ds.txt", "", "Orders", false},
		{"anchor dir without category", "/export/Server Scripts/LOOSE.srvscr", "", "LOOSE", false},
		{"unknown extension kept", "/repo/notes.md", "", "notes.md", false},
	}

	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			category, script, anchored := deriveScriptIdentity(tc.path)
			if category != tc.category || script != tc.script || anchored != tc.anchored {
				t.Errorf("deriveScriptIdentity(%q) = (%q, %q, %v), want (%q, %q, %v)",
					tc.path, category, script, anchored, tc.category, tc.script, tc.anchored)
			}
		})
	}
}
