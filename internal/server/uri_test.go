package server

import "testing"

// The URI helpers take the OS as a parameter, so the Windows cases run on
// any host (issue #258: the index found nothing on Windows because
// uriToPath kept the slash before the drive letter and pathToURI dropped
// the one before it).

// [spec feature.cross_file_resolution/A21]
func TestURIToPathFor(t *testing.T) {
	tests := []struct {
		name, goos, uri, want string
	}{
		{"linux plain", "linux", "file:///home/user/a.ssl", "/home/user/a.ssl"},
		{"linux escaped space", "linux", "file:///work/Server%20Scripts/CAT/S.srvscr", "/work/Server Scripts/CAT/S.srvscr"},
		{"linux unescaped space", "linux", "file:///work/Server Scripts/CAT/S.srvscr", "/work/Server Scripts/CAT/S.srvscr"},
		{"linux escaped hash and non-ASCII", "linux", "file:///work/a%23b/%C3%A9t%C3%A9.ssl", "/work/a#b/été.ssl"},
		{"linux localhost authority", "linux", "file://localhost/work/a.ssl", "/work/a.ssl"},
		{"plain path is returned as-is", "linux", "/some/path", "/some/path"},
		{"other scheme is returned as-is", "linux", "untitled:Untitled-1", "untitled:Untitled-1"},

		{"windows VS Code form", "windows", "file:///c%3A/Users/dev/Server%20Scripts/CAT/S.srvscr", `c:\Users\dev\Server Scripts\CAT\S.srvscr`},
		{"windows literal colon", "windows", "file:///C:/Users/dev/a.ssl", `C:\Users\dev\a.ssl`},
		{"windows UNC", "windows", "file://server/share/dir/a.ssl", `\\server\share\dir\a.ssl`},
		{"windows localhost authority", "windows", "file://localhost/c%3A/a.ssl", `c:\a.ssl`},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := uriToPathFor(tt.uri, tt.goos); got != tt.want {
				t.Errorf("uriToPathFor(%q, %s) = %q, want %q", tt.uri, tt.goos, got, tt.want)
			}
		})
	}
}

// [spec feature.cross_file_resolution/A21]
func TestPathToURIFor(t *testing.T) {
	tests := []struct {
		name, goos, path, want string
	}{
		{"linux plain", "linux", "/home/user/a.ssl", "file:///home/user/a.ssl"},
		{"linux space", "linux", "/work/Server Scripts/CAT/S.srvscr", "file:///work/Server%20Scripts/CAT/S.srvscr"},
		{"linux reserved and non-ASCII", "linux", "/work/a#b/été (1).ssl", "file:///work/a%23b/%C3%A9t%C3%A9%20%281%29.ssl"},
		{"linux unreserved kept", "linux", "/a-b_c.d~e/F1.ssl", "file:///a-b_c.d~e/F1.ssl"},

		{"windows drive", "windows", `C:\Users\dev\Server Scripts\CAT\S.srvscr`, "file:///c%3A/Users/dev/Server%20Scripts/CAT/S.srvscr"},
		{"windows lower drive", "windows", `d:\x.ssl`, "file:///d%3A/x.ssl"},
		{"windows forward slashes", "windows", `C:/Users/dev/a.ssl`, "file:///c%3A/Users/dev/a.ssl"},
		{"windows UNC", "windows", `\\server\share\dir\a.ssl`, "file://server/share/dir/a.ssl"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := pathToURIFor(tt.path, tt.goos); got != tt.want {
				t.Errorf("pathToURIFor(%q, %s) = %q, want %q", tt.path, tt.goos, got, tt.want)
			}
		})
	}
}

// Every spelling of one file canonicalizes to the URI pathToURI builds for
// it, and canonicalization is idempotent.
// [spec feature.cross_file_resolution/A21]
func TestCanonicalURIFor(t *testing.T) {
	tests := []struct {
		name, goos string
		spellings  []string
		want       string
	}{
		{
			"linux space", "linux",
			[]string{
				"file:///work/Server%20Scripts/CAT/S.srvscr",
				"file:///work/Server Scripts/CAT/S.srvscr",
				"file:///work/Server%20Scripts/CAT/%53.srvscr",
				"file://localhost/work/Server%20Scripts/CAT/S.srvscr",
			},
			"file:///work/Server%20Scripts/CAT/S.srvscr",
		},
		{
			"windows drive", "windows",
			[]string{
				"file:///c%3A/Users/dev/Server%20Scripts/S.srvscr",
				"file:///C:/Users/dev/Server Scripts/S.srvscr",
				"file:///C%3a/Users/dev/Server%20Scripts/S.srvscr",
			},
			"file:///c%3A/Users/dev/Server%20Scripts/S.srvscr",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			for _, uri := range tt.spellings {
				got := canonicalURIFor(uri, tt.goos)
				if got != tt.want {
					t.Errorf("canonicalURIFor(%q) = %q, want %q", uri, got, tt.want)
				}
				if again := canonicalURIFor(got, tt.goos); again != got {
					t.Errorf("not idempotent: %q -> %q", got, again)
				}
			}
		})
	}

	// Non-file and malformed URIs pass through untouched.
	for _, uri := range []string{"untitled:Untitled-1", "file:///bad%zzescape.ssl"} {
		if got := canonicalURIFor(uri, "linux"); got != uri {
			t.Errorf("canonicalURIFor(%q) = %q, want it unchanged", uri, got)
		}
	}
}

// Path -> URI -> path is the identity on both OS families.
// [spec feature.cross_file_resolution/A21]
func TestURIPathRoundTrip(t *testing.T) {
	for _, tc := range []struct{ goos, path string }{
		{"linux", "/work/Server Scripts/CAT/S.srvscr"},
		{"linux", "/work/a#b/été (1).ssl"},
		{"windows", `c:\Users\dev\Server Scripts\CAT\S.srvscr`},
		{"windows", `\\server\share\dir\a.ssl`},
	} {
		uri := pathToURIFor(tc.path, tc.goos)
		if got := uriToPathFor(uri, tc.goos); got != tc.path {
			t.Errorf("%s: %q -> %q -> %q", tc.goos, tc.path, uri, got)
		}
	}
}
