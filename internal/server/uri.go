package server

import (
	"net/url"
	"path/filepath"
	"runtime"
	"strings"
)

// File URIs have one canonical spelling inside the server (issue #258).
// Clients escape URIs their own way: VS Code sends `file:///c%3A/Server%20Scripts/x.srvscr`
// where a hand-built URI would read `file:///C:/Server Scripts/x.srvscr`.
// The document store and the workspace index are keyed by URI string, so
// two spellings of one file used to become two entries: rename skipped
// call sites, workspace symbols listed an open file twice, and Delete
// events missed the indexed entry. Every URI a handler receives passes
// through canonicalURI, and every URI the index builds from a path comes
// from pathToURI, so both sides agree.
//
// The canonical form is the one VS Code produces, so the URIs the server
// sends back are byte-identical to the ones that client sent: unreserved
// characters (A-Z a-z 0-9 - . _ ~) and `/` stay literal, every other byte
// is percent-encoded with upper-case hex, and on Windows the drive letter
// is lower case (`file:///c%3A/...`).

// uriToPath converts a file:// URI to a filesystem path for this OS.
// Anything that is not a file URI is returned unchanged.
func uriToPath(uri string) string {
	return uriToPathFor(uri, runtime.GOOS)
}

// pathToURI converts a filesystem path to the canonical file:// URI for
// this OS, making the path absolute first.
func pathToURI(path string) string {
	abs, err := filepath.Abs(path)
	if err != nil {
		abs = path
	}
	return pathToURIFor(abs, runtime.GOOS)
}

// canonicalURI returns the canonical spelling of a file:// URI for this
// OS. Other schemes (untitled:, and so on) are returned unchanged.
func canonicalURI(uri string) string {
	return canonicalURIFor(uri, runtime.GOOS)
}

// isFileURI reports whether uri uses the file scheme (case-insensitive).
func isFileURI(uri string) bool {
	return len(uri) >= 5 && strings.EqualFold(uri[:5], "file:")
}

// uriToPathFor is uriToPath for the named GOOS, kept pure so the Windows
// behavior is testable on any host.
func uriToPathFor(uri, goos string) string {
	if !isFileURI(uri) {
		// Already a path (or a scheme the server cannot read): as-is.
		return uri
	}
	parsed, err := url.Parse(uri)
	if err != nil {
		return strings.TrimPrefix(uri[5:], "//")
	}
	p := parsed.Path
	if goos != "windows" {
		return p
	}
	// "/c:/dir" -> "c:/dir": the leading slash belongs to the URI, not the path.
	if hasDriveLetter(p, 1) && p[0] == '/' {
		p = p[1:]
	}
	p = strings.ReplaceAll(p, "/", `\`)
	if host := parsed.Host; host != "" && !strings.EqualFold(host, "localhost") {
		// UNC path: file://server/share/x -> \\server\share\x.
		p = `\\` + host + p
	}
	return p
}

// pathToURIFor is pathToURI for the named GOOS without making the path
// absolute, kept pure so the Windows behavior is testable on any host.
func pathToURIFor(path, goos string) string {
	p := path
	host := ""
	if goos == "windows" {
		p = strings.ReplaceAll(p, `\`, "/")
		switch {
		case strings.HasPrefix(p, "//"):
			// UNC path: \\server\share\x -> file://server/share/x.
			rest := p[2:]
			if i := strings.IndexByte(rest, '/'); i >= 0 {
				host, p = rest[:i], rest[i:]
			} else {
				host, p = rest, "/"
			}
		case hasDriveLetter(p, 0):
			p = "/" + p
		}
		if hasDriveLetter(p, 1) && p[0] == '/' {
			p = "/" + strings.ToLower(p[1:2]) + p[2:]
		}
	}
	return "file://" + host + escapeURIPath(p)
}

// canonicalURIFor is canonicalURI for the named GOOS.
func canonicalURIFor(uri, goos string) string {
	if !isFileURI(uri) {
		return uri
	}
	if _, err := url.Parse(uri); err != nil {
		// Malformed escapes: leave it alone rather than guess.
		return uri
	}
	p := uriToPathFor(uri, goos)
	if p == "" {
		return uri
	}
	return pathToURIFor(p, goos)
}

// hasDriveLetter reports whether p holds a drive letter ("c:") at index i.
func hasDriveLetter(p string, i int) bool {
	if len(p) < i+2 || p[i+1] != ':' {
		return false
	}
	c := p[i]
	return ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z')
}

// escapeURIPath percent-encodes every byte of a slash-separated path
// except the RFC 3986 unreserved characters and `/`.
func escapeURIPath(p string) string {
	const hex = "0123456789ABCDEF"
	var b strings.Builder
	b.Grow(len(p))
	for i := 0; i < len(p); i++ {
		c := p[i]
		if ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') || ('0' <= c && c <= '9') ||
			c == '-' || c == '.' || c == '_' || c == '~' || c == '/' {
			b.WriteByte(c)
			continue
		}
		b.WriteByte('%')
		b.WriteByte(hex[c>>4])
		b.WriteByte(hex[c&0xF])
	}
	return b.String()
}
